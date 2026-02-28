use crate::engine::{Engine, FILE_HASH_TABLE};
use crate::observer::CycleEvent;
use holo_ast::Module;
use holo_base::Result;
use holo_base::{hash_string, DiagnosticKind, SharedString, SourceDiagnostic, SourceFile};
use holo_lexer::{BasicLexer, Lexer};
use holo_parser::{BasicParser, Parser};
use std::mem::take;

/// Represents the state and logic of a single compilation cycle.
pub struct Cycle<'a> {
    engine: &'a mut Engine,
    diagnostics: Vec<SourceDiagnostic>,
}

pub struct CycleResult {
    pub diagnostics: Vec<SourceDiagnostic>,
}

struct ParseStageResult {
    modules: Vec<Module>,
}

impl<'a> Cycle<'a> {
    /// Creates a new cycle for the given engine.
    pub fn new(engine: &'a mut Engine) -> Self {
        Self {
            engine,
            diagnostics: Vec::new(),
        }
    }

    /// Runs the cycle logic.
    pub fn run(&mut self) -> Result<CycleResult> {
        self.diagnostics.clear();
        let _parse_result = self.stage_1_parse()?;
        // Stages 2-5 will be implemented here.
        Ok(CycleResult {
            diagnostics: take(&mut self.diagnostics),
        })
    }

    /// Stage 1: Tokenize and parse changed files to ASTs.
    fn stage_1_parse(&mut self) -> Result<ParseStageResult> {
        let dirty_files = std::mem::take(&mut self.engine.dirty_files);

        let lexer = BasicLexer;
        let parser = BasicParser;
        let mut result = ParseStageResult {
            modules: Vec::new(),
        };

        let tx = self.engine.database.begin_tx()?;

        for file_path in dirty_files {
            let content = match self.engine.filesystem.read_to_string(&file_path) {
                Ok(content) => content,
                Err(err) => {
                    let error_message = err.to_string();
                    self.engine.observer.on_event(CycleEvent::FileReadError(
                        file_path.clone(),
                        error_message.clone().into(),
                    ));

                    let diagnostic = SourceDiagnostic::new(
                        DiagnosticKind::Io,
                        format!("failed to read file: {}", error_message),
                    );
                    self.diagnostics.push(diagnostic);
                    continue;
                }
            };

            let content_hash = hash_string(&content);
            let key = SharedString::from(file_path.as_str());

            let stored_hash = tx.get(FILE_HASH_TABLE, std::slice::from_ref(&key));
            let should_parse = match stored_hash {
                Ok(results) if results.len() == 1 => {
                    let stored = results[0].as_ref();
                    match stored {
                        Some(bytes) => {
                            let stored_hash = u64::from_le_bytes(
                                bytes.as_slice().try_into().expect("hash should be 8 bytes"),
                            );
                            stored_hash != content_hash
                        }
                        None => true,
                    }
                }
                _ => true,
            };

            if !should_parse {
                self.engine
                    .observer
                    .on_event(CycleEvent::FileParseSkipped(file_path));
                continue;
            }

            let lexed = lexer.lex(&content);

            let source_file = SourceFile::new(&content, file_path.clone());
            let parsed = parser.parse_module(&lexed.tokens, &source_file);

            let mut diagnostics = lexed.diagnostics;
            diagnostics.extend(parsed.diagnostics);
            self.diagnostics.extend(diagnostics.iter().cloned());

            let hash_bytes = content_hash.to_le_bytes();
            if let Err(err) = tx.put(FILE_HASH_TABLE, &[(key, hash_bytes.to_vec())]) {
                eprintln!("failed to store hash: {}", err);
            }

            result.modules.push(parsed.module);

            self.engine
                .observer
                .on_event(CycleEvent::FileParsed(file_path));
        }
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::observer::TestObserver;
    use holo_base::FilePath;
    use holo_fs::{FileSystem, InMemoryFileSystem};
    use std::sync::Arc;

    #[test]
    fn test_incremental_compilation_skips_unchanged_files() {
        let fs = Arc::new(InMemoryFileSystem::default());
        let observer = Arc::new(TestObserver::default());
        let mut engine = Engine::new()
            .with_filesystem(fs.clone())
            .with_observer(observer.clone());

        let path = FilePath::from("main.holo");
        fs.write_string(&path, "fn main() -> i32 { let x: i32 = 0; x; }")
            .unwrap();

        // 1. First cycle: File is new, should be parsed
        engine.record_change(path.clone());
        engine.run_cycle().unwrap();

        let events = observer.events();
        assert!(events.contains(&CycleEvent::FileParsed(path.clone())));

        // 2. Second cycle: Content is identical, should be skipped
        observer.clear_events();
        engine.record_change(path.clone());
        engine.run_cycle().unwrap();

        let events = observer.events();
        assert!(events.contains(&CycleEvent::FileParseSkipped(path.clone())));

        // 3. Third cycle: Content changed, should be re-parsed
        observer.clear_events();
        fs.write_string(&path, "fn main() -> i32 { let x: i32 = 1; x; }")
            .unwrap();
        engine.record_change(path.clone());
        engine.run_cycle().unwrap();

        let events = observer.events();
        // Check events after third cycle. Clear the vec first if observer supported it,
        // but since it's just a growing list, we check if the last event is FileParsed.
        assert_eq!(
            events.last().unwrap(),
            &CycleEvent::FileParsed(path.clone())
        );
    }

    #[test]
    fn test_cycle_reports_io_errors_as_diagnostics() {
        let fs = Arc::new(InMemoryFileSystem::default());
        let observer = Arc::new(TestObserver::default());
        let mut engine = Engine::new()
            .with_filesystem(fs.clone())
            .with_observer(observer.clone());

        let path = FilePath::from("missing.holo");
        // We do NOT write to fs, so reading will fail

        engine.record_change(path.clone());
        let cycle_result = engine.run_cycle().unwrap();

        let events = observer.events();
        let error_event = events.iter().find_map(|e| {
            if let CycleEvent::FileReadError(p, msg) = e {
                if p == &path {
                    Some(msg)
                } else {
                    None
                }
            } else {
                None
            }
        });
        assert!(error_event.is_some());

        assert!(cycle_result.diagnostics[0]
            .message
            .contains("failed to read file"));
    }
}
