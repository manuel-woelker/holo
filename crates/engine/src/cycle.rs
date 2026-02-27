use crate::engine::{Engine, FileState};
use crate::observer::CycleEvent;
use holo_base::{hash_string, DiagnosticKind, SourceDiagnostic, SourceFile};
use holo_lexer::{BasicLexer, Lexer};
use holo_parser::{BasicParser, Parser};

/// Represents the state and logic of a single compilation cycle.
pub struct Cycle<'a> {
    engine: &'a mut Engine,
}

impl<'a> Cycle<'a> {
    /// Creates a new cycle for the given engine.
    pub fn new(engine: &'a mut Engine) -> Self {
        Self { engine }
    }

    /// Runs the cycle logic.
    pub fn run(&mut self) {
        self.stage_1_parse();
        // Stages 2-5 will be implemented here.
    }

    /// Stage 1: Tokenize and parse changed files to ASTs.
    fn stage_1_parse(&mut self) {
        let dirty_files = std::mem::take(&mut self.engine.dirty_files);

        let lexer = BasicLexer;
        let parser = BasicParser;

        for file_path in dirty_files {
            let content = match self.engine.filesystem.read_to_string(&file_path) {
                Ok(content) => content,
                Err(err) => {
                    let error_message = err.to_string();
                    self.engine.observer.on_event(CycleEvent::FileReadError(
                        file_path.clone(),
                        error_message.clone().into(),
                    ));

                    // Emit diagnostic for file read error
                    self.engine.file_states.insert(
                        file_path.clone(),
                        FileState {
                            content_hash: 0,
                            ast: None,
                            diagnostics: vec![SourceDiagnostic::new(
                                DiagnosticKind::Io,
                                format!("failed to read file: {}", error_message),
                            )],
                        },
                    );
                    continue;
                }
            };

            let content_hash = hash_string(&content);

            // Omission logic: Skip if content hash hasn't changed
            if let Some(state) = self.engine.file_states.get(&file_path) {
                if state.content_hash == content_hash {
                    self.engine
                        .observer
                        .on_event(CycleEvent::FileParseSkipped(file_path));
                    continue;
                }
            }

            // Perform tokenization
            let lexed = lexer.lex(&content);

            // Perform parsing
            let source_file = SourceFile::new(&content, file_path.clone());
            let parsed = parser.parse_module(&lexed.tokens, &source_file);

            // Update file state
            let mut diagnostics = lexed.diagnostics;
            diagnostics.extend(parsed.diagnostics);

            self.engine.file_states.insert(
                file_path.clone(),
                FileState {
                    content_hash,
                    ast: Some(parsed.module),
                    diagnostics,
                },
            );

            self.engine
                .observer
                .on_event(CycleEvent::FileParsed(file_path));
        }
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
        engine.run_cycle();

        let events = observer.events();
        assert!(events.contains(&CycleEvent::FileParsed(path.clone())));
        assert_eq!(engine.file_states.get(&path).unwrap().diagnostics.len(), 0);

        // 2. Second cycle: Content is identical, should be skipped
        observer.clear_events();
        engine.record_change(path.clone());
        engine.run_cycle();

        let events = observer.events();
        assert!(events.contains(&CycleEvent::FileParseSkipped(path.clone())));
        // Ensure diagnostic count is still zero (state maintained)
        assert_eq!(engine.file_states.get(&path).unwrap().diagnostics.len(), 0);

        // 3. Third cycle: Content changed, should be re-parsed
        observer.clear_events();
        fs.write_string(&path, "fn main() -> i32 { let x: i32 = 1; x; }")
            .unwrap();
        engine.record_change(path.clone());
        engine.run_cycle();

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
        engine.run_cycle();

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

        let state = engine
            .file_states
            .get(&path)
            .expect("FileState should exist even on error");
        assert_eq!(state.diagnostics.len(), 1);
        assert_eq!(state.diagnostics[0].kind, DiagnosticKind::Io);
        assert!(state.diagnostics[0].message.contains("failed to read file"));
    }
}
