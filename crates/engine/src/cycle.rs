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
