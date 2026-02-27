use crate::cycle::Cycle;

use holo_ast::Module;
use holo_base::{FilePath, SourceDiagnostic};
use holo_lexer::BasicLexer;
use holo_parser::BasicParser;
use std::collections::HashMap;

/// The core incremental compilation engine.
///
/// This engine manages the project state and coordinates the 5-stage
/// incremental compilation pipeline using a forward incremental approach.
pub struct Engine {
    pub(crate) lexer: BasicLexer,
    pub(crate) parser: BasicParser,
    pub(crate) file_states: HashMap<FilePath, FileState>,
    pub(crate) dirty_files: Vec<FilePath>,
}

/// Represents the cached state of a single source file.
#[derive(Debug, Default, Clone)]
pub struct FileState {
    /// Hash of the file content when last processed.
    pub content_hash: u64,
    /// The successfully parsed AST module, if any.
    pub ast: Option<Module>,
    /// Diagnostics produced for this file in its current state.
    pub diagnostics: Vec<SourceDiagnostic>,
}

impl Default for Engine {
    fn default() -> Self {
        Self {
            lexer: BasicLexer,
            parser: BasicParser,
            file_states: HashMap::new(),
            dirty_files: Vec::new(),
        }
    }
}

impl Engine {
    /// Creates a new instance of the engine.
    pub fn new() -> Self {
        Self::default()
    }

    /// Executes a single incremental compilation cycle.
    ///
    /// This method represents one "turn" of the compiler's forward incremental pipeline.
    /// It is called a "cycle" because it is a fundamental unit of the continuous
    /// development feedback loop, transforming recent file changes into updated
    /// diagnostics and test results.
    ///
    /// Each cycle progresses through the 5 stages of compilation, omitting
    /// work for components that haven't changed since the last pulse.
    pub fn run_cycle(&mut self) {
        let mut cycle = Cycle::new(self);
        cycle.run();
    }

    /// Marks a file as dirty and needing re-processing in the next cycle.
    pub fn record_change(&mut self, file_path: FilePath) {
        if !self.dirty_files.contains(&file_path) {
            self.dirty_files.push(file_path);
        }
    }
}
