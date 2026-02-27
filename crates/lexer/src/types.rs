use holo_base::SourceDiagnostic;
pub use holo_token::{Token, TokenKind};

/// Lexing abstraction used by higher-level compiler orchestration.
pub trait Lexer {
    /// Produces a token stream from source text.
    fn lex(&self, source: &str) -> LexResult;
}

/// Result payload produced by lexing.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct LexResult {
    /// Produced token stream.
    pub tokens: Vec<Token>,
    /// Lexing diagnostics encountered while tokenizing.
    pub diagnostics: Vec<SourceDiagnostic>,
}


/// Minimal lexer implementation used for initial pipeline wiring.
#[derive(Debug, Default)]
pub struct BasicLexer;
