use holo_base::SourceDiagnostic;
pub use holo_token::{Token, TokenKind};

/// Result payload produced by lexing.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct LexResult {
    /// Produced token stream.
    pub tokens: Vec<Token>,
    /// Lexing diagnostics encountered while tokenizing.
    pub diagnostics: Vec<SourceDiagnostic>,
}
