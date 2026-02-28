use holo_base::{SourceFile, Span};

use crate::TokenKind;

/// Source token with kind and byte span.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    /// Token category.
    pub kind: TokenKind,
    /// Byte range in original source.
    pub span: Span,
}

impl Token {
    /// Creates a new token from token kind and byte span.
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    /// Returns the raw lexeme for this token from the given source file.
    pub fn lexeme<'a>(&self, source: &'a SourceFile) -> &'a str {
        source.source_at(self.span)
    }
}
