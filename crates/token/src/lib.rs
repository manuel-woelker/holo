//! Token types for holo source processing.

use holo_base::{SourceFile, Span};

/// Token categories required by the initial parser milestone.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    True,
    False,
    Assert,
    Let,
    If,
    Else,
    While,
    Bang,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equals,
    DoubleEquals,
    BangEquals,
    LessThan,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
    Comma,
    Colon,
    Arrow,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,
    Hash,
    OpenBracket,
    CloseBracket,
    Fn,
    Identifier,
    Number,
    StringLiteral,
    TemplateString,
}

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

#[cfg(test)]
mod tests {
    use super::{Token, TokenKind};
    use holo_base::Span;

    #[test]
    fn creates_token_with_kind_and_span() {
        let token = Token::new(TokenKind::Identifier, Span::new(0, 4));
        assert_eq!(token.kind, TokenKind::Identifier);
        assert_eq!(token.span, Span::new(0, 4));
    }
}
