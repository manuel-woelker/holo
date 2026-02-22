//! Token types for holo source processing.

use holo_base::Span;

/// Type classification for lexical tokens.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenType {
    Identifier,
}

/// A source token with type classification and byte span.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Token {
    /// Lexical classification for the token.
    pub token_type: TokenType,
    /// Byte range in the source file.
    pub span: Span,
}

impl Token {
    /// Creates a new token from token type and byte span.
    pub fn new(token_type: TokenType, span: Span) -> Self {
        Self { token_type, span }
    }
}

#[cfg(test)]
mod tests {
    use super::{Token, TokenType};
    use holo_base::Span;

    #[test]
    fn creates_token_with_type_and_span() {
        let token = Token::new(TokenType::Identifier, Span::new(0, 4));
        assert_eq!(token.token_type, TokenType::Identifier);
        assert_eq!(token.span, Span::new(0, 4));
    }
}
