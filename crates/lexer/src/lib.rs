//! Tokenization interfaces for the minimal holo language.

use holo_base::{holo_message_error, Result, SharedString, Span};

/// Token categories required by the initial parser milestone.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    True,
    False,
    Assert,
    Bang,
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
}

/// Source token with kind and byte span.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    /// Token category.
    pub kind: TokenKind,
    /// Byte range in original source.
    pub span: Span,
    /// Raw lexeme for identifiers and debugging output.
    pub lexeme: SharedString,
}

/// Lexing abstraction used by higher-level compiler orchestration.
pub trait Lexer {
    /// Produces a token stream from source text.
    fn lex(&self, source: &str) -> Result<Vec<Token>>;
}

/// Minimal lexer implementation used for initial pipeline wiring.
#[derive(Debug, Default)]
pub struct BasicLexer;

impl Lexer for BasicLexer {
    fn lex(&self, source: &str) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();
        let bytes = source.as_bytes();
        let mut index = 0usize;

        while index < bytes.len() {
            let byte = bytes[index];

            if byte.is_ascii_whitespace() {
                index += 1;
                continue;
            }

            if byte.is_ascii_alphabetic() || byte == b'_' {
                let start = index;
                index += 1;
                while index < bytes.len()
                    && (bytes[index].is_ascii_alphanumeric() || bytes[index] == b'_')
                {
                    index += 1;
                }

                let lexeme = &source[start..index];
                let kind = match lexeme {
                    "true" => TokenKind::True,
                    "false" => TokenKind::False,
                    "assert" => TokenKind::Assert,
                    "fn" => TokenKind::Fn,
                    _ => TokenKind::Identifier,
                };

                tokens.push(Token {
                    kind,
                    span: Span::new(start, index),
                    lexeme: lexeme.into(),
                });
                continue;
            }

            let token = match byte {
                b'!' => TokenKind::Bang,
                b'(' => TokenKind::OpenParen,
                b')' => TokenKind::CloseParen,
                b'{' => TokenKind::OpenBrace,
                b'}' => TokenKind::CloseBrace,
                b';' => TokenKind::Semicolon,
                b'#' => TokenKind::Hash,
                b'[' => TokenKind::OpenBracket,
                b']' => TokenKind::CloseBracket,
                _ => {
                    return Err(holo_message_error!(
                        "unsupported character '{}' at byte {}",
                        byte as char,
                        index
                    ));
                }
            };

            tokens.push(Token {
                kind: token,
                span: Span::new(index, index + 1),
                lexeme: (byte as char).to_string().into(),
            });
            index += 1;
        }

        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::{BasicLexer, Lexer, TokenKind};

    #[test]
    fn lexes_keywords_and_symbols() {
        let lexer = BasicLexer;
        let tokens = lexer
            .lex("#[test] fn demo() { assert(!false); }")
            .expect("lexing should succeed");
        assert_eq!(
            tokens.first().map(|token| token.kind),
            Some(TokenKind::Hash)
        );
        assert!(tokens.iter().any(|token| token.kind == TokenKind::Assert));
        assert!(tokens.iter().any(|token| token.kind == TokenKind::Bang));
    }
}
