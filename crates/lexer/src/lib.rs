//! Tokenization interfaces for the minimal holo language.

use holo_base::{DiagnosticKind, SharedString, SourceDiagnostic, SourceExcerpt, Span};

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

impl Lexer for BasicLexer {
    fn lex(&self, source: &str) -> LexResult {
        let mut tokens = Vec::new();
        let mut diagnostics = Vec::new();
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
                    let display = display_byte(byte);
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Lexing,
                            format!("unsupported character `{display}`"),
                        )
                        .with_annotated_span(
                            Span::new(index, index + 1),
                            "this character is not part of the language",
                        )
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                    index += 1;
                    continue;
                }
            };

            tokens.push(Token {
                kind: token,
                span: Span::new(index, index + 1),
                lexeme: (byte as char).to_string().into(),
            });
            index += 1;
        }

        LexResult {
            tokens,
            diagnostics,
        }
    }
}

fn display_byte(byte: u8) -> String {
    if byte.is_ascii_graphic() || byte == b' ' {
        return (byte as char).to_string();
    }
    format!("0x{byte:02X}")
}

#[cfg(test)]
mod tests {
    use super::{BasicLexer, Lexer, TokenKind};

    #[test]
    fn lexes_keywords_and_symbols() {
        let lexer = BasicLexer;
        let result = lexer.lex("#[test] fn demo() { assert(!false); }");
        assert!(result.diagnostics.is_empty());
        let tokens = result.tokens;
        assert_eq!(
            tokens.first().map(|token| token.kind),
            Some(TokenKind::Hash)
        );
        assert!(tokens.iter().any(|token| token.kind == TokenKind::Assert));
        assert!(tokens.iter().any(|token| token.kind == TokenKind::Bang));
    }

    #[test]
    fn reports_unsupported_character_and_continues() {
        let lexer = BasicLexer;
        let result = lexer.lex("#[test] fn demo() { assert(@true); }");
        assert_eq!(result.diagnostics.len(), 1);
        assert!(result.diagnostics[0]
            .render_annotated()
            .contains("unsupported character"));
        assert!(result
            .tokens
            .iter()
            .any(|token| token.kind == TokenKind::True));
    }
}
