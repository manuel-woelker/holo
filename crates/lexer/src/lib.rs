//! Tokenization interfaces for the minimal holo language.

use holo_base::{DiagnosticKind, SharedString, SourceDiagnostic, SourceExcerpt, Span};

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
                    "let" => TokenKind::Let,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "while" => TokenKind::While,
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

            if byte == b'"' {
                let start = index;
                index += 1;
                while index < bytes.len() && bytes[index] != b'"' {
                    if bytes[index] == b'\\' && index + 1 < bytes.len() {
                        index += 2;
                    } else {
                        index += 1;
                    }
                }
                if index < bytes.len() {
                    index += 1;
                }
                let lexeme = &source[start..index];
                tokens.push(Token {
                    kind: TokenKind::StringLiteral,
                    span: Span::new(start, index),
                    lexeme: lexeme.into(),
                });
                continue;
            }

            // Handle line comments (// ...)
            if byte == b'/' && index + 1 < bytes.len() && bytes[index + 1] == b'/' {
                index += 2;
                while index < bytes.len() && bytes[index] != b'\n' {
                    index += 1;
                }
                continue;
            }

            // Handle block comments (/* ... */) with nesting
            if byte == b'/' && index + 1 < bytes.len() && bytes[index + 1] == b'*' {
                let start = index;
                index += 2;
                let mut depth = 1;
                while index + 1 < bytes.len() && depth > 0 {
                    if bytes[index] == b'/' && bytes[index + 1] == b'*' {
                        depth += 1;
                        index += 2;
                    } else if bytes[index] == b'*'
                        && index + 1 < bytes.len()
                        && bytes[index + 1] == b'/'
                    {
                        depth -= 1;
                        if depth > 0 {
                            index += 2;
                        }
                    } else {
                        index += 1;
                    }
                }
                if depth != 0 {
                    diagnostics.push(
                        SourceDiagnostic::new(DiagnosticKind::Lexing, "unterminated block comment")
                            .with_annotated_span(
                                Span::new(start, index),
                                "block comment never closes",
                            )
                            .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                }
                continue;
            }

            if byte.is_ascii_digit() {
                let start = index;
                index += 1;
                while index < bytes.len() && bytes[index].is_ascii_digit() {
                    index += 1;
                }
                if index < bytes.len()
                    && bytes[index] == b'.'
                    && index + 1 < bytes.len()
                    && bytes[index + 1].is_ascii_digit()
                {
                    index += 1;
                    while index < bytes.len() && bytes[index].is_ascii_digit() {
                        index += 1;
                    }
                }
                while index < bytes.len() && bytes[index].is_ascii_alphanumeric() {
                    index += 1;
                }

                let lexeme = &source[start..index];
                tokens.push(Token {
                    kind: TokenKind::Number,
                    span: Span::new(start, index),
                    lexeme: lexeme.into(),
                });
                continue;
            }

            let token = match byte {
                b'+' => TokenKind::Plus,
                b'-' => {
                    if index + 1 < bytes.len() && bytes[index + 1] == b'>' {
                        tokens.push(Token {
                            kind: TokenKind::Arrow,
                            span: Span::new(index, index + 2),
                            lexeme: "->".into(),
                        });
                        index += 2;
                        continue;
                    }
                    TokenKind::Minus
                }
                b'*' => TokenKind::Star,
                b'/' => TokenKind::Slash,
                b'%' => TokenKind::Percent,
                b'=' => {
                    if index + 1 < bytes.len() && bytes[index + 1] == b'=' {
                        tokens.push(Token {
                            kind: TokenKind::DoubleEquals,
                            span: Span::new(index, index + 2),
                            lexeme: "==".into(),
                        });
                        index += 2;
                        continue;
                    }
                    TokenKind::Equals
                }
                b'!' => {
                    if index + 1 < bytes.len() && bytes[index + 1] == b'=' {
                        tokens.push(Token {
                            kind: TokenKind::BangEquals,
                            span: Span::new(index, index + 2),
                            lexeme: "!=".into(),
                        });
                        index += 2;
                        continue;
                    }
                    TokenKind::Bang
                }
                b'<' => {
                    if index + 1 < bytes.len() && bytes[index + 1] == b'=' {
                        tokens.push(Token {
                            kind: TokenKind::LessThanEquals,
                            span: Span::new(index, index + 2),
                            lexeme: "<=".into(),
                        });
                        index += 2;
                        continue;
                    }
                    TokenKind::LessThan
                }
                b'>' => {
                    if index + 1 < bytes.len() && bytes[index + 1] == b'=' {
                        tokens.push(Token {
                            kind: TokenKind::GreaterThanEquals,
                            span: Span::new(index, index + 2),
                            lexeme: ">=".into(),
                        });
                        index += 2;
                        continue;
                    }
                    TokenKind::GreaterThan
                }
                b',' => TokenKind::Comma,
                b':' => TokenKind::Colon,
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
        let result = lexer.lex("#[test] fn demo(x: i64) -> i64 { let y = 1 + x; assert(!false); }");
        assert!(result.diagnostics.is_empty());
        let tokens = result.tokens;
        assert_eq!(
            tokens.first().map(|token| token.kind),
            Some(TokenKind::Hash)
        );
        assert!(tokens.iter().any(|token| token.kind == TokenKind::Assert));
        assert!(tokens.iter().any(|token| token.kind == TokenKind::Bang));
        assert!(tokens.iter().any(|token| token.kind == TokenKind::Let));
        assert!(tokens.iter().any(|token| token.kind == TokenKind::Number));
        assert!(tokens.iter().any(|token| token.kind == TokenKind::Arrow));
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

    #[test]
    fn lexes_control_flow_keywords() {
        let lexer = BasicLexer;
        let result = lexer.lex("fn f() -> () { if true { while false { } } else { } }");
        assert!(result.diagnostics.is_empty());
        assert!(result
            .tokens
            .iter()
            .any(|token| token.kind == TokenKind::If));
        assert!(result
            .tokens
            .iter()
            .any(|token| token.kind == TokenKind::Else));
        assert!(result
            .tokens
            .iter()
            .any(|token| token.kind == TokenKind::While));
    }
}
