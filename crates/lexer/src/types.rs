use holo_base::{SharedString, Span};

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
    pub diagnostics: Vec<holo_base::SourceDiagnostic>,
}

/// Minimal lexer implementation used for initial pipeline wiring.
#[derive(Debug, Default)]
pub struct BasicLexer;
