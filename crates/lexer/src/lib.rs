//! Tokenization interfaces for the minimal holo language.

pub mod lexer;
pub mod types;

pub use types::{BasicLexer, LexResult, Lexer, Token, TokenKind};
