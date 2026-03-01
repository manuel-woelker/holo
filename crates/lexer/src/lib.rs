//! Tokenization interfaces for the minimal holo language.

pub mod lexer;
pub mod types;

pub use lexer::Lexer;
pub use types::{LexResult, Token, TokenKind};
