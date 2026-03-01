//! Parser interfaces and parser implementation.

#![allow(unreachable_patterns)]
#![allow(irrefutable_let_patterns)]

mod parser;
mod parser_state;

#[cfg(test)]
mod tests;

pub use parser::{ParseResult, Parser};
