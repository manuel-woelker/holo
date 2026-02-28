//! Parser interfaces and parser implementation.

#![allow(unreachable_patterns)]
#![allow(irrefutable_let_patterns)]

mod api;
mod parser_state;

#[cfg(test)]
mod tests;

pub use api::{BasicParser, ParseResult, Parser};
