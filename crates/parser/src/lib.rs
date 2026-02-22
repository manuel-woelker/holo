//! Parser interfaces and a minimal parser implementation.

use holo_ast::Module;
use holo_base::Result;
use holo_lexer::Token;

/// Parser abstraction used by the coordinating compiler core.
pub trait Parser {
    /// Parses a full token stream into a module AST.
    fn parse_module(&self, tokens: &[Token]) -> Result<Module>;
}

/// Placeholder parser used during initial crate wiring.
#[derive(Debug, Default)]
pub struct BasicParser;

impl Parser for BasicParser {
    fn parse_module(&self, _tokens: &[Token]) -> Result<Module> {
        Ok(Module::default())
    }
}

#[cfg(test)]
mod tests {
    use super::{BasicParser, Parser};

    #[test]
    fn parses_empty_token_stream_to_empty_module() {
        let parser = BasicParser;
        let module = parser
            .parse_module(&[])
            .expect("empty token stream should parse");
        assert!(module.tests.is_empty());
    }
}
