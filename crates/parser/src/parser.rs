use holo_ast::Module;
use holo_base::{SourceDiagnostic, SourceFile};
use holo_lexer::Token;

use crate::parser_state::ParserState;

/// Result payload produced by parsing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseResult {
    /// Produced module AST.
    pub module: Module,
    /// Parsing diagnostics encountered while reading tokens.
    pub diagnostics: Vec<SourceDiagnostic>,
}

/// Parser implementation used by the compiler core.
#[derive(Debug, Default)]
pub struct Parser;

impl Parser {
    pub fn parse_module(&self, tokens: &[Token], source: &SourceFile) -> ParseResult {
        let mut parser = ParserState::new(tokens, source);
        parser.parse_module()
    }
}
