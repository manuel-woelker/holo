//! Parser interfaces and a minimal parser implementation.

use holo_ast::{AssertStatement, Expr, Module, Statement, TestItem};
use holo_base::{holo_message_error, Result, Span};
use holo_lexer::{Token, TokenKind};

/// Parser abstraction used by the coordinating compiler core.
pub trait Parser {
    /// Parses a full token stream into a module AST.
    fn parse_module(&self, tokens: &[Token]) -> Result<Module>;
}

/// Placeholder parser used during initial crate wiring.
#[derive(Debug, Default)]
pub struct BasicParser;

impl Parser for BasicParser {
    fn parse_module(&self, tokens: &[Token]) -> Result<Module> {
        let mut parser = ParserState::new(tokens);
        parser.parse_module()
    }
}

#[derive(Debug)]
struct ParserState<'a> {
    tokens: &'a [Token],
    index: usize,
}

impl<'a> ParserState<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, index: 0 }
    }

    fn parse_module(&mut self) -> Result<Module> {
        let mut tests = Vec::new();
        while self.peek().is_some() {
            tests.push(self.parse_test_item()?);
        }
        Ok(Module { tests })
    }

    fn parse_test_item(&mut self) -> Result<TestItem> {
        let hash = self.expect(TokenKind::Hash)?;
        self.expect(TokenKind::OpenBracket)?;
        let attribute = self.expect(TokenKind::Identifier)?;
        if attribute.lexeme != "test" {
            return Err(holo_message_error!(
                "expected #[test] attribute, found #[{}]",
                attribute.lexeme
            ));
        }
        self.expect(TokenKind::CloseBracket)?;
        self.expect(TokenKind::Fn)?;
        let name = self.expect(TokenKind::Identifier)?;
        self.expect(TokenKind::OpenParen)?;
        self.expect(TokenKind::CloseParen)?;
        self.expect(TokenKind::OpenBrace)?;

        let mut statements = Vec::new();
        while !self.check(TokenKind::CloseBrace) {
            statements.push(self.parse_statement()?);
        }
        let close_brace = self.expect(TokenKind::CloseBrace)?;

        Ok(TestItem {
            name: name.lexeme.clone(),
            statements,
            span: Span::new(hash.span.start, close_brace.span.end),
        })
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        let assert_keyword = self.expect(TokenKind::Assert)?;
        self.expect(TokenKind::OpenParen)?;
        let expression = self.parse_expression()?;
        self.expect(TokenKind::CloseParen)?;
        let semicolon = self.expect(TokenKind::Semicolon)?;

        Ok(Statement::Assert(AssertStatement {
            expression,
            span: Span::new(assert_keyword.span.start, semicolon.span.end),
        }))
    }

    fn parse_expression(&mut self) -> Result<Expr> {
        if let Some(bang) = self.consume_if(TokenKind::Bang) {
            let inner = self.parse_expression()?;
            let span = Span::new(bang.span.start, inner.span.end);
            return Ok(Expr::negation(inner, span));
        }

        let token = self
            .peek()
            .ok_or_else(|| holo_message_error!("expected expression, found end of input"))?;
        match token.kind {
            TokenKind::True => {
                let literal = self.advance().expect("peek guaranteed token");
                Ok(Expr::bool_literal(true, literal.span))
            }
            TokenKind::False => {
                let literal = self.advance().expect("peek guaranteed token");
                Ok(Expr::bool_literal(false, literal.span))
            }
            _ => Err(holo_message_error!(
                "expected expression (`true`, `false`, or `!<expr>`), found `{}`",
                token.lexeme
            )),
        }
    }

    fn peek(&self) -> Option<&'a Token> {
        self.tokens.get(self.index)
    }

    fn advance(&mut self) -> Option<&'a Token> {
        let token = self.tokens.get(self.index)?;
        self.index += 1;
        Some(token)
    }

    fn check(&self, kind: TokenKind) -> bool {
        self.peek().is_some_and(|token| token.kind == kind)
    }

    fn consume_if(&mut self, kind: TokenKind) -> Option<&'a Token> {
        if self.check(kind) {
            return self.advance();
        }
        None
    }

    fn expect(&mut self, kind: TokenKind) -> Result<&'a Token> {
        let token = self.advance().ok_or_else(|| {
            holo_message_error!(
                "expected token {:?}, found end of input while parsing",
                kind
            )
        })?;
        if token.kind != kind {
            return Err(holo_message_error!(
                "expected token {:?}, found `{}`",
                kind,
                token.lexeme
            ));
        }
        Ok(token)
    }
}

#[cfg(test)]
mod tests {
    use super::{BasicParser, Parser};
    use holo_ast::{ExprKind, Statement};
    use holo_lexer::{BasicLexer, Lexer};

    #[test]
    fn parses_empty_token_stream_to_empty_module() {
        let parser = BasicParser;
        let module = parser
            .parse_module(&[])
            .expect("empty token stream should parse");
        assert!(module.tests.is_empty());
    }

    #[test]
    fn parses_test_with_assertion_and_negation() {
        let source = "#[test] fn sample() { assert(!false); }";
        let tokens = BasicLexer.lex(source).expect("lexing should succeed");
        let module = BasicParser
            .parse_module(&tokens)
            .expect("parsing should succeed");

        assert_eq!(module.tests.len(), 1);
        assert_eq!(module.tests[0].name, "sample");
        match &module.tests[0].statements[0] {
            Statement::Assert(assertion) => {
                assert!(matches!(assertion.expression.kind, ExprKind::Negation(_)));
            }
        }
    }

    #[test]
    fn rejects_non_test_attribute() {
        let source = "#[bench] fn sample() { assert(true); }";
        let tokens = BasicLexer.lex(source).expect("lexing should succeed");
        let error = BasicParser
            .parse_module(&tokens)
            .expect_err("parser should reject unsupported attribute");
        assert!(
            error.to_string().contains("expected #[test]"),
            "actual error: {error}"
        );
    }
}
