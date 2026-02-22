//! Parser interfaces and a minimal parser implementation.

use holo_ast::{AssertStatement, Expr, Module, Statement, TestItem};
use holo_base::{DiagnosticKind, SharedString, SourceDiagnostic, SourceExcerpt, Span};
use holo_lexer::{Token, TokenKind};

/// Parser abstraction used by the coordinating compiler core.
pub trait Parser {
    /// Parses a full token stream into a module AST.
    fn parse_module(&self, tokens: &[Token], source: &str) -> ParseResult;
}

/// Result payload produced by parsing.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ParseResult {
    /// Produced module AST.
    pub module: Module,
    /// Parsing diagnostics encountered while reading tokens.
    pub diagnostics: Vec<SourceDiagnostic>,
}

/// Placeholder parser used during initial crate wiring.
#[derive(Debug, Default)]
pub struct BasicParser;

impl Parser for BasicParser {
    fn parse_module(&self, tokens: &[Token], source: &str) -> ParseResult {
        let mut parser = ParserState::new(tokens, source);
        parser.parse_module()
    }
}

#[derive(Debug)]
struct ParserState<'a> {
    tokens: &'a [Token],
    source: &'a str,
    index: usize,
    diagnostics: Vec<SourceDiagnostic>,
}

impl<'a> ParserState<'a> {
    fn new(tokens: &'a [Token], source: &'a str) -> Self {
        Self {
            tokens,
            source,
            index: 0,
            diagnostics: Vec::new(),
        }
    }

    fn parse_module(&mut self) -> ParseResult {
        let mut tests = Vec::new();
        while self.peek().is_some() {
            let previous_index = self.index;
            if let Some(test) = self.parse_test_item() {
                tests.push(test);
            }

            if self.index == previous_index {
                self.advance();
            }
        }
        ParseResult {
            module: Module { tests },
            diagnostics: std::mem::take(&mut self.diagnostics),
        }
    }

    fn parse_test_item(&mut self) -> Option<TestItem> {
        let hash = self.expect(TokenKind::Hash, "test item must start with `#`")?;
        self.expect(
            TokenKind::OpenBracket,
            "expected `[` after `#` in test attribute",
        )?;
        let attribute =
            self.expect(TokenKind::Identifier, "expected attribute name in `#[...]`")?;
        if attribute.lexeme != "test" {
            self.report_current(
                format!(
                    "expected `#[test]` attribute, found `#[{}]`",
                    attribute.lexeme
                )
                .into(),
                attribute.span,
                format!("unsupported test attribute `{}`", attribute.lexeme).into(),
            );
            self.recover_to_next_item();
            return None;
        }
        self.expect(TokenKind::CloseBracket, "expected `]` after attribute name")?;
        self.expect(TokenKind::Fn, "expected `fn` after `#[test]` attribute")?;
        let name = self.expect(
            TokenKind::Identifier,
            "expected test function name after `fn`",
        )?;
        self.expect(
            TokenKind::OpenParen,
            "expected `(` after test function name",
        )?;
        self.expect(
            TokenKind::CloseParen,
            "expected `)` after test function parameter list",
        )?;
        self.expect(
            TokenKind::OpenBrace,
            "expected `{` to start test function body",
        )?;

        let mut statements = Vec::new();
        while self.peek().is_some() && !self.check(TokenKind::CloseBrace) {
            let previous_index = self.index;
            if let Some(statement) = self.parse_statement() {
                statements.push(statement);
            } else {
                self.recover_statement();
            }

            if self.index == previous_index {
                self.advance();
            }
        }
        let close_brace = self.expect(TokenKind::CloseBrace, "expected `}` after test body")?;

        Some(TestItem {
            name: name.lexeme.clone(),
            statements,
            span: Span::new(hash.span.start, close_brace.span.end),
        })
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        let assert_keyword = self.expect(TokenKind::Assert, "expected `assert` statement")?;
        self.expect(TokenKind::OpenParen, "expected `(` after `assert`")?;
        let expression = self.parse_expression()?;
        self.expect(
            TokenKind::CloseParen,
            "expected `)` after `assert` expression",
        )?;
        let semicolon = self.expect(TokenKind::Semicolon, "expected `;` after statement")?;

        Some(Statement::Assert(AssertStatement {
            expression,
            span: Span::new(assert_keyword.span.start, semicolon.span.end),
        }))
    }

    fn parse_expression(&mut self) -> Option<Expr> {
        if let Some(bang) = self.consume_if(TokenKind::Bang) {
            let inner = self.parse_expression()?;
            let span = Span::new(bang.span.start, inner.span.end);
            return Some(Expr::negation(inner, span));
        }

        let token = self.peek()?;
        match token.kind {
            TokenKind::True => {
                let literal = self.advance().expect("peek guaranteed token");
                Some(Expr::bool_literal(true, literal.span))
            }
            TokenKind::False => {
                let literal = self.advance().expect("peek guaranteed token");
                Some(Expr::bool_literal(false, literal.span))
            }
            _ => {
                self.report_current(
                    "expected expression (`true`, `false`, or `!<expr>`)".into(),
                    token.span,
                    format!("found `{}` instead", token.lexeme).into(),
                );
                self.advance();
                None
            }
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

    fn expect(&mut self, kind: TokenKind, context: &str) -> Option<&'a Token> {
        let token = self.advance();
        let Some(token) = token else {
            let eof_span = self
                .tokens
                .last()
                .map(|token| Span::new(token.span.end, token.span.end + 1))
                .unwrap_or_else(|| Span::new(0, 1));
            self.report_current(
                context.into(),
                eof_span,
                format!("expected {}, found end of input", token_kind_name(kind)).into(),
            );
            return None;
        };

        if token.kind != kind {
            self.report_current(
                context.into(),
                token.span,
                format!(
                    "expected {}, found `{}`",
                    token_kind_name(kind),
                    token.lexeme
                )
                .into(),
            );
            return None;
        }

        Some(token)
    }

    fn report_current(&mut self, message: SharedString, span: Span, annotation: SharedString) {
        self.diagnostics.push(
            SourceDiagnostic::new(DiagnosticKind::Parsing, message)
                .with_annotated_span(span, annotation)
                .with_source_excerpt(SourceExcerpt::new(self.source, 1, 0)),
        );
    }

    fn recover_to_next_item(&mut self) {
        while let Some(token) = self.peek() {
            if token.kind == TokenKind::Hash {
                return;
            }
            self.advance();
        }
    }

    fn recover_statement(&mut self) {
        while let Some(token) = self.peek() {
            match token.kind {
                TokenKind::Semicolon => {
                    self.advance();
                    return;
                }
                TokenKind::CloseBrace => return,
                _ => {
                    self.advance();
                }
            }
        }
    }
}

fn token_kind_name(kind: TokenKind) -> &'static str {
    match kind {
        TokenKind::True => "`true`",
        TokenKind::False => "`false`",
        TokenKind::Assert => "`assert`",
        TokenKind::Bang => "`!`",
        TokenKind::OpenParen => "`(`",
        TokenKind::CloseParen => "`)`",
        TokenKind::OpenBrace => "`{`",
        TokenKind::CloseBrace => "`}`",
        TokenKind::Semicolon => "`;`",
        TokenKind::Hash => "`#`",
        TokenKind::OpenBracket => "`[`",
        TokenKind::CloseBracket => "`]`",
        TokenKind::Fn => "`fn`",
        TokenKind::Identifier => "identifier",
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
        let result = parser.parse_module(&[], "");
        let module = result.module;
        assert!(result.diagnostics.is_empty());
        assert!(module.tests.is_empty());
    }

    #[test]
    fn parses_test_with_assertion_and_negation() {
        let source = "#[test] fn sample() { assert(!false); }";
        let lexed = BasicLexer.lex(source);
        assert!(lexed.diagnostics.is_empty());
        let parsed = BasicParser.parse_module(&lexed.tokens, source);
        assert!(parsed.diagnostics.is_empty());
        let module = parsed.module;

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
        let lexed = BasicLexer.lex(source);
        let result = BasicParser.parse_module(&lexed.tokens, source);
        assert!(
            result
                .diagnostics
                .iter()
                .any(|diagnostic| diagnostic.message.contains("expected `#[test]`")),
            "actual diagnostics: {:?}",
            result.diagnostics
        );
    }

    #[test]
    fn continues_after_statement_error() {
        let source = "#[test] fn sample() { assert(); assert(true); }";
        let lexed = BasicLexer.lex(source);
        let result = BasicParser.parse_module(&lexed.tokens, source);
        assert!(!result.diagnostics.is_empty());
        assert_eq!(result.module.tests.len(), 1);
        assert_eq!(result.module.tests[0].statements.len(), 1);
    }
}
