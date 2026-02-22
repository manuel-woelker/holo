//! Parser interfaces and parser implementation.

use holo_ast::{
    AssertStatement, BinaryOperator, Expr, ExprStatement, FunctionItem, FunctionParameter,
    LetStatement, Module, Statement, TestItem, TypeRef,
};
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

/// Parser implementation used by the compiler core.
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
        let mut functions = Vec::new();
        let mut tests = Vec::new();
        while self.peek().is_some() {
            let previous_index = self.index;
            if let Some(function) = self.parse_item() {
                if function.is_test {
                    tests.push(TestItem {
                        name: function.name.clone(),
                        statements: function.statements.clone(),
                        span: function.span,
                    });
                }
                functions.push(function);
            }

            if self.index == previous_index {
                self.advance();
            }
        }
        ParseResult {
            module: Module { functions, tests },
            diagnostics: std::mem::take(&mut self.diagnostics),
        }
    }

    fn parse_item(&mut self) -> Option<FunctionItem> {
        let is_test = self.parse_optional_test_attribute()?;
        self.parse_function_item(is_test)
    }

    fn parse_optional_test_attribute(&mut self) -> Option<bool> {
        if !self.check(TokenKind::Hash) {
            return Some(false);
        }

        self.expect(TokenKind::Hash, "test item must start with `#`")?;
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

        Some(true)
    }

    fn parse_function_item(&mut self, is_test: bool) -> Option<FunctionItem> {
        let fn_keyword = self.expect(TokenKind::Fn, "expected `fn` for function item")?;
        let name = self.expect(TokenKind::Identifier, "expected function name after `fn`")?;
        self.expect(TokenKind::OpenParen, "expected `(` after function name")?;
        let parameters = self.parse_parameter_list()?;
        self.expect(
            TokenKind::CloseParen,
            "expected `)` after function parameter list",
        )?;
        let return_type = if self.consume_if(TokenKind::Arrow).is_some() {
            self.parse_type_ref()?
        } else {
            TypeRef::Unit
        };
        self.expect(TokenKind::OpenBrace, "expected `{` to start function body")?;

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
        let close_brace = self.expect(TokenKind::CloseBrace, "expected `}` after function body")?;

        Some(FunctionItem {
            name: name.lexeme.clone(),
            parameters,
            return_type,
            statements,
            is_test,
            span: Span::new(fn_keyword.span.start, close_brace.span.end),
        })
    }

    fn parse_parameter_list(&mut self) -> Option<Vec<FunctionParameter>> {
        let mut parameters = Vec::new();
        if self.check(TokenKind::CloseParen) {
            return Some(parameters);
        }

        loop {
            let name = self.expect(TokenKind::Identifier, "expected parameter name")?;
            self.expect(TokenKind::Colon, "expected `:` after parameter name")?;
            let ty = self.parse_type_ref()?;
            parameters.push(FunctionParameter {
                name: name.lexeme.clone(),
                ty,
                span: Span::new(name.span.start, self.previous_span_end()),
            });

            if self.consume_if(TokenKind::Comma).is_none() {
                break;
            }
        }

        Some(parameters)
    }

    fn parse_type_ref(&mut self) -> Option<TypeRef> {
        if self.consume_if(TokenKind::OpenParen).is_some() {
            self.expect(TokenKind::CloseParen, "expected `)` in unit type `()`")?;
            return Some(TypeRef::Unit);
        }

        let token = self.expect(TokenKind::Identifier, "expected type name")?;
        match token.lexeme.as_str() {
            "bool" => Some(TypeRef::Bool),
            "u32" => Some(TypeRef::U32),
            "u64" => Some(TypeRef::U64),
            "i32" => Some(TypeRef::I32),
            "i64" => Some(TypeRef::I64),
            "f32" => Some(TypeRef::F32),
            "f64" => Some(TypeRef::F64),
            _ => {
                self.report_current(
                    "unknown type name".into(),
                    token.span,
                    format!("unsupported type `{}`", token.lexeme).into(),
                );
                None
            }
        }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        if self.check(TokenKind::Let) {
            return self.parse_let_statement().map(Statement::Let);
        }
        if self.check(TokenKind::Assert) {
            return self.parse_assert_statement().map(Statement::Assert);
        }
        self.parse_expression_statement().map(Statement::Expr)
    }

    fn parse_let_statement(&mut self) -> Option<LetStatement> {
        let let_keyword = self.expect(TokenKind::Let, "expected `let`")?;
        let name = self.expect(TokenKind::Identifier, "expected identifier after `let`")?;
        let ty = if self.consume_if(TokenKind::Colon).is_some() {
            Some(self.parse_type_ref()?)
        } else {
            None
        };
        self.expect(TokenKind::Equals, "expected `=` in let statement")?;
        let value = self.parse_expression()?;
        let semicolon = self.expect(TokenKind::Semicolon, "expected `;` after let statement")?;

        Some(LetStatement {
            name: name.lexeme.clone(),
            ty,
            value,
            span: Span::new(let_keyword.span.start, semicolon.span.end),
        })
    }

    fn parse_assert_statement(&mut self) -> Option<AssertStatement> {
        let assert_keyword = self.expect(TokenKind::Assert, "expected `assert` statement")?;
        self.expect(TokenKind::OpenParen, "expected `(` after `assert`")?;
        let expression = self.parse_expression()?;
        self.expect(
            TokenKind::CloseParen,
            "expected `)` after `assert` expression",
        )?;
        let semicolon = self.expect(TokenKind::Semicolon, "expected `;` after statement")?;

        Some(AssertStatement {
            expression,
            span: Span::new(assert_keyword.span.start, semicolon.span.end),
        })
    }

    fn parse_expression_statement(&mut self) -> Option<ExprStatement> {
        let expression = self.parse_expression()?;
        let semicolon = self.expect(TokenKind::Semicolon, "expected `;` after expression")?;
        Some(ExprStatement {
            span: Span::new(expression.span.start, semicolon.span.end),
            expression,
        })
    }

    fn parse_expression(&mut self) -> Option<Expr> {
        self.parse_additive()
    }

    fn parse_additive(&mut self) -> Option<Expr> {
        let mut left = self.parse_multiplicative()?;
        loop {
            let operator = if self.consume_if(TokenKind::Plus).is_some() {
                Some(BinaryOperator::Add)
            } else if self.consume_if(TokenKind::Minus).is_some() {
                Some(BinaryOperator::Subtract)
            } else {
                None
            };
            let Some(operator) = operator else {
                break;
            };
            let right = self.parse_multiplicative()?;
            let span = Span::new(left.span.start, right.span.end);
            left = Expr::binary(operator, left, right, span);
        }
        Some(left)
    }

    fn parse_multiplicative(&mut self) -> Option<Expr> {
        let mut left = self.parse_unary()?;
        loop {
            let operator = if self.consume_if(TokenKind::Star).is_some() {
                Some(BinaryOperator::Multiply)
            } else if self.consume_if(TokenKind::Slash).is_some() {
                Some(BinaryOperator::Divide)
            } else if self.consume_if(TokenKind::Percent).is_some() {
                Some(BinaryOperator::Modulo)
            } else {
                None
            };
            let Some(operator) = operator else {
                break;
            };
            let right = self.parse_unary()?;
            let span = Span::new(left.span.start, right.span.end);
            left = Expr::binary(operator, left, right, span);
        }
        Some(left)
    }

    fn parse_unary(&mut self) -> Option<Expr> {
        if let Some(bang) = self.consume_if(TokenKind::Bang) {
            let inner = self.parse_unary()?;
            let span = Span::new(bang.span.start, inner.span.end);
            return Some(Expr::negation(inner, span));
        }
        if let Some(minus) = self.consume_if(TokenKind::Minus) {
            let inner = self.parse_unary()?;
            let span = Span::new(minus.span.start, inner.span.end);
            return Some(Expr::unary_minus(inner, span));
        }
        self.parse_call()
    }

    fn parse_call(&mut self) -> Option<Expr> {
        let mut callee = self.parse_primary()?;
        loop {
            if self.consume_if(TokenKind::OpenParen).is_none() {
                break;
            }
            let mut arguments = Vec::new();
            if !self.check(TokenKind::CloseParen) {
                loop {
                    arguments.push(self.parse_expression()?);
                    if self.consume_if(TokenKind::Comma).is_none() {
                        break;
                    }
                }
            }
            let close = self.expect(TokenKind::CloseParen, "expected `)` after call arguments")?;
            let span = Span::new(callee.span.start, close.span.end);
            callee = Expr::call(callee, arguments, span);
        }
        Some(callee)
    }

    fn parse_primary(&mut self) -> Option<Expr> {
        if let Some(token) = self.peek() {
            match token.kind {
                TokenKind::True => {
                    let literal = self.advance().expect("peek guaranteed token");
                    return Some(Expr::bool_literal(true, literal.span));
                }
                TokenKind::False => {
                    let literal = self.advance().expect("peek guaranteed token");
                    return Some(Expr::bool_literal(false, literal.span));
                }
                TokenKind::Number => {
                    let literal = self.advance().expect("peek guaranteed token");
                    return Some(Expr::number_literal(literal.lexeme.clone(), literal.span));
                }
                TokenKind::Identifier => {
                    let name = self.advance().expect("peek guaranteed token");
                    return Some(Expr::identifier(name.lexeme.clone(), name.span));
                }
                TokenKind::OpenParen => {
                    let open = self.advance().expect("peek guaranteed token");
                    let expr = self.parse_expression()?;
                    let close =
                        self.expect(TokenKind::CloseParen, "expected `)` after expression")?;
                    return Some(Expr {
                        span: Span::new(open.span.start, close.span.end),
                        kind: expr.kind,
                    });
                }
                _ => {}
            }
        }

        let token = self.peek()?;
        self.report_current(
            "expected expression".into(),
            token.span,
            format!("found `{}` instead", token.lexeme).into(),
        );
        self.advance();
        None
    }

    fn previous_span_end(&self) -> usize {
        self.tokens
            .get(self.index.saturating_sub(1))
            .map(|token| token.span.end)
            .unwrap_or(0)
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
            if token.kind == TokenKind::Hash || token.kind == TokenKind::Fn {
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
        TokenKind::Let => "`let`",
        TokenKind::Bang => "`!`",
        TokenKind::Plus => "`+`",
        TokenKind::Minus => "`-`",
        TokenKind::Star => "`*`",
        TokenKind::Slash => "`/`",
        TokenKind::Percent => "`%`",
        TokenKind::Equals => "`=`",
        TokenKind::Comma => "`,`",
        TokenKind::Colon => "`:`",
        TokenKind::Arrow => "`->`",
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
        TokenKind::Number => "number literal",
    }
}

#[cfg(test)]
mod tests {
    use super::{BasicParser, Parser};
    use holo_ast::{BinaryExpr, BinaryOperator, ExprKind, Statement, TypeRef};
    use holo_lexer::{BasicLexer, Lexer};

    #[test]
    fn parses_empty_token_stream_to_empty_module() {
        let parser = BasicParser;
        let result = parser.parse_module(&[], "");
        let module = result.module;
        assert!(result.diagnostics.is_empty());
        assert!(module.functions.is_empty());
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

        assert_eq!(module.functions.len(), 1);
        assert_eq!(module.tests.len(), 1);
        assert_eq!(module.tests[0].name, "sample");
        match &module.tests[0].statements[0] {
            Statement::Assert(assertion) => {
                assert!(matches!(assertion.expression.kind, ExprKind::Negation(_)));
            }
            _ => panic!("expected assert statement"),
        }
    }

    #[test]
    fn parses_function_signature_and_let_statement() {
        let source = "fn add(a: i64, b: i64) -> i64 { let result: i64 = a + b; result; }";
        let lexed = BasicLexer.lex(source);
        let parsed = BasicParser.parse_module(&lexed.tokens, source);
        assert!(parsed.diagnostics.is_empty(), "{:?}", parsed.diagnostics);
        let module = parsed.module;
        assert_eq!(module.functions.len(), 1);
        let function = &module.functions[0];
        assert_eq!(function.name, "add");
        assert_eq!(function.parameters.len(), 2);
        assert_eq!(function.parameters[0].ty, TypeRef::I64);
        assert_eq!(function.return_type, TypeRef::I64);
        assert!(matches!(function.statements[0], Statement::Let(_)));
    }

    #[test]
    fn parses_arithmetic_precedence() {
        let source = "fn calc() -> i64 { assert(1 + 2 * 3); }";
        let lexed = BasicLexer.lex(source);
        let parsed = BasicParser.parse_module(&lexed.tokens, source);
        assert!(parsed.diagnostics.is_empty(), "{:?}", parsed.diagnostics);
        let function = &parsed.module.functions[0];
        let Statement::Assert(assertion) = &function.statements[0] else {
            panic!("expected assert statement");
        };
        let ExprKind::Binary(BinaryExpr {
            operator: BinaryOperator::Add,
            right,
            ..
        }) = &assertion.expression.kind
        else {
            panic!("expected additive root binary expression");
        };
        assert!(matches!(
            right.kind,
            ExprKind::Binary(BinaryExpr {
                operator: BinaryOperator::Multiply,
                ..
            })
        ));
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
