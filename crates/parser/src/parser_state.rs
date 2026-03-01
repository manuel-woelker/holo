use holo_ast::{
    AssertStatement, BinaryOperator, Expr, ExprStatement, FunctionItem, FunctionParameter,
    LetStatement, Module, ModuleItem, QualifiedName, Statement, TemplatePart, TypeRef,
};
use holo_base::{DiagnosticKind, SharedString, SourceDiagnostic, SourceExcerpt, SourceFile, Span};
use holo_lexer::{Token, TokenKind};

use crate::parser::ParseResult;

#[derive(Debug)]
pub(crate) struct ParserState<'a> {
    module_name: QualifiedName,
    tokens: &'a [Token],
    source: &'a SourceFile,
    index: usize,
    diagnostics: Vec<SourceDiagnostic>,
}

impl<'a> ParserState<'a> {
    pub(crate) fn new(
        module_name: QualifiedName,
        tokens: &'a [Token],
        source: &'a SourceFile,
    ) -> Self {
        Self {
            module_name,
            tokens,
            source,
            index: 0,
            diagnostics: Vec::new(),
        }
    }

    fn lexeme(&self, token: &Token) -> &str {
        self.source.source_at(token.span)
    }

    pub(crate) fn parse_module(&mut self) -> ParseResult {
        let mut items = Vec::new();
        while self.peek().is_some() {
            if !self.check(TokenKind::Hash) && !self.check(TokenKind::Fn) {
                self.advance();
                continue;
            }

            let previous_index = self.index;
            if let Some(function) = self.parse_item() {
                items.push(ModuleItem::Function(function.clone()));
            } else {
                self.recover_to_next_item();
            }

            if self.index == previous_index {
                self.advance();
            }
        }
        ParseResult {
            module: Module {
                name: self.module_name.clone(),
                items,
            },
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
        if self.lexeme(attribute) != "test" {
            self.report_current(
                format!(
                    "expected `#[test]` attribute, found `#[{}]`",
                    self.lexeme(attribute)
                )
                .into(),
                attribute.span,
                format!("unsupported test attribute `{}`", self.lexeme(attribute)).into(),
                Some("use `#[test]` for executable test functions".into()),
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
            name: self.module_name.join(self.lexeme(name)),
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
                name: self.lexeme(name).into(),
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
        match self.lexeme(token) {
            "bool" => Some(TypeRef::Bool),
            "u32" => Some(TypeRef::U32),
            "u64" => Some(TypeRef::U64),
            "i32" => Some(TypeRef::I32),
            "i64" => Some(TypeRef::I64),
            "f32" => Some(TypeRef::F32),
            "f64" => Some(TypeRef::F64),
            "string" => Some(TypeRef::String),
            _ => {
                self.report_current(
                    "unknown type name".into(),
                    token.span,
                    format!("unsupported type `{}`", self.lexeme(token)).into(),
                    None,
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
            name: self.lexeme(name).into(),
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
        self.parse_equality()
    }

    fn parse_template_string_contents(
        &mut self,
        contents: &str,
        _full_span: Span,
    ) -> Option<Vec<TemplatePart>> {
        let mut parts = Vec::new();
        let mut current_literal = String::new();
        let bytes = contents.as_bytes();
        let mut i = 1; // Skip opening backtick
                       // TODO: Tokenize template string in lexer
        while i < bytes.len() {
            if bytes[i] == b'{' {
                if !current_literal.is_empty() {
                    parts.push(TemplatePart::Literal(current_literal.clone().into()));
                    current_literal.clear();
                }
                let mut depth = 1;
                let start = i + 1;
                i += 1;
                while i < bytes.len() && depth > 0 {
                    if bytes[i] == b'{' {
                        depth += 1;
                    } else if bytes[i] == b'}' {
                        depth -= 1;
                    }
                    i += 1;
                }
                let end = i - 1;
                let expr_source = &contents[start..end];
                use holo_lexer::Lexer as _;
                let lexer = holo_lexer::BasicLexer;
                let result = lexer.lex(expr_source);
                let tokens: Vec<_> = result
                    .tokens
                    .into_iter()
                    .map(|t| Token {
                        kind: t.kind,
                        span: Span::new(0, 0),
                    })
                    .collect();
                let expr_source_file = SourceFile::new(expr_source, "");
                let mut parser =
                    ParserState::new(QualifiedName::new(vec![]), &tokens, &expr_source_file);
                if let Some(expr) = parser.parse_expression() {
                    parts.push(TemplatePart::Expression(expr));
                }
            } else if bytes[i] == b'\\' && i + 1 < bytes.len() {
                let escaped = bytes[i + 1];
                let replacement = match escaped {
                    b'n' => '\n',
                    b'r' => '\r',
                    b't' => '\t',
                    b'\\' => '\\',
                    b'`' => '`',
                    b'0' => '\0',
                    b'b' => '\x08',
                    b'f' => '\x0c',
                    b'v' => '\x0b',
                    b'$' => '$',
                    _ => {
                        current_literal.push('\\');
                        bytes[i + 1] as char
                    }
                };
                current_literal.push(replacement);
                i += 2;
            } else if bytes[i] != b'`' {
                current_literal.push(bytes[i] as char);
                i += 1;
            } else {
                i += 1;
            }
        }

        if !current_literal.is_empty() {
            parts.push(TemplatePart::Literal(current_literal.into()));
        }

        if parts.is_empty() {
            parts.push(TemplatePart::Literal("".into()));
        }

        Some(parts)
    }

    fn parse_equality(&mut self) -> Option<Expr> {
        let mut left = self.parse_comparison()?;
        loop {
            let operator = if self.consume_if(TokenKind::DoubleEquals).is_some() {
                Some(BinaryOperator::Equals)
            } else if self.consume_if(TokenKind::BangEquals).is_some() {
                Some(BinaryOperator::NotEquals)
            } else {
                None
            };
            let Some(operator) = operator else {
                break;
            };
            let right = self.parse_comparison()?;
            let span = Span::new(left.span.start, right.span.end);
            left = Expr::binary(operator, left, right, span);
        }
        Some(left)
    }

    fn parse_comparison(&mut self) -> Option<Expr> {
        let mut left = self.parse_additive()?;
        loop {
            let operator = if self.consume_if(TokenKind::LessThan).is_some() {
                Some(BinaryOperator::LessThan)
            } else if self.consume_if(TokenKind::GreaterThan).is_some() {
                Some(BinaryOperator::GreaterThan)
            } else if self.consume_if(TokenKind::LessThanEquals).is_some() {
                Some(BinaryOperator::LessThanOrEqual)
            } else if self.consume_if(TokenKind::GreaterThanEquals).is_some() {
                Some(BinaryOperator::GreaterThanOrEqual)
            } else {
                None
            };
            let Some(operator) = operator else {
                break;
            };
            let right = self.parse_additive()?;
            let span = Span::new(left.span.start, right.span.end);
            left = Expr::binary(operator, left, right, span);
        }
        Some(left)
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
        if self.check(TokenKind::If) {
            return self.parse_if_expression();
        }
        if self.check(TokenKind::While) {
            return self.parse_while_expression();
        }

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
                    return Some(Expr::number_literal(self.lexeme(literal), literal.span));
                }
                TokenKind::StringLiteral => {
                    let literal = self.advance().expect("peek guaranteed token");
                    return Some(Expr::string_literal(self.lexeme(literal), literal.span));
                }
                TokenKind::TemplateString => {
                    let token = self.advance().expect("peek guaranteed token");
                    let lexeme = self.lexeme(token).to_string();
                    let span = token.span;
                    let parts = self.parse_template_string_contents(&lexeme, span)?;
                    return Some(Expr::template_string(parts, span));
                }
                TokenKind::Identifier => {
                    let name = self.advance().expect("peek guaranteed token");
                    return Some(Expr::identifier(
                        QualifiedName::from_ident(self.lexeme(name).into()),
                        name.span,
                    ));
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
                TokenKind::OpenBrace => {
                    let open = self.advance().expect("peek guaranteed token");
                    return self.parse_block_expression_after_open(open.span.start);
                }
                _ => {}
            }
        }

        let token = self.peek()?;
        self.report_current(
            "expected expression".into(),
            token.span,
            format!("found `{}` instead", self.lexeme(token)).into(),
            None,
        );
        self.advance();
        None
    }

    fn parse_if_expression(&mut self) -> Option<Expr> {
        let if_keyword = self.expect(TokenKind::If, "expected `if`")?;
        let condition = self.parse_expression()?;
        let then_open = self.expect(TokenKind::OpenBrace, "expected `{` after if condition")?;
        let then_branch = self.parse_block_expression_after_open(then_open.span.start)?;
        let else_branch = if self.consume_if(TokenKind::Else).is_some() {
            let else_open = self.expect(TokenKind::OpenBrace, "expected `{` after `else`")?;
            Some(self.parse_block_expression_after_open(else_open.span.start)?)
        } else {
            None
        };
        let end = else_branch
            .as_ref()
            .map(|expr| expr.span.end)
            .unwrap_or(then_branch.span.end);
        Some(Expr::if_expression(
            condition,
            then_branch,
            else_branch,
            Span::new(if_keyword.span.start, end),
        ))
    }

    fn parse_while_expression(&mut self) -> Option<Expr> {
        let while_keyword = self.expect(TokenKind::While, "expected `while`")?;
        let condition = self.parse_expression()?;
        let body_open = self.expect(TokenKind::OpenBrace, "expected `{` after while condition")?;
        let body = self.parse_block_expression_after_open(body_open.span.start)?;
        let end = body.span.end;
        Some(Expr::while_expression(
            condition,
            body,
            Span::new(while_keyword.span.start, end),
        ))
    }

    fn parse_block_expression_after_open(&mut self, start: usize) -> Option<Expr> {
        let mut statements = Vec::new();
        let mut result = None;

        while self.peek().is_some() && !self.check(TokenKind::CloseBrace) {
            if self.check(TokenKind::Let) {
                statements.push(Statement::Let(self.parse_let_statement()?));
                continue;
            }
            if self.check(TokenKind::Assert) {
                statements.push(Statement::Assert(self.parse_assert_statement()?));
                continue;
            }

            let expression = self.parse_expression()?;
            if let Some(semicolon) = self.consume_if(TokenKind::Semicolon) {
                statements.push(Statement::Expr(ExprStatement {
                    span: Span::new(expression.span.start, semicolon.span.end),
                    expression,
                }));
                continue;
            }

            if self.check(TokenKind::CloseBrace) {
                result = Some(expression);
                break;
            }

            let token = self.peek()?;
            self.report_current(
                "expected `;` after expression in block".into(),
                token.span,
                format!("found `{}` instead", self.lexeme(token)).into(),
                Some("add `;` or make this the trailing block expression".into()),
            );
            self.recover_statement();
        }

        let close_brace =
            self.expect(TokenKind::CloseBrace, "expected `}` after block expression")?;
        Some(Expr::block(
            statements,
            result,
            Span::new(start, close_brace.span.end),
        ))
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
                Some("add the missing token to complete this construct".into()),
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
                    self.lexeme(token)
                )
                .into(),
                None,
            );
            return None;
        }

        Some(token)
    }

    fn report_current(
        &mut self,
        message: SharedString,
        span: Span,
        annotation: SharedString,
        hint: Option<SharedString>,
    ) {
        let mut diagnostic = SourceDiagnostic::new(DiagnosticKind::Parsing, message)
            .with_error_code("P1000")
            .with_annotated_span(span, annotation)
            .with_source_excerpt(SourceExcerpt::new(&self.source.source, 1, 0));
        if let Some(hint) = hint {
            diagnostic = diagnostic.with_hint(hint);
        }
        self.diagnostics.push(diagnostic);
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
        TokenKind::If => "`if`",
        TokenKind::Else => "`else`",
        TokenKind::While => "`while`",
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
        TokenKind::StringLiteral => "string literal",
        TokenKind::TemplateString => "template string",
        TokenKind::DoubleEquals => "`==`",
        TokenKind::BangEquals => "`!=`",
        TokenKind::LessThan => "`<`",
        TokenKind::GreaterThan => "`>`",
        TokenKind::LessThanEquals => "`<=`",
        TokenKind::GreaterThanEquals => "`>=`",
    }
}
