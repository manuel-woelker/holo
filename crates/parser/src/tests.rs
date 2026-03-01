use crate::Parser;
use holo_ast::{expression::BinaryExpr, BinaryOperator, ExprKind, ModuleItem, Statement, TypeRef};
use holo_base::SourceFile;
use holo_lexer::{BasicLexer, Lexer};

#[test]
fn parses_empty_token_stream_to_empty_module() {
    let parser = Parser;
    let source_file = SourceFile::new("", "test.holo");
    let result = parser.parse_module(&[], &source_file);
    let module = result.module;
    assert!(result.diagnostics.is_empty());
    assert!(module.items.is_empty());
}

#[test]
fn parses_test_with_assertion_and_negation() {
    let source = "#[test] fn sample() { assert(!false); }";
    let lexed = BasicLexer.lex(source);
    assert!(lexed.diagnostics.is_empty());
    let source_file = SourceFile::new(source, "test.holo");
    let parsed = Parser.parse_module(&lexed.tokens, &source_file);
    assert!(parsed.diagnostics.is_empty());
    let module = parsed.module;

    let mut functions = 0;
    let mut tests = 0;
    for item in &module.items {
        match item {
            ModuleItem::Function(function) => {
                functions += 1;
                if function.is_test {
                    tests += 1;
                }
            }
        }
    }
    assert_eq!(functions, 1);
    assert_eq!(tests, 1);
}

#[test]
fn parses_function_signature_and_let_statement() {
    let source = "fn add(a: i64, b: i64) -> i64 { let result: i64 = a + b; result; }";
    let lexed = BasicLexer.lex(source);
    let source_file = SourceFile::new(source, "test.holo");
    let parsed = Parser.parse_module(&lexed.tokens, &source_file);
    assert!(parsed.diagnostics.is_empty(), "{:?}", parsed.diagnostics);
    let module = parsed.module;
    assert_eq!(module.items.len(), 1);
    let func = &module.items[0];
    if let ModuleItem::Function(function) = func {
        assert_eq!(function.name.to_string(), "add");
        assert_eq!(function.parameters.len(), 2);
        assert_eq!(function.parameters[0].ty, TypeRef::I64);
        assert_eq!(function.return_type, TypeRef::I64);
        assert!(matches!(function.statements[0], Statement::Let(_)));
    } else {
        panic!("Expected Function variant");
    }
}

#[test]
fn parses_arithmetic_precedence() {
    let source = "fn calc() -> i64 { assert(1 + 2 * 3); }";
    let lexed = BasicLexer.lex(source);
    let source_file = SourceFile::new(source, "test.holo");
    let parsed = Parser.parse_module(&lexed.tokens, &source_file);
    assert!(parsed.diagnostics.is_empty(), "{:?}", parsed.diagnostics);
    let func = &parsed.module.items[0];
    let function = match func {
        ModuleItem::Function(f) => f,
        _ => panic!("Expected Function variant"),
    };
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
    let source_file = SourceFile::new(source, "test.holo");
    let result = Parser.parse_module(&lexed.tokens, &source_file);
    assert!(
        result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("expected `#[test]`")),
        "actual diagnostics: {:?}",
        result.diagnostics
    );
    assert_eq!(result.diagnostics[0].error_code.as_deref(), Some("P1000"));
    assert!(result.diagnostics[0].hint.is_some());
}

#[test]
fn continues_after_statement_error() {
    let source = "#[test] fn sample() { assert(); assert(true); }";
    let lexed = BasicLexer.lex(source);
    let source_file = SourceFile::new(source, "test.holo");
    let result = Parser.parse_module(&lexed.tokens, &source_file);
    assert!(!result.diagnostics.is_empty());
    assert_eq!(result.module.items.len(), 1);
    if let ModuleItem::Function(t) = &result.module.items[0] {
        assert_eq!(t.name.to_string(), "sample");
        assert_eq!(t.statements.len(), 1);
    } else {
        panic!("Expected Test variant");
    }
}

#[test]
fn parses_numeric_suffix_literals_and_arithmetic_operators() {
    let cases = vec![
        "fn f() -> i64 { 1i64 + 2i64; }",
        "fn f() -> u32 { 1u32 % 1u32; }",
        "fn f() -> f64 { 1.0f64 / 2.0f64; }",
        "fn f() -> f32 { -1.0f32 * 3.0f32; }",
    ];

    for source in cases {
        let lexed = BasicLexer.lex(source);
        assert!(lexed.diagnostics.is_empty(), "{source}");
        let source_file = SourceFile::new(source, "test.holo");
        let parsed = Parser.parse_module(&lexed.tokens, &source_file);
        assert!(
            parsed.diagnostics.is_empty(),
            "{source}: {:?}",
            parsed.diagnostics
        );
        assert_eq!(parsed.module.items.len(), 1, "{source}");
    }
}

#[test]
fn recovers_to_next_top_level_definition_after_broken_function() {
    let source = "fn broken(a i64) -> i64 { a + ; } #[test] fn ok() { assert(true); }";
    let lexed = BasicLexer.lex(source);
    let source_file = SourceFile::new(source, "test.holo");
    let result = Parser.parse_module(&lexed.tokens, &source_file);

    assert!(!result.diagnostics.is_empty());
    assert_eq!(result.module.items.len(), 1);
    if let ModuleItem::Function(t) = &result.module.items[0] {
        assert_eq!(t.name.to_string(), "ok");
    } else {
        panic!("Expected Test variant");
    }
}

#[test]
fn parses_if_else_and_block_result_expression() {
    let source = "fn pick() -> i64 { if true { let a: i64 = 1i64; a } else { 2i64 }; }";
    let lexed = BasicLexer.lex(source);
    let source_file = SourceFile::new(source, "test.holo");
    let parsed = Parser.parse_module(&lexed.tokens, &source_file);
    assert!(parsed.diagnostics.is_empty(), "{:?}", parsed.diagnostics);
    let func = &parsed.module.items[0];
    let function = match func {
        ModuleItem::Function(f) => f,
        _ => panic!("Expected Function variant"),
    };
    let Statement::Expr(expr_statement) = &function.statements[0] else {
        panic!("expected expression statement");
    };
    let ExprKind::If(if_expression) = &expr_statement.expression.kind else {
        panic!("expected if expression");
    };
    assert!(matches!(if_expression.then_branch.kind, ExprKind::Block(_)));
    assert!(if_expression.else_branch.is_some());
}

#[test]
fn parses_while_expression_statement() {
    let source = "fn loop_test() -> () { while false { assert(true); }; }";
    let lexed = BasicLexer.lex(source);
    let source_file = SourceFile::new(source, "test.holo");
    let parsed = Parser.parse_module(&lexed.tokens, &source_file);
    assert!(parsed.diagnostics.is_empty(), "{:?}", parsed.diagnostics);
    let func = &parsed.module.items[0];
    let function = match func {
        ModuleItem::Function(f) => f,
        _ => panic!("Expected Function variant"),
    };
    let Statement::Expr(expr_statement) = &function.statements[0] else {
        panic!("expected expression statement");
    };
    assert!(matches!(expr_statement.expression.kind, ExprKind::While(_)));
}
