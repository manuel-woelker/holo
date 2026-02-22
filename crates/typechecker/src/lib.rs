//! Typechecking interfaces and the minimal boolean typechecker.

use std::collections::{HashMap, HashSet};

use holo_ast::{BinaryOperator, Expr, ExprKind, Module, Statement, TypeRef};
use holo_base::{
    DiagnosticKind, SharedString, SourceDiagnostic, SourceExcerpt, TaskTimer, TaskTiming,
};
use tracing::info;

/// Type representation for the minimal language.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Bool,
    U32,
    U64,
    I32,
    I64,
    F32,
    F64,
    Unit,
    Unknown,
}

/// Function type representation used during typechecking.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    /// Parameter types in declaration order.
    pub parameter_types: Vec<Type>,
    /// Return type.
    pub return_type: Type,
}

/// Summary result returned by module typechecking.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypecheckSummary {
    /// Number of tests that were typechecked.
    pub test_count: usize,
    /// Number of assertion statements validated.
    pub assertion_count: usize,
}

/// Result payload produced by typechecking.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypecheckResult {
    /// Typechecking summary for the module.
    pub summary: TypecheckSummary,
    /// Typechecking diagnostics encountered while validating the module.
    pub diagnostics: Vec<SourceDiagnostic>,
    /// Per-test typechecking timings.
    pub timings: Vec<TaskTiming>,
    /// Function signatures discovered in the module.
    pub function_types: HashMap<holo_base::SharedString, FunctionType>,
}

/// Typechecking abstraction used by the core compiler crate.
pub trait Typechecker {
    /// Validates semantic and type rules for a module.
    fn typecheck_module(&self, module: &Module, source: &str) -> TypecheckResult;
}

/// Minimal typechecker for boolean expressions and assert statements.
#[derive(Debug, Default)]
pub struct BasicTypechecker;

impl BasicTypechecker {
    fn type_name(ty: Type) -> &'static str {
        match ty {
            Type::Bool => "bool",
            Type::U32 => "u32",
            Type::U64 => "u64",
            Type::I32 => "i32",
            Type::I64 => "i64",
            Type::F32 => "f32",
            Type::F64 => "f64",
            Type::Unit => "()",
            Type::Unknown => "<unknown>",
        }
    }

    fn type_from_ref(type_ref: TypeRef) -> Type {
        match type_ref {
            TypeRef::Bool => Type::Bool,
            TypeRef::U32 => Type::U32,
            TypeRef::U64 => Type::U64,
            TypeRef::I32 => Type::I32,
            TypeRef::I64 => Type::I64,
            TypeRef::F32 => Type::F32,
            TypeRef::F64 => Type::F64,
            TypeRef::Unit => Type::Unit,
        }
    }

    fn infer_number_literal_type(literal: &str) -> Type {
        if literal.ends_with("u32") {
            Type::U32
        } else if literal.ends_with("u64") {
            Type::U64
        } else if literal.ends_with("i32") {
            Type::I32
        } else if literal.ends_with("i64") {
            Type::I64
        } else if literal.ends_with("f32") {
            Type::F32
        } else if literal.ends_with("f64") || literal.contains('.') {
            Type::F64
        } else {
            Type::I64
        }
    }

    fn is_integer_type(ty: Type) -> bool {
        matches!(ty, Type::U32 | Type::U64 | Type::I32 | Type::I64)
    }

    fn is_numeric_type(ty: Type) -> bool {
        Self::is_integer_type(ty) || matches!(ty, Type::F32 | Type::F64)
    }

    fn check_same_type(
        diagnostics: &mut Vec<SourceDiagnostic>,
        source: &str,
        left: Type,
        left_span: holo_base::Span,
        right: Type,
        right_span: holo_base::Span,
        message: &'static str,
    ) -> bool {
        if left == Type::Unknown || right == Type::Unknown {
            return false;
        }
        if left == right {
            return true;
        }
        diagnostics.push(
            SourceDiagnostic::new(DiagnosticKind::Typecheck, message)
                .with_annotated_span(
                    left_span,
                    format!("left operand has type `{}`", Self::type_name(left)),
                )
                .with_annotated_span(
                    right_span,
                    format!("right operand has type `{}`", Self::type_name(right)),
                )
                .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
        );
        false
    }

    fn check_same_arithmetic_operand_type(
        diagnostics: &mut Vec<SourceDiagnostic>,
        source: &str,
        left: Type,
        left_span: holo_base::Span,
        right: Type,
        right_span: holo_base::Span,
    ) -> bool {
        if left == Type::Unknown || right == Type::Unknown {
            return false;
        }
        if left == right {
            return true;
        }

        diagnostics.push(
            SourceDiagnostic::new(
                DiagnosticKind::Typecheck,
                "arithmetic operands must have the same type",
            )
            .with_source_excerpt_annotations(
                SourceExcerpt::new(source, 1, 0),
                [
                    (
                        left_span,
                        format!("left operand has type `{}`", Self::type_name(left)),
                    ),
                    (
                        right_span,
                        format!("right operand has type `{}`", Self::type_name(right)),
                    ),
                ],
            ),
        );
        false
    }

    fn typecheck_statement(
        statement: &Statement,
        diagnostics: &mut Vec<SourceDiagnostic>,
        source: &str,
        function_types: &HashMap<SharedString, FunctionType>,
        function_spans: &HashMap<SharedString, holo_base::Span>,
        locals: &mut HashMap<SharedString, Type>,
        assertion_count: &mut usize,
    ) {
        match statement {
            Statement::Assert(assertion) => {
                let expression_type = Self::typecheck_expression(
                    &assertion.expression,
                    diagnostics,
                    source,
                    function_types,
                    function_spans,
                    locals,
                );
                if expression_type != Type::Bool {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            "assert expects a boolean expression",
                        )
                        .with_annotated_span(
                            assertion.span,
                            "this assertion does not evaluate to `bool`",
                        )
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                    return;
                }

                *assertion_count += 1;
            }
            Statement::Let(let_statement) => {
                let value_type = Self::typecheck_expression(
                    &let_statement.value,
                    diagnostics,
                    source,
                    function_types,
                    function_spans,
                    locals,
                );
                if locals.contains_key(&let_statement.name) {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            format!("duplicate local binding `{}`", let_statement.name),
                        )
                        .with_annotated_span(
                            let_statement.span,
                            "this binding name is already defined in this scope",
                        )
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                    return;
                }
                let declared_type = let_statement.ty.map(Self::type_from_ref);
                let final_type = if let Some(declared_type) = declared_type {
                    if value_type != Type::Unknown
                        && !Self::check_same_type(
                            diagnostics,
                            source,
                            declared_type,
                            let_statement.span,
                            value_type,
                            let_statement.value.span,
                            "let binding type does not match initializer type",
                        )
                    {
                        Type::Unknown
                    } else {
                        declared_type
                    }
                } else {
                    value_type
                };
                locals.insert(let_statement.name.clone(), final_type);
            }
            Statement::Expr(expr_statement) => {
                let _ = Self::typecheck_expression(
                    &expr_statement.expression,
                    diagnostics,
                    source,
                    function_types,
                    function_spans,
                    locals,
                );
            }
        }
    }

    fn typecheck_expression(
        expression: &Expr,
        diagnostics: &mut Vec<SourceDiagnostic>,
        source: &str,
        function_types: &HashMap<SharedString, FunctionType>,
        function_spans: &HashMap<SharedString, holo_base::Span>,
        locals: &HashMap<SharedString, Type>,
    ) -> Type {
        match &expression.kind {
            ExprKind::BoolLiteral(_) => Type::Bool,
            ExprKind::NumberLiteral(literal) => Self::infer_number_literal_type(literal),
            ExprKind::Identifier(name) => {
                if let Some(ty) = locals.get(name) {
                    return *ty;
                }
                diagnostics.push(
                    SourceDiagnostic::new(
                        DiagnosticKind::Typecheck,
                        format!("unknown identifier `{name}`"),
                    )
                    .with_annotated_span(expression.span, "this name is not defined in scope")
                    .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                );
                Type::Unknown
            }
            ExprKind::Negation(inner) => {
                let inner_type = Self::typecheck_expression(
                    inner,
                    diagnostics,
                    source,
                    function_types,
                    function_spans,
                    locals,
                );
                if inner_type != Type::Bool && inner_type != Type::Unknown {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            "operator `!` expects a boolean operand",
                        )
                        .with_annotated_span(inner.span, "this operand is not `bool`")
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                }
                Type::Bool
            }
            ExprKind::UnaryMinus(inner) => {
                let inner_type = Self::typecheck_expression(
                    inner,
                    diagnostics,
                    source,
                    function_types,
                    function_spans,
                    locals,
                );
                if matches!(inner_type, Type::I32 | Type::I64 | Type::F32 | Type::F64) {
                    inner_type
                } else {
                    if inner_type != Type::Unknown {
                        diagnostics.push(
                            SourceDiagnostic::new(
                                DiagnosticKind::Typecheck,
                                "operator `-` expects a signed numeric operand",
                            )
                            .with_annotated_span(inner.span, "this operand is not signed numeric")
                            .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                        );
                    }
                    Type::Unknown
                }
            }
            ExprKind::Binary(binary) => {
                let left_type = Self::typecheck_expression(
                    &binary.left,
                    diagnostics,
                    source,
                    function_types,
                    function_spans,
                    locals,
                );
                let right_type = Self::typecheck_expression(
                    &binary.right,
                    diagnostics,
                    source,
                    function_types,
                    function_spans,
                    locals,
                );
                if left_type == Type::Unknown || right_type == Type::Unknown {
                    return Type::Unknown;
                }

                if !Self::is_numeric_type(left_type) || !Self::is_numeric_type(right_type) {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            "arithmetic operators require numeric operands",
                        )
                        .with_annotated_span(
                            binary.left.span,
                            format!("left operand has type `{}`", Self::type_name(left_type)),
                        )
                        .with_annotated_span(
                            binary.right.span,
                            format!("right operand has type `{}`", Self::type_name(right_type)),
                        )
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                    return Type::Unknown;
                }

                if !Self::check_same_arithmetic_operand_type(
                    diagnostics,
                    source,
                    left_type,
                    binary.left.span,
                    right_type,
                    binary.right.span,
                ) {
                    return Type::Unknown;
                }

                if binary.operator == BinaryOperator::Modulo && !Self::is_integer_type(left_type) {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            "operator `%` is only valid for integer types",
                        )
                        .with_annotated_span(
                            expression.span,
                            format!(
                                "operands have type `{}` but `%` requires integer types",
                                Self::type_name(left_type)
                            ),
                        )
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                    return Type::Unknown;
                }

                left_type
            }
            ExprKind::Call(call) => {
                let ExprKind::Identifier(callee_name) = &call.callee.kind else {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            "call target must be a function name",
                        )
                        .with_annotated_span(call.callee.span, "this expression is not callable")
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                    return Type::Unknown;
                };

                let Some(function_type) = function_types.get(callee_name) else {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            format!("unknown function `{callee_name}`"),
                        )
                        .with_annotated_span(call.callee.span, "this function is not defined")
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                    for argument in &call.arguments {
                        let _ = Self::typecheck_expression(
                            argument,
                            diagnostics,
                            source,
                            function_types,
                            function_spans,
                            locals,
                        );
                    }
                    return Type::Unknown;
                };

                if call.arguments.len() != function_type.parameter_types.len() {
                    let mut diagnostic = SourceDiagnostic::new(
                        DiagnosticKind::Typecheck,
                        format!(
                            "function `{callee_name}` expects {} argument(s) but got {}",
                            function_type.parameter_types.len(),
                            call.arguments.len()
                        ),
                    )
                    .with_annotated_span(
                        expression.span,
                        "call argument count does not match function signature",
                    );
                    if let Some(definition_span) = function_spans.get(callee_name) {
                        diagnostic = diagnostic.with_annotated_span(
                            *definition_span,
                            format!("function `{callee_name}` is defined here"),
                        );
                    }
                    diagnostics
                        .push(diagnostic.with_source_excerpt(SourceExcerpt::new(source, 1, 0)));
                    for argument in &call.arguments {
                        let _ = Self::typecheck_expression(
                            argument,
                            diagnostics,
                            source,
                            function_types,
                            function_spans,
                            locals,
                        );
                    }
                    return function_type.return_type;
                }

                for (index, argument) in call.arguments.iter().enumerate() {
                    let argument_type = Self::typecheck_expression(
                        argument,
                        diagnostics,
                        source,
                        function_types,
                        function_spans,
                        locals,
                    );
                    let expected_type = function_type.parameter_types[index];
                    if argument_type == Type::Unknown {
                        continue;
                    }
                    let _ = Self::check_same_type(
                        diagnostics,
                        source,
                        expected_type,
                        argument.span,
                        argument_type,
                        argument.span,
                        "call argument type does not match parameter type",
                    );
                }

                function_type.return_type
            }
        }
    }
}

impl Typechecker for BasicTypechecker {
    fn typecheck_module(&self, module: &Module, source: &str) -> TypecheckResult {
        let mut seen_test_names = HashMap::new();
        let mut assertion_count = 0usize;
        let mut diagnostics = Vec::new();
        let mut timings = Vec::new();
        let mut function_types = HashMap::new();
        let mut function_spans = HashMap::new();
        let mut seen_function_names = HashMap::new();
        for function in &module.functions {
            if let Some(first_span) = seen_function_names.get(function.name.as_str()).copied() {
                diagnostics.push(
                    SourceDiagnostic::new(
                        DiagnosticKind::Typecheck,
                        format!("duplicate function name `{}` in module", function.name),
                    )
                    .with_annotated_span(first_span, "first declaration")
                    .with_annotated_span(function.span, "duplicate declaration")
                    .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                );
            } else {
                seen_function_names.insert(function.name.clone(), function.span);
            }

            function_types.insert(
                function.name.clone(),
                FunctionType {
                    parameter_types: function
                        .parameters
                        .iter()
                        .map(|parameter| Self::type_from_ref(parameter.ty))
                        .collect(),
                    return_type: Self::type_from_ref(function.return_type),
                },
            );
            function_spans.insert(function.name.clone(), function.span);
        }

        for function in &module.functions {
            let mut parameter_names = HashSet::new();
            let mut locals = HashMap::new();
            for parameter in &function.parameters {
                if !parameter_names.insert(parameter.name.clone()) {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            format!(
                                "duplicate parameter name `{}` in function `{}`",
                                parameter.name, function.name
                            ),
                        )
                        .with_annotated_span(
                            parameter.span,
                            "this parameter name is already declared",
                        )
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                    continue;
                }
                locals.insert(parameter.name.clone(), Self::type_from_ref(parameter.ty));
            }

            for statement in &function.statements {
                Self::typecheck_statement(
                    statement,
                    &mut diagnostics,
                    source,
                    &function_types,
                    &function_spans,
                    &mut locals,
                    &mut assertion_count,
                );
            }
        }

        for test in &module.tests {
            let timer = TaskTimer::start(format!("typecheck test `{}`", test.name));
            if let Some(first_span) = seen_test_names.get(test.name.as_str()).copied() {
                diagnostics.push(
                    SourceDiagnostic::new(
                        DiagnosticKind::Typecheck,
                        format!("duplicate test function name `{}` in module", test.name),
                    )
                    .with_annotated_span(first_span, "first declaration of this test name")
                    .with_annotated_span(test.span, "duplicate declaration")
                    .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                );
            } else {
                seen_test_names.insert(test.name.clone(), test.span);
            }

            let mut locals = HashMap::new();
            for statement in &test.statements {
                Self::typecheck_statement(
                    statement,
                    &mut diagnostics,
                    source,
                    &function_types,
                    &function_spans,
                    &mut locals,
                    &mut assertion_count,
                );
            }

            let timing = timer.finish();
            timings.push(timing.clone());
            info!(
                test_name = %test.name,
                stage = "typecheck_test",
                elapsed_ms = timing.elapsed.as_secs_f64() * 1000.0,
                "test timing"
            );
        }

        TypecheckResult {
            summary: TypecheckSummary {
                test_count: module.tests.len(),
                assertion_count,
            },
            diagnostics,
            timings,
            function_types,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{BasicTypechecker, Typechecker};
    use holo_ast::{
        AssertStatement, BinaryOperator, Expr, FunctionItem, FunctionParameter, Module, Statement,
        TestItem, TypeRef,
    };
    use holo_base::Span;

    #[test]
    fn typechecks_assertion_count_for_module() {
        let module = Module {
            functions: Vec::new(),
            tests: vec![TestItem {
                name: "sample".into(),
                statements: vec![Statement::Assert(AssertStatement {
                    expression: Expr::bool_literal(true, Span::new(20, 24)),
                    span: Span::new(13, 25),
                })],
                span: Span::new(0, 26),
            }],
        };

        let result = BasicTypechecker.typecheck_module(&module, "#[test] fn sample() { ... }");
        assert_eq!(result.summary.test_count, 1);
        assert_eq!(result.summary.assertion_count, 1);
        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn reports_duplicate_test_names_and_continues() {
        let module = Module {
            functions: Vec::new(),
            tests: vec![
                TestItem {
                    name: "same_name".into(),
                    statements: vec![Statement::Assert(AssertStatement {
                        expression: Expr::bool_literal(true, Span::new(20, 24)),
                        span: Span::new(13, 25),
                    })],
                    span: Span::new(0, 26),
                },
                TestItem {
                    name: "same_name".into(),
                    statements: vec![Statement::Assert(AssertStatement {
                        expression: Expr::bool_literal(true, Span::new(50, 54)),
                        span: Span::new(43, 55),
                    })],
                    span: Span::new(30, 56),
                },
            ],
        };

        let result = BasicTypechecker.typecheck_module(&module, "#[test] fn same_name() { ... }");
        assert_eq!(result.summary.test_count, 2);
        assert_eq!(result.summary.assertion_count, 2);
        assert_eq!(result.diagnostics.len(), 1);
        assert!(result.diagnostics[0]
            .message
            .contains("duplicate test function name"));
    }

    #[test]
    fn infers_default_numeric_literal_types() {
        let module = Module {
            functions: Vec::new(),
            tests: vec![TestItem {
                name: "numeric".into(),
                statements: vec![Statement::Assert(AssertStatement {
                    expression: Expr::binary(
                        BinaryOperator::Add,
                        Expr::number_literal("1", Span::new(0, 1)),
                        Expr::number_literal("2", Span::new(4, 5)),
                        Span::new(0, 5),
                    ),
                    span: Span::new(0, 6),
                })],
                span: Span::new(0, 6),
            }],
        };

        let result = BasicTypechecker.typecheck_module(&module, "assert(1 + 2);");
        assert!(!result.diagnostics.is_empty());
        assert!(result.diagnostics.iter().any(|diagnostic| diagnostic
            .message
            .contains("assert expects a boolean expression")));
    }

    #[test]
    fn rejects_mixed_numeric_types_in_binary_ops() {
        let module = Module {
            functions: Vec::new(),
            tests: vec![TestItem {
                name: "mixed".into(),
                statements: vec![Statement::Assert(AssertStatement {
                    expression: Expr::binary(
                        BinaryOperator::Add,
                        Expr::number_literal("1i64", Span::new(0, 4)),
                        Expr::number_literal("2.0f64", Span::new(7, 13)),
                        Span::new(0, 13),
                    ),
                    span: Span::new(0, 14),
                })],
                span: Span::new(0, 14),
            }],
        };

        let result = BasicTypechecker.typecheck_module(&module, "assert(1i64 + 2.0f64);");
        let mismatch = result
            .diagnostics
            .iter()
            .find(|diagnostic| diagnostic.message.contains("same type"))
            .expect("expected arithmetic same-type diagnostic");
        assert_eq!(mismatch.source_excerpts.len(), 1);
        assert_eq!(mismatch.annotated_spans.len(), 2);
        let rendered = holo_base::display_source_diagnostics(&result.diagnostics);
        assert!(
            rendered.contains("left operand has type `i64`"),
            "{rendered}"
        );
        assert!(
            rendered.contains("right operand has type `f64`"),
            "{rendered}"
        );
    }

    #[test]
    fn reports_unknown_identifier() {
        let module = Module {
            functions: vec![FunctionItem {
                name: "entry".into(),
                parameters: Vec::new(),
                return_type: TypeRef::Unit,
                statements: vec![Statement::Expr(holo_ast::ExprStatement {
                    expression: Expr::identifier("missing", Span::new(0, 7)),
                    span: Span::new(0, 8),
                })],
                is_test: false,
                span: Span::new(0, 8),
            }],
            tests: Vec::new(),
        };

        let result = BasicTypechecker.typecheck_module(&module, "missing;");
        assert!(result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("unknown identifier")));
    }

    #[test]
    fn reports_unknown_function_call() {
        let module = Module {
            functions: vec![FunctionItem {
                name: "entry".into(),
                parameters: Vec::new(),
                return_type: TypeRef::Unit,
                statements: vec![Statement::Expr(holo_ast::ExprStatement {
                    expression: Expr::call(
                        Expr::identifier("unknown_fn", Span::new(0, 10)),
                        vec![],
                        Span::new(0, 12),
                    ),
                    span: Span::new(0, 13),
                })],
                is_test: false,
                span: Span::new(0, 13),
            }],
            tests: Vec::new(),
        };

        let result = BasicTypechecker.typecheck_module(&module, "unknown_fn();");
        assert!(result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("unknown function")));
    }

    #[test]
    fn reports_call_arity_mismatch() {
        let module = Module {
            functions: vec![
                FunctionItem {
                    name: "sum".into(),
                    parameters: vec![
                        FunctionParameter {
                            name: "a".into(),
                            ty: TypeRef::I64,
                            span: Span::new(0, 1),
                        },
                        FunctionParameter {
                            name: "b".into(),
                            ty: TypeRef::I64,
                            span: Span::new(3, 4),
                        },
                    ],
                    return_type: TypeRef::I64,
                    statements: Vec::new(),
                    is_test: false,
                    span: Span::new(0, 10),
                },
                FunctionItem {
                    name: "entry".into(),
                    parameters: Vec::new(),
                    return_type: TypeRef::Unit,
                    statements: vec![Statement::Expr(holo_ast::ExprStatement {
                        expression: Expr::call(
                            Expr::identifier("sum", Span::new(11, 14)),
                            vec![Expr::number_literal("1", Span::new(15, 16))],
                            Span::new(11, 17),
                        ),
                        span: Span::new(11, 18),
                    })],
                    is_test: false,
                    span: Span::new(11, 18),
                },
            ],
            tests: Vec::new(),
        };

        let result = BasicTypechecker.typecheck_module(&module, "sum(1);");
        assert!(result.diagnostics.iter().any(|diagnostic| diagnostic
            .message
            .contains("expects 2 argument(s) but got 1")));
        let rendered = holo_base::display_source_diagnostics(&result.diagnostics);
        assert!(
            rendered.contains("function `sum` is defined here"),
            "{rendered}"
        );
    }

    #[test]
    fn rejects_non_numeric_arithmetic_operands() {
        let module = Module {
            functions: Vec::new(),
            tests: vec![TestItem {
                name: "non_numeric".into(),
                statements: vec![Statement::Assert(AssertStatement {
                    expression: Expr::binary(
                        BinaryOperator::Add,
                        Expr::bool_literal(true, Span::new(0, 4)),
                        Expr::bool_literal(false, Span::new(7, 12)),
                        Span::new(0, 12),
                    ),
                    span: Span::new(0, 13),
                })],
                span: Span::new(0, 13),
            }],
        };

        let result = BasicTypechecker.typecheck_module(&module, "assert(true + false);");
        assert!(result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("require numeric operands")));
        let rendered = holo_base::display_source_diagnostics(&result.diagnostics);
        assert!(
            rendered.contains("left operand has type `bool`"),
            "{rendered}"
        );
        assert!(
            rendered.contains("right operand has type `bool`"),
            "{rendered}"
        );
    }

    #[test]
    fn rejects_float_modulo() {
        let module = Module {
            functions: Vec::new(),
            tests: vec![TestItem {
                name: "float_modulo".into(),
                statements: vec![Statement::Assert(AssertStatement {
                    expression: Expr::binary(
                        BinaryOperator::Modulo,
                        Expr::number_literal("1.0f64", Span::new(0, 6)),
                        Expr::number_literal("2.0f64", Span::new(9, 15)),
                        Span::new(0, 15),
                    ),
                    span: Span::new(0, 16),
                })],
                span: Span::new(0, 16),
            }],
        };

        let result = BasicTypechecker.typecheck_module(&module, "assert(1.0f64 % 2.0f64);");
        assert!(result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("only valid for integer types")));
        let rendered = holo_base::display_source_diagnostics(&result.diagnostics);
        assert!(rendered.contains("operands have type `f64`"), "{rendered}");
    }
}
