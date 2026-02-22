//! Typechecking interfaces and the minimal boolean typechecker.

use std::collections::HashMap;

use holo_ast::{BinaryOperator, Expr, ExprKind, Module, Statement, TypeRef};
use holo_base::{DiagnosticKind, SourceDiagnostic, SourceExcerpt, TaskTimer, TaskTiming};
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

    fn typecheck_expression(
        expression: &Expr,
        diagnostics: &mut Vec<SourceDiagnostic>,
        source: &str,
    ) -> Type {
        match &expression.kind {
            ExprKind::BoolLiteral(_) => Type::Bool,
            ExprKind::NumberLiteral(literal) => Self::infer_number_literal_type(literal),
            ExprKind::Identifier(_) => Type::Unknown,
            ExprKind::Negation(inner) => {
                let inner_type = Self::typecheck_expression(inner, diagnostics, source);
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
                let inner_type = Self::typecheck_expression(inner, diagnostics, source);
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
                let left_type = Self::typecheck_expression(&binary.left, diagnostics, source);
                let right_type = Self::typecheck_expression(&binary.right, diagnostics, source);
                if left_type == Type::Unknown || right_type == Type::Unknown {
                    return Type::Unknown;
                }

                if !Self::is_numeric_type(left_type) || !Self::is_numeric_type(right_type) {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            "arithmetic operators require numeric operands",
                        )
                        .with_annotated_span(binary.left.span, "left operand is not numeric")
                        .with_annotated_span(binary.right.span, "right operand is not numeric")
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                    return Type::Unknown;
                }

                if left_type != right_type {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            "arithmetic operands must have the same type",
                        )
                        .with_annotated_span(binary.left.span, "left operand type")
                        .with_annotated_span(binary.right.span, "right operand type")
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                    return Type::Unknown;
                }

                if binary.operator == BinaryOperator::Modulo && !Self::is_integer_type(left_type) {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            "operator `%` is only valid for integer types",
                        )
                        .with_annotated_span(expression.span, "non-integer modulo operation")
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                    return Type::Unknown;
                }

                left_type
            }
            ExprKind::Call(_) => Type::Unknown,
        }
    }
}

impl Typechecker for BasicTypechecker {
    fn typecheck_module(&self, module: &Module, source: &str) -> TypecheckResult {
        let mut seen_test_names = HashMap::new();
        let mut assertion_count = 0usize;
        let mut diagnostics = Vec::new();
        let mut timings = Vec::new();
        let function_types = module
            .functions
            .iter()
            .map(|function| {
                (
                    function.name.clone(),
                    FunctionType {
                        parameter_types: function
                            .parameters
                            .iter()
                            .map(|parameter| Self::type_from_ref(parameter.ty))
                            .collect(),
                        return_type: Self::type_from_ref(function.return_type),
                    },
                )
            })
            .collect::<HashMap<_, _>>();

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

            for statement in &test.statements {
                match statement {
                    Statement::Assert(assertion) => {
                        let expression_type = Self::typecheck_expression(
                            &assertion.expression,
                            &mut diagnostics,
                            source,
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
                            continue;
                        }

                        assertion_count += 1;
                    }
                    Statement::Let(_) | Statement::Expr(_) => {}
                }
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
    use holo_ast::{AssertStatement, BinaryOperator, Expr, Module, Statement, TestItem};
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
        assert!(result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("same type")));
    }
}
