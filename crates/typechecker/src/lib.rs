//! Typechecking interfaces and the minimal boolean typechecker.

use std::collections::HashMap;

use holo_ast::{Expr, ExprKind, Module, Statement};
use holo_base::{DiagnosticKind, SourceDiagnostic, SourceExcerpt, TaskTimer, TaskTiming};
use tracing::info;

/// Type representation for the minimal language.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Bool,
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
    fn typecheck_expression(expression: &Expr) -> Type {
        match &expression.kind {
            ExprKind::BoolLiteral(_) => Type::Bool,
            ExprKind::Negation(inner) => {
                let _inner_type = Self::typecheck_expression(inner);
                Type::Bool
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
                        let expression_type = Self::typecheck_expression(&assertion.expression);
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
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{BasicTypechecker, Typechecker};
    use holo_ast::{AssertStatement, Expr, Module, Statement, TestItem};
    use holo_base::Span;

    #[test]
    fn typechecks_assertion_count_for_module() {
        let module = Module {
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
}
