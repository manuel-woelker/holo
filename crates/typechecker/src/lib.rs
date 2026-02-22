//! Typechecking interfaces and the minimal boolean typechecker.

use std::collections::HashSet;

use holo_ast::{AssertStatement, Expr, ExprKind, Module, Statement};
use holo_base::{holo_message_error, Result};

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

/// Typechecking abstraction used by the core compiler crate.
pub trait Typechecker {
    /// Validates semantic and type rules for a module.
    fn typecheck_module(&self, module: &Module) -> Result<TypecheckSummary>;
}

/// Minimal typechecker for boolean expressions and assert statements.
#[derive(Debug, Default)]
pub struct BasicTypechecker;

impl BasicTypechecker {
    fn typecheck_expression(expression: &Expr) -> Result<Type> {
        match &expression.kind {
            ExprKind::BoolLiteral(_) => Ok(Type::Bool),
            ExprKind::Negation(inner) => {
                let inner_type = Self::typecheck_expression(inner)?;
                if inner_type != Type::Bool {
                    return Err(holo_message_error!(
                        "negation requires a boolean operand at span {}..{}",
                        expression.span.start,
                        expression.span.end
                    ));
                }
                Ok(Type::Bool)
            }
        }
    }

    fn typecheck_assert(statement: &AssertStatement) -> Result<Type> {
        let expression_type = Self::typecheck_expression(&statement.expression)?;
        if expression_type != Type::Bool {
            return Err(holo_message_error!(
                "assert expects a boolean expression at span {}..{}",
                statement.span.start,
                statement.span.end
            ));
        }
        Ok(Type::Bool)
    }
}

impl Typechecker for BasicTypechecker {
    fn typecheck_module(&self, module: &Module) -> Result<TypecheckSummary> {
        let mut seen_test_names = HashSet::new();
        let mut assertion_count = 0usize;
        for test in &module.tests {
            if !seen_test_names.insert(test.name.as_str()) {
                return Err(holo_message_error!(
                    "duplicate test function name `{}` in module",
                    test.name
                ));
            }

            for statement in &test.statements {
                match statement {
                    Statement::Assert(assertion) => {
                        let _assert_type = Self::typecheck_assert(assertion)?;
                        assertion_count += 1;
                    }
                }
            }
        }

        Ok(TypecheckSummary {
            test_count: module.tests.len(),
            assertion_count,
        })
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
                name: "sample".to_owned(),
                statements: vec![Statement::Assert(AssertStatement {
                    expression: Expr::bool_literal(true, Span::new(20, 24)),
                    span: Span::new(13, 25),
                })],
                span: Span::new(0, 26),
            }],
        };

        let summary = BasicTypechecker
            .typecheck_module(&module)
            .expect("typechecking should succeed");
        assert_eq!(summary.test_count, 1);
        assert_eq!(summary.assertion_count, 1);
    }

    #[test]
    fn rejects_duplicate_test_names() {
        let module = Module {
            tests: vec![
                TestItem {
                    name: "same_name".to_owned(),
                    statements: vec![Statement::Assert(AssertStatement {
                        expression: Expr::bool_literal(true, Span::new(20, 24)),
                        span: Span::new(13, 25),
                    })],
                    span: Span::new(0, 26),
                },
                TestItem {
                    name: "same_name".to_owned(),
                    statements: vec![Statement::Assert(AssertStatement {
                        expression: Expr::bool_literal(true, Span::new(50, 54)),
                        span: Span::new(43, 55),
                    })],
                    span: Span::new(30, 56),
                },
            ],
        };

        let error = BasicTypechecker
            .typecheck_module(&module)
            .expect_err("typechecking should reject duplicate test names");
        assert!(
            error.to_string().contains("duplicate test function name"),
            "actual error: {error}"
        );
    }
}
