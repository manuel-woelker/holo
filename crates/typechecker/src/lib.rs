//! Typechecking interfaces and the minimal boolean typechecker.

use holo_ast::{AssertStatement, Expr, ExprKind, Module, Statement};
use holo_base::Result;

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
    fn typecheck_expression(expression: &Expr) -> Type {
        match &expression.kind {
            ExprKind::BoolLiteral(_) => Type::Bool,
            ExprKind::Negation(inner) => {
                let _inner_type = Self::typecheck_expression(inner);
                Type::Bool
            }
        }
    }

    fn typecheck_assert(statement: &AssertStatement) -> Type {
        Self::typecheck_expression(&statement.expression)
    }
}

impl Typechecker for BasicTypechecker {
    fn typecheck_module(&self, module: &Module) -> Result<TypecheckSummary> {
        let mut assertion_count = 0usize;
        for test in &module.tests {
            for statement in &test.statements {
                match statement {
                    Statement::Assert(assertion) => {
                        let _assert_type = Self::typecheck_assert(assertion);
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
}
