//! Test interpreter for the minimal holo language.

use holo_ast::{Expr, ExprKind, Module, Statement};

/// Final status for a single test.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TestStatus {
    Passed,
    Failed,
}

/// Result for one executed test.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestResult {
    /// Executed test name.
    pub name: String,
    /// Final pass/fail status.
    pub status: TestStatus,
}

/// Aggregate outcome across a full module test run.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct TestRunSummary {
    /// Number of tests executed.
    pub executed: usize,
    /// Number of tests that passed.
    pub passed: usize,
    /// Number of tests that failed.
    pub failed: usize,
    /// Per-test outcomes.
    pub results: Vec<TestResult>,
}

/// Interpreter abstraction used by the core crate.
pub trait Interpreter {
    /// Executes tests in a module and returns run summary details.
    fn run_tests(&self, module: &Module) -> TestRunSummary;
}

/// Basic interpreter for boolean expressions and assertions.
#[derive(Debug, Default)]
pub struct BasicInterpreter;

impl BasicInterpreter {
    fn eval_expr(expression: &Expr) -> bool {
        match &expression.kind {
            ExprKind::BoolLiteral(value) => *value,
            ExprKind::Negation(inner) => !Self::eval_expr(inner),
        }
    }
}

impl Interpreter for BasicInterpreter {
    fn run_tests(&self, module: &Module) -> TestRunSummary {
        let mut summary = TestRunSummary::default();

        for test in &module.tests {
            let mut status = TestStatus::Passed;
            for statement in &test.statements {
                let assertion_holds = match statement {
                    Statement::Assert(assertion) => Self::eval_expr(&assertion.expression),
                };
                if !assertion_holds {
                    status = TestStatus::Failed;
                    break;
                }
            }

            summary.executed += 1;
            match status {
                TestStatus::Passed => summary.passed += 1,
                TestStatus::Failed => summary.failed += 1,
            }
            summary.results.push(TestResult {
                name: test.name.clone(),
                status,
            });
        }

        summary
    }
}

#[cfg(test)]
mod tests {
    use super::{BasicInterpreter, Interpreter, TestStatus};
    use holo_ast::{AssertStatement, Expr, Module, Statement, TestItem};
    use holo_base::Span;

    #[test]
    fn marks_false_assertion_as_failed_test() {
        let module = Module {
            tests: vec![TestItem {
                name: "fails".to_owned(),
                statements: vec![Statement::Assert(AssertStatement {
                    expression: Expr::negation(
                        Expr::bool_literal(true, Span::new(17, 21)),
                        Span::new(16, 21),
                    ),
                    span: Span::new(9, 22),
                })],
                span: Span::new(0, 24),
            }],
        };

        let summary = BasicInterpreter.run_tests(&module);
        assert_eq!(summary.executed, 1);
        assert_eq!(summary.failed, 1);
        assert_eq!(summary.results[0].status, TestStatus::Failed);
    }
}
