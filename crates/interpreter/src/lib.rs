//! Test interpreter for the minimal holo language.

use holo_ast::{Expr, ExprKind, Module, Statement, TestItem};
use holo_base::{SharedString, Span};

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
    pub name: SharedString,
    /// Final pass/fail status.
    pub status: TestStatus,
    /// Span of the failing assertion when status is failed.
    pub failure_span: Option<Span>,
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
    /// Executes one collected test item.
    fn run_test(&self, test: &TestItem) -> TestResult;
    /// Executes a collected test set and returns run summary details.
    fn run_collected_tests(&self, tests: &[TestItem]) -> TestRunSummary;
    /// Executes tests in a module and returns run summary details.
    fn run_tests(&self, module: &Module) -> TestRunSummary {
        self.run_collected_tests(&module.tests)
    }
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
    fn run_test(&self, test: &TestItem) -> TestResult {
        let mut status = TestStatus::Passed;
        let mut failure_span = None;
        for statement in &test.statements {
            let (assertion_holds, span) = match statement {
                Statement::Assert(assertion) => (
                    Self::eval_expr(&assertion.expression),
                    Some(assertion.expression.span),
                ),
            };
            if !assertion_holds {
                status = TestStatus::Failed;
                failure_span = span;
                break;
            }
        }

        TestResult {
            name: test.name.clone(),
            status,
            failure_span,
        }
    }

    fn run_collected_tests(&self, tests: &[TestItem]) -> TestRunSummary {
        let mut summary = TestRunSummary::default();

        for test in tests {
            let result = self.run_test(test);
            summary.executed += 1;
            match result.status {
                TestStatus::Passed => summary.passed += 1,
                TestStatus::Failed => summary.failed += 1,
            }
            summary.results.push(result);
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
                name: "fails".into(),
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
        assert_eq!(summary.results[0].failure_span, Some(Span::new(16, 21)));
    }

    #[test]
    fn runs_collected_tests_slice() {
        let tests = vec![
            TestItem {
                name: "pass".into(),
                statements: vec![Statement::Assert(AssertStatement {
                    expression: Expr::bool_literal(true, Span::new(17, 21)),
                    span: Span::new(9, 22),
                })],
                span: Span::new(0, 24),
            },
            TestItem {
                name: "fail".into(),
                statements: vec![Statement::Assert(AssertStatement {
                    expression: Expr::bool_literal(false, Span::new(17, 22)),
                    span: Span::new(9, 23),
                })],
                span: Span::new(25, 49),
            },
        ];

        let summary = BasicInterpreter.run_collected_tests(&tests);
        assert_eq!(summary.executed, 2);
        assert_eq!(summary.passed, 1);
        assert_eq!(summary.failed, 1);
        assert_eq!(summary.results[0].failure_span, None);
        assert_eq!(summary.results[1].failure_span, Some(Span::new(17, 22)));
    }
}
