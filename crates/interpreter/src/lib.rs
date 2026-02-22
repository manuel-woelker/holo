//! Test interpreter for the minimal holo language.

use std::collections::HashMap;

use holo_ast::{BinaryOperator, Expr, ExprKind, FunctionItem, Module, Statement, TestItem};
use holo_base::{SharedString, Span, TaskTimer, TaskTiming};
use tracing::info;

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
    /// Per-test execution timings.
    pub timings: Vec<TaskTiming>,
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

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Bool(bool),
    U32(u32),
    U64(u64),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Unit,
}

#[derive(Debug, Clone)]
struct RuntimeError {
    span: Span,
}

impl BasicInterpreter {
    fn parse_number_literal(literal: &str) -> Option<Value> {
        if let Some(raw) = literal.strip_suffix("u32") {
            return raw.parse().ok().map(Value::U32);
        }
        if let Some(raw) = literal.strip_suffix("u64") {
            return raw.parse().ok().map(Value::U64);
        }
        if let Some(raw) = literal.strip_suffix("i32") {
            return raw.parse().ok().map(Value::I32);
        }
        if let Some(raw) = literal.strip_suffix("i64") {
            return raw.parse().ok().map(Value::I64);
        }
        if let Some(raw) = literal.strip_suffix("f32") {
            return raw.parse().ok().map(Value::F32);
        }
        if let Some(raw) = literal.strip_suffix("f64") {
            return raw.parse().ok().map(Value::F64);
        }
        if literal.contains('.') {
            return literal.parse().ok().map(Value::F64);
        }
        literal.parse().ok().map(Value::I64)
    }

    fn eval_expr(
        expression: &Expr,
        locals: &HashMap<SharedString, Value>,
        functions: &HashMap<SharedString, FunctionItem>,
    ) -> Result<Value, RuntimeError> {
        match &expression.kind {
            ExprKind::BoolLiteral(value) => Ok(Value::Bool(*value)),
            ExprKind::NumberLiteral(literal) => {
                Self::parse_number_literal(literal).ok_or(RuntimeError {
                    span: expression.span,
                })
            }
            ExprKind::Identifier(name) => locals.get(name).cloned().ok_or(RuntimeError {
                span: expression.span,
            }),
            ExprKind::Negation(inner) => {
                let inner_value = Self::eval_expr(inner, locals, functions)?;
                match inner_value {
                    Value::Bool(value) => Ok(Value::Bool(!value)),
                    _ => Err(RuntimeError {
                        span: expression.span,
                    }),
                }
            }
            ExprKind::UnaryMinus(inner) => {
                let inner_value = Self::eval_expr(inner, locals, functions)?;
                match inner_value {
                    Value::I32(value) => Ok(Value::I32(-value)),
                    Value::I64(value) => Ok(Value::I64(-value)),
                    Value::F32(value) => Ok(Value::F32(-value)),
                    Value::F64(value) => Ok(Value::F64(-value)),
                    _ => Err(RuntimeError {
                        span: expression.span,
                    }),
                }
            }
            ExprKind::Binary(binary) => {
                let left = Self::eval_expr(&binary.left, locals, functions)?;
                let right = Self::eval_expr(&binary.right, locals, functions)?;
                Self::eval_binary(expression.span, binary.operator, left, right)
            }
            ExprKind::Call(call) => {
                let ExprKind::Identifier(callee_name) = &call.callee.kind else {
                    return Err(RuntimeError {
                        span: call.callee.span,
                    });
                };
                let Some(function) = functions.get(callee_name) else {
                    return Err(RuntimeError {
                        span: call.callee.span,
                    });
                };
                if call.arguments.len() != function.parameters.len() {
                    return Err(RuntimeError {
                        span: expression.span,
                    });
                }

                let mut call_locals = HashMap::new();
                for (argument, parameter) in call.arguments.iter().zip(function.parameters.iter()) {
                    let value = Self::eval_expr(argument, locals, functions)?;
                    call_locals.insert(parameter.name.clone(), value);
                }
                Self::run_function(function, &mut call_locals, functions)
            }
        }
    }

    fn eval_binary(
        span: Span,
        operator: BinaryOperator,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        match (left, right) {
            (Value::U32(left), Value::U32(right)) => match operator {
                BinaryOperator::Add => Ok(Value::U32(left.wrapping_add(right))),
                BinaryOperator::Subtract => Ok(Value::U32(left.wrapping_sub(right))),
                BinaryOperator::Multiply => Ok(Value::U32(left.wrapping_mul(right))),
                BinaryOperator::Divide => {
                    if right == 0 {
                        Err(RuntimeError { span })
                    } else {
                        Ok(Value::U32(left / right))
                    }
                }
                BinaryOperator::Modulo => {
                    if right == 0 {
                        Err(RuntimeError { span })
                    } else {
                        Ok(Value::U32(left % right))
                    }
                }
            },
            (Value::U64(left), Value::U64(right)) => match operator {
                BinaryOperator::Add => Ok(Value::U64(left.wrapping_add(right))),
                BinaryOperator::Subtract => Ok(Value::U64(left.wrapping_sub(right))),
                BinaryOperator::Multiply => Ok(Value::U64(left.wrapping_mul(right))),
                BinaryOperator::Divide => {
                    if right == 0 {
                        Err(RuntimeError { span })
                    } else {
                        Ok(Value::U64(left / right))
                    }
                }
                BinaryOperator::Modulo => {
                    if right == 0 {
                        Err(RuntimeError { span })
                    } else {
                        Ok(Value::U64(left % right))
                    }
                }
            },
            (Value::I32(left), Value::I32(right)) => match operator {
                BinaryOperator::Add => Ok(Value::I32(left.wrapping_add(right))),
                BinaryOperator::Subtract => Ok(Value::I32(left.wrapping_sub(right))),
                BinaryOperator::Multiply => Ok(Value::I32(left.wrapping_mul(right))),
                BinaryOperator::Divide => {
                    if right == 0 {
                        Err(RuntimeError { span })
                    } else {
                        Ok(Value::I32(left / right))
                    }
                }
                BinaryOperator::Modulo => {
                    if right == 0 {
                        Err(RuntimeError { span })
                    } else {
                        Ok(Value::I32(left % right))
                    }
                }
            },
            (Value::I64(left), Value::I64(right)) => match operator {
                BinaryOperator::Add => Ok(Value::I64(left.wrapping_add(right))),
                BinaryOperator::Subtract => Ok(Value::I64(left.wrapping_sub(right))),
                BinaryOperator::Multiply => Ok(Value::I64(left.wrapping_mul(right))),
                BinaryOperator::Divide => {
                    if right == 0 {
                        Err(RuntimeError { span })
                    } else {
                        Ok(Value::I64(left / right))
                    }
                }
                BinaryOperator::Modulo => {
                    if right == 0 {
                        Err(RuntimeError { span })
                    } else {
                        Ok(Value::I64(left % right))
                    }
                }
            },
            (Value::F32(left), Value::F32(right)) => match operator {
                BinaryOperator::Add => Ok(Value::F32(left + right)),
                BinaryOperator::Subtract => Ok(Value::F32(left - right)),
                BinaryOperator::Multiply => Ok(Value::F32(left * right)),
                BinaryOperator::Divide => {
                    if right == 0.0 {
                        Err(RuntimeError { span })
                    } else {
                        Ok(Value::F32(left / right))
                    }
                }
                BinaryOperator::Modulo => Ok(Value::F32(left % right)),
            },
            (Value::F64(left), Value::F64(right)) => match operator {
                BinaryOperator::Add => Ok(Value::F64(left + right)),
                BinaryOperator::Subtract => Ok(Value::F64(left - right)),
                BinaryOperator::Multiply => Ok(Value::F64(left * right)),
                BinaryOperator::Divide => {
                    if right == 0.0 {
                        Err(RuntimeError { span })
                    } else {
                        Ok(Value::F64(left / right))
                    }
                }
                BinaryOperator::Modulo => Ok(Value::F64(left % right)),
            },
            _ => Err(RuntimeError { span }),
        }
    }

    fn run_function(
        function: &FunctionItem,
        locals: &mut HashMap<SharedString, Value>,
        functions: &HashMap<SharedString, FunctionItem>,
    ) -> Result<Value, RuntimeError> {
        let mut last_value = Value::Unit;
        for statement in &function.statements {
            match statement {
                Statement::Assert(assertion) => {
                    let value = Self::eval_expr(&assertion.expression, locals, functions)?;
                    match value {
                        Value::Bool(true) => {}
                        Value::Bool(false) => {
                            return Err(RuntimeError {
                                span: assertion.expression.span,
                            });
                        }
                        _ => {
                            return Err(RuntimeError {
                                span: assertion.expression.span,
                            });
                        }
                    }
                }
                Statement::Let(let_statement) => {
                    let value = Self::eval_expr(&let_statement.value, locals, functions)?;
                    locals.insert(let_statement.name.clone(), value);
                }
                Statement::Expr(expr_statement) => {
                    last_value = Self::eval_expr(&expr_statement.expression, locals, functions)?;
                }
            }
        }
        Ok(last_value)
    }

    fn run_test_with_functions(
        &self,
        test: &TestItem,
        functions: &HashMap<SharedString, FunctionItem>,
    ) -> TestResult {
        let mut locals = HashMap::new();
        let mut status = TestStatus::Passed;
        let mut failure_span = None;
        for statement in &test.statements {
            match statement {
                Statement::Assert(assertion) => {
                    let value = Self::eval_expr(&assertion.expression, &locals, functions);
                    match value {
                        Ok(Value::Bool(true)) => {}
                        Ok(Value::Bool(false)) => {
                            status = TestStatus::Failed;
                            failure_span = Some(assertion.expression.span);
                            break;
                        }
                        Ok(_) => {
                            status = TestStatus::Failed;
                            failure_span = Some(assertion.expression.span);
                            break;
                        }
                        Err(error) => {
                            status = TestStatus::Failed;
                            failure_span = Some(error.span);
                            break;
                        }
                    }
                }
                Statement::Let(let_statement) => {
                    let value = Self::eval_expr(&let_statement.value, &locals, functions);
                    match value {
                        Ok(value) => {
                            locals.insert(let_statement.name.clone(), value);
                        }
                        Err(error) => {
                            status = TestStatus::Failed;
                            failure_span = Some(error.span);
                            break;
                        }
                    }
                }
                Statement::Expr(expr_statement) => {
                    if let Err(error) =
                        Self::eval_expr(&expr_statement.expression, &locals, functions)
                    {
                        status = TestStatus::Failed;
                        failure_span = Some(error.span);
                        break;
                    }
                }
            }
        }

        TestResult {
            name: test.name.clone(),
            status,
            failure_span,
        }
    }

    fn function_map_from_module(module: &Module) -> HashMap<SharedString, FunctionItem> {
        module
            .functions
            .iter()
            .map(|function| (function.name.clone(), function.clone()))
            .collect()
    }
}

impl Interpreter for BasicInterpreter {
    fn run_test(&self, test: &TestItem) -> TestResult {
        let functions = HashMap::new();
        self.run_test_with_functions(test, &functions)
    }

    fn run_collected_tests(&self, tests: &[TestItem]) -> TestRunSummary {
        let mut summary = TestRunSummary::default();

        for test in tests {
            let timer = TaskTimer::start(format!("run test `{}`", test.name));
            let result = self.run_test(test);
            let timing = timer.finish();
            summary.timings.push(timing.clone());
            info!(
                test_name = %test.name,
                stage = "run_test",
                elapsed_ms = timing.elapsed.as_secs_f64() * 1000.0,
                status = ?result.status,
                "test timing"
            );
            summary.executed += 1;
            match result.status {
                TestStatus::Passed => summary.passed += 1,
                TestStatus::Failed => summary.failed += 1,
            }
            summary.results.push(result);
        }

        summary
    }

    fn run_tests(&self, module: &Module) -> TestRunSummary {
        let mut summary = TestRunSummary::default();
        let functions = Self::function_map_from_module(module);

        for test in &module.tests {
            let timer = TaskTimer::start(format!("run test `{}`", test.name));
            let result = self.run_test_with_functions(test, &functions);
            let timing = timer.finish();
            info!(
                test_name = %test.name,
                stage = "run_test",
                elapsed_ms = timing.elapsed.as_secs_f64() * 1000.0,
                status = ?result.status,
                "test timing"
            );
            summary.timings.push(timing.clone());
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
    use holo_ast::{
        AssertStatement, BinaryOperator, Expr, ExprStatement, FunctionItem, FunctionParameter,
        Module, Statement, TestItem, TypeRef,
    };
    use holo_base::Span;

    #[test]
    fn marks_false_assertion_as_failed_test() {
        let module = Module {
            functions: Vec::new(),
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

    #[test]
    fn evaluates_numeric_arithmetic_in_test_assertion() {
        let module = Module {
            functions: vec![FunctionItem {
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
                statements: vec![Statement::Expr(ExprStatement {
                    expression: Expr::binary(
                        BinaryOperator::Add,
                        Expr::identifier("a", Span::new(0, 1)),
                        Expr::identifier("b", Span::new(4, 5)),
                        Span::new(0, 5),
                    ),
                    span: Span::new(0, 6),
                })],
                is_test: false,
                span: Span::new(0, 6),
            }],
            tests: vec![TestItem {
                name: "arith".into(),
                statements: vec![Statement::Assert(AssertStatement {
                    expression: Expr::negation(
                        Expr::binary(
                            BinaryOperator::Subtract,
                            Expr::call(
                                Expr::identifier("sum", Span::new(0, 3)),
                                vec![
                                    Expr::number_literal("3i64", Span::new(4, 8)),
                                    Expr::number_literal("3i64", Span::new(10, 14)),
                                ],
                                Span::new(0, 15),
                            ),
                            Expr::number_literal("0i64", Span::new(18, 22)),
                            Span::new(0, 22),
                        ),
                        Span::new(0, 22),
                    ),
                    span: Span::new(0, 23),
                })],
                span: Span::new(0, 23),
            }],
        };

        let summary = BasicInterpreter.run_tests(&module);
        assert_eq!(summary.executed, 1);
        assert_eq!(summary.passed, 0);
        assert_eq!(summary.failed, 1);
    }

    #[test]
    fn fails_test_on_division_by_zero() {
        let module = Module {
            functions: Vec::new(),
            tests: vec![TestItem {
                name: "div_zero".into(),
                statements: vec![Statement::Assert(AssertStatement {
                    expression: Expr::binary(
                        BinaryOperator::Divide,
                        Expr::number_literal("1i64", Span::new(0, 4)),
                        Expr::number_literal("0i64", Span::new(7, 11)),
                        Span::new(0, 11),
                    ),
                    span: Span::new(0, 12),
                })],
                span: Span::new(0, 12),
            }],
        };

        let summary = BasicInterpreter.run_tests(&module);
        assert_eq!(summary.executed, 1);
        assert_eq!(summary.failed, 1);
        assert_eq!(summary.results[0].failure_span, Some(Span::new(0, 11)));
    }
}
