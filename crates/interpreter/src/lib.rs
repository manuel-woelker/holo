//! Test interpreter for the minimal holo language.

pub mod native_functions;

use std::collections::HashMap;
use std::sync::Arc;

use holo_base::{SharedString, Span, TaskTimer, TaskTiming};
pub use holo_ir::Type;
use holo_ir::{BinaryOperator, Expr, ExprKind, FunctionItem, Module, Statement, TestItem};
use tracing::info;

/// Trait for native (host-provided) functions.
///
/// Implement this trait to create Rust functions that can be called from holo code.
/// Native functions are available globally and are checked at compile-time.
///
/// # Example
///
/// ```rust
/// use holo_interpreter::{NativeFunction, Type, Value, RuntimeError};
/// use holo_base::SharedString;
///
/// struct MyFunction {
///     name: SharedString,
/// }
///
/// impl MyFunction {
///     fn new() -> Self {
///         Self { name: "my_func".into() }
///     }
/// }
///
/// impl NativeFunction for MyFunction {
///     fn name(&self) -> &SharedString {
///         &self.name
///     }
///
///     fn param_types(&self) -> &[Type] {
///         &[Type::I64]
///     }
///
///     fn return_type(&self) -> Type {
///         Type::Unit
///     }
///
///     fn call(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
///         // Implementation here
///         Ok(Value::Unit)
///     }
/// }
/// ```
pub trait NativeFunction: Send + Sync {
    /// Returns the function's name as it will be called from holo code.
    fn name(&self) -> &SharedString;

    /// Returns the parameter types in order.
    fn param_types(&self) -> &[Type];

    /// Returns the return type.
    fn return_type(&self) -> Type;

    /// Invokes the function with the given arguments.
    ///
    /// # Arguments
    ///
    /// * `args` - Vector of evaluated argument values in declaration order
    ///
    /// # Returns
    ///
    /// * `Ok(Value)` - The return value of the function
    /// * `Err(RuntimeError)` - An error if the function call fails
    fn call(&self, args: Vec<Value>) -> Result<Value, RuntimeError>;
}

/// Global registry for native functions.
///
/// Holds all registered native functions and provides lookup by name.
/// Functions registered here are available globally in holo code and
/// are type-checked at compile-time.
///
/// # Example
///
/// ```rust
/// use holo_interpreter::{NativeFunctionRegistry, native_functions};
/// use std::sync::Arc;
///
/// // Create registry with built-in functions
/// let registry = native_functions::create_builtin_registry();
///
/// // Or create empty registry and register custom functions
/// let mut registry = NativeFunctionRegistry::default();
/// // registry.register(MyCustomFunction::new());
/// ```
#[derive(Default)]
pub struct NativeFunctionRegistry {
    functions: HashMap<SharedString, Arc<dyn NativeFunction>>,
}

impl std::fmt::Debug for NativeFunctionRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeFunctionRegistry")
            .field("functions", &self.functions.keys().collect::<Vec<_>>())
            .finish()
    }
}

impl NativeFunctionRegistry {
    /// Creates a new empty native function registry.
    pub fn new() -> Self {
        Self::default()
    }

    /// Registers a native function in the registry.
    ///
    /// # Arguments
    ///
    /// * `function` - The native function to register (must implement `NativeFunction`)
    pub fn register(&mut self, function: impl NativeFunction + 'static) {
        let name = function.name().clone();
        self.functions.insert(name, Arc::new(function));
    }

    /// Looks up a native function by name.
    ///
    /// # Arguments
    ///
    /// * `name` - The function name to look up
    ///
    /// # Returns
    ///
    /// * `Some(Arc<dyn NativeFunction>)` - The registered function if found
    /// * `None` - If no function with the given name is registered
    pub fn lookup(&self, name: &SharedString) -> Option<Arc<dyn NativeFunction>> {
        self.functions.get(name).cloned()
    }

    /// Checks if a native function with the given name exists.
    ///
    /// # Arguments
    ///
    /// * `name` - The function name to check
    pub fn contains(&self, name: &SharedString) -> bool {
        self.functions.contains_key(name)
    }

    /// Returns an iterator over all registered native functions.
    pub fn iter(&self) -> impl Iterator<Item = (&SharedString, &Arc<dyn NativeFunction>)> {
        self.functions.iter()
    }
}

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
    /// Failure reason when status is failed.
    pub failure_reason: Option<SharedString>,
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
    /// Executes one test item using functions from a typed module.
    fn run_test_in_module(&self, module: &Module, test: &TestItem) -> TestResult;
    /// Executes a collected test set and returns run summary details.
    fn run_collected_tests(&self, tests: &[TestItem]) -> TestRunSummary;
    /// Executes tests in a module and returns run summary details.
    fn run_tests(&self, module: &Module) -> TestRunSummary {
        self.run_collected_tests(&module.tests)
    }
}

/// Basic interpreter for boolean expressions and assertions.
#[derive(Debug)]
pub struct BasicInterpreter {
    native_functions: Arc<NativeFunctionRegistry>,
}

impl BasicInterpreter {
    /// Creates a new interpreter with the given native function registry.
    pub fn new(native_functions: Arc<NativeFunctionRegistry>) -> Self {
        Self { native_functions }
    }

    /// Creates a new interpreter with an empty native function registry.
    pub fn with_empty_registry() -> Self {
        Self::new(Arc::new(NativeFunctionRegistry::default()))
    }
}

impl Default for BasicInterpreter {
    fn default() -> Self {
        Self::with_empty_registry()
    }
}

/// Runtime value types supported by the interpreter.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    U32(u32),
    U64(u64),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Unit,
}

/// Runtime error with span information for error reporting.
#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub span: Span,
    pub message: SharedString,
}

#[derive(Debug, Default)]
struct RuntimeScopes {
    scopes: Vec<HashMap<SharedString, Value>>,
}

impl RuntimeScopes {
    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        let _ = self.scopes.pop();
    }

    fn insert(&mut self, name: SharedString, value: Value) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, value);
        }
    }

    fn lookup(&self, name: &SharedString) -> Option<Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value.clone());
            }
        }
        None
    }
}

impl BasicInterpreter {
    fn ordered_tests(tests: &[TestItem]) -> Vec<&TestItem> {
        let mut ordered = tests.iter().collect::<Vec<_>>();
        ordered.sort_by(|left, right| {
            left.name
                .as_str()
                .cmp(right.name.as_str())
                .then_with(|| left.span.start.cmp(&right.span.start))
                .then_with(|| left.span.end.cmp(&right.span.end))
        });
        ordered
    }

    fn has_explicit_numeric_suffix(literal: &str) -> bool {
        ["u32", "u64", "i32", "i64", "f32", "f64"]
            .iter()
            .any(|suffix| literal.ends_with(suffix))
    }

    fn parse_number_literal(
        literal: &str,
        expected_type: Option<holo_ir::TypeRef>,
    ) -> Option<Value> {
        if !Self::has_explicit_numeric_suffix(literal) {
            if let Some(expected_type) = expected_type {
                if let Ok(number) = literal.parse::<i64>() {
                    return match expected_type {
                        holo_ir::TypeRef::U32 => u32::try_from(number).ok().map(Value::U32),
                        holo_ir::TypeRef::U64 => u64::try_from(number).ok().map(Value::U64),
                        holo_ir::TypeRef::I32 => i32::try_from(number).ok().map(Value::I32),
                        holo_ir::TypeRef::I64 => Some(Value::I64(number)),
                        holo_ir::TypeRef::F32 => Some(Value::F32(number as f32)),
                        holo_ir::TypeRef::F64 => Some(Value::F64(number as f64)),
                        _ => None,
                    };
                }
                if let Ok(number) = literal.parse::<f64>() {
                    return match expected_type {
                        holo_ir::TypeRef::F32 => Some(Value::F32(number as f32)),
                        holo_ir::TypeRef::F64 => Some(Value::F64(number)),
                        _ => None,
                    };
                }
            }
        }

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
        scopes: &mut RuntimeScopes,
        functions: &HashMap<SharedString, FunctionItem>,
        native_functions: &Arc<NativeFunctionRegistry>,
        expected_type: Option<holo_ir::TypeRef>,
    ) -> Result<Value, RuntimeError> {
        match &expression.kind {
            ExprKind::BoolLiteral(value) => Ok(Value::Bool(*value)),
            ExprKind::NumberLiteral(literal) => Self::parse_number_literal(literal, expected_type)
                .ok_or(RuntimeError {
                    span: expression.span,
                    message: "invalid number literal".into(),
                }),
            ExprKind::Identifier(name) => scopes.lookup(name).ok_or(RuntimeError {
                span: expression.span,
                message: format!("unknown identifier `{name}`").into(),
            }),
            ExprKind::Negation(inner) => {
                let inner_value =
                    Self::eval_expr(inner, scopes, functions, native_functions, None)?;
                match inner_value {
                    Value::Bool(value) => Ok(Value::Bool(!value)),
                    _ => Err(RuntimeError {
                        span: expression.span,
                        message: "operator `!` expects bool".into(),
                    }),
                }
            }
            ExprKind::UnaryMinus(inner) => {
                let inner_value =
                    Self::eval_expr(inner, scopes, functions, native_functions, None)?;
                match inner_value {
                    Value::I32(value) => Ok(Value::I32(-value)),
                    Value::I64(value) => Ok(Value::I64(-value)),
                    Value::F32(value) => Ok(Value::F32(-value)),
                    Value::F64(value) => Ok(Value::F64(-value)),
                    _ => Err(RuntimeError {
                        span: expression.span,
                        message: "operator `-` expects signed numeric".into(),
                    }),
                }
            }
            ExprKind::Binary(binary) => {
                let left =
                    Self::eval_expr(&binary.left, scopes, functions, native_functions, None)?;
                let right =
                    Self::eval_expr(&binary.right, scopes, functions, native_functions, None)?;
                Self::eval_binary(expression.span, binary.operator, left, right)
            }
            ExprKind::Call(call) => {
                let ExprKind::Identifier(callee_name) = &call.callee.kind else {
                    return Err(RuntimeError {
                        span: call.callee.span,
                        message: "call target is not a function name".into(),
                    });
                };

                // First check native functions
                if let Some(native) = native_functions.lookup(callee_name) {
                    // Validate argument count
                    if call.arguments.len() != native.param_types().len() {
                        return Err(RuntimeError {
                            span: expression.span,
                            message: format!(
                                "function `{callee_name}` expects {} argument(s) but got {}",
                                native.param_types().len(),
                                call.arguments.len()
                            )
                            .into(),
                        });
                    }

                    // Evaluate arguments with expected types
                    let mut evaluated_args = Vec::with_capacity(call.arguments.len());
                    for (argument, param_ty) in
                        call.arguments.iter().zip(native.param_types().iter())
                    {
                        let value = Self::eval_expr(
                            argument,
                            scopes,
                            functions,
                            native_functions,
                            Some(param_ty.clone()),
                        )?;
                        evaluated_args.push(value);
                    }

                    // Call native function
                    return native.call(evaluated_args);
                }

                // Fall back to user-defined functions
                let Some(function) = functions.get(callee_name) else {
                    return Err(RuntimeError {
                        span: call.callee.span,
                        message: format!("unknown function `{callee_name}`").into(),
                    });
                };
                if call.arguments.len() != function.parameters.len() {
                    return Err(RuntimeError {
                        span: expression.span,
                        message: format!(
                            "function `{callee_name}` expects {} argument(s) but got {}",
                            function.parameters.len(),
                            call.arguments.len()
                        )
                        .into(),
                    });
                }

                let mut call_locals = HashMap::new();
                for (argument, parameter) in call.arguments.iter().zip(function.parameters.iter()) {
                    let value = Self::eval_expr(
                        argument,
                        scopes,
                        functions,
                        native_functions,
                        Some(parameter.ty.clone()),
                    )?;
                    call_locals.insert(parameter.name.clone(), value);
                }
                Self::run_function(function, call_locals, functions, native_functions)
            }
            ExprKind::If(if_expression) => {
                let condition = Self::eval_expr(
                    &if_expression.condition,
                    scopes,
                    functions,
                    native_functions,
                    None,
                )?;
                let is_true = match condition {
                    Value::Bool(value) => value,
                    _ => {
                        return Err(RuntimeError {
                            span: if_expression.condition.span,
                            message: "if condition must evaluate to bool".into(),
                        });
                    }
                };

                if is_true {
                    Self::eval_expr(
                        &if_expression.then_branch,
                        scopes,
                        functions,
                        native_functions,
                        expected_type,
                    )
                } else if let Some(else_branch) = &if_expression.else_branch {
                    Self::eval_expr(
                        else_branch,
                        scopes,
                        functions,
                        native_functions,
                        expected_type,
                    )
                } else {
                    Ok(Value::Unit)
                }
            }
            ExprKind::While(while_expression) => {
                loop {
                    let condition = Self::eval_expr(
                        &while_expression.condition,
                        scopes,
                        functions,
                        native_functions,
                        None,
                    )?;
                    let is_true = match condition {
                        Value::Bool(value) => value,
                        _ => {
                            return Err(RuntimeError {
                                span: while_expression.condition.span,
                                message: "while condition must evaluate to bool".into(),
                            });
                        }
                    };
                    if !is_true {
                        break;
                    }
                    let _ = Self::eval_expr(
                        &while_expression.body,
                        scopes,
                        functions,
                        native_functions,
                        None,
                    )?;
                }
                Ok(Value::Unit)
            }
            ExprKind::Block(block_expression) => {
                scopes.push_scope();
                for statement in &block_expression.statements {
                    let _ = Self::run_statement(statement, scopes, functions, native_functions)?;
                }
                let value = if let Some(result) = &block_expression.result {
                    Self::eval_expr(result, scopes, functions, native_functions, expected_type)?
                } else {
                    Value::Unit
                };
                scopes.pop_scope();
                Ok(value)
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
                        Err(RuntimeError {
                            span,
                            message: "division by zero".into(),
                        })
                    } else {
                        Ok(Value::U32(left / right))
                    }
                }
                BinaryOperator::Modulo => {
                    if right == 0 {
                        Err(RuntimeError {
                            span,
                            message: "modulo by zero".into(),
                        })
                    } else {
                        Ok(Value::U32(left % right))
                    }
                }
                BinaryOperator::Equals => Ok(Value::Bool(left == right)),
                BinaryOperator::NotEquals => Ok(Value::Bool(left != right)),
                BinaryOperator::LessThan => Ok(Value::Bool(left < right)),
                BinaryOperator::GreaterThan => Ok(Value::Bool(left > right)),
                BinaryOperator::LessThanOrEqual => Ok(Value::Bool(left <= right)),
                BinaryOperator::GreaterThanOrEqual => Ok(Value::Bool(left >= right)),
            },
            (Value::U64(left), Value::U64(right)) => match operator {
                BinaryOperator::Add => Ok(Value::U64(left.wrapping_add(right))),
                BinaryOperator::Subtract => Ok(Value::U64(left.wrapping_sub(right))),
                BinaryOperator::Multiply => Ok(Value::U64(left.wrapping_mul(right))),
                BinaryOperator::Divide => {
                    if right == 0 {
                        Err(RuntimeError {
                            span,
                            message: "division by zero".into(),
                        })
                    } else {
                        Ok(Value::U64(left / right))
                    }
                }
                BinaryOperator::Modulo => {
                    if right == 0 {
                        Err(RuntimeError {
                            span,
                            message: "modulo by zero".into(),
                        })
                    } else {
                        Ok(Value::U64(left % right))
                    }
                }
                BinaryOperator::Equals => Ok(Value::Bool(left == right)),
                BinaryOperator::NotEquals => Ok(Value::Bool(left != right)),
                BinaryOperator::LessThan => Ok(Value::Bool(left < right)),
                BinaryOperator::GreaterThan => Ok(Value::Bool(left > right)),
                BinaryOperator::LessThanOrEqual => Ok(Value::Bool(left <= right)),
                BinaryOperator::GreaterThanOrEqual => Ok(Value::Bool(left >= right)),
            },
            (Value::I32(left), Value::I32(right)) => match operator {
                BinaryOperator::Add => Ok(Value::I32(left.wrapping_add(right))),
                BinaryOperator::Subtract => Ok(Value::I32(left.wrapping_sub(right))),
                BinaryOperator::Multiply => Ok(Value::I32(left.wrapping_mul(right))),
                BinaryOperator::Divide => {
                    if right == 0 {
                        Err(RuntimeError {
                            span,
                            message: "division by zero".into(),
                        })
                    } else {
                        Ok(Value::I32(left / right))
                    }
                }
                BinaryOperator::Modulo => {
                    if right == 0 {
                        Err(RuntimeError {
                            span,
                            message: "modulo by zero".into(),
                        })
                    } else {
                        Ok(Value::I32(left % right))
                    }
                }
                BinaryOperator::Equals => Ok(Value::Bool(left == right)),
                BinaryOperator::NotEquals => Ok(Value::Bool(left != right)),
                BinaryOperator::LessThan => Ok(Value::Bool(left < right)),
                BinaryOperator::GreaterThan => Ok(Value::Bool(left > right)),
                BinaryOperator::LessThanOrEqual => Ok(Value::Bool(left <= right)),
                BinaryOperator::GreaterThanOrEqual => Ok(Value::Bool(left >= right)),
            },
            (Value::I64(left), Value::I64(right)) => match operator {
                BinaryOperator::Add => Ok(Value::I64(left.wrapping_add(right))),
                BinaryOperator::Subtract => Ok(Value::I64(left.wrapping_sub(right))),
                BinaryOperator::Multiply => Ok(Value::I64(left.wrapping_mul(right))),
                BinaryOperator::Divide => {
                    if right == 0 {
                        Err(RuntimeError {
                            span,
                            message: "division by zero".into(),
                        })
                    } else {
                        Ok(Value::I64(left / right))
                    }
                }
                BinaryOperator::Modulo => {
                    if right == 0 {
                        Err(RuntimeError {
                            span,
                            message: "modulo by zero".into(),
                        })
                    } else {
                        Ok(Value::I64(left % right))
                    }
                }
                BinaryOperator::Equals => Ok(Value::Bool(left == right)),
                BinaryOperator::NotEquals => Ok(Value::Bool(left != right)),
                BinaryOperator::LessThan => Ok(Value::Bool(left < right)),
                BinaryOperator::GreaterThan => Ok(Value::Bool(left > right)),
                BinaryOperator::LessThanOrEqual => Ok(Value::Bool(left <= right)),
                BinaryOperator::GreaterThanOrEqual => Ok(Value::Bool(left >= right)),
            },
            (Value::F32(left), Value::F32(right)) => match operator {
                BinaryOperator::Add => Ok(Value::F32(left + right)),
                BinaryOperator::Subtract => Ok(Value::F32(left - right)),
                BinaryOperator::Multiply => Ok(Value::F32(left * right)),
                BinaryOperator::Divide => {
                    if right == 0.0 {
                        Err(RuntimeError {
                            span,
                            message: "division by zero".into(),
                        })
                    } else {
                        Ok(Value::F32(left / right))
                    }
                }
                BinaryOperator::Modulo => Ok(Value::F32(left % right)),
                BinaryOperator::Equals => Ok(Value::Bool(left == right)),
                BinaryOperator::NotEquals => Ok(Value::Bool(left != right)),
                BinaryOperator::LessThan => Ok(Value::Bool(left < right)),
                BinaryOperator::GreaterThan => Ok(Value::Bool(left > right)),
                BinaryOperator::LessThanOrEqual => Ok(Value::Bool(left <= right)),
                BinaryOperator::GreaterThanOrEqual => Ok(Value::Bool(left >= right)),
            },
            (Value::F64(left), Value::F64(right)) => match operator {
                BinaryOperator::Add => Ok(Value::F64(left + right)),
                BinaryOperator::Subtract => Ok(Value::F64(left - right)),
                BinaryOperator::Multiply => Ok(Value::F64(left * right)),
                BinaryOperator::Divide => {
                    if right == 0.0 {
                        Err(RuntimeError {
                            span,
                            message: "division by zero".into(),
                        })
                    } else {
                        Ok(Value::F64(left / right))
                    }
                }
                BinaryOperator::Modulo => Ok(Value::F64(left % right)),
                BinaryOperator::Equals => Ok(Value::Bool(left == right)),
                BinaryOperator::NotEquals => Ok(Value::Bool(left != right)),
                BinaryOperator::LessThan => Ok(Value::Bool(left < right)),
                BinaryOperator::GreaterThan => Ok(Value::Bool(left > right)),
                BinaryOperator::LessThanOrEqual => Ok(Value::Bool(left <= right)),
                BinaryOperator::GreaterThanOrEqual => Ok(Value::Bool(left >= right)),
            },
            (Value::Bool(left), Value::Bool(right)) => match operator {
                BinaryOperator::Equals => Ok(Value::Bool(left == right)),
                BinaryOperator::NotEquals => Ok(Value::Bool(left != right)),
                _ => Err(RuntimeError {
                    span,
                    message: "boolean operands only support equality operators".into(),
                }),
            },
            _ => Err(RuntimeError {
                span,
                message: "arithmetic operands are incompatible".into(),
            }),
        }
    }

    fn run_statement(
        statement: &Statement,
        scopes: &mut RuntimeScopes,
        functions: &HashMap<SharedString, FunctionItem>,
        native_functions: &Arc<NativeFunctionRegistry>,
    ) -> Result<Option<Value>, RuntimeError> {
        match statement {
            Statement::Assert(assertion) => {
                let value = Self::eval_expr(
                    &assertion.expression,
                    scopes,
                    functions,
                    native_functions,
                    None,
                )?;
                match value {
                    Value::Bool(true) => Ok(None),
                    Value::Bool(false) => Err(RuntimeError {
                        span: assertion.expression.span,
                        message: "assertion failed".into(),
                    }),
                    _ => Err(RuntimeError {
                        span: assertion.expression.span,
                        message: "assertion expression did not evaluate to bool".into(),
                    }),
                }
            }
            Statement::Let(let_statement) => {
                let value = Self::eval_expr(
                    &let_statement.value,
                    scopes,
                    functions,
                    native_functions,
                    let_statement.ty.clone(),
                )?;
                scopes.insert(let_statement.name.clone(), value);
                Ok(None)
            }
            Statement::Expr(expr_statement) => {
                let value = Self::eval_expr(
                    &expr_statement.expression,
                    scopes,
                    functions,
                    native_functions,
                    None,
                )?;
                Ok(Some(value))
            }
        }
    }

    fn run_function(
        function: &FunctionItem,
        initial_scope: HashMap<SharedString, Value>,
        functions: &HashMap<SharedString, FunctionItem>,
        native_functions: &Arc<NativeFunctionRegistry>,
    ) -> Result<Value, RuntimeError> {
        let mut scopes = RuntimeScopes::default();
        scopes.push_scope();
        for (name, value) in initial_scope {
            scopes.insert(name, value);
        }
        let mut last_value = Value::Unit;
        for statement in &function.statements {
            if let Some(value) =
                Self::run_statement(statement, &mut scopes, functions, native_functions)?
            {
                last_value = value;
            }
        }
        scopes.pop_scope();
        Ok(last_value)
    }

    fn run_test_with_functions(
        &self,
        test: &TestItem,
        functions: &HashMap<SharedString, FunctionItem>,
    ) -> TestResult {
        let mut scopes = RuntimeScopes::default();
        scopes.push_scope();
        let mut status = TestStatus::Passed;
        let mut failure_span = None;
        let mut failure_reason = None;
        for statement in &test.statements {
            if let Err(error) =
                Self::run_statement(statement, &mut scopes, functions, &self.native_functions)
            {
                status = TestStatus::Failed;
                failure_span = Some(error.span);
                failure_reason = Some(error.message);
                break;
            }
        }
        scopes.pop_scope();

        TestResult {
            name: test.name.clone(),
            status,
            failure_span,
            failure_reason,
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

        for test in Self::ordered_tests(tests) {
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

        for test in Self::ordered_tests(&module.tests) {
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

    fn run_test_in_module(&self, module: &Module, test: &TestItem) -> TestResult {
        let functions = Self::function_map_from_module(module);
        self.run_test_with_functions(test, &functions)
    }
}

#[cfg(test)]
mod tests {
    use super::{BasicInterpreter, Interpreter, TestStatus};
    use holo_base::Span;
    use holo_ir::{
        AssertStatement, BinaryOperator, Expr, ExprStatement, FunctionItem, FunctionParameter,
        Module, Statement, TestItem, TypeRef,
    };

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

        let interpreter = BasicInterpreter::default();
        let summary = interpreter.run_tests(&module);
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

        let interpreter = BasicInterpreter::default();
        let summary = interpreter.run_collected_tests(&tests);
        assert_eq!(summary.executed, 2);
        assert_eq!(summary.passed, 1);
        assert_eq!(summary.failed, 1);
        let passing = summary
            .results
            .iter()
            .find(|result| result.name == "pass")
            .expect("passing result should exist");
        let failing = summary
            .results
            .iter()
            .find(|result| result.name == "fail")
            .expect("failing result should exist");
        assert_eq!(passing.failure_span, None);
        assert_eq!(failing.failure_span, Some(Span::new(17, 22)));
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

        let interpreter = BasicInterpreter::default();
        let summary = interpreter.run_tests(&module);
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

        let interpreter = BasicInterpreter::default();
        let summary = interpreter.run_tests(&module);
        assert_eq!(summary.executed, 1);
        assert_eq!(summary.failed, 1);
        assert_eq!(summary.results[0].failure_span, Some(Span::new(0, 11)));
    }

    #[test]
    fn executes_numeric_operations_for_all_supported_number_types() {
        let cases = vec![
            ("u32_ops", "1u32", "2u32", BinaryOperator::Add, TypeRef::U32),
            (
                "u64_ops",
                "8u64",
                "3u64",
                BinaryOperator::Subtract,
                TypeRef::U64,
            ),
            (
                "i32_ops",
                "2i32",
                "3i32",
                BinaryOperator::Multiply,
                TypeRef::I32,
            ),
            (
                "i64_ops",
                "8i64",
                "2i64",
                BinaryOperator::Divide,
                TypeRef::I64,
            ),
            (
                "f32_ops",
                "7.0f32",
                "2.0f32",
                BinaryOperator::Modulo,
                TypeRef::F32,
            ),
            (
                "f64_ops",
                "9.0f64",
                "3.0f64",
                BinaryOperator::Divide,
                TypeRef::F64,
            ),
        ];

        for (name, left_literal, right_literal, operator, return_type) in cases {
            let module = Module {
                functions: vec![FunctionItem {
                    name: name.into(),
                    parameters: Vec::new(),
                    return_type,
                    statements: vec![Statement::Expr(ExprStatement {
                        expression: Expr::binary(
                            operator,
                            Expr::number_literal(left_literal, Span::new(0, left_literal.len())),
                            Expr::number_literal(
                                right_literal,
                                Span::new(10, 10 + right_literal.len()),
                            ),
                            Span::new(0, 10 + right_literal.len()),
                        ),
                        span: Span::new(0, 11 + right_literal.len()),
                    })],
                    is_test: false,
                    span: Span::new(0, 11 + right_literal.len()),
                }],
                tests: vec![TestItem {
                    name: "smoke".into(),
                    statements: vec![
                        Statement::Expr(ExprStatement {
                            expression: Expr::call(
                                Expr::identifier(name, Span::new(0, name.len())),
                                vec![],
                                Span::new(0, name.len() + 2),
                            ),
                            span: Span::new(0, name.len() + 3),
                        }),
                        Statement::Assert(AssertStatement {
                            expression: Expr::bool_literal(true, Span::new(20, 24)),
                            span: Span::new(13, 25),
                        }),
                    ],
                    span: Span::new(0, 26),
                }],
            };

            let interpreter = BasicInterpreter::default();
            let summary = interpreter.run_tests(&module);
            assert_eq!(summary.executed, 1, "{name}");
            assert_eq!(summary.passed, 1, "{name}");
            assert_eq!(summary.failed, 0, "{name}");
        }
    }

    #[test]
    fn evaluates_if_expression_branch() {
        let module = Module {
            functions: Vec::new(),
            tests: vec![TestItem {
                name: "if_branch".into(),
                statements: vec![
                    Statement::Let(holo_ir::LetStatement {
                        name: "picked".into(),
                        ty: Some(TypeRef::Bool),
                        value: Expr::if_expression(
                            Expr::bool_literal(true, Span::new(0, 4)),
                            Expr::block(
                                Vec::new(),
                                Some(Expr::bool_literal(true, Span::new(10, 14))),
                                Span::new(8, 16),
                            ),
                            Some(Expr::block(
                                Vec::new(),
                                Some(Expr::bool_literal(false, Span::new(24, 29))),
                                Span::new(22, 31),
                            )),
                            Span::new(0, 31),
                        ),
                        span: Span::new(0, 31),
                    }),
                    Statement::Assert(AssertStatement {
                        expression: Expr::identifier("picked", Span::new(32, 38)),
                        span: Span::new(32, 39),
                    }),
                ],
                span: Span::new(0, 39),
            }],
        };

        let interpreter = BasicInterpreter::default();
        let summary = interpreter.run_tests(&module);
        assert_eq!(summary.failed, 0);
    }

    #[test]
    fn while_expression_executes_and_returns_unit() {
        let module = Module {
            functions: Vec::new(),
            tests: vec![TestItem {
                name: "while_once".into(),
                statements: vec![
                    Statement::Let(holo_ir::LetStatement {
                        name: "ran".into(),
                        ty: Some(TypeRef::Bool),
                        value: Expr::bool_literal(false, Span::new(0, 5)),
                        span: Span::new(0, 5),
                    }),
                    Statement::Expr(ExprStatement {
                        expression: Expr::while_expression(
                            Expr::identifier("ran", Span::new(6, 9)),
                            Expr::block(Vec::new(), None, Span::new(10, 12)),
                            Span::new(6, 12),
                        ),
                        span: Span::new(6, 13),
                    }),
                    Statement::Assert(AssertStatement {
                        expression: Expr::bool_literal(true, Span::new(14, 18)),
                        span: Span::new(14, 19),
                    }),
                ],
                span: Span::new(0, 19),
            }],
        };

        let interpreter = BasicInterpreter::default();
        let summary = interpreter.run_tests(&module);
        assert_eq!(summary.failed, 0);
    }

    #[test]
    fn block_expression_scopes_locals_at_runtime() {
        let module = Module {
            functions: Vec::new(),
            tests: vec![TestItem {
                name: "block_scope".into(),
                statements: vec![
                    Statement::Expr(ExprStatement {
                        expression: Expr::block(
                            vec![Statement::Let(holo_ir::LetStatement {
                                name: "inner".into(),
                                ty: Some(TypeRef::Bool),
                                value: Expr::bool_literal(true, Span::new(0, 4)),
                                span: Span::new(0, 4),
                            })],
                            Some(Expr::identifier("inner", Span::new(5, 10))),
                            Span::new(0, 11),
                        ),
                        span: Span::new(0, 12),
                    }),
                    Statement::Expr(ExprStatement {
                        expression: Expr::identifier("inner", Span::new(13, 18)),
                        span: Span::new(13, 19),
                    }),
                ],
                span: Span::new(0, 19),
            }],
        };

        let interpreter = BasicInterpreter::default();
        let summary = interpreter.run_tests(&module);
        assert_eq!(summary.failed, 1);
        assert!(summary.results[0]
            .failure_reason
            .as_ref()
            .is_some_and(|reason| reason.contains("unknown identifier `inner`")));
    }

    #[test]
    fn uses_let_annotation_to_interpret_unsuffixed_numeric_literal() {
        let module = Module {
            functions: Vec::new(),
            tests: vec![TestItem {
                name: "typed_let".into(),
                statements: vec![
                    Statement::Let(holo_ir::LetStatement {
                        name: "value".into(),
                        ty: Some(TypeRef::U32),
                        value: Expr::number_literal("1", Span::new(0, 1)),
                        span: Span::new(0, 2),
                    }),
                    Statement::Expr(ExprStatement {
                        expression: Expr::binary(
                            BinaryOperator::Add,
                            Expr::identifier("value", Span::new(3, 8)),
                            Expr::number_literal("1u32", Span::new(9, 13)),
                            Span::new(3, 13),
                        ),
                        span: Span::new(3, 14),
                    }),
                    Statement::Assert(AssertStatement {
                        expression: Expr::bool_literal(true, Span::new(15, 19)),
                        span: Span::new(15, 20),
                    }),
                ],
                span: Span::new(0, 20),
            }],
        };

        let interpreter = BasicInterpreter::default();
        let summary = interpreter.run_tests(&module);
        assert_eq!(summary.failed, 0);
    }

    #[test]
    fn uses_parameter_type_to_interpret_unsuffixed_call_literal() {
        let module = Module {
            functions: vec![FunctionItem {
                name: "inc".into(),
                parameters: vec![FunctionParameter {
                    name: "value".into(),
                    ty: TypeRef::U32,
                    span: Span::new(0, 1),
                }],
                return_type: TypeRef::U32,
                statements: vec![Statement::Expr(ExprStatement {
                    expression: Expr::binary(
                        BinaryOperator::Add,
                        Expr::identifier("value", Span::new(0, 5)),
                        Expr::number_literal("1u32", Span::new(6, 10)),
                        Span::new(0, 10),
                    ),
                    span: Span::new(0, 11),
                })],
                is_test: false,
                span: Span::new(0, 11),
            }],
            tests: vec![TestItem {
                name: "call_infer".into(),
                statements: vec![
                    Statement::Expr(ExprStatement {
                        expression: Expr::call(
                            Expr::identifier("inc", Span::new(12, 15)),
                            vec![Expr::number_literal("1", Span::new(16, 17))],
                            Span::new(12, 18),
                        ),
                        span: Span::new(12, 19),
                    }),
                    Statement::Assert(AssertStatement {
                        expression: Expr::bool_literal(true, Span::new(20, 24)),
                        span: Span::new(20, 25),
                    }),
                ],
                span: Span::new(12, 25),
            }],
        };

        let interpreter = BasicInterpreter::default();
        let summary = interpreter.run_tests(&module);
        assert_eq!(summary.failed, 0);
    }

    #[test]
    fn executes_tests_in_deterministic_order() {
        let tests = vec![
            TestItem {
                name: "z_case".into(),
                statements: vec![Statement::Assert(AssertStatement {
                    expression: Expr::bool_literal(true, Span::new(0, 4)),
                    span: Span::new(0, 5),
                })],
                span: Span::new(40, 45),
            },
            TestItem {
                name: "a_case".into(),
                statements: vec![Statement::Assert(AssertStatement {
                    expression: Expr::bool_literal(true, Span::new(0, 4)),
                    span: Span::new(0, 5),
                })],
                span: Span::new(0, 5),
            },
        ];

        let interpreter = BasicInterpreter::default();
        let summary = interpreter.run_collected_tests(&tests);
        assert_eq!(summary.executed, 2);
        assert_eq!(summary.results[0].name.as_str(), "a_case");
        assert_eq!(summary.results[1].name.as_str(), "z_case");
    }

    #[test]
    fn isolates_each_test_scope() {
        let module = Module {
            functions: Vec::new(),
            tests: vec![
                TestItem {
                    name: "a_setup".into(),
                    statements: vec![
                        Statement::Let(holo_ir::LetStatement {
                            name: "shared".into(),
                            ty: Some(TypeRef::Bool),
                            value: Expr::bool_literal(true, Span::new(0, 4)),
                            span: Span::new(0, 5),
                        }),
                        Statement::Assert(AssertStatement {
                            expression: Expr::bool_literal(true, Span::new(6, 10)),
                            span: Span::new(6, 11),
                        }),
                    ],
                    span: Span::new(0, 11),
                },
                TestItem {
                    name: "b_consumer".into(),
                    statements: vec![Statement::Assert(AssertStatement {
                        expression: Expr::identifier("shared", Span::new(12, 18)),
                        span: Span::new(12, 19),
                    })],
                    span: Span::new(12, 19),
                },
            ],
        };

        let interpreter = BasicInterpreter::default();
        let summary = interpreter.run_tests(&module);
        assert_eq!(summary.executed, 2);
        assert_eq!(summary.passed, 1);
        assert_eq!(summary.failed, 1);
        assert!(summary
            .results
            .iter()
            .any(|result| result.name == "b_consumer" && result.status == TestStatus::Failed));
    }
}
