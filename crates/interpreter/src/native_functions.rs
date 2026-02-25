//! Built-in native functions for the holo language.
//!
//! This module provides native (host) functions that are available to all holo programs.
//! These functions are implemented in Rust and can be called from holo code.
//!
//! # Available Functions
//!
//! - [`print()`](PrintFunction) - Prints an i64 value to stdout without a newline
//! - [`println()`](PrintlnFunction) - Prints an i64 value to stdout with a newline
//!
//! # Usage
//!
//! The built-in functions are automatically registered when using [`create_builtin_registry()`].
//! The [`CompilerCore`](crate::CompilerCore) uses this by default.
//!
//! ```holo
//! // In holo code:
//! println(42);  // prints "42" followed by newline
//! print(10);    // prints "10" without newline
//! ```
//!
//! # Creating Custom Native Functions
//!
//! To create your own native functions, implement the [`NativeFunction`](crate::NativeFunction) trait:
//!
//! ```rust
//! use holo_interpreter::{NativeFunction, Type, Value, RuntimeError};
//! use holo_base::SharedString;
//!
//! struct MyFunction {
//!     name: SharedString,
//! }
//!
//! impl MyFunction {
//!     fn new() -> Self {
//!         Self { name: "my_func".into() }
//!     }
//! }
//!
//! impl NativeFunction for MyFunction {
//!     fn name(&self) -> &SharedString { &self.name }
//!     fn param_types(&self) -> &[Type] { &[Type::I64] }
//!     fn return_type(&self) -> Type { Type::Unit }
//!
//!     fn call(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
//!         // Your implementation here
//!         Ok(Value::Unit)
//!     }
//! }
//! ```

use crate::output_stream::{OutputStream, ProductionOutputStream};
use crate::{NativeFunction, NativeFunctionRegistry, RuntimeError, Type, Value};
use holo_base::SharedString;
use std::sync::{Arc, Mutex};

/// Native function that prints an i64 value to stdout.
///
/// # Signature
///
/// ```holo
/// fn print(value: i64) -> ()
/// ```
///
/// # Example
///
/// ```holo
/// print(42);  // outputs "42" without newline
/// ```
pub struct PrintFunction {
    name: SharedString,
    output: Arc<dyn OutputStream>,
}

impl Default for PrintFunction {
    fn default() -> Self {
        Self {
            name: "print".into(),
            output: Arc::new(ProductionOutputStream::stdout()),
        }
    }
}

impl PrintFunction {
    /// Creates a new print function with the default stdout output stream.
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new print function with a custom output stream.
    pub fn with_output(output: Arc<dyn OutputStream>) -> Self {
        Self {
            name: "print".into(),
            output,
        }
    }
}

impl NativeFunction for PrintFunction {
    fn name(&self) -> &SharedString {
        &self.name
    }

    fn param_types(&self) -> &[Type] {
        &[Type::I64]
    }

    fn return_type(&self) -> Type {
        Type::Unit
    }

    fn call(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if let Some(Value::I64(n)) = args.first() {
            self.output.write(&n.to_string());
        }
        Ok(Value::Unit)
    }
}

/// Native function that prints an i64 value followed by a newline.
///
/// # Signature
///
/// ```holo
/// fn println(value: i64) -> ()
/// ```
///
/// # Example
///
/// ```holo
/// println(42);  // outputs "42" followed by newline
/// ```
pub struct PrintlnFunction {
    name: SharedString,
    output: Arc<dyn OutputStream>,
}

impl Default for PrintlnFunction {
    fn default() -> Self {
        Self {
            name: "println".into(),
            output: Arc::new(ProductionOutputStream::stdout()),
        }
    }
}

impl PrintlnFunction {
    /// Creates a new println function with the default stdout output stream.
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new println function with a custom output stream.
    pub fn with_output(output: Arc<dyn OutputStream>) -> Self {
        Self {
            name: "println".into(),
            output,
        }
    }
}

impl NativeFunction for PrintlnFunction {
    fn name(&self) -> &SharedString {
        &self.name
    }

    fn param_types(&self) -> &[Type] {
        &[Type::I64]
    }

    fn return_type(&self) -> Type {
        Type::Unit
    }

    fn call(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if let Some(Value::I64(n)) = args.first() {
            self.output.write_line(&n.to_string());
        }
        Ok(Value::Unit)
    }
}

/// Creates a registry with all built-in native functions registered.
///
/// This function creates a new [`NativeFunctionRegistry`] and registers
/// all built-in native functions:
///
/// - [`print`](PrintFunction) - Print i64 without newline
/// - [`println`](PrintlnFunction) - Print i64 with newline
///
/// # Returns
///
/// An `Arc<NativeFunctionRegistry>` containing all built-in functions.
///
/// # Example
///
/// ```rust
/// use holo_interpreter::native_functions;
///
/// let registry = native_functions::create_builtin_registry();
/// assert!(registry.contains(&"print".into()));
/// assert!(registry.contains(&"println".into()));
/// ```
pub fn create_builtin_registry() -> Arc<NativeFunctionRegistry> {
    let mut registry = NativeFunctionRegistry::default();
    registry.register(PrintFunction::new());
    registry.register(PrintlnFunction::new());
    Arc::new(registry)
}

/// Creates a registry with test output streams for capturing print output.
///
/// This function creates print functions that write to a shared buffer,
/// useful for testing and conformance tests where print output needs to be
/// captured for comparison.
///
/// # Returns
///
/// A tuple containing:
/// - The registry with test-configured print functions
/// - An Arc to the shared buffer for reading captured output
///
/// # Example
///
/// ```rust
/// use holo_interpreter::{native_functions, Value};
/// use std::sync::{Arc, Mutex};
/// use holo_base::SharedString;
///
/// let (registry, buffer) = native_functions::create_test_registry();
/// // Get the print function and call it
/// let print_fn = registry.lookup(&"print".into()).unwrap();
/// print_fn.call(vec![Value::I64(42)]).ok();
/// // Check the captured output
/// let output = buffer.lock().unwrap();
/// assert_eq!(output.as_str(), "42");
/// ```
pub fn create_test_registry() -> (Arc<NativeFunctionRegistry>, Arc<Mutex<SharedString>>) {
    let buffer = Arc::new(Mutex::new(SharedString::new()));
    let output = Arc::new(crate::output_stream::TestOutputStream::with_buffer(
        buffer.clone(),
    ));

    let mut registry = NativeFunctionRegistry::default();
    registry.set_output_buffer(buffer.clone());
    registry.register(PrintFunction::with_output(output.clone()));
    registry.register(PrintlnFunction::with_output(output));

    (Arc::new(registry), buffer)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::output_stream::TestOutputStream;
    use holo_base::SharedString;
    use std::sync::{Arc, Mutex};

    #[test]
    fn print_function_has_correct_signature() {
        let print = PrintFunction::new();
        assert_eq!(print.name().as_str(), "print");
        assert_eq!(print.param_types(), &[Type::I64]);
        assert_eq!(print.return_type(), Type::Unit);
    }

    #[test]
    fn println_function_has_correct_signature() {
        let println = PrintlnFunction::new();
        assert_eq!(println.name().as_str(), "println");
        assert_eq!(println.param_types(), &[Type::I64]);
        assert_eq!(println.return_type(), Type::Unit);
    }

    #[test]
    fn print_function_call_with_i64() {
        let buffer = Arc::new(Mutex::new(SharedString::new()));
        let output = Arc::new(TestOutputStream::with_buffer(buffer.clone()));
        let print = PrintFunction::with_output(output);
        let result = print.call(vec![Value::I64(42)]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Unit);
        assert_eq!(buffer.lock().unwrap().as_str(), "42");
    }

    #[test]
    fn println_function_call_with_i64() {
        let buffer = Arc::new(Mutex::new(SharedString::new()));
        let output = Arc::new(TestOutputStream::with_buffer(buffer.clone()));
        let println = PrintlnFunction::with_output(output);
        let result = println.call(vec![Value::I64(42)]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Unit);
        assert_eq!(buffer.lock().unwrap().as_str(), "42\n");
    }

    #[test]
    fn builtin_registry_contains_functions() {
        let registry = create_builtin_registry();
        assert!(registry.contains(&"print".into()));
        assert!(registry.contains(&"println".into()));
    }
}
