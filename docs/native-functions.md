# Native Functions in holo

Native functions are Rust functions that can be called from holo code. They provide a way to extend holo with host-provided functionality.

## Overview

Native functions:
- Are available **globally** in all holo modules
- Are **type-checked at compile-time**
- Work across all backends (interpreter, bytecode VM, compiled)
- Are implemented using the `NativeFunction` trait in Rust

## Built-in Functions

The following native functions are available by default:

### `print(value: i64) -> ()`

Prints an integer value to stdout without a trailing newline.

```holo
print(42);    // outputs: 42
print(10);    // outputs: 10 (on same line)
```

### `println(value: i64) -> ()`

Prints an integer value to stdout with a trailing newline.

```holo
println(42);  // outputs: 42\n
println(10);  // outputs: 10\n (on next line)
```

## Creating Custom Native Functions

To create your own native functions:

### 1. Implement the `NativeFunction` trait

```rust
use holo_interpreter::{NativeFunction, Type, Value, RuntimeError};
use holo_base::SharedString;

struct MyFunction {
    name: SharedString,
}

impl MyFunction {
    fn new() -> Self {
        Self { name: "my_func".into() }
    }
}

impl NativeFunction for MyFunction {
    fn name(&self) -> &SharedString {
        &self.name
    }

    fn param_types(&self) -> &[Type] {
        &[Type::I64, Type::I64]  // Takes two i64 arguments
    }

    fn return_type(&self) -> Type {
        Type::I64  // Returns i64
    }

    fn call(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        // Extract arguments
        let Value::I64(a) = args[0] else {
            return Err(RuntimeError { /* ... */ });
        };
        let Value::I64(b) = args[1] else {
            return Err(RuntimeError { /* ... */ });
        };

        // Implement function logic
        Ok(Value::I64(a + b))
    }
}
```

### 2. Register the function

```rust
use holo_interpreter::NativeFunctionRegistry;
use std::sync::Arc;

let mut registry = NativeFunctionRegistry::default();
registry.register(MyFunction::new());
```

### 3. Use in holo code

```holo
let result = my_func(10, 20);  // Calls the Rust function
println(result);               // outputs: 30
```

## Type System

Native function signatures are enforced at compile-time:

- **Argument count** must match exactly
- **Argument types** are type-checked against the declared `param_types()`
- **Return type** is inferred from `return_type()`

Type errors are caught during typechecking, before execution.

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│  NativeFunctionRegistry                                 │
│  ┌─────────────────────────────────────────────────┐   │
│  │  HashMap<SharedString, Arc<dyn NativeFunction>> │   │
│  │  - "print"    → PrintFunction                   │   │
│  │  - "println"  → PrintlnFunction                 │   │
│  │  - "my_func"  → MyFunction                      │   │
│  └─────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────┘
                          │
                          │ Shared via Arc
                          ▼
┌──────────────────┐  ┌──────────────────────┐
│  Typechecker     │  │  Interpreter         │
│  - Looks up      │  │  - Looks up          │
│    signatures    │  │    at runtime        │
│  - Type checks   │  │  - Validates args    │
│    calls         │  │  - Executes call     │
└──────────────────┘  └──────────────────────┘
```

## Future Extensions

Future versions may add:
- More built-in functions (string operations, I/O, etc.)
- FFI support for calling external C libraries
- Convenience macros for defining native functions
- Support for passing native functions as values
