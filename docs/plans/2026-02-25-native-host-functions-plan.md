# Native/Host Functions Implementation Plan

## Overview

This plan outlines implementing native (host) functions that work across the interpreter, future bytecode VM, and compiled backends. Native functions are Rust functions provided by the host environment that holo code can call.

## Requirements

1. **Global scope** - Native functions are available globally, not per-module
2. **Compile-time type-checking** - Native function calls are type-checked during compilation
3. **FFI deferred** - Foreign function interface for external libraries will be added later
4. **Cross-backend support** - Must work in interpreter, bytecode VM, and compiled backends

## Current Architecture

- **Interpreter** (`crates/interpreter`): Direct IR execution via `BasicInterpreter`
  - Functions looked up by name from `HashMap<SharedString, FunctionItem>`
  - Call resolution at runtime in `ExprKind::Call` handler (lines 225-256)
- **IR** (`crates/ir`): `Module` struct with `functions: Vec<FunctionItem>` and `tests: Vec<TestItem>`
- **No bytecode VM or compiled backend exists yet**

## Implementation Steps

### Phase 1: Core Infrastructure

#### 1.1 Define NativeFunction trait

Create in `crates/interpreter/src/lib.rs` (or new `crates/runtime/src/lib.rs`):

```rust
use holo_ir::{Type, Value};
use holo_base::SharedString;

/// Trait for native (host-provided) functions.
pub trait NativeFunction: Send + Sync {
    /// Returns the function's name as it will be called from holo code.
    fn name(&self) -> &SharedString;
    
    /// Returns the parameter types in order.
    fn param_types(&self) -> &[Type];
    
    /// Returns the return type.
    fn return_type(&self) -> Type;
    
    /// Invokes the function with the given arguments.
    fn call(&self, args: Vec<Value>) -> Result<Value, RuntimeError>;
}
```

#### 1.2 Extend Value enum

The interpreter's `Value` enum needs to represent native function handles:

```rust
enum Value {
    Bool(bool),
    U32(u32),
    U64(u64),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Unit,
    // Add: Native function reference (for passing as values if needed later)
    // NativeFunctionRef(Box<dyn NativeFunction>),
}
```

#### 1.3 Create NativeFunctionRegistry

```rust
use std::sync::Arc;
use std::collections::HashMap;

/// Global registry for native functions.
#[derive(Default)]
pub struct NativeFunctionRegistry {
    functions: HashMap<SharedString, Arc<dyn NativeFunction>>,
}

impl NativeFunctionRegistry {
    pub fn register(&mut self, function: impl NativeFunction + 'static) {
        let name = function.name().clone();
        self.functions.insert(name, Arc::new(function));
    }
    
    pub fn lookup(&self, name: &SharedString) -> Option<Arc<dyn NativeFunction>> {
        self.functions.get(name).cloned()
    }
    
    pub fn contains(&self, name: &SharedString) -> bool {
        self.functions.contains_key(name)
    }
}
```

### Phase 2: Interpreter Integration

#### 2.1 Modify BasicInterpreter

Add the registry as a field:

```rust
pub struct BasicInterpreter {
    native_functions: Arc<NativeFunctionRegistry>,
}

impl BasicInterpreter {
    pub fn new(native_functions: Arc<NativeFunctionRegistry>) -> Self {
        Self { native_functions }
    }
}
```

#### 2.2 Update Call resolution in eval_expr

In `ExprKind::Call` handler (around line 225), check native functions first:

```rust
ExprKind::Call(call) => {
    let ExprKind::Identifier(callee_name) = &call.callee.kind else {
        return Err(RuntimeError {
            span: call.callee.span,
            message: "call target is not a function name".into(),
        });
    };
    
    // First check native functions
    if let Some(native) = self.native_functions.lookup(callee_name) {
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
        
        // Evaluate arguments
        let mut evaluated_args = Vec::with_capacity(call.arguments.len());
        for (argument, param_ty) in call.arguments.iter().zip(native.param_types().iter()) {
            let value = Self::eval_expr(argument, scopes, functions, Some(*param_ty))?;
            evaluated_args.push(value);
        }
        
        // Call native function
        return native.call(evaluated_args);
    }
    
    // Fall back to user-defined functions
    // ... existing code ...
}
```

### Phase 3: Type System Integration

#### 3.1 Extend IR Type for native functions

Add a new type variant to represent native function signatures:

```rust
// In crates/ir/src/lib.rs
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
    // Add: Native function type with signature
    NativeFunction {
        param_types: Vec<Type>,
        return_type: Box<Type>,
    },
}
```

#### 3.2 Type representation for native calls

When lowering from AST, native functions need special handling:

```rust
// In IR lowering (holo_ir/src/lib.rs)
// Create a type for a native function signature
pub fn native_function_type(param_types: Vec<Type>, return_type: Type) -> Type {
    Type::NativeFunction {
        param_types,
        return_type: Box::new(return_type),
    }
}
```

#### 3.3 Typechecker integration

In the typechecker, when resolving a call:

1. Look up the name in both user functions AND native function registry
2. If found in native registry, use the registered signature for type inference
3. Emit appropriate type errors if arguments don't match

### Phase 4: Bytecode VM Design (Future)

#### 4.1 Bytecode representation

```rust
enum Opcode {
    // ... existing opcodes ...
    CallNative(u16),  // Index into native function table
    Call(u16),       // Index into user function table
}
```

#### 4.2 VM integration

The bytecode VM would have a native function table:

```rust
struct Vm {
    // ... existing fields ...
    native_functions: Arc<NativeFunctionRegistry>,
    // Native function indices for fast lookup
    native_indices: HashMap<SharedString, u16>,
}
```

### Phase 5: Compiled Backend Design (Future)

#### 5.1 Codegen strategy

For native function calls in compiled code:

1. Generate direct Rust function calls via function pointers
2. Or generate extern "C" declarations for FFI later
3. Include type signatures for validation at link time

### Phase 6: User-Facing API

#### 6.1 Registering native functions

```rust
// Example: Creating and registering a native function
struct PrintFunction;

impl NativeFunction for PrintFunction {
    fn name(&self) -> &SharedString {
        &"print".into()
    }
    
    fn param_types(&self) -> &[Type] {
        &[Type::I64]
    }
    
    fn return_type(&self) -> Type {
        Type::Unit
    }
    
    fn call(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if let Some(Value::I64(n)) = args.first() {
            println!("{}", n);
        }
        Ok(Value::Unit)
    }
}

// Registration
let mut registry = NativeFunctionRegistry::default();
registry.register(PrintFunction);
let interpreter = BasicInterpreter::new(Arc::new(registry));
```

#### 6.2 Convenience macros

Consider adding macros to reduce boilerplate:

```rust
holo_native_function! {
    fn print(value: i64) -> Unit = "print"
}
```

## File Changes Summary

| File | Changes |
|------|---------|
| `crates/interpreter/src/lib.rs` | Add `NativeFunction` trait, extend `Value`, modify `BasicInterpreter`, update call resolution |
| `crates/ir/src/lib.rs` | Add `Type::NativeFunction` variant, helper functions |
| `crates/typechecker/src/lib.rs` | Integrate native function lookup in call resolution |
| `crates/core/src/lib.rs` | Pass registry through compilation pipeline |

## Testing Strategy

1. Unit tests for `NativeFunctionRegistry` lookup
2. Interpreter integration tests with registered native functions
3. Typechecker tests for native function signature validation
4. Edge cases: wrong argument count, type mismatches, unknown native function

## Open Questions

1. **Error handling**: Should native function type mismatches fail at compile-time or runtime?
2. **Overloading**: Should we support multiple native functions with same name but different signatures?
3. **Module namespacing**: Should native functions be namespaced (e.g., `io::print`)?
4. **Performance**: Should native functions be inlined in the bytecode/compiled output?

## Implementation Checklist

### Phase 1: Core Infrastructure ✅

- [x] **1.1** Define `NativeFunction` trait in `crates/interpreter/src/lib.rs`
  - [x] `name()` method returning `&SharedString`
  - [x] `param_types()` method returning `&[Type]`
  - [x] `return_type()` method returning `Type`
  - [x] `call()` method with `Vec<Value>` -> `Result<Value, RuntimeError>`
- [x] **1.2** Extend `Value` enum (if needed for function handles)
- [x] **1.3** Create `NativeFunctionRegistry` struct
  - [x] `register()` method
  - [x] `lookup()` method
  - [x] `contains()` method
- [x] Run tests: `cargo test --workspace`

### Phase 2: Interpreter Integration

- [ ] **2.1** Add `native_functions: Arc<NativeFunctionRegistry>` to `BasicInterpreter`
- [ ] **2.2** Update `BasicInterpreter::new()` to accept registry
- [ ] **2.3** Modify `ExprKind::Call` handler in `eval_expr`
  - [ ] Check native functions first
  - [ ] Validate argument count
  - [ ] Evaluate arguments with type coercion
  - [ ] Call native function and return result
  - [ ] Fall back to user-defined functions
- [ ] Run tests: `cargo test --workspace`
- [ ] Run checks: `./scripts/check-code.sh`

### Phase 3: Type System Integration

- [ ] **3.1** Add `Type::NativeFunction` variant to IR
  - [ ] `param_types: Vec<Type>`
  - [ ] `return_type: Box<Type>`
- [ ] **3.2** Add `native_function_type()` helper function
- [ ] **3.3** Integrate native function lookup in typechecker
  - [ ] Look up in both user functions AND native registry
  - [ ] Use registered signature for type inference
  - [ ] Emit type errors for mismatches
- [ ] Run tests: `cargo test --workspace`
- [ ] Run checks: `./scripts/check-code.sh`

### Phase 4: Typechecker Integration

- [ ] Update `crates/typechecker/src/lib.rs` to accept registry
- [ ] Modify call resolution to check native functions
- [ ] Add compile-time argument count validation
- [ ] Add compile-time type mismatch errors
- [ ] Run tests: `cargo test --workspace`
- [ ] Run checks: `./scripts/check-code.sh`

### Phase 5: Core Pipeline Integration

- [ ] Update `crates/core/src/lib.rs` to pass registry through pipeline
- [ ] Update daemon to initialize registry
- [ ] Update CLI to pass registry to interpreter
- [ ] Run tests: `cargo test --workspace`
- [ ] Run checks: `./scripts/check-code.sh`

### Phase 6: Example Native Functions

- [ ] Create example `print()` function
- [ ] Create example `println()` function
- [ ] Test native functions from holo code
- [ ] Add integration tests
- [ ] Run tests: `cargo test --workspace`
- [ ] Run checks: `./scripts/check-code.sh`

### Phase 7: Documentation & Cleanup

- [ ] Add inline documentation for public API
- [ ] Document usage in `docs/` directory
- [ ] Add example holo files demonstrating native function usage
- [ ] Run final checks: `./scripts/check-code.sh`
- [ ] Write journal entry and commit

### Future Phases (Deferred)

- [ ] **Phase 8:** Bytecode VM integration (`CallNative` opcode)
- [ ] **Phase 9:** Compiled backend codegen
- [ ] **Phase 10:** Convenience macros (`holo_native_function!`)
- [ ] **Phase 11:** FFI support for external libraries
