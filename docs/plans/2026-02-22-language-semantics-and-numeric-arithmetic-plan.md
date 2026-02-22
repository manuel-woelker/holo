# How Do We Extend The Compiler Beyond Boolean-Only Tests?

## What Is The Goal?
Define and implement the next language slice after the minimal boolean/test pipeline:
- Variables and bindings.
- User-defined functions.
- Richer expressions.
- Number literals and typed numeric values.
- Arithmetic operations.

This plan focuses on language/frontend/runtime capability, not performance work.

## What Is In Scope?
- Expressions:
  - Boolean literals: `true`, `false`.
  - Numeric literals for `u32`, `u64`, `i32`, `i64`, `f32`, `f64`.
  - Identifiers.
  - Unary operators: `!`, unary `-`.
  - Binary arithmetic: `+`, `-`, `*`, `/`, `%`.
  - Parenthesized expressions.
  - Function calls.
- Statements:
  - `let` bindings.
  - `assert(expr);`.
  - Expression statements (optional, if required for parser simplicity).
- Declarations:
  - Functions with parameters and return type annotations.
  - Existing `#[test]` functions remain supported.
- Typechecker:
  - Numeric/operator compatibility rules.
  - Function call arity/type checking.
  - Name resolution for variables/functions.
- Interpreter:
  - Numeric runtime values and arithmetic evaluation.
  - Local scopes and call frames.
  - Test execution over new expression forms.

## What Is Explicitly Out Of Scope For This Slice?
- Generics and traits.
- Modules/imports across files.
- Closures and higher-order functions.
- Borrowing/ownership model.
- Optimizing compiler backend/IR.
- Advanced control flow (`if`, `while`, `match`) unless required by parser scaffolding.

## What Semantic Decisions Do We Lock In First?
- Numeric literals are inferred from context when possible; otherwise default:
  - Integer literal default: `i64`.
  - Float literal default: `f64`.
- Mixed-type arithmetic is disallowed in V1 unless an explicit conversion function exists.
- Division/modulo by zero is a runtime test failure (interpreter diagnostic), not a crash.
- `%` is valid only for integer types.
- Unary `!` is valid only for `bool`.
- Unary `-` is valid only for signed integers and floats.
- Functions must be declared before use in the same file for V1 (can be relaxed later).

## What Milestones Should We Implement In Order?
1. **AST + Parser foundation - Done**
- Add AST variants for identifiers, numeric literals, unary/binary ops, call expressions, `let`, and function signatures.
- Implement expression precedence and associativity.
- Add parser diagnostics/recovery for malformed arithmetic and calls.

2. **Type system primitives - Done**
- Add `Type` variants: `Bool`, `U32`, `U64`, `I32`, `I64`, `F32`, `F64`, and `Unit`.
- Define operator typing tables for unary/binary operations.
- Introduce function type representation (param types + return type).

3. **Name resolution + function checking - Done**
- Track variable scopes and function symbol table.
- Validate `let` redefinitions/shadowing policy (allow shadowing in inner scopes only).
- Validate function declarations and call sites (existence, arity, argument types, return type use).

4. **Interpreter numeric + function runtime - Done**
- Add runtime value enum for all supported primitives.
- Implement arithmetic evaluation with checked behavior and diagnostics.
- Implement call frames, parameter binding, and return values.

5. **Diagnostics + UX - In Progress**
- Expand `SourceDiagnostic` usage for type and runtime errors in new constructs.
- Add concise primary message plus annotated spans for operator/type mismatch and bad calls.
- Ensure deck issue titles remain concise and line-addressable.

6. **Core/daemon integration - Pending**
- Ensure compile-and-test cycles continue after non-fatal per-file diagnostics.
- Keep dependency/query invalidation correct for function and expression changes.
- Preserve persistent cache compatibility via schema bump where needed.

7. **Test coverage hardening - Pending**
- Add data-driven parser/typechecker/interpreter tests for all numeric types and operators.
- Add negative tests for unsupported mixes (`i64 + f64`, `bool + bool`, etc.).
- Add end-to-end CLI/daemon tests over multi-file examples using functions + arithmetic.

## What Are The Minimum Acceptance Criteria?
- A file can define typed functions and `#[test]` functions that use:
  - `let` bindings,
  - numeric literals (`u32`, `u64`, `i32`, `i64`, `f32`, `f64`),
  - arithmetic operations.
- Typechecker rejects invalid operator/type combinations with span-annotated diagnostics.
- Interpreter executes valid tests and reports runtime failures (e.g., division by zero) as diagnostics.
- Deck shows resulting diagnostics without regression in current issue formatting.

## What Risks Should We Track?
- Parser ambiguity and precedence bugs when introducing calls and unary/binary operators.
- Type inference corner cases for literals without context.
- Runtime mismatch between typechecker rules and interpreter behavior.
- Cache invalidation bugs after AST/type schema expansion.

## How Do We Sequence Delivery Practically?
- Start with integer-only arithmetic (`i64`) behind the full AST shape.
- Then add remaining numeric types and operator matrix.
- Then enable function calls with typed signatures.
- Then tighten diagnostics and end-to-end integration tests.
