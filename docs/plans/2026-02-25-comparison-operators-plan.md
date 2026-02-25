# Comparison Operators Implementation Plan

## Overview

Implement comparison operators (`==`, `!=`, `<`, `>`, `<=`, `>=`) for the holo programming language. These operators compare two values and return a boolean result.

## Operator Reference

| Operator | Description | Example |
|----------|-------------|---------|
| `==` | Equal to | `1 == 1` → `true` |
| `!=` | Not equal to | `1 != 2` → `true` |
| `<` | Less than | `1 < 2` → `true` |
| `>` | Greater than | `2 > 1` → `true` |
| `<=` | Less than or equal | `1 <= 1` → `true` |
| `>=` | Greater than or equal | `2 >= 1` → `true` |

## Implementation Steps

### 1. Lexer

**File:** `crates/lexer/src/lib.rs`

- Add new `TokenKind` variants:
  - `DoubleEquals` for `==`
  - `BangEquals` for `!=`
  - `LessThan` for `<`
  - `GreaterThan` for `>`
  - `LessThanEquals` for `<=`
  - `GreaterThanEquals` for `>=`
- Update lexer to recognize these multi-character tokens

### 2. AST

**File:** `crates/ast/src/lib.rs`

- Add comparison operators to `BinaryOperator` enum:
  ```rust
  pub enum BinaryOperator {
      // ... existing arithmetic operators ...
      Equals,
      NotEquals,
      LessThan,
      GreaterThan,
      LessThanOrEqual,
      GreaterThanOrEqual,
  }
  ```

### 3. Parser

**File:** `crates/parser/src/lib.rs`

- Add new precedence levels between `parse_expression` and `parse_additive`:
  - `parse_equality` for `==` and `!=`
  - `parse_comparison` for `<`, `>`, `<=`, `>=`
- Update precedence chain:
  ```
  parse_expression → parse_equality → parse_comparison → parse_additive
  ```

### 4. Typechecker

**File:** `crates/typechecker/src/lib.rs`

- Add handling for comparison operators in `typecheck_expression`
- Type rules:
  - **Equality** (`==`, `!=`): Both operands must have the same type
  - **Ordering** (`<`, `>`, `<=`, `>=`): Both operands must be numeric types
- Return type is always `Bool`

### 5. IR

**File:** `crates/ir/src/lib.rs`

- Add comparison operators to IR's `BinaryOperator` enum
- Update `lower_binary_operator` function to map new operators
- Set expression type to `Type::Bool` for comparison results

### 6. Interpreter

**File:** `crates/interpreter/src/lib.rs`

- Add comparison implementations in `evaluate_binary_expression`
- Implement for all numeric types:
  - `U32`, `U64`, `I32`, `I64`, `F32`, `F64`
- Implement equality for `Bool` type

## Testing

### Parser Tests
- Parse `1 == 2`, `1 != 2`, `1 < 2`, `1 > 2`, `1 <= 2`, `1 >= 2`
- Verify precedence: `1 + 2 < 3 + 4` groups as `(1 + 2) < (3 + 4)`
- Verify chaining: `1 < 2 < 3`

### Typechecker Tests
- `1 < 2` returns `Bool`
- `true == false` returns `Bool`
- `1 == "str"` type error
- `1 < "str"` type error

### Interpreter Tests
- `1 < 2` → `true`
- `2 > 3` → `false`
- `1 <= 1` → `true`
- `2 >= 3` → `false`
- `1 == 1` → `true`
- `1 != 1` → `false`
- `1.5 < 2.5` → `true`
- `true == true` → `true`

## Notes

- Comparison operators should have lower precedence than arithmetic operators
- Floating-point comparisons should follow IEEE 754 semantics
- Equality works with all types including bool and unit
