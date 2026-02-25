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

### 1. Lexer [DONE]

**File:** `crates/lexer/src/lib.rs`

- [x] Add new `TokenKind` variants:
  - [x] `DoubleEquals` for `==`
  - [x] `BangEquals` for `!=`
  - [x] `LessThan` for `<`
  - [x] `GreaterThan` for `>`
  - [x] `LessThanEquals` for `<=`
  - [x] `GreaterThanEquals` for `>=`
- [x] Update lexer to recognize these multi-character tokens

### 2. AST [PENDING]

### 2. AST [DONE]

**File:** `crates/ast/src/lib.rs`

- [x] Add comparison operators to `BinaryOperator` enum:
  - [x] `Equals`
  - [x] `NotEquals`
  - [x] `LessThan`
  - [x] `GreaterThan`
  - [x] `LessThanOrEqual`
  - [x] `GreaterThanOrEqual`

### 3. Parser [DONE]

**File:** `crates/parser/src/lib.rs`

- [x] Add new precedence levels between `parse_expression` and `parse_additive`:
  - [x] `parse_equality` for `==` and `!=`
  - [x] `parse_comparison` for `<`, `>`, `<=`, `>=`
- [x] Update precedence chain

### 4. Typechecker [DONE]

**File:** `crates/typechecker/src/lib.rs`

- [x] Add handling for comparison operators in `typecheck_expression`
- [x] Type rules for equality and ordering operators

### 5. IR [DONE]

**File:** `crates/ir/src/lib.rs`

- [x] Add comparison operators to IR's `BinaryOperator` enum
- [x] Update `lower_binary_operator` function

### 6. Interpreter [DONE]

**File:** `crates/interpreter/src/lib.rs`

- [x] Add comparison operators to IR's `BinaryOperator` enum (done in IR step)
- [x] Add comparison implementations for all numeric types
- [x] Implement equality for Bool type

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
