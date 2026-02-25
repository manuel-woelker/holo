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

### 2. AST [PENDING]

**File:** `crates/ast/src/lib.rs`

- [ ] Add comparison operators to `BinaryOperator` enum:
  - [ ] `Equals`
  - [ ] `NotEquals`
  - [ ] `LessThan`
  - [ ] `GreaterThan`
  - [ ] `LessThanOrEqual`
  - [ ] `GreaterThanOrEqual`

### 3. Parser [PENDING]

**File:** `crates/parser/src/lib.rs`

- [ ] Add new precedence levels between `parse_expression` and `parse_additive`:
  - [ ] `parse_equality` for `==` and `!=`
  - [ ] `parse_comparison` for `<`, `>`, `<=`, `>=`
- [ ] Update precedence chain

### 4. Typechecker [PENDING]

**File:** `crates/typechecker/src/lib.rs`

- [ ] Add handling for comparison operators in `typecheck_expression`
- [ ] Type rules for equality and ordering operators

### 5. IR [PENDING]

**File:** `crates/ir/src/lib.rs`

- [ ] Add comparison operators to IR's `BinaryOperator` enum
- [ ] Update `lower_binary_operator` function

### 6. Interpreter [PENDING]

**File:** `crates/interpreter/src/lib.rs`

- [ ] Add comparison implementations for all numeric types
- [ ] Implement equality for Bool type

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
