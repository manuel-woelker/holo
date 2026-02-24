# Typechecker Conformance Cases

## Case: rejects mixed numeric types

```holo
fn bad() -> i64 { 1i64 + 2.0f64; }
```

### Fails typecheck

```text
⚒️ Typecheck: arithmetic operands must have the same type

conformance-case.holo:1
   1 │ fn bad() -> i64 { 1i64 + 2.0f64; }
     │                   ───┬   ────── right operand has type `f64`
     │                      └─ left operand has type `i64`
```

## Case: rejects non-boolean assert

```holo
#[test]
fn bad_assert() { assert(1i64); }
```

### Fails typecheck

```text
⚒️ Typecheck: assert expects a boolean expression

conformance-case.holo:2
   2 │ fn bad_assert() { assert(1i64); }
     │                   ───────────── this assertion does not evaluate to `bool`
```

## Case: accepts simple numeric function

```holo
fn add(a: i64, b: i64) -> i64 { a + b; }
```

### Succeeds

## Case: accepts unary operators for valid types

> Edge case note: `!` only applies to bool and unary `-` only applies to signed numeric operands.

```holo
fn ops() -> i64 {
    let truth: bool = !false;
    let signed: i64 = -3i64;
    signed;
}
```

### Succeeds

## Case: rejects modulo on floating point operands

> Edge case note: `%` is restricted to integer types, even when both operand types match.

```holo
fn bad_mod() -> f64 { 5.0f64 % 2.0f64; }
```

### Fails typecheck

```text
⚒️ Typecheck: operator `%` is only valid for integer types

conformance-case.holo:1
   1 │ fn bad_mod() -> f64 { 5.0f64 % 2.0f64; }
     │                       ─────────────── operands have type `f64` but `%` requires integer types
```

## Case: rejects call argument count mismatch

> Edge case note: call arity mismatch should include a pointer to both call site and definition.

```holo
fn add(a: i64, b: i64) -> i64 { a + b; }
fn use_it() -> i64 { add(1i64); }
```

### Fails typecheck

```text
⚒️ Typecheck: function `add` expects 2 argument(s) but got 1

conformance-case.holo:1
   1 │ fn add(a: i64, b: i64) -> i64 { a + b; }
     │ ──────────────────────────────────────── function `add` is defined here

conformance-case.holo:2
   2 │ fn use_it() -> i64 { add(1i64); }
     │                      ───────── call argument count does not match function signature
```

## Case: rejects duplicate local binding

> Edge case note: duplicate names in the same function scope are rejected.

```holo
fn dup() -> i64 {
    let value: i64 = 1i64;
    let value: i64 = 2i64;
    value;
}
```

### Fails typecheck

```text
⚒️ Typecheck: duplicate local binding `value`

conformance-case.holo:3
   3 │     let value: i64 = 2i64;
     │     ────────────────────── this binding name is already defined in this scope
```

## Case: accepts matching return type

```holo
fn returns_i64() -> i64 { 42i64; }
```

### Succeeds

## Case: rejects unknown function call

```holo
fn use_unknown() -> i64 { unknown_fn(); }
```

### Fails typecheck

```text
⚒️ Typecheck: unknown function `unknown_fn`

conformance-case.holo:1
   1 │ fn use_unknown() -> i64 { unknown_fn(); }
     │                           ────────── this function is not defined
```

## Case: rejects call with wrong argument type

```holo
fn takes_i64(x: i64) -> i64 { x; }
fn wrong_arg() -> i64 { takes_i64(1.0f64); }
```

### Fails typecheck

```text
⚒️ Typecheck: call argument type does not match parameter type

conformance-case.holo:2
   2 │ fn wrong_arg() -> i64 { takes_i64(1.0f64); }
     │                                   ────── left operand has type `i64`
     │                                        └─ right operand has type `f64`
     │                                        └─ implicit numeric conversions are not allowed; use explicit literal suffixes
```
