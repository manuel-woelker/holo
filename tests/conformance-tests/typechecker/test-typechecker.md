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
