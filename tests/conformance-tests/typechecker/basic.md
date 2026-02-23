# Typechecker Conformance Cases

## Case: rejects mixed numeric types

```holo
fn bad() -> i64 { 1i64 + 2.0f64; }
```

```fails-typecheck
error: arithmetic operands must have the same type
```

## Case: rejects non-boolean assert

```holo
#[test]
fn bad_assert() { assert(1i64); }
```

```fails-typecheck
error: assert expects a boolean expression
```

## Case: accepts simple numeric function

```holo
fn add(a: i64, b: i64) -> i64 { a + b; }
```

```text
ok
```
