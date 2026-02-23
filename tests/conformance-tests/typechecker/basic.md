# Typechecker Conformance Cases

## Case: rejects mixed numeric types

```holo
fn bad() -> i64 { 1i64 + 2.0f64; }
```

```fails-typecheck
typecheck error: arithmetic operands must have the same type
--> line 1, column 19
   1 | fn bad() -> i64 { 1i64 + 2.0f64; }
     |                   ^^^^ left operand has type `i64`
--> line 1, column 26
   1 | fn bad() -> i64 { 1i64 + 2.0f64; }
     |                          ^^^^^^ right operand has type `f64`
```

## Case: rejects non-boolean assert

```holo
#[test]
fn bad_assert() { assert(1i64); }
```

```fails-typecheck
typecheck error: assert expects a boolean expression
--> line 2, column 19
   2 | fn bad_assert() { assert(1i64); }
     |                   ^^^^^^^^^^^^^ this assertion does not evaluate to `bool`
```

## Case: accepts simple numeric function

```holo
fn add(a: i64, b: i64) -> i64 { a + b; }
```

```text
ok
```
