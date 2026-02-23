# End-to-End Conformance Cases

## Case: simple test passes

```holo
#[test]
fn ok() { assert(true); }
```

### Succeeds

## Case: compile error blocks execution

```holo
fn bad() -> i64 { 1i64 + 2.0f64; }

#[test]
fn ok() { assert(true); }
```

### Fails typecheck

```text
⚒️ Typecheck: arithmetic operands must have the same type

conformance-case.holo:1
   1 │ fn bad() -> i64 { 1i64 + 2.0f64; }
     │                   ───┬   ────── right operand has type `f64`
     │                      └─ left operand has type `i64`
```

## Case: runtime failure reports error

```holo
fn boom() -> i64 { 1i64 / 0i64; }

#[test]
fn division_by_zero() {
    let value: i64 = boom();
    value;
    assert(true);
}
```

### Fails interpreter

```text
🧪 Test: division by zero

conformance-case.holo:1
   1 │ fn boom() -> i64 { 1i64 / 0i64; }
     │                    ─────────── test failed here
```
