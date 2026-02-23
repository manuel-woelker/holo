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

## Case: multiple tests pass in one module

> Edge case note: compile-and-test should execute all discovered tests in the module.

```holo
fn helper(value: i64) -> i64 { value + 1i64; }

#[test]
fn first() {
    let value: i64 = helper(1i64);
    value;
    assert(true);
}

#[test]
fn second() {
    let value: i64 = helper(2i64);
    value;
    assert(true);
}
```

### Succeeds

## Case: parse error blocks execution

> Edge case note: parser diagnostics prevent successful end-to-end execution.

```holo
fn broken(a: i64 -> i64 { a; }

#[test]
fn still_present() { assert(true); }
```

### Fails parsing

```text
⚒️ Parsing: expected `)` after function parameter list

conformance-case.holo:1
   1 │ fn broken(a: i64 -> i64 { a; }
     │                  ── expected `)`, found `->`
```

## Case: assertion failure propagates as runtime failure

```holo
#[test]
fn fails() {
    assert(false);
}
```

### Fails interpreter

```text
🧪 Test: assertion failed

conformance-case.holo:3
   3 │     assert(false);
     │            ───── test failed here
```
