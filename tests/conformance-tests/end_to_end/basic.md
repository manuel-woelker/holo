# End-to-End Conformance Cases

## Case: simple test passes

```holo
#[test]
fn ok() { assert(true); }
```

```text
ok
```

## Case: compile error blocks execution

```holo
fn bad() -> i64 { 1i64 + 2.0f64; }

#[test]
fn ok() { assert(true); }
```

```fails-typecheck
error: arithmetic operands must have the same type
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

```fails-interpreter
error: division by zero
```
