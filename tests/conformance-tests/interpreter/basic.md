# Interpreter Conformance Cases

## Case: evaluates arithmetic

```holo
fn sum() -> i64 { 1i64 + 2i64 * 3i64; }

#[test]
fn arithmetic_runs() {
    let value: i64 = sum();
    value;
    assert(true);
}
```

```text
ok
```

## Case: reports division by zero

```holo
fn boom() -> i64 { 1i64 / 0i64; }

#[test]
fn division_by_zero_fails() {
    let value: i64 = boom();
    value;
    assert(true);
}
```

```fails-interpreter
error: division by zero
```
