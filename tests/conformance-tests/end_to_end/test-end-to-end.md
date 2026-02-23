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
typecheck error: arithmetic operands must have the same type
--> line 1, column 19
   1 | fn bad() -> i64 { 1i64 + 2.0f64; }
     |                   ^^^^ left operand has type `i64`
--> line 1, column 26
   1 | fn bad() -> i64 { 1i64 + 2.0f64; }
     |                          ^^^^^^ right operand has type `f64`
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
test failure: division by zero
--> line 1, column 20
   1 | fn boom() -> i64 { 1i64 / 0i64; }
     |                    ^^^^^^^^^^^ test failed here
```
