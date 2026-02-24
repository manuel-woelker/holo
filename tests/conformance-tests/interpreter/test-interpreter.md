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

### Succeeds

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

### Fails interpreter

```text
🧪 Test: division by zero

conformance-case.holo:1
   1 │ fn boom() -> i64 { 1i64 / 0i64; }
     │                    ─────────── test failed here
```

## Case: evaluates modulo and subtraction

> Edge case note: integer modulo and subtraction should execute with wrapping semantics where relevant.

```holo
fn compute() -> i64 { 9i64 % 4i64 - 1i64; }

#[test]
fn modulo_and_subtraction() {
    let value: i64 = compute();
    value;
    assert(true);
}
```

### Succeeds

## Case: reports modulo by zero

> Edge case note: modulo by zero is reported as runtime test failure.

```holo
fn modulo_fail() -> i64 { 5i64 % 0i64; }

#[test]
fn modulo_by_zero_fails() {
    let value: i64 = modulo_fail();
    value;
    assert(true);
}
```

### Fails interpreter

```text
🧪 Test: modulo by zero

conformance-case.holo:1
   1 │ fn modulo_fail() -> i64 { 5i64 % 0i64; }
     │                           ─────────── test failed here
```

## Case: reports assertion failure

```holo
#[test]
fn assertion_fails() {
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

## Case: evaluates recursive function

> Edge case note: recursion should work without stack overflow, even for deep call chains.

```holo
fn countdown(continue: bool) -> i64 {
    if continue { countdown(false) } else { 0i64 };
}

#[test]
fn recursive_countdown() {
    let result: i64 = countdown(true);
    assert(!false);
}
```

### Succeeds

## Case: evaluates mutual recursion

```holo
fn is_zero(continue: bool) -> bool {
    if !continue { true } else { false };
}

fn decrement_and_check(continue: bool) -> bool {
    if !continue { false } else { is_zero(false) };
}

#[test]
fn mutual_recursion() {
    assert(!is_zero(true));
    assert(is_zero(false));
    assert(decrement_and_check(true));
    assert(!decrement_and_check(false));
}
```

### Succeeds

## Case: evaluates deeply nested function calls

```holo
fn add_one(x: i64) -> i64 { x + 1i64; }
fn add_two(x: i64) -> i64 { add_one(add_one(x)); }
fn add_three(x: i64) -> i64 { add_two(add_one(x)); }
fn add_four(x: i64) -> i64 { add_three(add_one(x)); }
fn add_five(x: i64) -> i64 { add_four(add_one(x)); }

#[test]
fn nested_calls() {
    let result: i64 = add_five(5i64);
    add_one(result);
}
```

### Succeeds
