# Template Strings

## Case: simple template string

```holo
fn greeting() {
    println_string(`Hello { "World" }!`);
}
```

### Succeeds

```
Hello World!
```

## Case: multiple interpolations

```holo
fn add_message(a: i64, b: i64) {
    println_string(`The sum of {a} and {b} is {a + b}`);
}

fn test() {
    add_message(10i64, 20i64);
}
```

### Succeeds

```
The sum of 10 and 20 is 30
```

## Case: nested template strings

```holo
fn nested() {
    let name = "Alice";
    println_string(`Outer { `Inner {name}` }`);
}
```

### Succeeds

```
Outer Inner Alice
```

## Case: typecheck error inside interpolation

```holo
fn bad_type() {
    let value = `This is { 10i64 + "a" }`;
}
```

### Fails typecheck

```text
⚒️ Typecheck: arithmetic operators require numeric operands

conformance-case.holo:1
   1 │ fn bad_type() {
     │ ─ left operand has type `i64`
     │ └─ right operand has type `string`
```

## Case: unterminated template string

```holo
fn missing_brace() {
    let a = `hello
}
```

### Fails parsing

```text
⚒️ Parsing: expected `;` after let statement

conformance-case.holo:3
   3 │ }
     │ ┬─ expected `;`, found end of input
```

