# String Type Conformance Cases

## Case: string literal

```holo
#[test]
fn string_literal() {
    let s: string = "hello";
    assert(true);
}
```

### Succeeds

## Case: string in function

```holo
fn greet(name: string) -> string { name; }

#[test]
fn string_in_function() {
    let result: string = greet("world");
    assert(true);
}
```

### Succeeds

## Case: string with println_string

```holo
#[test]
fn string_with_println() {
    println_string("hello");
}
```

### Succeeds

### Output

```text
hello

```
