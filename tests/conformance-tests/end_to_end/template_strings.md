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

## Case: escape sequence newline

```holo
fn test_newline() {
    println_string(`Line1\nLine2`);
}
```

### Succeeds

```
Line1
Line2
```

## Case: escape sequence tab

```holo
fn test_tab() {
    println_string(`Col1\tCol2`);
}
```

### Succeeds

```
Col1	Col2
```

## Case: escape sequence backslash

```holo
fn test_backslash() {
    println_string(`Path: C:\\Users\\test`);
}
```

### Succeeds

```
Path: C:\Users\test
```

## Case: escape sequence dollar sign

```holo
fn test_dollar() {
    println_string(`Price: \$100`);
}
```

### Succeeds

```
Price: $100
```

## Case: unicode characters

```holo
fn test_unicode() {
    println_string(`Café résumé naïve`);
}
```

### Succeeds

```
Café résumé naïve
```

## Case: emojis

```holo
fn test_emoji() {
    println_string(`Hello 👋 World 🌍`);
}
```

### Succeeds

```
Hello 👋 World 🌍
```

## Case: mixed escapes and interpolation

```holo
fn test_mixed(name: string) {
    println_string(`Hello {name}!\nWelcome\tto\tholo`);
}
```

### Succeeds

```
Hello Alice!
Welcome	to	holo
```

## Case: escaped braces

```holo
fn test_braces() {
    println_string(`Use \{ and \} for interpolation`);
}

#[test]
fn test_escaped_braces() { test_braces(); }
```

### Succeeds

```
Use { and } for interpolation
```

## Case: escaped backtick

```holo
fn test_backtick() {
    println_string(`Use \` to create templates`);
}

#[test]
fn test_escaped_backtick() { test_backtick(); }
```

### Succeeds

```
Use ` to create templates
```

## Case: multiple escapes in sequence

```holo
fn test_multiple() {
    println_string(`\\n\t\r\\\`\{`);
}

#[test]
fn test_multiple_escapes() { test_multiple(); }
```

### Succeeds

```
\n\t\r\`{
```

