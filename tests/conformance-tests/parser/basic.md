# Parser Conformance Cases

## Case: parses basic function

```holo
fn add(a: i64, b: i64) -> i64 { a + b; }
```

```text
ok
```

## Case: reports missing close paren

```holo
fn broken() -> i64 { let value: i64 = (1i64 + 2i64; value; }
```

```fails-parse
error: expected `)` after expression
```
