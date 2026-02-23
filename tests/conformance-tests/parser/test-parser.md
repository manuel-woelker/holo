# Parser Conformance Cases

## Case: parses basic function

```holo
fn add(a: i64, b: i64) -> i64 { a + b; }
```

### Succeeds

## Case: reports missing close paren

```holo
fn broken() -> i64 { let value: i64 = (1i64 + 2i64; value; }
```

### Fails parsing

```text
⚒️ Parsing: expected `)` after expression

conformance-case.holo:1
   1 │ fn broken() -> i64 { let value: i64 = (1i64 + 2i64; value; }
     │                                                   ─ expected `)`, found `;`
```
