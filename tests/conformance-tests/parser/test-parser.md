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

## Case: parses numeric suffixes and precedence

> Edge case note: mixed operator precedence with explicit numeric suffixes should parse without diagnostics.

```holo
fn calc() -> f64 { -1.0f64 + 2.0f64 * 3.0f64; }
```

### Succeeds

## Case: rejects non-test attribute

> Edge case note: only `#[test]` is accepted as an attribute marker.

```holo
#[bench]
fn not_a_test() { assert(true); }
```

### Fails parsing

```text
⚒️ Parsing: expected `#[test]` attribute, found `#[bench]`

conformance-case.holo:1
   1 │ #[bench]
     │   ───── unsupported test attribute `bench`
```
