# Comment Syntax Conformance Cases

## Case: line comment

```holo
// This is a line comment
#[test]
fn line_comment() {
    let x: i64 = 1i64; // comment at end of line
    assert(x == 1i64);
}
```

### Succeeds

## Case: block comment

```holo
/* This is a block comment */
#[test]
fn block_comment() {
    let x: i64 = 2i64;
    assert(x == 2i64);
}
```

### Succeeds

## Case: nested block comments

```holo
/* Outer /* inner /* deeply nested */ end */ done */
#[test]
fn nested_block_comments() {
    let x: i64 = 3i64;
    assert(x == 3i64);
}
```

### Succeeds

## Case: comments around code

```holo
/* before */
let x: i64 = 4i64; /* after */
// middle
assert(x == 4i64);
```

### Succeeds
