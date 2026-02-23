# How Do We Run And Add Tests?

## What Is The Goal Of The Testing Infrastructure?
Provide a data-driven, markdown-based test system that validates parsing, typechecking, interpretation, and end-to-end compile/test behavior. Fixtures should be readable and serve as living documentation.

## What Commands Should I Run?
- `scripts/check-code.sh`
- `cargo test -p conformance-tests --manifest-path crates/Cargo.toml`

## Which Markdown Suites Are Executed Today?
- `tests/conformance-tests/end_to_end/*.md` is executed against `CompilerCore` and compared against fixture expectations.
- `tests/conformance-tests/parser/*.md`, `tests/conformance-tests/typechecker/*.md`, and `tests/conformance-tests/interpreter/*.md` are currently validated for fixture parsing/loading shape.

## How Are Tests Driven By Markdown?
- Test cases live in markdown files and are loaded by the test harness.
- Each case includes a name, an input `holo` code block, and expected output in a `text` block.
- Tables are used to enumerate example inputs and outputs (truth tables, arithmetic tables), not metadata.
- The harness normalizes output into snapshot-friendly text.
- Failure expectations should contain the full annotated diagnostic output (multi-line), not only the first error line.

## Where Do Fixture Files Live?
- `tests/conformance-tests/parser/*.md`
- `tests/conformance-tests/typechecker/*.md`
- `tests/conformance-tests/interpreter/*.md`
- `tests/conformance-tests/end_to_end/*.md`

## What Is The Standard Case Format?
```markdown
## Case: simple addition

```holo
fn add(a: i64, b: i64) -> i64 { a + b; }
```

```text
ok
```
```

## How Do I Add A New End-To-End Case Without Writing Rust Test Code?
1. Add a new `## Case: ...` section to `tests/conformance-tests/end_to_end/*.md`.
2. Add exactly one `holo` fenced block with the source under test.
3. Add one expected-output block:
4. Use `text` for success with `ok`, or `fails-typecheck` / `fails-interpreter` for expected failures.
5. Run `cargo test -p conformance-tests --manifest-path crates/Cargo.toml`.

## How Do We Encode Compilation Failures?
- Use a code block whose info string encodes the failure kind, e.g. `fails-parse` or `fails-typecheck`.
- Put the expected full pretty-printed diagnostic output in the failure block.
- Prefer `expect_test` snapshots for diagnostics rendering.

## How Do We Encode Execution Failures?
- Use a code block whose info string encodes the failure kind, e.g. `fails-interpreter` or `fails-execution`.
- Include the full pretty-printed runtime failure diagnostic output.

## What Does Failure Annotation Look Like?
```markdown
## Case: mixed numeric types

```holo
fn bad() -> i64 { 1i64 + 2.0f64; }
```

```fails-typecheck
error: cannot add `i64` and `f64`
```
```

```markdown
## Case: missing close paren

```holo
fn broken() -> i64 { let value: i64 = (1i64 + 2i64; value; }
```

```fails-parse
error: expected `)` after expression
```
```

## How Do We Keep Outputs Stable?
- Normalize paths and line endings in the harness.
- Use `expect_test` snapshots for any user-facing output.
- Keep fixtures minimal and deterministic.

## How Should We Extend The Suite?
- Add new cases to existing fixture files.
- Create a new fixture file when the topic is distinct.
- Cover both success and failure cases for each feature.
- Keep success output as `ok`; include full annotated diagnostics for failures.

## What Do Example Tables Look Like?
```markdown
## Case: truth table for `!`

| input | output |
| --- | --- |
| true | false |
| false | true |

```holo
fn not(value: bool) -> bool { !value; }
```

```text
ok
```
```

```markdown
## Case: addition table

| a | b | sum |
| --- | --- | --- |
| 1i64 | 2i64 | 3i64 |
| 5i64 | -2i64 | 3i64 |

```holo
fn add(a: i64, b: i64) -> i64 { a + b; }
```

```text
ok
```
```
