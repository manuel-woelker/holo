# How Should We Build The Markdown-Driven Testing Infrastructure?

## What Is The Goal?
Create a test harness that loads markdown fixtures and executes data-driven cases for parsing, typechecking, interpreting, and end-to-end compile/test runs. The harness should support both successful runs and expected failures with snapshot-friendly outputs.

## What Are The Guiding Principles?
- Keep fixtures readable by humans and useful as documentation.
- Use code blocks and tables to define inputs and expected outputs.
- Prefer `expect_test` snapshots for user-facing output.
- Keep tests colocated with code.

## What Fixture Format Should We Standardize?
- Each markdown file contains one or more cases.
- A case includes a name, a `holo` code block, and expected output blocks.
- Tables are reserved for enumerating examples (truth tables, arithmetic tables), not metadata.
- Expected output blocks are plain text in `text` code blocks.
- Failure cases use a code block info string like `fails-parse`, `fails-typecheck`, or `fails-interpreter` to declare intent.

## What Does A Minimal Case Look Like?
```markdown
## Case: simple addition

```holo
fn add(a: i64, b: i64) -> i64 { a + b; }
```

```text
ok
```
```

## How Do We Represent Failure Cases?
- Provide the expected diagnostics output in a `text` block.
- For runtime failures, include the interpreter diagnostics output.

## What Categories Of Tests Should We Implement First?
- Parser fixtures that include valid syntax and intentional parse errors.
- Typechecker fixtures for operator/type matrix and call validation.
- Interpreter fixtures for numeric ops and runtime errors.
- End-to-end fixtures that run full compile + tests on multi-file setups.

## Where Should Fixtures Live?
- `test-suite/parser/*.md`
- `test-suite/typechecker/*.md`
- `test-suite/interpreter/*.md`
- `test-suite/end_to_end/*.md`

## How Should The Harness Be Structured?
- A small loader that parses markdown into `Case` structs.
- Reusable executors per phase that produce a normalized text report.
- Snapshot tests that compare the report to the expected output block.

## What Are The Milestones?
1. Define `Case` schema and markdown parser.
2. Add parser fixtures and tests.
3. Add typechecker fixtures and tests.
4. Add interpreter fixtures and tests.
5. Add end-to-end fixtures and tests.
6. Document usage and add contributor guidance in `docs/TESTING.md`.

## What Are The Acceptance Criteria?
- A new markdown fixture can add a passing or failing case without new test code.
- Diagnostics output is captured via snapshots.
- All test suites run via `scripts/check-code.sh`.
