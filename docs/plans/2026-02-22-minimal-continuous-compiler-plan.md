# How Should We Implement A Minimal Continuously Compiling And Testing Compiler?

## What Is The Scope Of This First Version?
The first version should provide a continuously running daemon that:
- Watches project files for changes.
- Re-parses and re-checks only affected inputs.
- Re-runs affected tests automatically.

Language scope is intentionally tiny:
- Expression grammar supports `true`, `false`, unary `!` negation, and `assert(<expr>)`.
- Tests are discovered from items annotated with `#[test]`.
- Test bodies execute one or more `assert(<expr>)` statements.

## What Is Explicitly Out Of Scope?
To keep delivery fast and reduce architectural risk, this phase excludes:
- Variables, user-defined functions (except test items as top-level declarations), and operators other than unary `!`.
- Type inference beyond the two boolean literals.
- Optimization and machine code generation.
- Parallel scheduling, remote caching, and advanced diagnostics UX.

## What Minimal Source Model Should We Support?
Use one source model for files:
- A file contains zero or more test items.
- A test item shape is `#[test]` + `fn <name>() { <stmt>* }`.
- `<stmt>` is `assert(<expr>);`.
- `<expr>` is `true`, `false`, or `!<expr>`.

Example:
```holo
#[test]
fn passing_case() { assert(true); }

#[test]
fn failing_case() { assert(!true); }
```

## What Compiler Pipeline Should The Daemon Run?
For each changed file, run this fixed pipeline:
1. Parse file into AST.
2. Lower AST into a tiny IR with boolean constants, unary negation, and assert statements.
3. Typecheck IR (`!` requires bool input, `assert` requires bool argument).
4. Build a test manifest from `#[test]` items.
5. Execute tests and emit pass/fail diagnostics.

Each stage should be query-driven so results are cached by input identity.

## What Queries Should Exist In The Initial Query System?
Define stable query keys by file path + file content hash:
- `parse_file(path) -> ParseResult`
- `lower_file(path) -> LoweredFile`
- `typecheck_file(path) -> TypecheckResult`
- `collect_tests(path) -> Vec<TestCase>`
- `run_test(test_id) -> TestResult`
- `run_all_tests() -> TestRunSummary`

Invalidation rule:
- If file hash changes, invalidate all queries for that file.
- Recompute downstream queries only when requested by diagnostics or test run.

## How Should Continuous Compilation And Testing Be Triggered?
Use a daemon event loop:
1. Start file watcher.
2. On startup, enqueue all source files for initial compile.
3. On each file change event:
4. Debounce briefly (for editor save bursts).
5. Mark changed files dirty.
6. Recompute diagnostics for dirty files.
7. Re-run tests whose definitions live in dirty files.
8. Publish a summarized status update.

Status update should include:
- Parse/typecheck errors by file.
- Number of tests run, passed, failed.
- Names of failing tests.

## How Should Test Execution Work In This Minimal Runtime?
Test execution can be direct evaluation:
- Evaluate boolean expressions (`true`/`false`/`!expr`) recursively.
- Execute `assert(expr)` as pass when `expr == true`, fail when `expr == false`.
- Parse/typecheck errors mark test as error (not pass/fail).

No sandboxing is required in this phase because evaluation is pure and non-Turing complete.

## What Diagnostics Should Be Produced?
Minimum diagnostics set:
- Unknown syntax token.
- Missing/invalid `#[test]` placement.
- Invalid test body statement (must be `assert(<expr>);`).
- Invalid expression form (must be `true`, `false`, or unary `!` chain).
- Invalid `assert` argument type/arity.
- Duplicate test function name in same file.

Diagnostics should include file path, line, column, and short actionable message.

## What Milestones Should We Implement In Order?
1. **Project wiring**
Add crate/module boundaries for daemon loop, parser, test collector, evaluator, and diagnostics.
2. **Parser + AST**
Parse boolean literals, unary negation, assert statements, and test item declarations with spans.
3. **Typecheck + IR**
Validate negation/assert typing rules and generate lowered boolean/assert IR.
4. **Query engine shell**
Implement memoized per-file queries with hash-based invalidation.
5. **Test collector + runner**
Discover `#[test]` items and evaluate test bodies.
6. **File watch daemon**
Add watch + debounce + incremental recompute loop.
7. **Reporting**
Print stable diagnostics and per-cycle test summary.

## What Tests Should Validate This Plan?
Use colocated data-driven tests for:
- Parser acceptance/rejection cases.
- Typecheck behavior for valid and invalid negation/assert usage.
- Query invalidation when file content changes.
- Test discovery of `#[test]` functions.
- Test execution mapping `assert(true)` -> pass and `assert(!true)` -> fail.
- End-to-end daemon cycle where one changed file triggers selective rerun.

Use snapshot tests (`expect_test`) for diagnostics and summary output stability.

## What Are The Exit Criteria For This Minimal Version?
This plan is complete when:
- Editing a source file automatically triggers recompile and selective test rerun.
- `true`/`false`, unary `!`, and `assert(<expr>)` are fully supported end-to-end.
- `#[test]` functions are discovered and executed automatically.
- Failing tests and compile errors are reported deterministically.
- The behavior is covered by automated tests and passes repository checks.
