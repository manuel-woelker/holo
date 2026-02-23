# What Do The Examples Demonstrate?

## Which Example Shows Successful Functionality?
- `showcase_success.holo`
  - Uses typed functions and calls.
  - Uses `let` bindings.
  - Exercises numeric literals with suffixes (`u32`, `u64`, `i32`, `i64`, `f32`, `f64`).
  - Exercises arithmetic operators (`+`, `-`, `*`, `/`, `%`).
  - Contains passing tests.

## Which Example Highlights Variables And Function Calls?
- `showcase_variables_and_functions.holo`
  - Chains multiple `let` bindings within functions.
  - Uses unary `-` and parenthesized arithmetic.
  - Calls typed functions from tests.

## Which Example Focuses On Numeric Literals And Operators?
- `showcase_numeric_operations.holo`
  - Exercises arithmetic precedence and grouping.
  - Uses signed, unsigned, and floating-point literals with suffixes.
  - Shows division and modulo on integer types.

## Which Example Shows Failing Tests?
- `showcase_failing_tests.holo`
  - Shows direct assertion failure.
  - Shows runtime failure caused by division by zero during test execution.

## Which Example Shows Typechecking Errors?
- `showcase_type_errors.holo`
  - Unknown identifier.
  - Unknown function.
  - Wrong function call arity.
  - Mixed numeric arithmetic (`i64 + f64`).
  - Invalid modulo on float values.

## Which Example Adds Additional Typechecking Errors?
- `showcase_type_errors_more.holo`
  - Invalid unary `-` on an unsigned value.
  - Returning a value that does not match the function return type.
  - Using a non-boolean value in `assert`.

## Which Example Shows Parsing Errors?
- `showcase_parse_errors.holo`
  - Broken function signature and malformed expressions to trigger parser diagnostics.

## What Existing Minimal Examples Remain?
- `minimal_tests.holo` retains the original minimal boolean test examples.
- `fails_to_compile.holo` remains a minimal parse failure sample.
