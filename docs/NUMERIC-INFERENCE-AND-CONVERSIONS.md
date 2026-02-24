# How Does Numeric Inference Work?

Unsuffixed numeric literals are inferred by default as:
- integer literals -> `i64`
- floating literals -> `f64`

Unsuffixed literals are contextually inferred in these places:
- `let` initializer when the `let` has an explicit numeric type annotation.
- function call argument when the target parameter has an explicit numeric type.

If a literal already carries a suffix (`u32`, `u64`, `i32`, `i64`, `f32`, `f64`), that suffix always wins.

## What Is The Conversion Policy?

Implicit numeric coercions are not allowed.

Examples that remain errors:
- passing an `i64` variable to a `u32` parameter
- assigning an `f64` expression to an `i64` binding without an explicit literal suffix or future conversion API

Current policy is strict:
- contextual inference is only for unsuffixed numeric literals
- non-literal value conversions are rejected

## Which Conversions Are Approved Right Now?

No general explicit conversion function API is implemented yet.

For now, explicit numeric intent should be expressed via literal suffixes.
