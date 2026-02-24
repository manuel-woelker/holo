# How Should We Evolve The Compiler Implementation Next?

## What Is The Goal?
Define a high-level implementation roadmap for the next compiler evolution, with clear pending milestones that can be delivered incrementally.

## What Is In Scope?
- Frontend semantic correctness and language feature growth.
- Typechecker and diagnostics maturity.
- Compiler architecture improvements for incremental and backend-ready design.
- Runtime/testing model clarity.

## What Is Out Of Scope For This Plan?
- Detailed per-week scheduling.
- Exact API signatures or file-by-file implementation details.
- Release management and packaging workflows.

## What Milestones Should We Track?
1. **Lexical Scope And Symbol Resolution**
- Status: `Done`
- Implement lexical scopes, symbol tables, and shadowing rules for locals/parameters.
- Ensure identifier lookup diagnostics are consistent and span-annotated.

2. **Control Flow Constructs**
- Status: `Done`
- Add `if`, `while`, and block-expression semantics.
- Enforce type consistency across branches and loop bodies.

3. **Type Inference And Conversion Policy**
- Status: `Done`
- Expand inference where appropriate and define explicit conversion behavior.
- Document strict/no-coercion rules or approved coercion paths.

4. **Diagnostics System Hardening**
- Status: `Done`
- Introduce stable error codes for parser/type/runtime diagnostics.
- Add actionable hints/fix guidance where deterministic.

5. **Typed Intermediate Representation (IR)**
- Status: `Done`
- Add a typed IR layer between frontend analysis and execution/backend.
- Keep AST-focused parsing/typechecking concerns separate from execution concerns.

6. **Finer-Grained Incremental Queries**
- Status: `Pending`
- Move from coarse file-level invalidation toward item/function-level invalidation.
- Preserve correctness while reducing unnecessary recomputation.

7. **Module And Import System**
- Status: `Pending`
- Implement modules/import resolution and dependency tracking.
- Add robust cycle diagnostics and import error reporting.

8. **Runtime Test Execution Semantics**
- Status: `Pending`
- Define deterministic ordering and isolation semantics for test execution.
- Ensure runtime failures map cleanly into diagnostics and summaries.

9. **Semantic Optimization Passes**
- Status: `Pending`
- Add foundational optimizations (for example constant folding) in dedicated passes.
- Validate optimization correctness with targeted conformance cases.

10. **Backend Abstraction Boundary**
- Status: `Pending`
- Formalize backend interfaces so interpreter and future codegen backends can coexist.
- Keep frontend and backend contracts stable as language features grow.

## What Are The Acceptance Criteria For This Plan Document?
- Every milestone is explicitly marked as `Pending`.
- Milestones are high-level and implementation-oriented.
- The sequence supports incremental delivery without architectural lock-in.

## What Is The Suggested Execution Order?
- Start with semantic correctness foundations (`1`-`4`).
- Establish architecture boundaries (`5`, `10`).
- Improve incremental performance and scaling (`6`, `7`).
- Harden runtime behavior and optimization confidence (`8`, `9`).
