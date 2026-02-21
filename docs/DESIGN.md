# What Is Holo?

Holo is an experimental programming language designed to provide immediate, actionable, and friendly feedback while developers write code. Its core idea is that language tooling should feel continuously available, not manually invoked.

# What Problem Is Holo Trying To Solve?

Most language workflows are batch-oriented: edit, save, run checks, then interpret delayed results. This creates feedback latency and context switching.

Holo addresses this by running a background daemon that continuously:

- Parses source code
- Typechecks changes
- Compiles incrementally
- Runs relevant tests

The result is a development loop where errors and guidance appear as soon as possible and remain connected to current code context.

# What Are The Primary Design Goals?

- Immediate feedback: surface diagnostics and test outcomes with minimal delay.
- Actionable diagnostics: provide concrete, fix-oriented messages instead of opaque failures.
- Friendly developer experience: prefer helpful language and progressive guidance.
- Predictable semantics: keep runtime and type behavior understandable and consistent.
- Tool-first architecture: treat compiler services as always-on capabilities exposed through stable interfaces.

# What Non-Goals Shape The Initial Scope?

- Full ecosystem parity with mature languages in early versions.
- Complex metaprogramming features before core correctness and feedback quality.
- Heavy runtime magic that makes behavior hard to reason about.
- IDE-specific coupling; integrations should be possible across editors and terminals.

# How Does The High-Level Architecture Work?

Holo is organized around a long-running daemon plus client interfaces.

- Daemon core: owns project graph, file state, incremental compiler pipeline, and test scheduler.
- Client adapters: editor plugins, CLI commands, and other frontends that send requests and receive events.
- Incremental pipeline:
  - File change ingestion
  - Parse + syntax diagnostics
  - Typecheck + semantic diagnostics
  - Incremental compile artifacts
  - Targeted test execution
- Feedback channel: streams structured diagnostics, fix hints, and test status back to clients.

# How Does The Daemon Maintain Fast Feedback?

- Uses content hashing and dependency tracking to recompute only affected units.
- Keeps warm caches for syntax trees, symbol tables, and intermediate representations.
- Schedules work by priority (active file first, then dependents, then background validation).
- Supports cancellation and restart of obsolete work when new edits arrive.

# Why Should The Compiler Be Query Driven?

The compiler should be query driven so every meaningful computation is modeled as a reusable, memoized query instead of a fixed pass over the entire program. This makes the system naturally incremental and aligns with Holo's always-on daemon model.

In a query-driven design:

- Compiler work is requested on demand by asking questions like "what is the type of this expression?" or "what diagnostics apply to this file?"
- Query results are cached and reused until their tracked inputs change.
- Invalidations are precise because dependencies are recorded at the query level.
- Frontends and tools can request exactly the information they need without forcing unrelated work.

# What Does Query Driven Mean For Compiler Architecture?

- Stable query keys: each query must be keyed by deterministic identifiers (file ID, module ID, symbol ID, source revision).
- Explicit dependency tracking: query execution records upstream queries and input reads for correct invalidation.
- Pure query semantics: queries should behave like deterministic functions over compiler state to maximize cache correctness.
- Layered query groups: syntax, name resolution, typing, lowering, codegen, and diagnostics should expose clear query APIs.
- Demand-driven scheduling: background tasks and user-facing operations should be expressed as query requests with priorities.
- Shared query runtime: parser, typechecker, lints, codegen, and test-impact analysis should run on the same dependency/caching substrate.

# How Is The Language Experience Intended To Feel?

- Errors should identify cause, impact, and likely fix in plain language.
- Warnings should be educational, not noisy.
- Feedback should appear progressively: quick parse checks first, deeper results after.
- Developers should be able to trust that shown diagnostics reflect current source state.

# What Is The Type System Direction?

The language targets a statically checked type system with strong local inference and explicitness at boundaries.

- Prefer explicit API contracts.
- Infer obvious local types to reduce boilerplate.
- Make unsafe or ambiguous operations visible in code.
- Keep type errors specific and traceable through source locations and constraints.

# What Is The Compilation And Execution Model?

- Source files are incrementally compiled through query evaluation into executable artifacts.
- Compile stages expose intermediate metadata through query interfaces for diagnostics and tooling.
- Runtime behavior should align with compile-time reasoning as closely as possible.
- Test execution is integrated into the same incremental graph to avoid duplicated work.

# How Will Query-Driven Compilation Be Applied In Practice?

- Parsing queries provide syntax trees and token maps per file revision.
- Semantic queries provide scopes, symbol resolution, inferred/declared types, and constraint failures.
- Codegen queries provide lowered IR and target artifacts for requested roots.
- Diagnostic queries aggregate results from syntax/semantic/codegen queries and return stable diagnostic identities.
- Test-selection queries map changed symbols and modules to impacted tests.

This enables partial recomputation: a small edit should invalidate only affected queries and their transitive dependents, not the full project.

# What Could A Query API Look Like?

The exact implementation may change, but the compiler can expose a small set of typed query entry points with stable keys:

```rust
/// Stable context shared by all compiler queries.
pub trait QueryContext {
    fn source_text(&self, file: FileId, rev: SourceRevision) -> Arc<str>;
    fn syntax_tree(&self, file: FileId, rev: SourceRevision) -> Arc<SyntaxTree>;
    fn module_scope(&self, module: ModuleId, rev: SourceRevision) -> Arc<Scope>;
    fn symbol_type(&self, symbol: SymbolId, rev: SourceRevision) -> Type;
    fn lowered_ir(&self, item: ItemId, rev: SourceRevision) -> Arc<LoweredIr>;
    fn diagnostics(&self, file: FileId, rev: SourceRevision) -> Arc<[Diagnostic]>;
    fn impacted_tests(&self, changed: Arc<[ChangeKey]>, rev: SourceRevision) -> Arc<[TestId]>;
}

/// Engine contract for memoized query evaluation and invalidation.
pub trait QueryEngine {
    fn eval<Q: Query>(&self, key: Q::Key, rev: SourceRevision) -> Q::Value;
    fn invalidate_file(&mut self, file: FileId, new_rev: SourceRevision);
}
```

Design expectations for this API:

- Query keys are deterministic and cheap to compare/hash.
- Query values are immutable snapshots associated with a source revision.
- Query execution records dependencies automatically so invalidation stays precise.
- Frontends call high-level queries (`diagnostics`, `impacted_tests`) while compiler internals compose lower-level ones.

# How Are Testing And Quality Integrated?

Testing is a first-class part of the feedback loop:

- The daemon identifies impacted tests based on code changes.
- Test outcomes are reported continuously with clear mapping to source edits.
- Failures are presented with concise repro context and actionable pointers.
- Full-suite runs remain available, but change-based selection is the default fast path.

# How Do Clients Integrate With Holo?

- A transport protocol (local IPC initially) allows clients to subscribe to project events.
- Requests cover operations such as diagnostics, symbol information, and test status.
- Responses and events are structured for machine-readability and consistent UI rendering.
- Protocol evolution should be versioned to preserve compatibility.

# What Are The Key Risks And Mitigations?

- Risk: feedback becomes noisy or unstable.
  - Mitigation: ranking, deduplication, and deterministic diagnostic IDs.
- Risk: daemon resource usage grows on large codebases.
  - Mitigation: bounded caches, adaptive scheduling, and configurable limits.
- Risk: stale diagnostics after rapid edits.
  - Mitigation: cancellation tokens, generation IDs, and strict source versioning.
- Risk: architecture complexity slows iteration.
  - Mitigation: clean subsystem boundaries and traceable performance metrics.

# What Milestones Define The Initial Roadmap?

- Milestone 1: minimal parser, checker skeleton, daemon file-watching loop, and CLI diagnostics.
- Milestone 2: incremental dependency graph, basic type inference, and targeted test runner integration.
- Milestone 3: client protocol, editor integration, richer actionable diagnostics, and stability hardening.
- Milestone 4: performance optimization, large-workspace reliability, and language feature expansion.

# How Will Success Be Measured?

- Median and tail latency from edit to first diagnostic.
- Diagnostic quality metrics (resolution rate, recurrence rate).
- Test feedback latency for impacted tests.
- Daemon stability under long-running sessions.
- Developer-reported usefulness and clarity of feedback.
