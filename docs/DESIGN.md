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

- Source files are incrementally compiled into executable artifacts.
- Compile stages are designed to expose intermediate metadata for diagnostics and tooling.
- Runtime behavior should align with compile-time reasoning as closely as possible.
- Test execution is integrated into the same incremental graph to avoid duplicated work.

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
