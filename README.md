# holo

holo is an experimental programming language focused on immediate, actionable, and friendly feedback.

A background daemon continuously parses, typechecks, compiles source files, and executes tests so issues are surfaced quickly while you work to enable the 
fastest possible feedback loop.

> Work in progress: this project is actively evolving and interfaces/behavior may change.

> Motivation: https://manuel-woelker.github.io/blog/posts/2026-02-23-i-wish-i-had-a-hot-reloading-compiler/


## What is in this repository?

- `crates/` Rust workspace crates for language components and tooling
- `examples/` language example programs
- `tests/conformance-tests/` markdown-driven conformance fixtures
- `docs/` design notes, testing guidance, and development journal
- `scripts/` local development and validation scripts

## How do I build and test?

Run commands from repository root:

```bash
cargo build --manifest-path crates/Cargo.toml --workspace --all-targets --all-features
cargo test --manifest-path crates/Cargo.toml --workspace --all-targets --all-features
cargo clippy --manifest-path crates/Cargo.toml --workspace --all-targets --all-features -- -D warnings
cargo fmt --manifest-path crates/Cargo.toml --all -- --check
```

Run conformance tests:

```bash
cargo run -p conformance-tests --manifest-path crates/Cargo.toml
```

## Where is project documentation?

- `docs/DESIGN.md` for language and architecture notes
- `docs/TESTING.md` for testing strategy and fixture format
- `docs/journal/` for development history and decisions

## How should contributions be made?

- Keep documentation in English.
- Add tests for feature changes.
- Add a development journal entry for every code change in `docs/journal/YYYY-MM-DD.md`.
- Use conventional commits for commit messages.
