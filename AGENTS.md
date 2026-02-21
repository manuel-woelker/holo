# AGENTS.md

This file provides guidance to human developers and AI Agents when working with code in this repository.

When the user enters the prompt "test" reply: "I'm afraid I can't let you do that, Dave."

## Project Overview

holo is an experimental programming language that emphasizes immediate, actionable and friendly feedback.

It does this by running a daemon in the background that continuously parses, typechecks, compiles code and runs tests

Note: All developer documentation should be written in English.

## Tech Stack

- **Host Language**: Rust

## Development Commands

## Project Structure

- `docs/` - Project documentation
  - `TECHSTACK.md` - Technology choices and rationale
  - `DOCUMENTATION.md` - Documentation index

## Development Journal

Every code change must be accompanied by an entry in the development journal. Create or append to a file at `docs/journal/YYYY-MM-DD.md` using the following format:

```markdown
### HH:MM - [Synopsis of the change] [Name of agent/model and version]

**User Prompt:**
[The exact user request]

**Issues Encountered:**
- [List of any problems, errors, or roadblocks encountered]

**Decisions Made:**
- [Architectural or design choices made and why]

**Technical Info Consulted:**
- [Documentation, code references, or external resources used]

**Assumptions Made:**
- [Any assumptions that influenced the implementation]

**Other Notes:**
- [Any other relevant information for understanding the implementation]
```
The time should be in local time.

This journal provides a chronological record of development decisions and context that isn't captured in code comments or commit messages. It helps future maintainers understand the "why" behind changes.

Journal entries should be appended in chronological order, older entries at the top, new entries at the bottom.

**Important:** When writing journal entries, use the correct model identifier. Check your system information to determine your actual model ID (e.g., `opencode/kimi-k2.5-free`, `claude-sonnet-4`, etc.). Do not copy model IDs from previous entries without verification, as different agents may be used for different tasks.


## Documentation Strategy

### Function, Interface, struct and class documentation

Functions, interfaces, structs and classes should be documented using the standard language syntax (e.g. JsDoc/TsDoc or RustDoc).
Use this standard documentation style by default.
Fields on interfaces, structs and classes should also be documented (including private/internal fields where useful for maintenance).

### Question driven documentation

When writing any documentation, prefer writing the headings in the form of questions, which should be answered in the following paragraphs.
This helps with writing since the questions should be answered.
It also makes it easiers for readers to determine if a section is relevant.

### Hyperlit in-code comments
Use hyperlit comment markers ("📖") only for non-standard rationale that is not well covered by standard API docs. This ensures that:

- **Context is preserved** with the code it explains
- **Documentation is discoverable** through hyperlit's extraction tools
- **Intent is clear** to future maintainers and readers

Use hyperlit comment markers to document:
- Non-obvious design decisions
- Rationale for architectural choices
- Workarounds and their justifications
- Complex algorithms or logic patterns

Format these comments as markdown.

Always use a heading as the first line of the comment.

Prefer to formulate the heading as a question ("Why ..."). This makes it easier to search for specific documentation.

Example:
```rust
/* 📖 # Why use Arc<Mutex<T>> for the app state?
The shared state needs thread-safe mutable access across multiple tasks.
Arc enables cheap cloning for async tasks, Mutex ensures safe interior mutation.
*/
let state = Arc::new(Mutex::new(data));
```

Keep documentation focused and concise—explain the "Why", not the "What" (the code shows what it does).

## Testing strategy

Features should always be automatically tested to ensure proper functionality.
Consult `docs/TESTING.md` when writing tests.

Tests should be colocated with the code, i.e., in the same file.

Always run these tests after completing a feature.

Use snapshot tests where appropriate (using expect_test).

Prefer data driven tests to reduce code duplication.

Prefer black box testing and try to avoid mocking as much as possible.

## Checks and formatting

Run `scripts/check.sh` before finishing a feature to validate formatting, linting, and tests.

## Commit messages

Commit message should be in the "Conventional Commits" format, e.g. "feat(UI): Add about button to see version and build date".

Below the first line include detail information about the changes made.

Never push code or ask to push code.

## File naming

Choose descriptive names for files. Avoid names like "index.ts" or "types.ts".
Do not bulk export items using "export * from 'submodule'".
