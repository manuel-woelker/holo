# Design Document: Incremental Compilation

This document outlines the design for incremental compilation in the Holo compiler. The goal is to minimize recomputation by caching intermediate results and only executing stages when their inputs have changed.

## Compilation Pipeline Stages

The incremental compilation process is divided into five stages. Each stage is modeled as a forward incremental step that can be skipped if its inputs are identical to a previous successful run.

---

### 1. Tokenize and Parse
**Task:** Transform raw source text into a structured Abstract Syntax Tree (AST).

*   **Inputs:** File content (raw string).
*   **Outputs:** AST and syntax diagnostics.
*   **Omission Logic:** This stage can be omitted if the **content hash** of the file matches the hash used for the previous AST in the cache.
*   **Rationale:** Parsing is purely a function of the source text. If the text hasn't changed, the AST remains identical.

---

### 2. Symbol Extraction
**Task:** Extract top-level function and struct names from the AST and populate a global lookup table (Symbol Table).

*   **Inputs:** AST.
*   **Outputs:** A map of names to symbol identifiers and their high-level locations.
*   **Omission Logic:** This stage can be omitted if the **set of top-level identifiers** (function names, struct names) in the AST is identical to the previous version.
*   **Rationale:** Changes to function bodies or parameter internal names do not affect the global lookup table if the top-level names themselves are unchanged.

---

### 3. Signature Resolution
**Task:** Resolve types and parameter names within function and struct signatures.

*   **Inputs:** AST (Signature parts) and the Symbol Table (for type resolution).
*   **Outputs:** Resolved signatures (including types of parameters and return types).
*   **Omission Logic:** This stage can be omitted for a specific symbol if its **signature tokens** and all **types referenced in the signature** are unchanged.
*   **Rationale:** A function's signature is stable even if its body changes. If the signature remains the same, downstream stages that depend only on the signature (like typechecking other functions) do not need to be re-triggered for signature changes.

---

### 4. Body Typechecking
**Task:** Resolve and typecheck the bodies of functions.

*   **Inputs:** AST (Body parts), Symbol Table, and Resolved Signatures of all dependencies.
*   **Outputs:** Type-checked body, semantic diagnostics, and dependency information.
*   **Omission Logic:** This stage can be omitted for a specific function if:
    1.  The **AST node for the function body** is identical to the previous version.
    2.  The **signatures of all functions/structs called** or referenced within the body have not changed.
*   **Rationale:** Even if a function body is unchanged, a change in the signature of a called function might introduce new type errors or change inference, requiring a re-check.

---

### 5. Targeted Test Execution
**Task:** Execute tests affected by the current changes.

*   **Inputs:** Test AST, Compiled Bodies, and Test Impact Graph.
*   **Outputs:** Test results (Pass/Fail).
*   **Omission Logic:** This stage can be omitted for a specific test if:
    1.  The **test definition itself** (body/setup) has not changed.
    2.  No **transitive dependency** of the test (functions or data structures it relies on) has changed its **typechecked implementation** in the current cycle.
*   **Rationale:** If the test code and all code reachable from it are semantically identical to the last successful run, the test result is guaranteed to be the same.

## Summary of Invalidation

| Stage | Cache Key | Skipped if... |
| :--- | :--- | :--- |
| **1. Parse** | `FileHash` | File content hash matches. |
| **2. Symbol Extraction** | `AST(Names)` | Set of top-level names in AST matches. |
| **3. Resolution** | `AST(Signature)` | Signature subtree and referenced type names match. |
| **4. Typecheck** | `AST(Body) + Deps(Signatures)` | Body subtree and all external signatures match. |
| **5. Test Execution** | `TestID + TransitveDeps(Implementation)` | Test body and all reachable code implementations match. |
