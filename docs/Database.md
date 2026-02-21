# What Is The Purpose Of The Database Layer?

The database layer stores and serves compilation artifacts for Holo's query-driven, incremental compiler.

It must support:

- Low-latency reads for active developer workflows.
- Precise invalidation after source edits.
- Dependency-aware recomputation scheduling.
- Deterministic behavior across daemon restarts.

# What Requirements Drive The Design?

- Incremental compilation: recompute only what is affected.
- Dependency tracking: maintain forward and reverse edges between compilation nodes.
- Query support: resolve artifacts and graph relationships quickly.
- Multi-artifact support: separate artifact kinds cleanly.
- Consistency: avoid partially written graph/artifact state.

# What Core Concepts Should The API Model?

- Artifact Kind: logical bucket (for example parse tree, typed IR, diagnostics, test-impact data).
- Artifact Key: stable identity within a kind.
- Artifact Record: stored bytes plus metadata.
- Dependency Node: query result identity used in the incremental graph.
- Revision: monotonically increasing source/project version.

# Why Should The API Include Artifact Kinds (Buckets)?

Kinds are required for correctness and maintainability:

- They prevent key collisions between unrelated artifact domains.
- They allow kind-specific retention, eviction, and serialization policy.
- They make invalidation policy explicit per artifact category.

A kind should be part of the logical key, even if a backend physically stores data in one table/tree.

# What Is A Recommended Key Model?

Keys should be stable, deterministic, and cheap to compare/hash.

Recommended structure:

- `ArtifactKind`: enum-like identifier (string or numeric ID in storage).
- `ArtifactKey`: opaque stable key within that kind.
- `NodeId`: identity used for dependency indexing (often `kind + key + query revision domain`).

Key stability is critical because dependency indexes and cache reuse depend on it.

# What Artifact Record Shape Is Needed?

Each stored artifact should include:

- Payload bytes (`Vec<u8>`).
- Producer revision (`Revision`).
- Content hash of payload (for quick equality/change checks).
- Optional schema/version tag (for safe decode/migration).
- Optional timestamps for diagnostics and observability.

# What Dependency Indexes Are Required?

At minimum, two indexes are required:

- Forward index: `node -> dependencies`.
- Reverse index: `dependency -> dependents`.

Why both are necessary:

- Forward index supports recomputation and introspection.
- Reverse index enables efficient "what must be invalidated after change X?" traversal.

# What Queries Must The Database Answer Efficiently?

- Artifact read by `(kind, key)`.
- Artifact existence by `(kind, key)`.
- Dependencies of a node.
- Dependents of a node.
- Invalidation closure from a set of changed nodes.
- Optional: nodes/artifacts by revision range for cleanup.

# What API Surface Should The Database Expose?

```rust
use holo_base::Result;
use std::collections::BTreeSet;

pub type Revision = u64;

pub trait ArtifactKind: Clone + Eq + Ord + std::hash::Hash {
    fn as_str(&self) -> &'static str;
}

pub trait ArtifactKey: Clone + Eq + Ord + std::hash::Hash {}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId<K, A> {
    pub kind: K,
    pub key: A,
}

#[derive(Clone, Debug)]
pub struct ArtifactRecord {
    pub bytes: Vec<u8>,
    pub produced_at: Revision,
    pub content_hash: [u8; 32],
    pub schema_version: u32,
}

pub trait Database<K, A>
where
    K: ArtifactKind,
    A: ArtifactKey,
{
    fn put_artifact(&self, kind: &K, key: &A, record: ArtifactRecord) -> Result<()>;
    fn get_artifact(&self, kind: &K, key: &A) -> Result<Option<ArtifactRecord>>;
    fn delete_artifact(&self, kind: &K, key: &A) -> Result<()>;
    fn has_artifact(&self, kind: &K, key: &A) -> Result<bool>;

    fn set_dependencies(&self, node: &NodeId<K, A>, deps: BTreeSet<NodeId<K, A>>) -> Result<()>;
    fn get_dependencies(&self, node: &NodeId<K, A>) -> Result<BTreeSet<NodeId<K, A>>>;
    fn get_dependents(&self, node: &NodeId<K, A>) -> Result<BTreeSet<NodeId<K, A>>>;

    fn invalidate_from(&self, changed: BTreeSet<NodeId<K, A>>) -> Result<BTreeSet<NodeId<K, A>>>;
}
```

# Why Should Dependency Updates Be Replace-Not-Merge?

`set_dependencies(node, deps)` should replace the full dependency set for a node in one logical operation.

Reasons:

- Query evaluation naturally computes the complete, current dependency set.
- Replace semantics avoids stale edges persisting after logic changes.
- It simplifies correctness reasoning and reverse-index maintenance.

# How Should Invalidation Work?

Recommended invalidation flow:

1. Map changed inputs to seed `NodeId`s.
2. Traverse reverse index to compute transitive dependents.
3. Return invalidation set (including seeds).
4. Delete artifacts for invalidated nodes or mark them stale.
5. Keep graph edges until recomputation or run edge cleanup policy.

Traversal should be deterministic (sorted sets/queues) for reproducible behavior and tests.

# Does The API Need Transactions?

Yes. At least one atomic boundary is needed for each query writeback:

- Write artifact record.
- Replace forward dependency set.
- Update reverse dependency index accordingly.
- Commit all-or-nothing.

Without this, crashes can leave artifact and dependency state inconsistent.

# How Should Concurrency Be Handled?

The daemon is long-running and often concurrent. The database should support:

- Multiple concurrent readers.
- Serialized writes per key/node (global write lock is acceptable initially).
- Optimistic upgrade path to finer-grained locking later.

Correctness is higher priority than maximal write throughput in early versions.

# How Should The Database Support Query-Driven Compilation?

Each query evaluation should follow:

1. Check cache/artifact by `(kind, key)`.
2. On miss/stale: compute query.
3. Capture dependencies observed during execution.
4. Persist artifact + dependencies atomically.

This aligns the database model with query memoization and precise invalidation.

# What Storage Backends Should Be Supported?

Start with RocksDB first:

- Persistent embedded RocksDB for daemon restart resilience.
- RocksDB in-memory environment mode for tests and fast local sessions.

RocksDB supports both modes while keeping one implementation path and one data model.
Backend-independent trait design is still preferred so compiler layers do not depend on storage details.

# What Should Be Indexed For Cleanup And Maintenance?

Useful optional indexes:

- `produced_at -> nodes` for age/revision-based cleanup.
- `kind -> nodes` for kind-scoped maintenance.
- `schema_version -> nodes` for migration invalidation.

These are optimization indexes, not required for initial correctness.

# How Should Schema Evolution Be Managed?

- Include `schema_version` in each artifact record.
- Bump version on incompatible serialization or semantics change.
- On read mismatch, treat as cache miss and recompute.
- Optionally perform batch purge by version in maintenance tasks.

# What Observability Should Be Included?

The database should expose metrics/events for:

- Artifact hit/miss rates by kind.
- Invalidation set sizes and traversal latency.
- Read/write latency percentiles.
- Transaction failure/retry counts.

These metrics are needed to tune incremental performance.

# How Should This Be Tested?

Database implementations should include black-box trait tests covering:

- Artifact put/get/delete/exists behavior.
- Dependency replace semantics.
- Reverse index correctness.
- Invalidation closure correctness on small graphs.
- Atomicity expectations under simulated failure.

Prefer data-driven graph test cases to reduce duplication.

# What Is A Practical Rollout Plan?

1. Introduce bucket-aware artifact API and basic trait tests.
2. Implement RocksDB backend with column-family layout for artifacts and dependency indexes.
3. Add RocksDB in-memory mode configuration for tests and fast local runs.
4. Add dependency indexes and invalidation traversal.
5. Add atomic write boundary for artifact+graph updates.
6. Integrate query engine to persist dependencies per query evaluation.
7. Add observability and retention policies after correctness is stable.
