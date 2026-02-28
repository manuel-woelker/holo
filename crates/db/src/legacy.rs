//! Legacy database abstraction for compiler artifacts.
//!
//! This module contains the original database trait and its RocksDB implementation.
//! These are considered legacy and will be replaced by a simpler, more focused API.

use std::collections::BTreeSet;
use std::hash::Hash;

use bitcode::{Decode, Encode};
use holo_base::Result;

/// Monotonic source/project revision identifier.
pub type Revision = u64;

/// Kind marker for artifact buckets.
pub trait ArtifactKind: Clone + Eq + Ord + Hash {
    /// Returns the stable name of this kind.
    fn as_str(&self) -> &'static str;
}

/// Key marker for artifact identifiers within a kind.
pub trait ArtifactKey: Clone + Eq + Ord + Hash {}

/// Dependency graph node identity.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Encode, Decode)]
pub struct NodeId<K, A> {
    /// Artifact kind/bucket.
    pub kind: K,
    /// Artifact key in that kind.
    pub key: A,
}

/// Stored artifact payload and metadata.
#[derive(Clone, Debug, PartialEq, Eq, Encode, Decode)]
pub struct ArtifactRecord {
    /// Serialized payload bytes.
    pub bytes: Vec<u8>,
    /// Revision that produced this record.
    pub produced_at: Revision,
    /// Content hash for quick change checks.
    pub content_hash: [u8; 32],
    /// Serialization schema version.
    pub schema_version: u32,
}

/// Legacy Database API for artifact storage and dependency indexing.
pub trait LegacyDatabase<K, A>
where
    K: ArtifactKind,
    A: ArtifactKey,
{
    /// Stores or replaces the artifact at `(kind, key)`.
    fn put_artifact(&self, kind: &K, key: &A, record: ArtifactRecord) -> Result<()>;

    /// Loads the artifact at `(kind, key)`.
    fn get_artifact(&self, kind: &K, key: &A) -> Result<Option<ArtifactRecord>>;

    /// Deletes the artifact at `(kind, key)`.
    fn delete_artifact(&self, kind: &K, key: &A) -> Result<()>;

    /// Returns whether an artifact exists at `(kind, key)`.
    fn has_artifact(&self, kind: &K, key: &A) -> Result<bool>;

    /// Replaces the full dependency set for `node`.
    fn set_dependencies(&self, node: &NodeId<K, A>, deps: BTreeSet<NodeId<K, A>>) -> Result<()>;

    /// Reads forward dependencies for `node`.
    fn get_dependencies(&self, node: &NodeId<K, A>) -> Result<BTreeSet<NodeId<K, A>>>;

    /// Reads reverse dependencies for `node`.
    fn get_dependents(&self, node: &NodeId<K, A>) -> Result<BTreeSet<NodeId<K, A>>>;

    /// Returns the transitive invalidation closure from changed nodes.
    fn invalidate_from(&self, changed: BTreeSet<NodeId<K, A>>) -> Result<BTreeSet<NodeId<K, A>>>;
}
