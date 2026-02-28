//! Database abstraction for compiler artifacts.

pub mod legacy;
pub mod legacy_rocksdb;

pub use legacy::{ArtifactKey, ArtifactKind, ArtifactRecord, LegacyDatabase, NodeId, Revision};
pub use legacy_rocksdb::{LegacyRocksDbDatabase, RocksDbMode};
