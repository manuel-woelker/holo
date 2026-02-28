//! Database abstraction for compiler artifacts.

pub mod database;
pub mod legacy;
pub mod legacy_rocksdb;
pub mod table;

pub use database::Database;
pub use legacy::{ArtifactKey, ArtifactKind, ArtifactRecord, LegacyDatabase, NodeId, Revision};
pub use legacy_rocksdb::{LegacyRocksDbDatabase, RocksDbMode};
pub use table::{Table, TableKey, TableValue};
