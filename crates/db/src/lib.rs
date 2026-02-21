//! Database abstraction for compiler artifacts.

pub mod database;
pub mod rocksdb_database;

pub use database::{ArtifactKey, ArtifactKind, ArtifactRecord, Database, NodeId, Revision};
pub use rocksdb_database::{RocksDbDatabase, RocksDbMode};
