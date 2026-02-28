//! Database abstraction for compiler artifacts.

pub mod database;
pub mod legacy;
pub mod legacy_rocksdb;
pub mod rocksdb;
pub mod table;

pub use database::{Database, DatabaseTransaction};
pub use legacy::{ArtifactKey, ArtifactKind, ArtifactRecord, LegacyDatabase, NodeId, Revision};
pub use legacy_rocksdb::{LegacyRocksDbDatabase, RocksDbMode};
pub use rocksdb::{RocksDbDatabase, RocksDbMode as RocksDbDatabaseMode, RocksDbTransaction};
pub use table::{Table, TableKey, TableValue};
