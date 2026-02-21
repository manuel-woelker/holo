//! Database abstraction for compiler artifacts.

pub mod database;

pub use database::{
    ArtifactKey, ArtifactKind, ArtifactRecord, Database, NodeId, Revision, RocksDbDatabase,
    RocksDbMode,
};
