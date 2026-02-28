//! RocksDB implementation of the database trait.

use std::path::PathBuf;
use std::sync::Arc;
use std::time::SystemTime;

use holo_base::{Result, SharedString};
use rocksdb::{ColumnFamilyDescriptor, Env, Options, WriteBatch, DB};

use crate::database::{Database, DynTransaction, Transaction};

/// RocksDB initialization mode.
#[derive(Clone, Debug)]
pub enum RocksDbMode {
    /// Persist data on disk at the provided path.
    Persistent(PathBuf),
    /// Keep data in-memory using RocksDB's in-memory environment.
    InMemory,
}

/// RocksDB-backed implementation of [`Database`].
pub struct RocksDbDatabase {
    db: Arc<DB>,
}

impl RocksDbDatabase {
    /// Creates a new RocksDB database with the specified tables.
    pub fn new(mode: RocksDbMode, table_names: &[SharedString]) -> Result<Self> {
        let mut opts = Options::default();
        opts.create_if_missing(true);
        opts.create_missing_column_families(true);

        let cf_descriptors: Vec<ColumnFamilyDescriptor> = table_names
            .iter()
            .map(|name| ColumnFamilyDescriptor::new(name.to_string(), Options::default()))
            .collect();

        let path = match mode {
            RocksDbMode::Persistent(path) => path,
            RocksDbMode::InMemory => {
                let env = Env::mem_env().map_err(|error| {
                    holo_base::holo_message_error!("failed to create in-memory environment")
                        .with_std_source(error)
                })?;
                opts.set_env(&env);
                let timestamp = SystemTime::now()
                    .duration_since(SystemTime::UNIX_EPOCH)
                    .map_err(|error| {
                        holo_base::holo_message_error!("failed to get timestamp")
                            .with_std_source(error)
                    })?
                    .as_nanos();
                let temp_path = std::env::temp_dir().join(format!("holo-rocksdb-{timestamp}"));
                std::fs::create_dir_all(&temp_path).map_err(|error| {
                    holo_base::holo_message_error!("failed to create temp directory")
                        .with_std_source(error)
                })?;
                temp_path
            }
        };

        let db = DB::open_cf_descriptors(&opts, path, cf_descriptors).map_err(|error| {
            holo_base::holo_message_error!("failed to open RocksDB database").with_std_source(error)
        })?;

        Ok(Self { db: Arc::new(db) })
    }
}

impl Database for RocksDbDatabase {
    fn open(path: PathBuf, table_names: Vec<SharedString>) -> Result<Self> {
        Self::new(RocksDbMode::Persistent(path), &table_names)
    }

    fn new_in_memory(table_names: Vec<SharedString>) -> Result<Self> {
        Self::new(RocksDbMode::InMemory, &table_names)
    }

    fn begin_tx(&self) -> Result<Transaction> {
        Ok(Transaction::new(Box::new(RocksDbTransaction {
            db: Arc::clone(&self.db),
        })))
    }
}

/// RocksDB transaction implementation.
pub struct RocksDbTransaction {
    db: Arc<DB>,
}

impl DynTransaction for RocksDbTransaction {
    fn get(&self, table: &str, keys: &[SharedString]) -> Result<Vec<Option<Vec<u8>>>> {
        let cf = self
            .db
            .cf_handle(table)
            .ok_or_else(|| holo_base::holo_message_error!("missing column family: {table}"))?;

        keys.iter()
            .map(|key| {
                self.db.get_cf(cf, key.as_bytes()).map_err(|error| {
                    holo_base::holo_message_error!("failed to read from table")
                        .with_std_source(error)
                })
            })
            .collect()
    }

    fn put(&self, table: &str, entries: &[(SharedString, Vec<u8>)]) -> Result<()> {
        let cf = self
            .db
            .cf_handle(table)
            .ok_or_else(|| holo_base::holo_message_error!("missing column family: {table}"))?;

        let mut batch = WriteBatch::default();
        for (key, value) in entries {
            batch.put_cf(cf, key.as_bytes(), value);
        }

        self.db.write(batch).map_err(|error| {
            holo_base::holo_message_error!("failed to write to table").with_std_source(error)
        })
    }

    fn remove(&self, table: &str, keys: &[SharedString]) -> Result<()> {
        let cf = self
            .db
            .cf_handle(table)
            .ok_or_else(|| holo_base::holo_message_error!("missing column family: {table}"))?;

        let mut batch = WriteBatch::default();
        for key in keys {
            batch.delete_cf(cf, key.as_bytes());
        }

        self.db.write(batch).map_err(|error| {
            holo_base::holo_message_error!("failed to remove from table").with_std_source(error)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stores_and_reads_values() {
        let db = RocksDbDatabase::new(RocksDbMode::InMemory, &[SharedString::from("test_table")])
            .expect("db should open");

        let tx = db.begin_tx().expect("tx should begin");
        tx.put(
            "test_table",
            &[
                (SharedString::from("key1"), b"value1".to_vec()),
                (SharedString::from("key2"), b"value2".to_vec()),
            ],
        )
        .expect("put should succeed");

        let tx = db.begin_tx().expect("tx should begin");
        let results = tx
            .get(
                "test_table",
                &[
                    SharedString::from("key1"),
                    SharedString::from("key2"),
                    SharedString::from("key3"),
                ],
            )
            .expect("get should succeed");

        assert_eq!(results[0], Some(b"value1".to_vec()));
        assert_eq!(results[1], Some(b"value2".to_vec()));
        assert_eq!(results[2], None);
    }

    #[test]
    fn removes_values() {
        let db = RocksDbDatabase::new(RocksDbMode::InMemory, &[SharedString::from("test_table")])
            .expect("db should open");

        let tx = db.begin_tx().expect("tx should begin");
        tx.put(
            "test_table",
            &[(SharedString::from("key1"), b"value1".to_vec())],
        )
        .expect("put should succeed");

        let tx = db.begin_tx().expect("tx should begin");
        tx.remove("test_table", &[SharedString::from("key1")])
            .expect("remove should succeed");

        let tx = db.begin_tx().expect("tx should begin");
        let results = tx
            .get("test_table", &[SharedString::from("key1")])
            .expect("get should succeed");

        assert_eq!(results[0], None);
    }
}
