//! Database abstraction for compiler artifacts.
//!
//! This module provides traits for managing database tables and transactions.
//! The database uses a table-based approach where each table is identified
//! by a name and contains key-value pairs.

use std::path::PathBuf;

use holo_base::Result;
use holo_base::SharedString;

/// A transaction for reading and writing to database tables.
///
/// Transactions provide atomic operations on tables. Changes made
/// within a transaction are not visible until the transaction is
/// committed.
pub trait DynTransaction {
    /// Retrieves values for the given keys from a table.
    ///
    /// Returns a vector of optional values. Each entry corresponds
    /// to the key at the same index in the input iterator.
    fn get(&self, table: &str, keys: &[SharedString]) -> Result<Vec<Option<Vec<u8>>>>;

    /// Writes key-value entries to a table.
    ///
    /// All entries are written atomically within this operation.
    fn put(&self, table: &str, entries: &[(SharedString, Vec<u8>)]) -> Result<()>;

    /// Removes entries from a table by keys.
    fn remove(&self, table: &str, keys: &[SharedString]) -> Result<()>;
}

/// A transaction handle for database operations.
///
/// This type wraps the underlying transaction implementation and provides
/// a type-safe interface for interacting with database tables.
pub struct Transaction {
    tx: Box<dyn DynTransaction>,
}

impl Transaction {
    /// Creates a new transaction from a boxed transaction.
    pub fn new(tx: Box<dyn DynTransaction>) -> Self {
        Self { tx }
    }

    /// Retrieves values for the given keys from a table.
    pub fn get(&self, table: &str, keys: &[SharedString]) -> Result<Vec<Option<Vec<u8>>>> {
        self.tx.get(table, keys)
    }

    /// Writes key-value entries to a table.
    pub fn put(&self, table: &str, entries: &[(SharedString, Vec<u8>)]) -> Result<()> {
        self.tx.put(table, entries)
    }

    /// Removes entries from a table by keys.
    pub fn remove(&self, table: &str, keys: &[SharedString]) -> Result<()> {
        self.tx.remove(table, keys)
    }
}

/// Database trait for managing table storage.
///
/// This trait defines the interface for opening databases and
/// creating transactions. Implementations provide persistent or
/// in-memory storage using various backends.
pub trait Database {
    /// Opens a database at the given path with the specified table names.
    ///
    /// # Arguments
    /// * `path` - The path where the database is stored
    /// * `table_names` - Names of the tables to create
    ///
    /// # Errors
    /// Returns an error if the database cannot be opened or tables cannot be created.
    fn open(path: PathBuf, table_names: Vec<SharedString>) -> Result<Self>
    where
        Self: Sized;

    /// Opens an in-memory database with the specified table names.
    ///
    /// # Arguments
    /// * `table_names` - Names of the tables to create
    ///
    /// # Errors
    /// Returns an error if the database cannot be created.
    fn new_in_memory(table_names: Vec<SharedString>) -> Result<Self>
    where
        Self: Sized;

    /// Begins a new transaction on this database.
    ///
    /// Transactions may be read-only or read-write depending on
    /// the implementation.
    fn begin_tx(&self) -> Result<Transaction>;
}
