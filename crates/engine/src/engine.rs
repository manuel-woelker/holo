use crate::cycle::{Cycle, CycleResult};
use crate::observer::{CycleObserver, StdoutObserver};
use holo_base::{FilePath, Result, SharedString};
use holo_db::Database;
use holo_fs::{FileSystem, StdFileSystem};
use std::collections::HashSet;
use std::sync::Arc;

pub(crate) const FILE_HASH_TABLE: &str = "file_hash";

/// The core incremental compilation engine.
///
/// This engine manages the project state and coordinates the 5-stage
/// incremental compilation pipeline using a forward incremental approach.
pub struct Engine {
    /// Stage 1: Files that have changed since the last cycle.
    pub(crate) dirty_files: HashSet<FilePath>,
    pub(crate) filesystem: Arc<dyn FileSystem>,
    pub(crate) database: Arc<dyn Database>,
    pub(crate) observer: Arc<dyn CycleObserver>,
}

impl Default for Engine {
    fn default() -> Self {
        let table_names = vec![SharedString::from(FILE_HASH_TABLE)];
        let database = Arc::new(holo_db::RocksDbDatabase::new_in_memory(table_names).unwrap());
        Self {
            dirty_files: HashSet::new(),
            filesystem: Arc::new(StdFileSystem::new_at_cwd()),
            database,
            observer: Arc::new(StdoutObserver),
        }
    }
}

impl Engine {
    /// Creates a new instance of the engine.
    pub fn new() -> Self {
        Self::default()
    }

    /// Configures the engine to use a specific filesystem.
    pub fn with_filesystem(mut self, filesystem: Arc<dyn FileSystem>) -> Self {
        self.filesystem = filesystem;
        self
    }

    /// Configures the engine to use a specific database.
    pub fn with_database(mut self, database: Arc<dyn Database>) -> Self {
        self.database = database;
        self
    }

    /// Configures the engine to use a specific cycle observer.
    pub fn with_observer(mut self, observer: Arc<dyn CycleObserver>) -> Self {
        self.observer = observer;
        self
    }

    /// Executes a single incremental compilation cycle.
    ///
    /// This method represents one "turn" of the compiler's forward incremental pipeline.
    /// It is called a "cycle" because it is a fundamental unit of the continuous
    /// development feedback loop, transforming recent file changes into updated
    /// diagnostics and test results.
    ///
    /// Each cycle progresses through the 5 stages of compilation, omitting
    /// work for components that haven't changed since the last pulse.
    pub fn run_cycle(&mut self) -> Result<CycleResult> {
        let mut cycle = Cycle::new(self);
        cycle.run()
    }

    /// Marks a file as dirty and needing re-processing in the next cycle.
    pub fn record_change(&mut self, file_path: FilePath) {
        self.dirty_files.insert(file_path);
    }
}
