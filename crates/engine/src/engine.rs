use holo_query::InMemoryQueryStore;

/// The core incremental compilation engine.
///
/// This engine manages the project state and coordinates the 5-stage
/// incremental compilation pipeline using a query-driven approach.
pub struct Engine {
    /// Internal query store for memoization and dependency tracking.
    pub query_store: InMemoryQueryStore,
}

impl Default for Engine {
    fn default() -> Self {
        Self {
            query_store: InMemoryQueryStore::default(),
        }
    }
}

impl Engine {
    /// Creates a new instance of the engine.
    pub fn new() -> Self {
        Self::default()
    }
}
