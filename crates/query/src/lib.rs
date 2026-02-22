//! Query keys and in-memory cache used by compiler orchestration.

use std::collections::HashMap;

/// Pipeline stage used to namespace query entries.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum QueryStage {
    Lex,
    Parse,
    Typecheck,
    RunTests,
}

/// Key for one cached query output.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct QueryKey {
    /// File path the query is associated with.
    pub file_path: String,
    /// Pipeline stage this query belongs to.
    pub stage: QueryStage,
    /// Hash of the file contents used to produce the value.
    pub content_hash: u64,
}

/// Cached value variants for the initial query store.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum QueryValue {
    /// Marker for successful stage completion.
    Complete,
    /// Human-readable status or diagnostic payload.
    Message(String),
}

/// Query cache abstraction used by `holo-core`.
pub trait QueryStore {
    /// Inserts or updates a cached value for a key.
    fn put(&mut self, key: QueryKey, value: QueryValue);
    /// Returns a cached value for a key when present.
    fn get(&self, key: &QueryKey) -> Option<&QueryValue>;
    /// Removes all entries associated with a file path.
    fn invalidate_file(&mut self, file_path: &str);
}

/// In-memory query cache for local daemon execution.
#[derive(Debug, Default)]
pub struct InMemoryQueryStore {
    entries: HashMap<QueryKey, QueryValue>,
}

impl QueryStore for InMemoryQueryStore {
    fn put(&mut self, key: QueryKey, value: QueryValue) {
        self.entries.insert(key, value);
    }

    fn get(&self, key: &QueryKey) -> Option<&QueryValue> {
        self.entries.get(key)
    }

    fn invalidate_file(&mut self, file_path: &str) {
        self.entries.retain(|key, _| key.file_path != file_path);
    }
}

#[cfg(test)]
mod tests {
    use super::{InMemoryQueryStore, QueryKey, QueryStage, QueryStore, QueryValue};

    #[test]
    fn invalidates_entries_for_specific_file() {
        let mut store = InMemoryQueryStore::default();
        let key = QueryKey {
            file_path: "sample.holo".to_owned(),
            stage: QueryStage::Lex,
            content_hash: 11,
        };
        store.put(key.clone(), QueryValue::Complete);
        assert_eq!(store.get(&key), Some(&QueryValue::Complete));
        store.invalidate_file("sample.holo");
        assert_eq!(store.get(&key), None);
    }
}
