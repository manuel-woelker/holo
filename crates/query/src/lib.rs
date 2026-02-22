//! Query keys and in-memory cache used by compiler orchestration.

use holo_base::SharedString;
use std::collections::HashMap;

/// Pipeline stage used to namespace query entries.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum QueryStage {
    Lex,
    Parse,
    Typecheck,
    CollectTests,
    RunTests,
}

/// Key for one cached query output.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct QueryKey {
    /// File path the query is associated with.
    pub file_path: SharedString,
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
    Message(SharedString),
}

/// Query cache abstraction used by `holo-core`.
pub trait QueryStore {
    /// Inserts or updates a cached value for a key.
    fn put(&mut self, key: QueryKey, value: QueryValue);
    /// Returns a cached value for a key when present.
    fn get(&self, key: &QueryKey) -> Option<&QueryValue>;
    /// Invalidates file entries only when content hash changed.
    fn invalidate_if_hash_changed(&mut self, file_path: &str, content_hash: u64) -> bool;
    /// Returns whether this file/hash pair is already current in the store.
    fn is_current_hash(&self, file_path: &str, content_hash: u64) -> bool;
    /// Removes all entries associated with a file path.
    fn invalidate_file(&mut self, file_path: &str);
}

/// In-memory query cache for local daemon execution.
#[derive(Debug, Default)]
pub struct InMemoryQueryStore {
    entries: HashMap<QueryKey, QueryValue>,
    file_hashes: HashMap<SharedString, u64>,
}

impl QueryStore for InMemoryQueryStore {
    fn put(&mut self, key: QueryKey, value: QueryValue) {
        self.entries.insert(key, value);
    }

    fn get(&self, key: &QueryKey) -> Option<&QueryValue> {
        self.entries.get(key)
    }

    fn invalidate_if_hash_changed(&mut self, file_path: &str, content_hash: u64) -> bool {
        if self.is_current_hash(file_path, content_hash) {
            return false;
        }

        self.invalidate_file(file_path);
        self.file_hashes.insert(file_path.into(), content_hash);
        true
    }

    fn is_current_hash(&self, file_path: &str, content_hash: u64) -> bool {
        self.file_hashes
            .get(file_path)
            .is_some_and(|existing_hash| *existing_hash == content_hash)
    }

    fn invalidate_file(&mut self, file_path: &str) {
        self.entries.retain(|key, _| key.file_path != file_path);
        self.file_hashes.remove(file_path);
    }
}

#[cfg(test)]
mod tests {
    use super::{InMemoryQueryStore, QueryKey, QueryStage, QueryStore, QueryValue};

    #[test]
    fn invalidates_entries_for_specific_file() {
        let mut store = InMemoryQueryStore::default();
        let key = QueryKey {
            file_path: "sample.holo".into(),
            stage: QueryStage::Lex,
            content_hash: 11,
        };
        store.put(key.clone(), QueryValue::Complete);
        assert_eq!(store.get(&key), Some(&QueryValue::Complete));
        store.invalidate_file("sample.holo");
        assert_eq!(store.get(&key), None);
    }

    #[test]
    fn keeps_cache_when_hash_is_unchanged() {
        let mut store = InMemoryQueryStore::default();
        assert!(store.invalidate_if_hash_changed("sample.holo", 11));
        assert!(!store.invalidate_if_hash_changed("sample.holo", 11));
        assert!(store.is_current_hash("sample.holo", 11));
    }

    #[test]
    fn invalidates_cache_when_hash_changes() {
        let mut store = InMemoryQueryStore::default();
        let key = QueryKey {
            file_path: "sample.holo".into(),
            stage: QueryStage::Lex,
            content_hash: 11,
        };
        store.invalidate_if_hash_changed("sample.holo", 11);
        store.put(key.clone(), QueryValue::Complete);

        assert!(store.invalidate_if_hash_changed("sample.holo", 12));
        assert_eq!(store.get(&key), None);
        assert!(store.is_current_hash("sample.holo", 12));
    }
}
