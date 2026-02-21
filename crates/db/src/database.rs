use holo_base::Result;

/// Stable key used to store and retrieve compilation artifacts.
pub type ArtifactKey = str;

/// Storage interface for compilation artifacts produced by the compiler.
pub trait Database {
    /// Stores the artifact bytes for a key, replacing any existing artifact.
    fn put_artifact(&self, key: &ArtifactKey, artifact: Vec<u8>) -> Result<()>;

    /// Loads artifact bytes for a key.
    fn get_artifact(&self, key: &ArtifactKey) -> Result<Option<Vec<u8>>>;

    /// Deletes artifact bytes for a key.
    fn delete_artifact(&self, key: &ArtifactKey) -> Result<()>;

    /// Returns `true` when an artifact currently exists for the key.
    fn has_artifact(&self, key: &ArtifactKey) -> Result<bool>;
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use holo_base::Mutex;

    use super::{ArtifactKey, Database};

    struct InMemoryDatabase {
        artifacts: Mutex<HashMap<String, Vec<u8>>>,
    }

    impl InMemoryDatabase {
        fn new() -> Self {
            Self {
                artifacts: Mutex::new(HashMap::new()),
            }
        }
    }

    impl Database for InMemoryDatabase {
        fn put_artifact(&self, key: &ArtifactKey, artifact: Vec<u8>) -> holo_base::Result<()> {
            self.artifacts.lock().insert(key.to_owned(), artifact);
            Ok(())
        }

        fn get_artifact(&self, key: &ArtifactKey) -> holo_base::Result<Option<Vec<u8>>> {
            Ok(self.artifacts.lock().get(key).cloned())
        }

        fn delete_artifact(&self, key: &ArtifactKey) -> holo_base::Result<()> {
            self.artifacts.lock().remove(key);
            Ok(())
        }

        fn has_artifact(&self, key: &ArtifactKey) -> holo_base::Result<bool> {
            Ok(self.artifacts.lock().contains_key(key))
        }
    }

    #[test]
    fn round_trips_artifact_bytes() {
        let db = InMemoryDatabase::new();
        let key = "module:foo";
        let artifact = vec![0xde, 0xad, 0xbe, 0xef];

        db.put_artifact(key, artifact.clone())
            .expect("storing should succeed");

        let loaded = db
            .get_artifact(key)
            .expect("load should succeed")
            .expect("artifact should exist");

        assert_eq!(loaded, artifact);
    }

    #[test]
    fn reports_missing_artifact_as_none() {
        let db = InMemoryDatabase::new();
        assert!(db
            .get_artifact("module:missing")
            .expect("load should succeed")
            .is_none());
    }

    #[test]
    fn deletes_artifacts() {
        let db = InMemoryDatabase::new();
        let key = "module:bar";
        db.put_artifact(key, vec![1, 2, 3])
            .expect("store should succeed");

        db.delete_artifact(key).expect("delete should succeed");

        assert!(!db.has_artifact(key).expect("exists should succeed"));
    }
}
