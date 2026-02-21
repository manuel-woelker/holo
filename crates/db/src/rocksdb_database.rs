use std::collections::{BTreeSet, VecDeque};
use std::marker::PhantomData;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};

use bitcode::{Decode, Encode};
use holo_base::Result;
use rocksdb::{ColumnFamilyDescriptor, Env, Options, WriteBatch, DB};

use crate::database::{ArtifactKey, ArtifactKind, ArtifactRecord, Database, NodeId};

const ARTIFACTS_CF: &str = "artifacts";
const FORWARD_DEPS_CF: &str = "forward_dependencies";
const REVERSE_DEPS_CF: &str = "reverse_dependencies";

static IN_MEMORY_DB_COUNTER: AtomicU64 = AtomicU64::new(0);

#[derive(Clone, Debug, PartialEq, Eq, Encode, Decode)]
struct ArtifactStorageKey {
    kind: String,
    key_bytes: Vec<u8>,
}

/// RocksDB initialization mode.
pub enum RocksDbMode {
    /// Persist data on disk at the provided path.
    Persistent(PathBuf),
    /// Keep data in-memory using RocksDB's in-memory environment when supported.
    /// On platforms without mem-env support, this uses a target-local temp path.
    InMemory,
}

/// RocksDB-backed implementation of [`Database`].
pub struct RocksDbDatabase<K, A> {
    db: DB,
    _env: Option<Env>,
    _marker: PhantomData<(K, A)>,
}

impl<K, A> RocksDbDatabase<K, A> {
    /// Opens a new database with required column families.
    pub fn open(mode: RocksDbMode) -> Result<Self> {
        let mut opts = Options::default();
        opts.create_if_missing(true);
        opts.create_missing_column_families(true);

        let cf_descriptors = vec![
            ColumnFamilyDescriptor::new(ARTIFACTS_CF, Options::default()),
            ColumnFamilyDescriptor::new(FORWARD_DEPS_CF, Options::default()),
            ColumnFamilyDescriptor::new(REVERSE_DEPS_CF, Options::default()),
        ];

        let (path, env) = match mode {
            RocksDbMode::Persistent(path) => (path, None),
            RocksDbMode::InMemory => {
                #[cfg(target_os = "windows")]
                {
                    (next_in_memory_db_path()?, None)
                }

                #[cfg(not(target_os = "windows"))]
                {
                    let env = Env::mem_env().map_err(|error| {
                        holo_base::holo_message_error!(
                            "failed to initialize RocksDB in-memory environment"
                        )
                        .with_std_source(error)
                    })?;
                    opts.set_env(&env);
                    (next_in_memory_db_path()?, Some(env))
                }
            }
        };

        let db = DB::open_cf_descriptors(&opts, path, cf_descriptors).map_err(|error| {
            holo_base::holo_message_error!("failed to open RocksDB database").with_std_source(error)
        })?;

        Ok(Self {
            db,
            _env: env,
            _marker: PhantomData,
        })
    }
}

impl<K, A> RocksDbDatabase<K, A>
where
    K: ArtifactKind + Encode + for<'a> Decode<'a>,
    A: ArtifactKey + Encode + for<'a> Decode<'a>,
{
    fn encode_artifact_key(kind: &K, key: &A) -> Vec<u8> {
        bitcode::encode(&ArtifactStorageKey {
            kind: kind.as_str().to_owned(),
            key_bytes: bitcode::encode(key),
        })
    }

    fn encode_node_id(node: &NodeId<K, A>) -> Vec<u8> {
        bitcode::encode(node)
    }

    fn decode_node_set(bytes: &[u8], label: &str) -> Result<BTreeSet<NodeId<K, A>>> {
        bitcode::decode(bytes)
            .map_err(|error| holo_base::holo_message_error!("failed to decode {label}: {error}"))
    }

    fn get_cf_handle<'a>(&'a self, name: &'static str) -> Result<&'a rocksdb::ColumnFamily> {
        self.db
            .cf_handle(name)
            .ok_or_else(|| holo_base::holo_message_error!("missing RocksDB column family: {name}"))
    }

    fn get_forward_set(&self, node_key: &[u8]) -> Result<BTreeSet<NodeId<K, A>>> {
        let cf = self.get_cf_handle(FORWARD_DEPS_CF)?;
        let value = self.db.get_cf(cf, node_key).map_err(|error| {
            holo_base::holo_message_error!("failed to read forward dependencies")
                .with_std_source(error)
        })?;
        match value {
            Some(bytes) => Self::decode_node_set(&bytes, "forward dependency set"),
            None => Ok(BTreeSet::new()),
        }
    }

    fn get_reverse_set(&self, node_key: &[u8]) -> Result<BTreeSet<NodeId<K, A>>> {
        let cf = self.get_cf_handle(REVERSE_DEPS_CF)?;
        let value = self.db.get_cf(cf, node_key).map_err(|error| {
            holo_base::holo_message_error!("failed to read reverse dependencies")
                .with_std_source(error)
        })?;
        match value {
            Some(bytes) => Self::decode_node_set(&bytes, "reverse dependency set"),
            None => Ok(BTreeSet::new()),
        }
    }
}

impl<K, A> Database<K, A> for RocksDbDatabase<K, A>
where
    K: ArtifactKind + Encode + for<'a> Decode<'a>,
    A: ArtifactKey + Encode + for<'a> Decode<'a>,
{
    fn put_artifact(&self, kind: &K, key: &A, record: ArtifactRecord) -> Result<()> {
        let cf = self.get_cf_handle(ARTIFACTS_CF)?;
        self.db
            .put_cf(
                cf,
                Self::encode_artifact_key(kind, key),
                bitcode::encode(&record),
            )
            .map_err(|error| {
                holo_base::holo_message_error!("failed to write artifact").with_std_source(error)
            })
    }

    fn get_artifact(&self, kind: &K, key: &A) -> Result<Option<ArtifactRecord>> {
        let cf = self.get_cf_handle(ARTIFACTS_CF)?;
        let value = self
            .db
            .get_cf(cf, Self::encode_artifact_key(kind, key))
            .map_err(|error| {
                holo_base::holo_message_error!("failed to read artifact").with_std_source(error)
            })?;

        match value {
            Some(bytes) => bitcode::decode(&bytes).map(Some).map_err(|error| {
                holo_base::holo_message_error!("failed to decode artifact: {error}")
            }),
            None => Ok(None),
        }
    }

    fn delete_artifact(&self, kind: &K, key: &A) -> Result<()> {
        let cf = self.get_cf_handle(ARTIFACTS_CF)?;
        self.db
            .delete_cf(cf, Self::encode_artifact_key(kind, key))
            .map_err(|error| {
                holo_base::holo_message_error!("failed to delete artifact").with_std_source(error)
            })
    }

    fn has_artifact(&self, kind: &K, key: &A) -> Result<bool> {
        Ok(self.get_artifact(kind, key)?.is_some())
    }

    fn set_dependencies(&self, node: &NodeId<K, A>, deps: BTreeSet<NodeId<K, A>>) -> Result<()> {
        let node_key = Self::encode_node_id(node);
        let previous_deps = self.get_forward_set(&node_key)?;
        let mut batch = WriteBatch::default();

        let forward_cf = self.get_cf_handle(FORWARD_DEPS_CF)?;
        let reverse_cf = self.get_cf_handle(REVERSE_DEPS_CF)?;

        for old_dep in previous_deps {
            let dep_key = Self::encode_node_id(&old_dep);
            let mut dependents = self.get_reverse_set(&dep_key)?;
            dependents.remove(node);
            if dependents.is_empty() {
                batch.delete_cf(reverse_cf, dep_key);
            } else {
                batch.put_cf(reverse_cf, dep_key, bitcode::encode(&dependents));
            }
        }

        for dep in &deps {
            let dep_key = Self::encode_node_id(dep);
            let mut dependents = self.get_reverse_set(&dep_key)?;
            dependents.insert(node.clone());
            batch.put_cf(reverse_cf, dep_key, bitcode::encode(&dependents));
        }

        if deps.is_empty() {
            batch.delete_cf(forward_cf, &node_key);
        } else {
            batch.put_cf(forward_cf, &node_key, bitcode::encode(&deps));
        }

        self.db.write(batch).map_err(|error| {
            holo_base::holo_message_error!("failed to update dependency indexes")
                .with_std_source(error)
        })
    }

    fn get_dependencies(&self, node: &NodeId<K, A>) -> Result<BTreeSet<NodeId<K, A>>> {
        let node_key = Self::encode_node_id(node);
        self.get_forward_set(&node_key)
    }

    fn get_dependents(&self, node: &NodeId<K, A>) -> Result<BTreeSet<NodeId<K, A>>> {
        let node_key = Self::encode_node_id(node);
        self.get_reverse_set(&node_key)
    }

    fn invalidate_from(&self, changed: BTreeSet<NodeId<K, A>>) -> Result<BTreeSet<NodeId<K, A>>> {
        let mut invalidated = changed.clone();
        let mut queue: VecDeque<NodeId<K, A>> = changed.into_iter().collect();

        while let Some(current) = queue.pop_front() {
            for dependent in self.get_dependents(&current)? {
                if invalidated.insert(dependent.clone()) {
                    queue.push_back(dependent);
                }
            }
        }

        Ok(invalidated)
    }
}

fn next_in_memory_db_path() -> Result<PathBuf> {
    let in_memory_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("target")
        .join("rocksdb-in-memory");
    std::fs::create_dir_all(&in_memory_root).map_err(|error| {
        holo_base::holo_message_error!("failed to create in-memory RocksDB directory")
            .with_std_source(error)
    })?;

    let id = IN_MEMORY_DB_COUNTER.fetch_add(1, Ordering::Relaxed);
    Ok(in_memory_root.join(format!("db-{id}")))
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use bitcode::{Decode, Encode};

    use crate::database::{ArtifactKey, ArtifactKind, ArtifactRecord, Database, NodeId};
    use crate::rocksdb_database::{RocksDbDatabase, RocksDbMode};

    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Encode, Decode)]
    enum TestKind {
        Parse,
        Typecheck,
    }

    impl ArtifactKind for TestKind {
        fn as_str(&self) -> &'static str {
            match self {
                Self::Parse => "parse",
                Self::Typecheck => "typecheck",
            }
        }
    }

    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Encode, Decode)]
    struct TestKey(String);

    impl ArtifactKey for TestKey {}

    fn node(kind: TestKind, key: &str) -> NodeId<TestKind, TestKey> {
        NodeId {
            kind,
            key: TestKey(key.to_owned()),
        }
    }

    fn make_db() -> RocksDbDatabase<TestKind, TestKey> {
        RocksDbDatabase::open(RocksDbMode::InMemory).expect("db should open")
    }

    #[test]
    fn stores_and_reads_artifacts() {
        let db = make_db();
        let kind = TestKind::Parse;
        let key = TestKey("module:foo".to_owned());
        let record = ArtifactRecord {
            bytes: vec![1, 2, 3],
            produced_at: 7,
            content_hash: [9; 32],
            schema_version: 1,
        };

        db.put_artifact(&kind, &key, record.clone())
            .expect("put should succeed");

        let loaded = db
            .get_artifact(&kind, &key)
            .expect("get should succeed")
            .expect("artifact should exist");
        assert_eq!(loaded, record);
        assert!(db.has_artifact(&kind, &key).expect("has should succeed"));
    }

    #[test]
    fn replaces_dependencies_and_updates_reverse_index() {
        let db = make_db();
        let root = node(TestKind::Typecheck, "root");
        let dep_a = node(TestKind::Parse, "a");
        let dep_b = node(TestKind::Parse, "b");

        db.set_dependencies(&root, BTreeSet::from([dep_a.clone(), dep_b.clone()]))
            .expect("set deps should succeed");

        db.set_dependencies(&root, BTreeSet::from([dep_b.clone()]))
            .expect("replace deps should succeed");

        assert!(!db
            .get_dependents(&dep_a)
            .expect("get dependents should succeed")
            .contains(&root));
        assert!(db
            .get_dependents(&dep_b)
            .expect("get dependents should succeed")
            .contains(&root));
    }

    #[test]
    fn computes_invalidation_closure_from_reverse_dependencies() {
        let db = make_db();
        let parse_c = node(TestKind::Parse, "c");
        let type_b = node(TestKind::Typecheck, "b");
        let type_a = node(TestKind::Typecheck, "a");

        db.set_dependencies(&type_b, BTreeSet::from([parse_c.clone()]))
            .expect("set deps should succeed");
        db.set_dependencies(&type_a, BTreeSet::from([type_b.clone()]))
            .expect("set deps should succeed");

        let invalidated = db
            .invalidate_from(BTreeSet::from([parse_c.clone()]))
            .expect("invalidation should succeed");

        assert_eq!(invalidated, BTreeSet::from([parse_c, type_b, type_a]));
    }
}
