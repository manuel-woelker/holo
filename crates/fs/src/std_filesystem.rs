use std::path::PathBuf;

use holo_base::{ErrorKind, FilePath, HoloError, Result, SharedString};

use crate::FileSystem;

/// Standard-library-backed filesystem implementation.
#[derive(Debug, Default, Clone)]
pub struct StdFileSystem {
    root: PathBuf,
}

impl StdFileSystem {
    /// Creates a new filesystem anchored at the current working directory.
    pub fn new_at_cwd() -> Self {
        Self {
            root: std::env::current_dir().unwrap_or_default(),
        }
    }

    /// Creates a new filesystem anchored at the given root.
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self { root: root.into() }
    }

    fn resolve(&self, path: &FilePath) -> PathBuf {
        self.root.join(path.as_str())
    }
}

impl FileSystem for StdFileSystem {
    fn read_to_string(&self, path: &FilePath) -> Result<SharedString> {
        let abs_path = self.resolve(path);
        std::fs::read_to_string(&abs_path)
            .map(|value| value.into())
            .map_err(|error| {
                holo_base::holo_message_error!("failed to read {}", abs_path.display())
                    .with_source(HoloError::new(ErrorKind::Io).with_std_source(error))
            })
    }

    fn write_string(&self, path: &FilePath, contents: &str) -> Result<()> {
        let abs_path = self.resolve(path);
        std::fs::write(&abs_path, contents).map_err(|error| {
            holo_base::holo_message_error!("failed to write {}", abs_path.display())
                .with_source(HoloError::new(ErrorKind::Io).with_std_source(error))
        })
    }

    fn create_dir_all(&self, path: &FilePath) -> Result<()> {
        let abs_path = self.resolve(path);
        std::fs::create_dir_all(&abs_path).map_err(|error| {
            holo_base::holo_message_error!("failed to create directory {}", abs_path.display())
                .with_source(HoloError::new(ErrorKind::Io).with_std_source(error))
        })
    }

    fn exists(&self, path: &FilePath) -> bool {
        self.resolve(path).exists()
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::time::{SystemTime, UNIX_EPOCH};

    use holo_base::FilePath;

    use crate::{FileSystem, StdFileSystem};

    static NEXT_ID: AtomicU64 = AtomicU64::new(0);

    fn unique_temp_path() -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time before unix epoch")
            .as_nanos();
        let id = NEXT_ID.fetch_add(1, Ordering::Relaxed);
        std::env::temp_dir().join(format!("holo_fs_test_{nanos}_{id}"))
    }

    #[test]
    fn writes_and_reads_file_content() {
        let root = unique_temp_path();
        let fs = StdFileSystem::new(&root);
        let file = FilePath::from("subdir/sample.txt");

        fs.create_dir_all(&FilePath::from("subdir"))
            .expect("should create subdir");
        fs.write_string(&file, "hello world")
            .expect("should write file");
        let text = fs.read_to_string(&file).expect("should read file");

        assert_eq!(text, "hello world");

        std::fs::remove_dir_all(&root).expect("should clean up temp dir");
    }

    #[test]
    fn reports_path_existence() {
        let root = unique_temp_path();
        let fs = StdFileSystem::new(&root);
        let file = FilePath::from("exists.txt");

        assert!(!fs.exists(&file));
        std::fs::create_dir_all(&root).expect("should create root dir");
        fs.write_string(&file, "x").expect("should write file");
        assert!(fs.exists(&file));

        std::fs::remove_dir_all(&root).expect("should clean up temp dir");
    }
}
