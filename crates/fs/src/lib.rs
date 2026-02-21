//! Filesystem abstraction for holo components.

use std::path::Path;

use holo_base::{ErrorKind, HoloError, Result};

/// Filesystem operations used by holo services.
pub trait FileSystem {
    /// Reads UTF-8 text from a file path.
    fn read_to_string(&self, path: &Path) -> Result<String>;

    /// Writes UTF-8 text to a file path, replacing existing content.
    fn write_string(&self, path: &Path, contents: &str) -> Result<()>;

    /// Ensures a directory exists, creating parents as needed.
    fn create_dir_all(&self, path: &Path) -> Result<()>;

    /// Returns `true` if the path currently exists.
    fn exists(&self, path: &Path) -> bool;
}

/// Standard-library-backed filesystem implementation.
#[derive(Debug, Default, Clone, Copy)]
pub struct StdFileSystem;

impl FileSystem for StdFileSystem {
    fn read_to_string(&self, path: &Path) -> Result<String> {
        std::fs::read_to_string(path).map_err(|error| {
            holo_base::holo_message_error!("failed to read {}", path.display())
                .with_source(HoloError::new(ErrorKind::Io).with_std_source(error))
        })
    }

    fn write_string(&self, path: &Path, contents: &str) -> Result<()> {
        std::fs::write(path, contents).map_err(|error| {
            holo_base::holo_message_error!("failed to write {}", path.display())
                .with_source(HoloError::new(ErrorKind::Io).with_std_source(error))
        })
    }

    fn create_dir_all(&self, path: &Path) -> Result<()> {
        std::fs::create_dir_all(path).map_err(|error| {
            holo_base::holo_message_error!("failed to create directory {}", path.display())
                .with_source(HoloError::new(ErrorKind::Io).with_std_source(error))
        })
    }

    fn exists(&self, path: &Path) -> bool {
        path.exists()
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::time::{SystemTime, UNIX_EPOCH};

    use super::{FileSystem, StdFileSystem};

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
        let fs = StdFileSystem;
        let dir = unique_temp_path();
        let file = dir.join("sample.txt");

        fs.create_dir_all(&dir)
            .expect("should create temp test dir");
        fs.write_string(&file, "hello world")
            .expect("should write file");
        let text = fs.read_to_string(&file).expect("should read file");

        assert_eq!(text, "hello world");

        std::fs::remove_dir_all(&dir).expect("should clean up temp dir");
    }

    #[test]
    fn reports_path_existence() {
        let fs = StdFileSystem;
        let dir = unique_temp_path();
        let file = dir.join("exists.txt");

        assert!(!fs.exists(&file));
        fs.create_dir_all(&dir)
            .expect("should create temp test dir");
        fs.write_string(&file, "x").expect("should write file");
        assert!(fs.exists(&file));

        std::fs::remove_dir_all(&dir).expect("should clean up temp dir");
    }
}
