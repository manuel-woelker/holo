use holo_base::{FilePath, Result, SharedString};

/// Filesystem operations used by holo services.
pub trait FileSystem: Send + Sync {
    /// Reads UTF-8 text from a file path.
    fn read_to_string(&self, path: &FilePath) -> Result<SharedString>;

    /// Writes UTF-8 text to a file path, replacing existing content.
    fn write_string(&self, path: &FilePath, contents: &str) -> Result<()>;

    /// Ensures a directory exists, creating parents as needed.
    fn create_dir_all(&self, path: &FilePath) -> Result<()>;

    /// Returns `true` if the path currently exists.
    fn exists(&self, path: &FilePath) -> bool;
}
