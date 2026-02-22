use std::path::Path;

use holo_base::{Result, SharedString};

/// Filesystem operations used by holo services.
pub trait FileSystem {
    /// Reads UTF-8 text from a file path.
    fn read_to_string(&self, path: &Path) -> Result<SharedString>;

    /// Writes UTF-8 text to a file path, replacing existing content.
    fn write_string(&self, path: &Path, contents: &str) -> Result<()>;

    /// Ensures a directory exists, creating parents as needed.
    fn create_dir_all(&self, path: &Path) -> Result<()>;

    /// Returns `true` if the path currently exists.
    fn exists(&self, path: &Path) -> bool;
}
