//! File path wrapper type for relative path handling.

use serde::{Deserialize, Serialize};

/// A wrapper around relative_path::RelativePathBuf for file path handling.
///
/// This type provides a platform-independent way to represent file paths
/// relative to a project root or workspace, making it ideal for compiler
/// internal path representation.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct FilePath(pub relative_path::RelativePathBuf);

impl FilePath {
    /// Creates a new FilePath from the given path string.
    pub fn new<P: AsRef<std::path::Path>>(path: P) -> Self {
        Self(relative_path::RelativePathBuf::from_path(path).expect("Invalid relative path"))
    }

    /// Creates a new FilePath from a string.
    pub fn from_string(s: impl Into<String>) -> Self {
        Self(relative_path::RelativePathBuf::from(s.into()))
    }

    /// Returns the path as a string.
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    /// Returns the path as a relative_path::RelativePath.
    pub fn as_relative_path(&self) -> &relative_path::RelativePath {
        &self.0
    }

    /// Returns the underlying RelativePathBuf.
    pub fn into_relative_path_buf(self) -> relative_path::RelativePathBuf {
        self.0
    }

    /// Joins this path with another path component.
    pub fn join<P: AsRef<std::path::Path>>(&self, path: P) -> Self {
        let path_str = path.as_ref().to_string_lossy();
        Self(
            self.0
                .join(&relative_path::RelativePath::from_path(&*path_str).unwrap()),
        )
    }

    /// Returns the parent directory of this path, if any.
    pub fn parent(&self) -> Option<Self> {
        self.0
            .parent()
            .map(|p| Self(relative_path::RelativePathBuf::from(p.as_str())))
    }

    /// Returns the file name of this path, if any.
    pub fn file_name(&self) -> Option<&str> {
        self.0.file_name()
    }

    /// Returns the file stem (name without extension) of this path, if any.
    pub fn file_stem(&self) -> Option<&str> {
        self.0.file_stem()
    }

    /// Returns the extension of this path, if any.
    pub fn extension(&self) -> Option<&str> {
        self.0.extension()
    }

    /// Returns true if this path is absolute.
    pub fn is_absolute(&self) -> bool {
        self.0.as_str().starts_with('/') || self.0.as_str().starts_with(std::path::is_separator)
    }

    /// Returns true if this path is relative.
    pub fn is_relative(&self) -> bool {
        !self.is_absolute()
    }

    /// Normalizes the path by removing redundant components.
    pub fn normalize(&self) -> Self {
        Self(self.0.normalize())
    }
}

impl Default for FilePath {
    fn default() -> Self {
        Self(relative_path::RelativePathBuf::new())
    }
}

impl From<String> for FilePath {
    fn from(s: String) -> Self {
        Self(relative_path::RelativePathBuf::from(s))
    }
}

impl From<&str> for FilePath {
    fn from(s: &str) -> Self {
        Self(relative_path::RelativePathBuf::from(s))
    }
}

impl From<relative_path::RelativePathBuf> for FilePath {
    fn from(path: relative_path::RelativePathBuf) -> Self {
        Self(path)
    }
}

impl From<&FilePath> for FilePath {
    fn from(path: &FilePath) -> Self {
        path.clone()
    }
}

impl AsRef<relative_path::RelativePath> for FilePath {
    fn as_ref(&self) -> &relative_path::RelativePath {
        &self.0
    }
}

impl AsRef<std::path::Path> for FilePath {
    fn as_ref(&self) -> &std::path::Path {
        std::path::Path::new(self.0.as_str())
    }
}

impl std::fmt::Display for FilePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::ops::Deref for FilePath {
    type Target = relative_path::RelativePathBuf;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_file_path_creation() {
        let p1 = FilePath::new("src/main.rs");
        let p2 = FilePath::from_string("test.holo");
        let p3: FilePath = "lib/core.rs".into();

        assert_eq!(p1.as_str(), "src/main.rs");
        assert_eq!(p2.as_str(), "test.holo");
        assert_eq!(p3.as_str(), "lib/core.rs");
    }

    #[test]
    fn test_file_path_equality() {
        let p1 = FilePath::new("src/main.rs");
        let p2 = FilePath::new("src/main.rs");
        let p3 = FilePath::new("src/lib.rs");

        assert_eq!(p1, p2);
        assert_ne!(p1, p3);
    }

    #[test]
    fn test_file_path_clone() {
        let p1 = FilePath::new("src/main.rs");
        let p2 = p1.clone();

        assert_eq!(p1, p2);
        assert_eq!(p1.as_str(), p2.as_str());
    }

    #[test]
    fn test_file_path_join() {
        let base = FilePath::new("src");
        let joined = base.join("main.rs");
        assert_eq!(joined.as_str(), "src/main.rs");
    }

    #[test]
    fn test_file_path_components() {
        let path = FilePath::new("src/main.rs");

        assert_eq!(path.file_name(), Some("main.rs"));
        assert_eq!(path.file_stem(), Some("main"));
        assert_eq!(path.extension(), Some("rs"));

        let parent = path.parent().unwrap();
        assert_eq!(parent.as_str(), "src");
    }

    #[test]
    fn test_file_path_normalize() {
        let path = FilePath::new("src/../src/main.rs");
        let normalized = path.normalize();
        assert_eq!(normalized.as_str(), "src/main.rs");
    }

    #[test]
    fn test_file_path_display() {
        let path = FilePath::new("src/main.rs");
        assert_eq!(format!("{}", path), "src/main.rs");
    }

    #[test]
    fn test_file_path_default() {
        let path = FilePath::default();
        assert!(path.as_str().is_empty());
    }
}
