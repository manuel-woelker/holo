//! Common infrastructure shared by holo components.

pub mod error;
pub mod file_path;
pub mod hash;
pub mod logging;
pub mod shared_string;
pub mod source_diagnostic;
pub mod source_file;
pub mod span;
pub mod timing;

pub use error::{ErrorKind, HoloError, Result};
pub use file_path::FilePath;
pub use hash::hash_string;
pub use parking_lot::Mutex;
pub use shared_string::SharedString;
pub use source_diagnostic::{
    display_source_diagnostics, AnnotatedSpan, DiagnosticKind, SourceDiagnostic, SourceExcerpt,
};
pub use source_file::SourceFile;
pub use span::Span;
pub use timing::{time_task, TaskTimer, TaskTiming};

/// Build-time project revision string.
pub const PROJECT_REVISION: &str = env!("HOLO_PROJECT_REVISION");

/// Returns the build-time project revision string.
pub fn project_revision() -> &'static str {
    PROJECT_REVISION
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn project_revision_is_non_empty() {
        assert!(!project_revision().trim().is_empty());
    }

    #[test]
    fn test_hash_string() {
        let h1 = hash_string("hello");
        let h2 = hash_string("hello");
        let h3 = hash_string("world");
        assert_eq!(h1, h2);
        assert_ne!(h1, h3);
    }
}
