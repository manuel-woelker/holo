//! Common infrastructure shared by holo components.

pub mod error;
pub mod logging;
pub mod source_diagnostic;
pub mod source_file;
pub mod span;
pub mod timing;

pub use error::{ErrorKind, HoloError, Result};
pub use parking_lot::Mutex;
pub use source_diagnostic::{
    display_source_diagnostics, AnnotatedSpan, DiagnosticKind, SourceDiagnostic, SourceExcerpt,
};
pub use source_file::SourceFile;
pub use span::Span;
pub use timing::{time_task, TaskTimer, TaskTiming};
pub type FilePath = relative_path::RelativePathBuf;
pub type SharedString = ecow::EcoString;

/// Build-time project revision string.
pub const PROJECT_REVISION: &str = env!("HOLO_PROJECT_REVISION");

/// Returns the build-time project revision string.
pub fn project_revision() -> &'static str {
    PROJECT_REVISION
}

#[cfg(test)]
mod tests {
    use super::project_revision;

    #[test]
    fn project_revision_is_non_empty() {
        assert!(!project_revision().trim().is_empty());
    }
}
