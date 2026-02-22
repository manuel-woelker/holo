//! Common infrastructure shared by holo components.

pub mod error;
pub mod logging;
pub mod span;

pub use error::{ErrorKind, HoloError, Result};
pub use parking_lot::Mutex;
pub use span::Span;
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
