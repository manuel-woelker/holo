//! Source file representation with source text and path.

use serde::{Deserialize, Serialize};

use crate::{FilePath, SharedString, Span};

/// Represents a source file with its content and path.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceFile {
    /// The source code content.
    pub source: SharedString,
    /// The file path.
    pub path: FilePath,
}

impl SourceFile {
    /// Creates a new source file from source text and path.
    pub fn new(source: impl Into<SharedString>, path: impl Into<FilePath>) -> Self {
        Self {
            source: source.into(),
            path: path.into(),
        }
    }

    /// Returns the source text corresponding to the given span.
    ///
    /// # Panics
    ///
    /// Panics if the span is out of bounds.
    pub fn source_at(&self, span: Span) -> &str {
        &self.source[span.start..span.end]
    }
}

#[cfg(test)]
mod tests {
    use super::{SourceFile, Span};

    #[test]
    fn source_at_returns_correct_slice() {
        let source: crate::SharedString = "hello world".into();
        let file = SourceFile::new(source, "test.holo");
        let span = Span::new(0, 5);
        assert_eq!(file.source_at(span), "hello");
    }

    #[test]
    fn source_at_returns_slice_in_middle() {
        let source: crate::SharedString = "hello world".into();
        let file = SourceFile::new(source, "test.holo");
        let span = Span::new(6, 11);
        assert_eq!(file.source_at(span), "world");
    }
}
