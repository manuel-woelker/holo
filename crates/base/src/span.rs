use serde::{Deserialize, Serialize};
use speedy::{Readable, Writable};

/// Byte range in a source file represented as a half-open interval `[start, end)`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Readable, Writable)]
pub struct Span {
    /// Start byte offset (inclusive).
    pub start: usize,
    /// End byte offset (exclusive).
    pub end: usize,
}

impl Span {
    /// Creates a new span from `start` to `end`.
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[cfg(test)]
mod tests {
    use super::Span;

    #[test]
    fn creates_span_with_expected_bounds() {
        let span = Span::new(3, 9);
        assert_eq!(span.start, 3);
        assert_eq!(span.end, 9);
    }
}
