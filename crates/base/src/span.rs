use serde::{Deserialize, Serialize};

/// Byte range in a source file represented as a half-open interval `[start, end)`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
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

impl<'a, C> speedy::Readable<'a, C> for Span
where
    C: speedy::Context,
{
    fn read_from<R: speedy::Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
        let start = usize::read_from(reader)?;
        let end = usize::read_from(reader)?;
        Ok(Span { start, end })
    }
}

impl<C> speedy::Writable<C> for Span
where
    C: speedy::Context,
{
    fn write_to<W>(&self, writer: &mut W) -> Result<(), C::Error>
    where
        W: speedy::Writer<C> + ?Sized,
    {
        self.start.write_to(writer)?;
        self.end.write_to(writer)
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
