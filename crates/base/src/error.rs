use std::error::Error as StdError;
use std::fmt::{self, Display, Formatter};

/// High-level classification for [`HoloError`].
#[derive(Debug)]
pub enum ErrorKind {
    /// Free-form error message.
    Message(String),
    /// I/O failure category.
    Io,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Message(message) => f.write_str(message),
            Self::Io => f.write_str("I/O error"),
        }
    }
}

/// Error type used by shared holo infrastructure.
#[derive(Debug)]
pub struct HoloError {
    kind: ErrorKind,
    source: Option<Box<dyn StdError + Send + Sync + 'static>>,
}

impl HoloError {
    /// Creates a new [`HoloError`] from an [`ErrorKind`].
    pub fn new(kind: ErrorKind) -> Self {
        Self { kind, source: None }
    }

    /// Returns the top-level kind for this error.
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    /// Attaches a source error as the cause of this error.
    pub fn with_source(
        mut self,
        source: impl Into<Box<dyn StdError + Send + Sync + 'static>>,
    ) -> Self {
        self.source = Some(source.into());
        self
    }

    /// Returns the chained source error, if one exists.
    pub fn source(&self) -> Option<&(dyn StdError + 'static)> {
        self.source
            .as_deref()
            .map(|error| error as &(dyn StdError + 'static))
    }
}

impl Display for HoloError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl StdError for HoloError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        HoloError::source(self)
    }
}

impl From<std::io::Error> for HoloError {
    fn from(value: std::io::Error) -> Self {
        Self::new(ErrorKind::Io).with_source(value)
    }
}

impl From<String> for HoloError {
    fn from(value: String) -> Self {
        Self::new(ErrorKind::Message(value))
    }
}

impl From<&str> for HoloError {
    fn from(value: &str) -> Self {
        Self::new(ErrorKind::Message(value.to_owned()))
    }
}

/// Result alias that uses [`HoloError`] as its error type.
pub type Result<T> = std::result::Result<T, HoloError>;

#[cfg(test)]
mod tests {
    use super::{ErrorKind, HoloError};

    #[test]
    fn converts_str_to_message_error() {
        let error = HoloError::from("boom");
        assert!(matches!(error.kind(), ErrorKind::Message(message) if message == "boom"));
    }

    #[test]
    fn converts_io_to_io_kind_with_source() {
        let io_error = std::io::Error::new(std::io::ErrorKind::NotFound, "missing");
        let error = HoloError::from(io_error);
        assert!(matches!(error.kind(), ErrorKind::Io));
        assert!(error.source().is_some());
    }

    #[test]
    fn supports_manual_cause_chaining() {
        let error = HoloError::new(ErrorKind::Message("top-level".to_owned()))
            .with_source(std::io::Error::other("root cause"));
        assert!(matches!(error.kind(), ErrorKind::Message(message) if message == "top-level"));
        assert!(error.source().is_some());
    }
}
