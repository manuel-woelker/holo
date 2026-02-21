use std::error::Error as StdError;
use std::fmt::{self, Display, Formatter};

/// High-level classification for [`HoloError`].
#[derive(Debug)]
pub enum ErrorKind {
    /// Free-form error message.
    Message(String),
    /// I/O failure category.
    Io,
    /// Generic wrapper around external standard errors.
    Std(Box<dyn StdError + Send + Sync + 'static>),
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Message(message) => f.write_str(message),
            Self::Io => f.write_str("I/O error"),
            Self::Std(error) => Display::fmt(error, f),
        }
    }
}

/// Error type used by shared holo infrastructure.
///
/// # How should this error type be used?
///
/// - Use [`ErrorKind::Message`] for domain-level messages created by holo code.
/// - Use [`ErrorKind::Io`] when the top-level failure category is I/O.
/// - Use [`ErrorKind::Std`] to wrap external library errors that do not have a
///   dedicated holo category.
/// - Use [`HoloError::with_source`] or [`HoloError::with_std_source`] to chain
///   context from high-level failures to underlying causes.
///
/// # What is a recommended construction pattern?
///
/// ```rust
/// use holo_base::{ErrorKind, HoloError, Result};
///
/// fn read_config() -> Result<String> {
///     std::fs::read_to_string("holo.toml")
///         .map_err(|error| HoloError::new(ErrorKind::Message("failed to read config".into())).with_std_source(error))
/// }
/// ```
///
/// # Why chain errors?
///
/// Chaining preserves both user-facing context and low-level root causes,
/// which is important for actionable diagnostics and debugging.
#[derive(Debug)]
pub struct HoloError {
    kind: ErrorKind,
    source: Option<Box<HoloError>>,
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
    pub fn with_source(mut self, source: impl Into<Box<HoloError>>) -> Self {
        self.source = Some(source.into());
        self
    }

    /// Attaches any standard error as a generic chained source.
    pub fn with_std_source(
        self,
        source: impl Into<Box<dyn StdError + Send + Sync + 'static>>,
    ) -> Self {
        self.with_source(HoloError::from_std_error(source))
    }

    /// Creates a [`HoloError`] from an external standard error.
    pub fn from_std_error(error: impl Into<Box<dyn StdError + Send + Sync + 'static>>) -> Self {
        Self::new(ErrorKind::Std(error.into()))
    }

    /// Returns the chained source error, if one exists.
    pub fn source(&self) -> Option<&HoloError> {
        self.source.as_deref()
    }
}

impl Display for HoloError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl StdError for HoloError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        self.source()
            .map(|error| error as &(dyn StdError + 'static))
    }
}

impl From<std::io::Error> for HoloError {
    fn from(value: std::io::Error) -> Self {
        Self::new(ErrorKind::Io).with_std_source(value)
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
            .with_std_source(std::io::Error::other("root cause"));
        assert!(matches!(error.kind(), ErrorKind::Message(message) if message == "top-level"));
        assert!(error.source().is_some());
    }

    #[test]
    fn supports_generic_std_error_kind() {
        let error = HoloError::from_std_error(std::io::Error::other("wrapped"));
        assert!(matches!(error.kind(), ErrorKind::Std(_)));
    }
}
