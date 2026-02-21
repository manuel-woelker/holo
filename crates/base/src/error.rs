use std::error::Error as StdError;
use std::fmt::{self, Display, Formatter};

/// Error type used by shared holo infrastructure.
#[derive(Debug)]
pub enum HoloError {
    /// Free-form error message.
    Message(String),
    /// Wrapper for I/O failures.
    Io(std::io::Error),
}

impl Display for HoloError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Message(message) => f.write_str(message),
            Self::Io(error) => write!(f, "I/O error: {error}"),
        }
    }
}

impl StdError for HoloError {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            Self::Message(_) => None,
            Self::Io(error) => Some(error),
        }
    }
}

impl From<std::io::Error> for HoloError {
    fn from(value: std::io::Error) -> Self {
        Self::Io(value)
    }
}

impl From<String> for HoloError {
    fn from(value: String) -> Self {
        Self::Message(value)
    }
}

impl From<&str> for HoloError {
    fn from(value: &str) -> Self {
        Self::Message(value.to_owned())
    }
}

/// Result alias that uses [`HoloError`] as its error type.
pub type Result<T> = std::result::Result<T, HoloError>;

#[cfg(test)]
mod tests {
    use super::HoloError;

    #[test]
    fn converts_str_to_message_error() {
        let error = HoloError::from("boom");
        assert!(matches!(error, HoloError::Message(message) if message == "boom"));
    }

    #[test]
    fn converts_io_to_io_variant() {
        let io_error = std::io::Error::new(std::io::ErrorKind::NotFound, "missing");
        let error = HoloError::from(io_error);
        assert!(matches!(error, HoloError::Io(_)));
    }
}
