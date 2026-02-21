use std::sync::Once;

use tracing_subscriber::EnvFilter;

use crate::error::Result;
use crate::{ErrorKind, HoloError};

static LOGGING_INIT: Once = Once::new();

/// Initializes global logging for holo processes.
///
/// Repeated calls are treated as no-ops once initialization succeeds.
pub fn init_logging() -> Result<()> {
    let mut init_result = Ok(());

    LOGGING_INIT.call_once(|| {
        init_result = tracing_subscriber::fmt()
            .with_env_filter(default_env_filter())
            .try_init()
            .map_err(|error| {
                HoloError::new(ErrorKind::Message(
                    "failed to initialize logging".to_owned(),
                ))
                .with_source(error)
            });
    });

    init_result
}

fn default_env_filter() -> EnvFilter {
    match EnvFilter::try_from_default_env() {
        Ok(filter) => filter,
        Err(_) => EnvFilter::new("info"),
    }
}

#[cfg(test)]
mod tests {
    use super::init_logging;

    #[test]
    fn init_logging_is_idempotent() {
        assert!(init_logging().is_ok());
        assert!(init_logging().is_ok());
    }
}
