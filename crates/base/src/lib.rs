//! Common infrastructure shared by holo components.

pub mod error;
pub mod logging;
pub mod span;

pub use error::{ErrorKind, HoloError, Result};
pub use parking_lot::Mutex;
pub use span::Span;
