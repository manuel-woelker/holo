//! Common infrastructure shared by holo components.

pub mod error;
pub mod logging;

pub use error::{ErrorKind, HoloError, Result};
pub use parking_lot::Mutex;
