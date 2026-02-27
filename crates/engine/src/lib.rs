//! Incremental compilation engine for holo.
//!
//! This crate implements the 5-stage incremental compilation pipeline
//! as defined in the project design documents.

pub mod engine;

pub use engine::Engine;

pub fn version() -> &'static str {
    env!("CARGO_PKG_VERSION")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gets_version() {
        assert_eq!(version(), "0.1.0");
    }
}
