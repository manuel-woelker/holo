//! Output stream abstraction for native print functions.
//!
//! This module provides an [`OutputStream`] trait that allows print functions
//! to write output to different destinations depending on the runtime context:
//! - Production: stdout or stderr
//! - Testing: an in-memory buffer for comparison

use holo_base::{Mutex, SharedString};
use std::io::Write;
use std::sync::Arc;

/// Trait for outputting text from native print functions.
///
/// Implementations determine where print output goes:
/// - [`ProductionOutputStream`]: writes to stdout or stderr
/// - [`TestOutputStream`]: writes to an in-memory buffer for testing
pub trait OutputStream: Send + Sync {
    /// Writes a string to the output stream.
    fn write(&self, value: &str);

    /// Writes a string followed by a newline to the output stream.
    fn write_line(&self, value: &str) {
        self.write(value);
        self.write("\n");
    }
}

/// Production output stream that writes to stdout or stderr.
///
/// In interactive mode, output goes to stderr for a better user experience.
/// In non-interactive mode, output goes to stdout.
pub struct ProductionOutputStream {
    use_stderr: bool,
}

impl ProductionOutputStream {
    /// Creates a new production output stream.
    ///
    /// # Arguments
    ///
    /// * `use_stderr` - If true, writes to stderr; otherwise writes to stdout
    pub fn new(use_stderr: bool) -> Self {
        Self { use_stderr }
    }

    /// Creates a new production output stream that writes to stdout.
    pub fn stdout() -> Self {
        Self::new(false)
    }

    /// Creates a new production output stream that writes to stderr.
    pub fn stderr() -> Self {
        Self::new(true)
    }
}

impl OutputStream for ProductionOutputStream {
    fn write(&self, value: &str) {
        if self.use_stderr {
            std::io::stderr().write_all(value.as_bytes()).ok();
        } else {
            std::io::stdout().write_all(value.as_bytes()).ok();
        }
    }
}

/// Test output stream that writes to an in-memory buffer.
///
/// This implementation is useful for testing print functions by capturing
/// their output for comparison.
pub struct TestOutputStream {
    buffer: Arc<Mutex<SharedString>>,
}

impl TestOutputStream {
    /// Creates a new test output stream with an empty buffer.
    pub fn new() -> Self {
        Self {
            buffer: Arc::new(Mutex::new(SharedString::new())),
        }
    }

    /// Creates a new test output stream with the given shared buffer.
    ///
    /// # Arguments
    ///
    /// * `buffer` - An Arc<Mutex<SharedString>> to write to
    pub fn with_buffer(buffer: Arc<Mutex<SharedString>>) -> Self {
        Self { buffer }
    }

    /// Returns the buffered output.
    ///
    /// This clones the internal buffer, so it's safe to call while
    /// the output stream may still be in use.
    pub fn buffer(&self) -> SharedString {
        self.buffer.lock().clone()
    }

    /// Clears the buffer.
    pub fn clear(&self) {
        self.buffer.lock().clear();
    }
}

impl Default for TestOutputStream {
    fn default() -> Self {
        Self::new()
    }
}

impl OutputStream for TestOutputStream {
    fn write(&self, value: &str) {
        let mut buffer = self.buffer.lock();
        buffer.push_str(value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_output_stream_writes_to_buffer() {
        let stream = TestOutputStream::new();
        stream.write("hello");
        stream.write(" world");
        assert_eq!(stream.buffer(), "hello world");
    }

    #[test]
    fn test_output_stream_writes_line() {
        let stream = TestOutputStream::new();
        stream.write_line("hello");
        assert_eq!(stream.buffer(), "hello\n");
    }

    #[test]
    fn test_output_stream_clears_buffer() {
        let stream = TestOutputStream::new();
        stream.write("test");
        stream.clear();
        assert_eq!(stream.buffer(), "");
    }

    #[test]
    fn test_output_stream_shared_buffer() {
        let buffer = Arc::new(Mutex::new(SharedString::new()));
        let stream1 = TestOutputStream::with_buffer(buffer.clone());
        let stream2 = TestOutputStream::with_buffer(buffer.clone());

        stream1.write("stream1");
        assert_eq!(stream2.buffer(), "stream1");

        stream2.write("stream2");
        assert_eq!(stream1.buffer(), "stream1stream2");
    }
}
