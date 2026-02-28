//! Shared string wrapper type for efficient string handling.

use serde::{Deserialize, Serialize};

/// A wrapper around ecow::EcoString for efficient shared string storage.
///
/// This type provides copy-on-write semantics with cheap cloning,
/// making it ideal for storing strings that are shared across multiple
/// parts of the compiler without unnecessary allocations.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct SharedString(pub ecow::EcoString);

impl SharedString {
    /// Creates a new SharedString from the given string.
    pub fn new(s: impl Into<String>) -> Self {
        Self(ecow::EcoString::from(s.into()))
    }

    /// Creates a new empty SharedString.
    pub fn empty() -> Self {
        Self(ecow::EcoString::new())
    }

    /// Clears the contents of the string.
    pub fn clear(&mut self) {
        self.0.clear();
    }

    /// Appends a string slice to this string.
    pub fn push_str(&mut self, string: &str) {
        self.0.push_str(string);
    }

    /// Returns the underlying string as a string slice.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Returns the length of the string.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns true if the string is empty.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl Default for SharedString {
    fn default() -> Self {
        Self::empty()
    }
}

impl From<String> for SharedString {
    fn from(s: String) -> Self {
        Self(ecow::EcoString::from(s))
    }
}

impl From<&str> for SharedString {
    fn from(s: &str) -> Self {
        Self(ecow::EcoString::from(s))
    }
}

impl From<Box<str>> for SharedString {
    fn from(s: Box<str>) -> Self {
        Self(ecow::EcoString::from(&*s))
    }
}

impl From<&SharedString> for SharedString {
    fn from(s: &SharedString) -> Self {
        s.clone()
    }
}

impl AsRef<str> for SharedString {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl std::borrow::Borrow<str> for SharedString {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for SharedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::ops::Deref for SharedString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl PartialEq<str> for SharedString {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl PartialEq<&str> for SharedString {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<SharedString> for str {
    fn eq(&self, other: &SharedString) -> bool {
        self == other.as_str()
    }
}

impl PartialEq<SharedString> for &str {
    fn eq(&self, other: &SharedString) -> bool {
        *self == other.as_str()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shared_string_creation() {
        let s1 = SharedString::new("hello");
        let s2 = SharedString::from("world");
        let s3: SharedString = "test".into();

        assert_eq!(s1.as_str(), "hello");
        assert_eq!(s2.as_str(), "world");
        assert_eq!(s3.as_str(), "test");
    }

    #[test]
    fn test_shared_string_equality() {
        let s1 = SharedString::new("hello");
        let s2 = SharedString::new("hello");
        let s3 = SharedString::new("world");

        assert_eq!(s1, s2);
        assert_ne!(s1, s3);
    }

    #[test]
    fn test_shared_string_clone() {
        let s1 = SharedString::new("hello");
        let s2 = s1.clone();

        assert_eq!(s1, s2);
        assert_eq!(s1.as_str(), s2.as_str());
    }

    #[test]
    fn test_shared_string_default() {
        let s = SharedString::default();
        assert!(s.is_empty());
        assert_eq!(s.len(), 0);
    }

    #[test]
    fn test_shared_string_deref() {
        let s = SharedString::new("hello");
        assert_eq!(s.len(), 5);
        assert_eq!(&s[0..2], "he");
    }

    #[test]
    fn test_shared_string_display() {
        let s = SharedString::new("hello");
        assert_eq!(format!("{}", s), "hello");
    }
}
