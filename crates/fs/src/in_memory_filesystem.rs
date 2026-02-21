use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use holo_base::{ErrorKind, HoloError, Mutex, Result};

use crate::FileSystem;

#[derive(Debug, Default)]
struct State {
    files: HashMap<PathBuf, String>,
    dirs: HashSet<PathBuf>,
}

/// In-memory filesystem implementation for tests and deterministic behavior.
#[derive(Debug, Default)]
pub struct InMemoryFileSystem {
    state: Mutex<State>,
}

impl InMemoryFileSystem {
    fn lock_state(&self) -> impl std::ops::DerefMut<Target = State> + '_ {
        self.state.lock()
    }

    fn not_found_error(path: &Path) -> HoloError {
        let io = std::io::Error::new(std::io::ErrorKind::NotFound, "path not found");
        holo_base::holo_message_error!("failed to read {}", path.display())
            .with_source(HoloError::new(ErrorKind::Io).with_std_source(io))
    }
}

impl FileSystem for InMemoryFileSystem {
    fn read_to_string(&self, path: &Path) -> Result<String> {
        let state = self.lock_state();
        match state.files.get(path) {
            Some(contents) => Ok(contents.clone()),
            None => Err(Self::not_found_error(path)),
        }
    }

    fn write_string(&self, path: &Path, contents: &str) -> Result<()> {
        let mut state = self.lock_state();
        state.files.insert(path.to_path_buf(), contents.to_owned());
        Ok(())
    }

    fn create_dir_all(&self, path: &Path) -> Result<()> {
        let mut state = self.lock_state();
        for ancestor in path.ancestors() {
            if ancestor.as_os_str().is_empty() {
                continue;
            }
            state.dirs.insert(ancestor.to_path_buf());
        }
        Ok(())
    }

    fn exists(&self, path: &Path) -> bool {
        let state = self.lock_state();
        state.files.contains_key(path) || state.dirs.contains(path)
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use holo_base::ErrorKind;

    use crate::{FileSystem, InMemoryFileSystem};

    #[test]
    fn writes_and_reads_file_content() {
        let fs = InMemoryFileSystem::default();
        let file = Path::new("/virtual/sample.txt");

        fs.write_string(file, "hello in-memory")
            .expect("should write content");
        let text = fs.read_to_string(file).expect("should read content");

        assert_eq!(text, "hello in-memory");
    }

    #[test]
    fn tracks_directory_existence() {
        let fs = InMemoryFileSystem::default();
        let dir = Path::new("/virtual/nested/dir");

        assert!(!fs.exists(dir));
        fs.create_dir_all(dir).expect("should create directory");
        assert!(fs.exists(dir));
        assert!(fs.exists(Path::new("/virtual")));
    }

    #[test]
    fn returns_not_found_error_for_missing_file() {
        let fs = InMemoryFileSystem::default();
        let missing = Path::new("/virtual/missing.txt");

        let error = fs
            .read_to_string(missing)
            .expect_err("missing file should fail");

        assert!(
            matches!(error.kind(), ErrorKind::Message(message) if message == "failed to read /virtual/missing.txt")
        );
        assert!(error.source().is_some());
    }
}
