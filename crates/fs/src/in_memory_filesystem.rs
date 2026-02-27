use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

use holo_base::{ErrorKind, FilePath, HoloError, Mutex, Result, SharedString};

use crate::FileSystem;

#[derive(Debug, Default)]
struct State {
    files: HashMap<PathBuf, SharedString>,
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

    fn not_found_error(path: &FilePath) -> HoloError {
        let io = std::io::Error::new(std::io::ErrorKind::NotFound, "path not found");
        holo_base::holo_message_error!("failed to read {}", path)
            .with_source(HoloError::new(ErrorKind::Io).with_std_source(io))
    }
}

impl FileSystem for InMemoryFileSystem {
    fn read_to_string(&self, path: &FilePath) -> Result<SharedString> {
        let state = self.lock_state();
        let path_buf = PathBuf::from(path.as_str());
        match state.files.get(&path_buf) {
            Some(contents) => Ok(contents.clone()),
            None => Err(Self::not_found_error(path)),
        }
    }

    fn write_string(&self, path: &FilePath, contents: &str) -> Result<()> {
        let mut state = self.lock_state();
        state.files.insert(PathBuf::from(path.as_str()), contents.into());
        Ok(())
    }

    fn create_dir_all(&self, path: &FilePath) -> Result<()> {
        let mut state = self.lock_state();
        let path_buf = PathBuf::from(path.as_str());
        for ancestor in path_buf.ancestors() {
            if ancestor.as_os_str().is_empty() {
                continue;
            }
            state.dirs.insert(ancestor.to_path_buf());
        }
        Ok(())
    }

    fn exists(&self, path: &FilePath) -> bool {
        let state = self.lock_state();
        let path_buf = PathBuf::from(path.as_str());
        state.files.contains_key(&path_buf) || state.dirs.contains(&path_buf)
    }
}

#[cfg(test)]
mod tests {
    use holo_base::{ErrorKind, FilePath};

    use crate::{FileSystem, InMemoryFileSystem};

    #[test]
    fn writes_and_reads_file_content() {
        let fs = InMemoryFileSystem::default();
        let file = FilePath::from("/virtual/sample.txt");

        fs.write_string(&file, "hello in-memory")
            .expect("should write content");
        let text = fs.read_to_string(&file).expect("should read content");

        assert_eq!(text, "hello in-memory");
    }

    #[test]
    fn tracks_directory_existence() {
        let fs = InMemoryFileSystem::default();
        let dir = FilePath::from("/virtual/nested/dir");

        assert!(!fs.exists(&dir));
        fs.create_dir_all(&dir).expect("should create directory");
        assert!(fs.exists(&dir));
        assert!(fs.exists(&FilePath::from("/virtual")));
    }

    #[test]
    fn returns_not_found_error_for_missing_file() {
        let fs = InMemoryFileSystem::default();
        let missing = FilePath::from("/virtual/missing.txt");

        let error = fs
            .read_to_string(&missing)
            .expect_err("missing file should fail");

        assert!(
            matches!(error.kind(), ErrorKind::Message(message) if message == "failed to read /virtual/missing.txt")
        );
        assert!(error.source().is_some());
    }
}
