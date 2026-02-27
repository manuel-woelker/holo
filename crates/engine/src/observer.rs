use holo_base::{FilePath, SharedString};

/// Events that can occur during a compilation cycle.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CycleEvent {
    /// A file was successfully parsed.
    FileParsed(FilePath),
    /// A file was skipped because its content hash hasn't changed.
    FileParseSkipped(FilePath),
    /// A file failed to be read.
    FileReadError(FilePath, SharedString),
    /// A general error occurred during the cycle.
    Error(SharedString),
}

/// A trait for observing events during a compilation cycle.
pub trait CycleObserver: Send + Sync {
    /// Records a cycle event.
    fn on_event(&self, event: CycleEvent);
}

/// An observer that prints events to stdout.
pub struct StdoutObserver;

impl CycleObserver for StdoutObserver {
    fn on_event(&self, event: CycleEvent) {
        match event {
            CycleEvent::FileParsed(path) => println!("Parsed: {}", path),
            CycleEvent::FileParseSkipped(path) => println!("Skipped: {}", path),
            CycleEvent::FileReadError(path, err) => println!("Read Error: {} ({})", path, err),
            CycleEvent::Error(err) => println!("Error: {}", err),
        }
    }
}

/// An observer that collects events in a Vec for testing.
#[derive(Default)]
pub struct TestObserver {
    events: std::sync::Mutex<Vec<CycleEvent>>,
}

impl TestObserver {
    /// Returns a copy of the collected events.
    pub fn events(&self) -> Vec<CycleEvent> {
        self.events.lock().unwrap().clone()
    }

    /// Clears the collected events.
    pub fn clear_events(&self) {
        self.events.lock().unwrap().clear();
    }
}

impl CycleObserver for TestObserver {
    fn on_event(&self, event: CycleEvent) {
        self.events.lock().unwrap().push(event);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_observer_collects_events() {
        let observer = TestObserver::default();
        let path = FilePath::from("test.holo");

        observer.on_event(CycleEvent::FileParsed(path.clone()));
        observer.on_event(CycleEvent::FileParseSkipped(path.clone()));

        let events = observer.events();
        assert_eq!(events.len(), 2);
        assert_eq!(events[0], CycleEvent::FileParsed(path.clone()));
        assert_eq!(events[1], CycleEvent::FileParseSkipped(path));
    }
}
