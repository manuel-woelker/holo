use std::time::{Duration, Instant};

use crate::SharedString;

/// Duration measurement for a named task execution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TaskTiming {
    /// Human-readable task name.
    pub task_name: SharedString,
    /// Measured elapsed duration.
    pub elapsed: Duration,
}

/// Running timer for one named task.
#[derive(Debug)]
pub struct TaskTimer {
    task_name: SharedString,
    started_at: Instant,
}

impl TaskTimer {
    /// Starts a timer for a named task.
    pub fn start(task_name: impl Into<SharedString>) -> Self {
        Self {
            task_name: task_name.into(),
            started_at: Instant::now(),
        }
    }

    /// Finishes the task and returns its timing data.
    pub fn finish(self) -> TaskTiming {
        TaskTiming {
            task_name: self.task_name,
            elapsed: self.started_at.elapsed(),
        }
    }
}

/// Times a closure as one named task.
///
/// Returns the closure result and the task timing.
pub fn time_task<T>(
    task_name: impl Into<SharedString>,
    operation: impl FnOnce() -> T,
) -> (T, TaskTiming) {
    let timer = TaskTimer::start(task_name);
    let output = operation();
    (output, timer.finish())
}

#[cfg(test)]
mod tests {
    use super::{time_task, TaskTimer};
    use std::time::Duration;

    #[test]
    fn times_closure_execution() {
        let (value, timing) = time_task("compute", || 2 + 3);
        assert_eq!(value, 5);
        assert_eq!(timing.task_name, "compute");
        assert!(timing.elapsed >= Duration::ZERO);
    }

    #[test]
    fn timer_reports_elapsed_after_finish() {
        let timer = TaskTimer::start("unit");
        let timing = timer.finish();
        assert_eq!(timing.task_name, "unit");
        assert!(timing.elapsed >= Duration::ZERO);
    }
}
