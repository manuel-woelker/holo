//! Daemon loop primitives for incremental compile-and-test execution.

use std::collections::HashMap;

use holo_base::Result;

use crate::{CompilerCore, CoreCycleSummary};

/// File-level diagnostic emitted by one daemon tick.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileDiagnostic {
    /// File path associated with the diagnostic.
    pub file_path: String,
    /// Diagnostic message text.
    pub message: String,
}

/// Per-file result produced during one daemon tick.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcessedFile {
    /// File path that was recomputed.
    pub file_path: String,
    /// Pipeline summary when processing succeeded.
    pub summary: CoreCycleSummary,
}

/// Aggregate status payload emitted by one daemon tick.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct DaemonStatusUpdate {
    /// Files processed during this tick.
    pub processed_files: Vec<ProcessedFile>,
    /// Files that failed during this tick.
    pub errors: Vec<FileDiagnostic>,
    /// Total tests executed across processed files.
    pub tests_run: usize,
    /// Total passed tests across processed files.
    pub tests_passed: usize,
    /// Total failed tests across processed files.
    pub tests_failed: usize,
    /// Names of failing tests.
    pub failing_tests: Vec<String>,
}

impl DaemonStatusUpdate {
    /// Renders a deterministic text report for one daemon cycle.
    pub fn to_report(&self) -> String {
        let mut lines = Vec::new();
        lines.push(format!("processed_files: {}", self.processed_files.len()));
        lines.push(format!("errors: {}", self.errors.len()));
        lines.push(format!(
            "tests: run={} passed={} failed={}",
            self.tests_run, self.tests_passed, self.tests_failed
        ));

        let mut processed_paths = self
            .processed_files
            .iter()
            .map(|file| file.file_path.clone())
            .collect::<Vec<_>>();
        processed_paths.sort();
        lines.push(format!("processed: {}", processed_paths.join(", ")));

        let mut failing = self.failing_tests.clone();
        failing.sort();
        lines.push(format!("failing_tests: {}", failing.join(", ")));

        let mut errors = self
            .errors
            .iter()
            .map(|error| format!("{}: {}", error.file_path, error.message))
            .collect::<Vec<_>>();
        errors.sort();
        for error in errors {
            lines.push(format!("error: {error}"));
        }

        lines.join("\n")
    }
}

/// Debounced daemon loop state for file-change driven recompilation.
#[derive(Debug)]
pub struct CoreDaemon {
    debounce_ms: u64,
    pending_changes: HashMap<String, PendingChange>,
}

#[derive(Debug, Clone)]
struct PendingChange {
    source: String,
    last_change_ms: u64,
}

impl CoreDaemon {
    /// Creates a daemon with debounce duration in milliseconds.
    pub fn new(debounce_ms: u64) -> Self {
        Self {
            debounce_ms,
            pending_changes: HashMap::new(),
        }
    }

    /// Enqueues startup sources for initial compile and test run.
    pub fn enqueue_startup_sources(&mut self, sources: Vec<(String, String)>, now_ms: u64) {
        for (file_path, source) in sources {
            self.record_change(file_path, source, now_ms);
        }
    }

    /// Records an updated source payload for a file.
    pub fn record_change(&mut self, file_path: String, source: String, change_ms: u64) {
        self.pending_changes.insert(
            file_path,
            PendingChange {
                source,
                last_change_ms: change_ms,
            },
        );
    }

    /// Processes files that are ready after debounce has elapsed.
    pub fn process_ready(
        &mut self,
        core: &mut CompilerCore,
        now_ms: u64,
    ) -> Result<DaemonStatusUpdate> {
        let mut ready_files = Vec::new();
        for (file_path, pending) in &self.pending_changes {
            let elapsed = now_ms.saturating_sub(pending.last_change_ms);
            if elapsed >= self.debounce_ms {
                ready_files.push(file_path.clone());
            }
        }
        ready_files.sort();

        let mut update = DaemonStatusUpdate::default();
        for file_path in ready_files {
            let pending = self
                .pending_changes
                .remove(&file_path)
                .expect("file was selected from pending set");

            match core.process_source(&file_path, &pending.source) {
                Ok(summary) => {
                    update.tests_run += summary.tests.executed;
                    update.tests_passed += summary.tests.passed;
                    update.tests_failed += summary.tests.failed;
                    for test in &summary.tests.results {
                        if test.status == holo_interpreter::TestStatus::Failed {
                            update.failing_tests.push(test.name.clone());
                        }
                    }
                    update
                        .processed_files
                        .push(ProcessedFile { file_path, summary });
                }
                Err(error) => {
                    update.errors.push(FileDiagnostic {
                        file_path,
                        message: error.to_string(),
                    });
                }
            }
        }

        Ok(update)
    }

    /// Returns the number of files still pending debounce.
    pub fn pending_file_count(&self) -> usize {
        self.pending_changes.len()
    }
}

#[cfg(test)]
mod tests {
    use super::{CoreDaemon, DaemonStatusUpdate, FileDiagnostic, ProcessedFile};
    use crate::CompilerCore;
    use crate::CoreCycleSummary;
    use holo_interpreter::{TestResult, TestRunSummary, TestStatus};
    use holo_typechecker::TypecheckSummary;

    #[test]
    fn waits_for_debounce_before_processing() {
        let mut daemon = CoreDaemon::new(50);
        let mut core = CompilerCore::default();
        daemon.record_change(
            "sample.holo".to_owned(),
            "#[test] fn pass() { assert(true); }".to_owned(),
            100,
        );

        let early = daemon
            .process_ready(&mut core, 120)
            .expect("tick should succeed");
        assert_eq!(early.processed_files.len(), 0);
        assert_eq!(daemon.pending_file_count(), 1);

        let ready = daemon
            .process_ready(&mut core, 150)
            .expect("tick should succeed");
        assert_eq!(ready.processed_files.len(), 1);
        assert_eq!(ready.tests_run, 1);
        assert_eq!(ready.tests_passed, 1);
        assert_eq!(daemon.pending_file_count(), 0);
    }

    #[test]
    fn reruns_only_changed_files() {
        let mut daemon = CoreDaemon::new(10);
        let mut core = CompilerCore::default();
        daemon.enqueue_startup_sources(
            vec![
                (
                    "a.holo".to_owned(),
                    "#[test] fn a_test() { assert(true); }".to_owned(),
                ),
                (
                    "b.holo".to_owned(),
                    "#[test] fn b_test() { assert(true); }".to_owned(),
                ),
            ],
            0,
        );

        let first = daemon
            .process_ready(&mut core, 20)
            .expect("initial tick should succeed");
        assert_eq!(first.processed_files.len(), 2);
        assert_eq!(first.tests_run, 2);

        daemon.record_change(
            "b.holo".to_owned(),
            "#[test] fn b_test() { assert(false); }".to_owned(),
            30,
        );
        let second = daemon
            .process_ready(&mut core, 40)
            .expect("follow-up tick should succeed");
        assert_eq!(second.processed_files.len(), 1);
        assert_eq!(second.processed_files[0].file_path, "b.holo");
        assert_eq!(second.tests_failed, 1);
        assert_eq!(second.failing_tests, vec!["b_test".to_owned()]);
    }

    #[test]
    fn renders_stable_cycle_report() {
        let update = DaemonStatusUpdate {
            processed_files: vec![
                ProcessedFile {
                    file_path: "b.holo".to_owned(),
                    summary: CoreCycleSummary {
                        token_count: 1,
                        typecheck: TypecheckSummary {
                            test_count: 1,
                            assertion_count: 1,
                        },
                        tests: TestRunSummary::default(),
                    },
                },
                ProcessedFile {
                    file_path: "a.holo".to_owned(),
                    summary: CoreCycleSummary {
                        token_count: 1,
                        typecheck: TypecheckSummary {
                            test_count: 1,
                            assertion_count: 1,
                        },
                        tests: TestRunSummary::default(),
                    },
                },
            ],
            errors: vec![FileDiagnostic {
                file_path: "z.holo".to_owned(),
                message: "typecheck failed".to_owned(),
            }],
            tests_run: 2,
            tests_passed: 1,
            tests_failed: 1,
            failing_tests: vec!["fail_b".to_owned(), "fail_a".to_owned()],
        };

        let report = update.to_report();
        let expected = "\
processed_files: 2
errors: 1
tests: run=2 passed=1 failed=1
processed: a.holo, b.holo
failing_tests: fail_a, fail_b
error: z.holo: typecheck failed";
        assert_eq!(report, expected);
    }

    #[test]
    fn includes_failing_test_names_in_report_after_processing() {
        let mut daemon = CoreDaemon::new(0);
        let mut core = CompilerCore::default();
        daemon.record_change(
            "suite.holo".to_owned(),
            "#[test] fn pass_case() { assert(true); } #[test] fn fail_case() { assert(false); }"
                .to_owned(),
            0,
        );

        let update = daemon
            .process_ready(&mut core, 0)
            .expect("tick should succeed");
        let report = update.to_report();

        assert!(report.contains("tests: run=2 passed=1 failed=1"));
        assert!(report.contains("failing_tests: fail_case"));
        assert_eq!(
            update
                .processed_files
                .first()
                .and_then(|file| file.summary.tests.results.get(1)),
            Some(&TestResult {
                name: "fail_case".to_owned(),
                status: TestStatus::Failed
            })
        );
    }
}
