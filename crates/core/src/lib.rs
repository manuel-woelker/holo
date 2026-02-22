//! Core orchestration for the minimal compile-and-test pipeline.

pub mod daemon;

use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

use bitcode::{Decode, Encode};
use holo_ast::{Module, TestItem};
use holo_base::{
    holo_message_error, project_revision, time_task, DiagnosticKind, Result, SharedString,
    SourceDiagnostic, Span, TaskTiming,
};
use holo_db::{ArtifactKey, ArtifactKind, ArtifactRecord, Database, RocksDbDatabase, RocksDbMode};
use holo_interpreter::{BasicInterpreter, Interpreter, TestRunSummary, TestStatus};
use holo_lexer::{BasicLexer, Lexer};
use holo_parser::{BasicParser, Parser};
use holo_query::{InMemoryQueryStore, QueryKey, QueryStage, QueryStore, QueryValue};
use holo_typechecker::{BasicTypechecker, TypecheckSummary, Typechecker};
use tracing::{debug, info, instrument};

/// Per-file outcome of one compile-and-test cycle.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CoreCycleSummary {
    /// Number of produced tokens.
    pub token_count: usize,
    /// Typechecking summary.
    pub typecheck: TypecheckSummary,
    /// Test execution summary.
    pub tests: TestRunSummary,
    /// Collected source diagnostics for this file.
    pub diagnostics: Vec<SourceDiagnostic>,
    /// Timings captured for this file's pipeline and nested tasks.
    pub timings: Vec<TaskTiming>,
}

/// Coordinates compiler stages and query cache updates.
#[derive(Debug, Default)]
pub struct CompilerCore {
    lexer: BasicLexer,
    parser: BasicParser,
    typechecker: BasicTypechecker,
    interpreter: BasicInterpreter,
    query_store: InMemoryQueryStore,
    cycle_cache: HashMap<(SharedString, u64), CoreCycleSummary>,
    persisted_cache: Option<PersistedCache>,
}

impl CompilerCore {
    fn collect_tests(module: &Module) -> Vec<TestItem> {
        module.tests.clone()
    }

    /// Creates a core that persists cycle summaries in `<root_dir>/.holo/db/<revision>`.
    pub fn with_persistent_cache(root_dir: &Path) -> Result<Self> {
        let db_dir = persistent_db_dir(root_dir);
        std::fs::create_dir_all(&db_dir).map_err(|error| {
            holo_message_error!(
                "failed to create persisted cache directory {}",
                db_dir.display()
            )
            .with_std_source(error)
        })?;

        let db = RocksDbDatabase::open(RocksDbMode::Persistent(db_dir)).map_err(|error| {
            holo_message_error!("failed to open persisted cache database").with_std_source(error)
        })?;

        Ok(Self {
            persisted_cache: Some(PersistedCache { db }),
            ..Self::default()
        })
    }

    /// Runs lexing, parsing, typechecking, and test execution for one source file.
    #[instrument(skip_all, fields(file_path = %file_path, source_len = source.len()))]
    pub fn process_source(&mut self, file_path: &str, source: &str) -> Result<CoreCycleSummary> {
        info!("starting compile-and-test cycle");
        let content_hash_bytes = content_hash_bytes(source);
        let content_hash = content_hash_u64(&content_hash_bytes);
        if !self
            .query_store
            .invalidate_if_hash_changed(file_path, content_hash)
        {
            if let Some(summary) = self
                .cycle_cache
                .get(&(file_path.into(), content_hash))
                .cloned()
            {
                info!("cache hit for unchanged source hash");
                return Ok(summary);
            }
        } else {
            self.cycle_cache.retain(|(path, _), _| path != file_path);
            info!("source changed; invalidated previous cache entries");
        }

        if let Some(cache) = &self.persisted_cache {
            if let Some(summary) = cache.load_summary(file_path, content_hash_bytes)? {
                info!("cache hit from persisted database");
                self.cycle_cache
                    .insert((file_path.into(), content_hash), summary.clone());
                return Ok(summary);
            }
        }

        info!("lexing source");
        let lexed = self.lexer.lex(source);
        let tokens = lexed.tokens;
        let mut diagnostics = lexed.diagnostics;
        debug!(
            token_count = tokens.len(),
            diagnostics = diagnostics.len(),
            "lexing completed"
        );
        self.query_store.put(
            QueryKey {
                file_path: file_path.into(),
                stage: QueryStage::Lex,
                content_hash,
            },
            QueryValue::Message(format!("{} token(s)", tokens.len()).into()),
        );

        info!("parsing tokens");
        let (parsed, parse_timing) = time_task(format!("parse `{file_path}`"), || {
            self.parser.parse_module(&tokens, source)
        });
        let module = parsed.module;
        diagnostics.extend(parsed.diagnostics);
        info!(
            file_path = %file_path,
            stage = "parse",
            elapsed_ms = parse_timing.elapsed.as_secs_f64() * 1000.0,
            "stage timing"
        );
        debug!(
            test_item_count = module.tests.len(),
            diagnostics = diagnostics.len(),
            "parsing completed"
        );
        self.query_store.put(
            QueryKey {
                file_path: file_path.into(),
                stage: QueryStage::Parse,
                content_hash,
            },
            QueryValue::Complete,
        );

        info!("typechecking module");
        let (typechecked, typecheck_timing) = time_task(format!("typecheck `{file_path}`"), || {
            self.typechecker.typecheck_module(&module, source)
        });
        let typecheck = typechecked.summary;
        let typecheck_timings = typechecked.timings;
        diagnostics.extend(typechecked.diagnostics);
        info!(
            file_path = %file_path,
            stage = "typecheck",
            elapsed_ms = typecheck_timing.elapsed.as_secs_f64() * 1000.0,
            "stage timing"
        );
        debug!(
            typechecked_tests = typecheck.test_count,
            assertions = typecheck.assertion_count,
            diagnostics = diagnostics.len(),
            "typechecking completed"
        );
        self.query_store.put(
            QueryKey {
                file_path: file_path.into(),
                stage: QueryStage::Typecheck,
                content_hash,
            },
            QueryValue::Complete,
        );

        info!("collecting tests");
        let collected_tests = Self::collect_tests(&module);
        debug!(
            collected_tests = collected_tests.len(),
            "test collection completed"
        );
        self.query_store.put(
            QueryKey {
                file_path: file_path.into(),
                stage: QueryStage::CollectTests,
                content_hash,
            },
            QueryValue::Message(format!("{} collected test(s)", collected_tests.len()).into()),
        );

        info!("running tests");
        let (tests, run_tests_timing) = time_task(format!("run tests `{file_path}`"), || {
            self.interpreter.run_collected_tests(&collected_tests)
        });
        info!(
            file_path = %file_path,
            stage = "run_tests",
            elapsed_ms = run_tests_timing.elapsed.as_secs_f64() * 1000.0,
            "stage timing"
        );
        info!(
            tests_run = tests.executed,
            tests_passed = tests.passed,
            tests_failed = tests.failed,
            "test execution completed"
        );
        self.query_store.put(
            QueryKey {
                file_path: file_path.into(),
                stage: QueryStage::RunTests,
                content_hash,
            },
            QueryValue::Message(
                format!(
                    "{} executed / {} passed / {} failed",
                    tests.executed, tests.passed, tests.failed
                )
                .into(),
            ),
        );

        let test_timings = tests.timings.clone();
        let summary = CoreCycleSummary {
            token_count: tokens.len(),
            typecheck,
            tests,
            diagnostics,
            timings: {
                let mut timings = Vec::new();
                timings.push(parse_timing);
                timings.push(typecheck_timing);
                timings.push(run_tests_timing);
                timings.extend(typecheck_timings);
                timings.extend(test_timings);
                timings
            },
        };
        self.cycle_cache
            .insert((file_path.into(), content_hash), summary.clone());
        if let Some(cache) = &self.persisted_cache {
            cache.store_summary(file_path, content_hash_bytes, &summary)?;
        }
        info!("compile-and-test cycle completed");

        Ok(summary)
    }

    /// Returns the latest query value for a stage when available.
    pub fn query_value(
        &self,
        file_path: &str,
        stage: QueryStage,
        source: &str,
    ) -> Option<&QueryValue> {
        let key = QueryKey {
            file_path: file_path.into(),
            stage,
            content_hash: content_hash_u64(&content_hash_bytes(source)),
        };
        self.query_store.get(&key)
    }
}

fn persistent_db_dir(root_dir: &Path) -> PathBuf {
    root_dir
        .join(".holo")
        .join("db")
        .join(sanitize_revision_for_path(project_revision()))
}

fn sanitize_revision_for_path(revision: &str) -> String {
    let mut sanitized = String::with_capacity(revision.len());
    for ch in revision.chars() {
        if ch.is_ascii_alphanumeric() || matches!(ch, '.' | '_' | '-') {
            sanitized.push(ch);
        } else {
            sanitized.push('_');
        }
    }

    if sanitized.is_empty() {
        return "unknown-revision".to_owned();
    }

    sanitized
}

fn content_hash_bytes(source: &str) -> [u8; 32] {
    let mut bytes = [0u8; 32];
    let mut hasher = DefaultHasher::new();
    source.hash(&mut hasher);
    let hash = hasher.finish();
    bytes[..8].copy_from_slice(&hash.to_le_bytes());
    bytes
}

fn content_hash_u64(content_hash: &[u8; 32]) -> u64 {
    let mut bytes = [0u8; 8];
    bytes.copy_from_slice(&content_hash[..8]);
    u64::from_le_bytes(bytes)
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Encode, Decode)]
enum CoreArtifactKind {
    CycleSummary,
}

impl ArtifactKind for CoreArtifactKind {
    fn as_str(&self) -> &'static str {
        match self {
            Self::CycleSummary => "cycle_summary",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Encode, Decode)]
struct CoreArtifactKey(String);

impl ArtifactKey for CoreArtifactKey {}

#[derive(Clone, Debug, PartialEq, Eq, Encode, Decode)]
struct PersistedCycleSummary {
    token_count: usize,
    typecheck_test_count: usize,
    typecheck_assertion_count: usize,
    tests_executed: usize,
    tests_passed: usize,
    tests_failed: usize,
    test_results: Vec<PersistedTestResult>,
    diagnostics: Vec<PersistedDiagnostic>,
    timings: Vec<PersistedTaskTiming>,
    test_timings: Vec<PersistedTaskTiming>,
}

#[derive(Clone, Debug, PartialEq, Eq, Encode, Decode)]
struct PersistedTestResult {
    name: String,
    status: PersistedTestStatus,
    failure_span_start: Option<usize>,
    failure_span_end: Option<usize>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Encode, Decode)]
enum PersistedTestStatus {
    Passed,
    Failed,
}

#[derive(Clone, Debug, PartialEq, Eq, Encode, Decode)]
struct PersistedDiagnostic {
    kind: PersistedDiagnosticKind,
    message: String,
    annotated_spans: Vec<PersistedAnnotatedSpan>,
    source_excerpts: Vec<PersistedSourceExcerpt>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Encode, Decode)]
enum PersistedDiagnosticKind {
    Lexing,
    Parsing,
    Typecheck,
    Test,
}

#[derive(Clone, Debug, PartialEq, Eq, Encode, Decode)]
struct PersistedAnnotatedSpan {
    start: usize,
    end: usize,
    message: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Encode, Decode)]
struct PersistedSourceExcerpt {
    source: String,
    starting_line: usize,
    starting_offset: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Encode, Decode)]
struct PersistedTaskTiming {
    task_name: String,
    elapsed_nanos: u64,
}

impl PersistedCycleSummary {
    fn from_runtime(summary: &CoreCycleSummary) -> Self {
        Self {
            token_count: summary.token_count,
            typecheck_test_count: summary.typecheck.test_count,
            typecheck_assertion_count: summary.typecheck.assertion_count,
            tests_executed: summary.tests.executed,
            tests_passed: summary.tests.passed,
            tests_failed: summary.tests.failed,
            test_results: summary
                .tests
                .results
                .iter()
                .map(|result| PersistedTestResult {
                    name: result.name.to_string(),
                    status: match result.status {
                        TestStatus::Passed => PersistedTestStatus::Passed,
                        TestStatus::Failed => PersistedTestStatus::Failed,
                    },
                    failure_span_start: result.failure_span.map(|span| span.start),
                    failure_span_end: result.failure_span.map(|span| span.end),
                })
                .collect(),
            diagnostics: summary
                .diagnostics
                .iter()
                .map(PersistedDiagnostic::from_runtime)
                .collect(),
            timings: summary
                .timings
                .iter()
                .map(|timing| PersistedTaskTiming {
                    task_name: timing.task_name.to_string(),
                    elapsed_nanos: timing.elapsed.as_nanos().min(u64::MAX as u128) as u64,
                })
                .collect(),
            test_timings: summary
                .tests
                .timings
                .iter()
                .map(|timing| PersistedTaskTiming {
                    task_name: timing.task_name.to_string(),
                    elapsed_nanos: timing.elapsed.as_nanos().min(u64::MAX as u128) as u64,
                })
                .collect(),
        }
    }

    fn into_runtime(self) -> CoreCycleSummary {
        CoreCycleSummary {
            token_count: self.token_count,
            typecheck: TypecheckSummary {
                test_count: self.typecheck_test_count,
                assertion_count: self.typecheck_assertion_count,
            },
            tests: TestRunSummary {
                executed: self.tests_executed,
                passed: self.tests_passed,
                failed: self.tests_failed,
                results: self
                    .test_results
                    .into_iter()
                    .map(|result| holo_interpreter::TestResult {
                        name: result.name.into(),
                        status: match result.status {
                            PersistedTestStatus::Passed => TestStatus::Passed,
                            PersistedTestStatus::Failed => TestStatus::Failed,
                        },
                        failure_span: match (result.failure_span_start, result.failure_span_end) {
                            (Some(start), Some(end)) => Some(Span::new(start, end)),
                            _ => None,
                        },
                    })
                    .collect(),
                timings: self
                    .test_timings
                    .iter()
                    .map(|timing| TaskTiming {
                        task_name: timing.task_name.clone().into(),
                        elapsed: std::time::Duration::from_nanos(timing.elapsed_nanos),
                    })
                    .collect(),
            },
            diagnostics: self
                .diagnostics
                .into_iter()
                .map(PersistedDiagnostic::into_runtime)
                .collect(),
            timings: self
                .timings
                .into_iter()
                .map(|timing| TaskTiming {
                    task_name: timing.task_name.into(),
                    elapsed: std::time::Duration::from_nanos(timing.elapsed_nanos),
                })
                .collect(),
        }
    }
}

impl PersistedDiagnostic {
    fn from_runtime(diagnostic: &SourceDiagnostic) -> Self {
        Self {
            kind: match diagnostic.kind {
                DiagnosticKind::Lexing => PersistedDiagnosticKind::Lexing,
                DiagnosticKind::Parsing => PersistedDiagnosticKind::Parsing,
                DiagnosticKind::Typecheck => PersistedDiagnosticKind::Typecheck,
                DiagnosticKind::Test => PersistedDiagnosticKind::Test,
            },
            message: diagnostic.message.to_string(),
            annotated_spans: diagnostic
                .annotated_spans
                .iter()
                .map(|span| PersistedAnnotatedSpan {
                    start: span.span.start,
                    end: span.span.end,
                    message: span.message.to_string(),
                })
                .collect(),
            source_excerpts: diagnostic
                .source_excerpts
                .iter()
                .map(|excerpt| PersistedSourceExcerpt {
                    source: excerpt.source.to_string(),
                    starting_line: excerpt.starting_line,
                    starting_offset: excerpt.starting_offset,
                })
                .collect(),
        }
    }

    fn into_runtime(self) -> SourceDiagnostic {
        let mut diagnostic = SourceDiagnostic::new(
            match self.kind {
                PersistedDiagnosticKind::Lexing => DiagnosticKind::Lexing,
                PersistedDiagnosticKind::Parsing => DiagnosticKind::Parsing,
                PersistedDiagnosticKind::Typecheck => DiagnosticKind::Typecheck,
                PersistedDiagnosticKind::Test => DiagnosticKind::Test,
            },
            self.message,
        );

        for annotated_span in self.annotated_spans {
            diagnostic.annotated_spans.push(holo_base::AnnotatedSpan {
                span: Span::new(annotated_span.start, annotated_span.end),
                message: annotated_span.message.into(),
            });
        }
        for excerpt in self.source_excerpts {
            diagnostic
                .source_excerpts
                .push(holo_base::SourceExcerpt::new(
                    excerpt.source,
                    excerpt.starting_line,
                    excerpt.starting_offset,
                ));
        }

        diagnostic
    }
}

struct PersistedCache {
    db: RocksDbDatabase<CoreArtifactKind, CoreArtifactKey>,
}

impl std::fmt::Debug for PersistedCache {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("PersistedCache { db: <rocksdb> }")
    }
}

impl PersistedCache {
    fn store_summary(
        &self,
        file_path: &str,
        content_hash: [u8; 32],
        summary: &CoreCycleSummary,
    ) -> Result<()> {
        let key = CoreArtifactKey(file_path.into());
        let persisted = PersistedCycleSummary::from_runtime(summary);
        let record = ArtifactRecord {
            bytes: bitcode::encode(&persisted),
            produced_at: 0,
            content_hash,
            schema_version: 4,
        };
        self.db
            .put_artifact(&CoreArtifactKind::CycleSummary, &key, record)
    }

    fn load_summary(
        &self,
        file_path: &str,
        content_hash: [u8; 32],
    ) -> Result<Option<CoreCycleSummary>> {
        let key = CoreArtifactKey(file_path.into());
        let Some(record) = self
            .db
            .get_artifact(&CoreArtifactKind::CycleSummary, &key)?
        else {
            return Ok(None);
        };

        if record.content_hash != content_hash {
            return Ok(None);
        }
        if record.schema_version != 4 {
            return Ok(None);
        }

        let persisted: PersistedCycleSummary = bitcode::decode(&record.bytes).map_err(|error| {
            holo_message_error!("failed to decode persisted cycle summary: {error}")
        })?;
        Ok(Some(persisted.into_runtime()))
    }
}

#[cfg(test)]
mod tests {
    use super::{persistent_db_dir, sanitize_revision_for_path, CompilerCore};
    use holo_query::{QueryStage, QueryValue};
    use std::fs;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn stores_query_statuses_for_each_stage() {
        let mut core = CompilerCore::default();
        let source = "#[test] fn smoke() { assert(true); }";
        let summary = core
            .process_source("smoke.holo", source)
            .expect("pipeline should run");

        assert!(summary.token_count > 0);
        assert_eq!(
            core.query_value("smoke.holo", QueryStage::Parse, source),
            Some(&QueryValue::Complete)
        );
    }

    #[test]
    fn caches_results_for_unchanged_source_hash() {
        let mut core = CompilerCore::default();
        let source = "#[test] fn smoke() { assert(true); }";

        let first = core
            .process_source("smoke.holo", source)
            .expect("first pipeline run should succeed");
        let second = core
            .process_source("smoke.holo", source)
            .expect("second pipeline run should hit cache");

        assert_eq!(first, second);
        assert_eq!(
            core.query_value("smoke.holo", QueryStage::CollectTests, source),
            Some(&QueryValue::Message("1 collected test(s)".into()))
        );
    }

    #[test]
    fn collects_tests_then_executes_through_interpreter() {
        let mut core = CompilerCore::default();
        let source =
            "#[test] fn pass_case() { assert(true); } #[test] fn fail_case() { assert(false); }";
        let summary = core
            .process_source("suite.holo", source)
            .expect("pipeline should run");

        assert_eq!(summary.tests.executed, 2);
        assert_eq!(summary.tests.passed, 1);
        assert_eq!(summary.tests.failed, 1);
        assert_eq!(
            core.query_value("suite.holo", QueryStage::CollectTests, source),
            Some(&QueryValue::Message("2 collected test(s)".into()))
        );
    }

    #[test]
    fn collects_diagnostics_and_continues_pipeline() {
        let mut core = CompilerCore::default();
        let source = "#[test] fn good() { assert(true); } @ #[test] fn bad() { assert(); }";
        let summary = core
            .process_source("diagnostics.holo", source)
            .expect("pipeline should continue despite diagnostics");

        assert!(summary.tests.executed >= 1);
        assert!(!summary.diagnostics.is_empty());
    }

    #[test]
    fn reuses_persisted_summary_between_core_instances() {
        let root = temp_root_dir("reuses_persisted_summary_between_core_instances");
        let source = "#[test] fn smoke() { assert(true); }";

        let mut first = CompilerCore::with_persistent_cache(&root)
            .expect("first core with persistence should initialize");
        let first_summary = first
            .process_source("persisted.holo", source)
            .expect("first run should succeed");
        drop(first);

        let mut second = CompilerCore::with_persistent_cache(&root)
            .expect("second core with persistence should initialize");
        let second_summary = second
            .process_source("persisted.holo", source)
            .expect("second run should succeed");

        assert_eq!(first_summary, second_summary);
        assert_eq!(
            second.query_value("persisted.holo", QueryStage::Parse, source),
            None
        );
        drop(second);

        fs::remove_dir_all(root).expect("cleanup should succeed");
    }

    #[test]
    fn persists_diagnostic_source_excerpts_between_core_instances() {
        let root = temp_root_dir("persists_diagnostic_source_excerpts_between_core_instances");
        let source = "foo";

        let mut first = CompilerCore::with_persistent_cache(&root)
            .expect("first core with persistence should initialize");
        let first_summary = first
            .process_source("broken.holo", source)
            .expect("first run should succeed with diagnostics");
        assert!(!first_summary.diagnostics.is_empty());
        drop(first);

        let mut second = CompilerCore::with_persistent_cache(&root)
            .expect("second core with persistence should initialize");
        let second_summary = second
            .process_source("broken.holo", source)
            .expect("second run should succeed with diagnostics");
        let rendered = holo_base::display_source_diagnostics(&second_summary.diagnostics);

        assert!(rendered.contains("foo"));
        assert!(!rendered.contains("at bytes 0..3"));
        drop(second);

        fs::remove_dir_all(root).expect("cleanup should succeed");
    }

    #[test]
    fn persistent_cache_uses_revision_subdirectory() {
        let root = temp_root_dir("persistent_cache_uses_revision_subdirectory");
        let expected = root
            .join(".holo")
            .join("db")
            .join(sanitize_revision_for_path(holo_base::project_revision()));

        let core =
            CompilerCore::with_persistent_cache(&root).expect("core with persistence should init");

        assert!(
            expected.exists(),
            "expected cache directory {} to exist",
            expected.display()
        );

        let actual = persistent_db_dir(root.as_path());
        assert_eq!(actual, expected);

        drop(core);
        fs::remove_dir_all(root).expect("cleanup should succeed");
    }

    #[test]
    fn revision_segment_is_sanitized_for_paths() {
        let sanitized = sanitize_revision_for_path("v1.2.3/4:main");
        assert_eq!(sanitized, "v1.2.3_4_main".to_owned());
    }

    fn temp_root_dir(name: &str) -> std::path::PathBuf {
        let suffix = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock should be after epoch")
            .as_nanos();
        let path = std::env::temp_dir().join(format!("holo-core-{name}-{suffix}"));
        fs::create_dir_all(&path).expect("temp root dir should be created");
        path
    }
}
