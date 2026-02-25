//! Core orchestration for the minimal compile-and-test pipeline.

pub mod daemon;

use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use bitcode::{Decode, Encode};
use holo_ast::Module as AstModule;
use holo_base::{
    holo_message_error, project_revision, time_task, DiagnosticKind, FilePath, Result,
    SharedString, SourceDiagnostic, SourceExcerpt, Span, TaskTiming,
};
use holo_db::{ArtifactKey, ArtifactKind, ArtifactRecord, Database, RocksDbDatabase, RocksDbMode};
use holo_interpreter::{
    native_functions, BasicInterpreter, Interpreter, NativeFunctionRegistry, TestRunSummary,
    TestStatus,
};
use holo_ir::{lower_module, Module as IrModule, TestItem as IrTestItem};
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
#[derive(Debug)]
pub struct CompilerCore {
    lexer: BasicLexer,
    parser: BasicParser,
    typechecker: BasicTypechecker,
    interpreter: BasicInterpreter,
    native_functions: Arc<NativeFunctionRegistry>,
    query_store: InMemoryQueryStore,
    cycle_cache: HashMap<(FilePath, u64), CoreCycleSummary>,
    test_result_cache: HashMap<(FilePath, holo_base::SharedString, u64), CachedTestRun>,
    persisted_cache: Option<PersistedCache>,
}

impl Default for CompilerCore {
    fn default() -> Self {
        let native_functions = native_functions::create_builtin_registry();
        Self {
            lexer: BasicLexer::default(),
            parser: BasicParser::default(),
            typechecker: BasicTypechecker::new(native_functions.clone()),
            interpreter: BasicInterpreter::new(native_functions.clone()),
            native_functions,
            query_store: InMemoryQueryStore::default(),
            cycle_cache: HashMap::new(),
            test_result_cache: HashMap::new(),
            persisted_cache: None,
        }
    }
}

impl CompilerCore {
    /// Gets the captured output from native function print calls.
    ///
    /// Returns None if no output buffer is set.
    pub fn get_captured_output(&self) -> Option<holo_base::SharedString> {
        self.interpreter.get_captured_output()
    }

    /// Clears the captured output buffer.
    pub fn clear_captured_output(&self) {
        self.interpreter.clear_captured_output();
    }

    /// Creates a core configured for testing with output capture enabled.
    ///
    /// This creates a native function registry that captures print/println output
    /// to a buffer that can be retrieved via [`get_captured_output`](Self::get_captured_output).
    pub fn with_output_capture() -> Self {
        use holo_interpreter::native_functions;

        let (registry, _buffer) = native_functions::create_test_registry();

        Self {
            lexer: BasicLexer::default(),
            parser: BasicParser::default(),
            typechecker: BasicTypechecker::new(registry.clone()),
            interpreter: BasicInterpreter::new(registry.clone()),
            native_functions: registry,
            query_store: InMemoryQueryStore::default(),
            cycle_cache: HashMap::new(),
            test_result_cache: HashMap::new(),
            persisted_cache: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CachedTestRun {
    result: holo_interpreter::TestResult,
    timing: TaskTiming,
}

#[derive(Debug, Clone)]
struct ImportDirective {
    module_name: SharedString,
    span: Span,
}

#[derive(Debug, Default)]
struct ImportResolution {
    functions: Vec<holo_ast::FunctionItem>,
    diagnostics: Vec<SourceDiagnostic>,
    resolved_dependency_count: usize,
}

impl CompilerCore {
    fn collect_tests(module: &IrModule) -> Vec<IrTestItem> {
        let mut tests = module.tests.clone();
        tests.sort_by(|left, right| {
            left.name
                .as_str()
                .cmp(right.name.as_str())
                .then_with(|| left.span.start.cmp(&right.span.start))
                .then_with(|| left.span.end.cmp(&right.span.end))
        });
        tests
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
    pub fn process_source(
        &mut self,
        file_path: &FilePath,
        source: &str,
    ) -> Result<CoreCycleSummary> {
        info!("starting compile-and-test cycle");
        let source_hash_bytes = content_hash_bytes(source);
        let content_hash = content_hash_u64(&source_hash_bytes);
        if !self
            .query_store
            .invalidate_if_hash_changed(file_path, content_hash)
        {
            if let Some(summary) = self
                .cycle_cache
                .get(&(file_path.clone(), content_hash))
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
            if let Some(summary) = cache.load_summary(file_path, source_hash_bytes)? {
                info!("cache hit from persisted database");
                self.cycle_cache
                    .insert((file_path.clone(), content_hash), summary.clone());
                return Ok(summary);
            }
        }

        info!("lexing source");
        let (root_imports, filtered_source, mut import_diagnostics) =
            extract_imports(source, file_path);
        let mut import_resolution = self.resolve_imports(file_path, &root_imports)?;
        import_diagnostics.append(&mut import_resolution.diagnostics);
        let lexed = self.lexer.lex(&filtered_source);
        let tokens = lexed.tokens;
        let mut diagnostics = lexed.diagnostics;
        diagnostics.append(&mut import_diagnostics);
        debug!(
            token_count = tokens.len(),
            diagnostics = diagnostics.len(),
            "lexing completed"
        );
        self.query_store.put(
            QueryKey {
                file_path: file_path.clone(),
                stage: QueryStage::Lex,
                item_name: None,
                content_hash,
            },
            QueryValue::Message(format!("{} token(s)", tokens.len()).into()),
        );

        info!("parsing tokens");
        let (parsed, parse_timing) = time_task(format!("parse `{}`", file_path), || {
            self.parser.parse_module(&tokens, &filtered_source)
        });
        let mut module: AstModule = parsed.module;
        module.functions.splice(0..0, import_resolution.functions);
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
                item_name: None,
                content_hash,
            },
            QueryValue::Complete,
        );
        self.query_store.put(
            QueryKey {
                file_path: file_path.clone(),
                stage: QueryStage::ResolveImports,
                item_name: None,
                content_hash,
            },
            QueryValue::Message(
                format!(
                    "{} resolved import dependency file(s)",
                    import_resolution.resolved_dependency_count
                )
                .into(),
            ),
        );

        info!("typechecking module");
        let (typechecked, typecheck_timing) =
            time_task(format!("typecheck `{}`", file_path), || {
                self.typechecker.typecheck_module(&module, &filtered_source)
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
                file_path: file_path.clone(),
                stage: QueryStage::Typecheck,
                item_name: None,
                content_hash,
            },
            QueryValue::Complete,
        );

        info!("lowering typed IR");
        let (typed_module, lower_ir_timing) =
            time_task(format!("lower ir `{}`", file_path), || {
                lower_module(&module)
            });
        info!(
            file_path = %file_path,
            stage = "lower_ir",
            elapsed_ms = lower_ir_timing.elapsed.as_secs_f64() * 1000.0,
            "stage timing"
        );
        self.query_store.put(
            QueryKey {
                file_path: file_path.clone(),
                stage: QueryStage::LowerIr,
                item_name: None,
                content_hash,
            },
            QueryValue::Complete,
        );

        info!("collecting tests");
        let collected_tests = Self::collect_tests(&typed_module);
        debug!(
            collected_tests = collected_tests.len(),
            "test collection completed"
        );
        self.query_store.put(
            QueryKey {
                file_path: file_path.clone(),
                stage: QueryStage::CollectTests,
                item_name: None,
                content_hash,
            },
            QueryValue::Message(format!("{} collected test(s)", collected_tests.len()).into()),
        );

        info!("running tests");
        let non_test_functions: Vec<_> = typed_module
            .functions
            .iter()
            .filter(|function| !function.is_test)
            .collect();
        let function_fingerprint =
            content_hash_u64(&content_hash_bytes(&format!("{:?}", non_test_functions)));
        let mut valid_item_keys = HashSet::new();
        let (tests, run_tests_timing) = time_task(format!("run tests `{}`", file_path), || {
            let mut summary = TestRunSummary::default();
            for test in &collected_tests {
                let test_fingerprint = {
                    let mut hasher = DefaultHasher::new();
                    function_fingerprint.hash(&mut hasher);
                    test.hash(&mut hasher);
                    hasher.finish()
                };
                let cache_key = (file_path.clone(), test.name.clone(), test_fingerprint);
                valid_item_keys.insert((test.name.clone(), test_fingerprint));

                if let Some(cached) = self.test_result_cache.get(&cache_key).cloned() {
                    summary.executed += 1;
                    match cached.result.status {
                        TestStatus::Passed => summary.passed += 1,
                        TestStatus::Failed => summary.failed += 1,
                    }
                    summary.timings.push(cached.timing.clone());
                    summary.results.push(cached.result);
                    self.query_store.put(
                        QueryKey {
                            file_path: file_path.clone(),
                            stage: QueryStage::RunSingleTest,
                            item_name: Some(test.name.clone()),
                            content_hash,
                        },
                        QueryValue::Message("cached result reused".into()),
                    );
                    continue;
                }

                let (result, timing) = time_task(format!("run test `{}`", test.name), || {
                    self.interpreter.run_test_in_module(&typed_module, test)
                });
                summary.executed += 1;
                match result.status {
                    TestStatus::Passed => summary.passed += 1,
                    TestStatus::Failed => summary.failed += 1,
                }
                summary.timings.push(timing.clone());
                summary.results.push(result.clone());
                self.test_result_cache.insert(
                    cache_key,
                    CachedTestRun {
                        result,
                        timing: timing.clone(),
                    },
                );
                self.query_store.put(
                    QueryKey {
                        file_path: file_path.clone(),
                        stage: QueryStage::RunSingleTest,
                        item_name: Some(test.name.clone()),
                        content_hash,
                    },
                    QueryValue::Message("executed".into()),
                );
            }
            summary
        });
        self.test_result_cache
            .retain(|(path, name, fingerprint), _| {
                if path != file_path {
                    return true;
                }
                valid_item_keys.contains(&(name.clone(), *fingerprint))
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
                file_path: file_path.clone(),
                stage: QueryStage::RunTests,
                item_name: None,
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

        for diagnostic in &mut diagnostics {
            for excerpt in &mut diagnostic.source_excerpts {
                if excerpt.source_name.is_none() {
                    excerpt.set_source_name(file_path.clone());
                }
            }
        }

        let test_timings = tests.timings.clone();
        let summary = CoreCycleSummary {
            token_count: tokens.len(),
            typecheck,
            tests,
            diagnostics,
            timings: {
                let mut timings = vec![
                    parse_timing,
                    typecheck_timing,
                    lower_ir_timing,
                    run_tests_timing,
                ];
                timings.extend(typecheck_timings);
                timings.extend(test_timings);
                timings
            },
        };
        self.cycle_cache
            .insert((file_path.clone(), content_hash), summary.clone());
        if let Some(cache) = &self.persisted_cache {
            cache.store_summary(file_path, source_hash_bytes, &summary)?;
        }
        info!("compile-and-test cycle completed");

        Ok(summary)
    }

    /// Returns the latest query value for a stage when available.
    pub fn query_value(
        &self,
        file_path: &FilePath,
        stage: QueryStage,
        source: &str,
    ) -> Option<&QueryValue> {
        let key = QueryKey {
            file_path: file_path.clone(),
            stage,
            item_name: None,
            content_hash: content_hash_u64(&content_hash_bytes(source)),
        };
        self.query_store.get(&key)
    }

    /// Returns the latest per-item query value for a stage when available.
    pub fn query_item_value(
        &self,
        file_path: &FilePath,
        stage: QueryStage,
        item_name: &str,
        source: &str,
    ) -> Option<&QueryValue> {
        let key = QueryKey {
            file_path: file_path.clone(),
            stage,
            item_name: Some(item_name.into()),
            content_hash: content_hash_u64(&content_hash_bytes(source)),
        };
        self.query_store.get(&key)
    }

    fn resolve_imports(
        &self,
        root_file: &FilePath,
        root_imports: &[ImportDirective],
    ) -> Result<ImportResolution> {
        let resolver = ImportResolver::new(&self.lexer, &self.parser);
        resolver.resolve(root_file, root_imports)
    }
}

#[derive(Debug)]
struct ImportResolver<'a> {
    lexer: &'a BasicLexer,
    parser: &'a BasicParser,
    visited: HashSet<FilePath>,
    stack: Vec<FilePath>,
    functions: Vec<holo_ast::FunctionItem>,
    diagnostics: Vec<SourceDiagnostic>,
}

impl<'a> ImportResolver<'a> {
    fn new(lexer: &'a BasicLexer, parser: &'a BasicParser) -> Self {
        Self {
            lexer,
            parser,
            visited: HashSet::new(),
            stack: Vec::new(),
            functions: Vec::new(),
            diagnostics: Vec::new(),
        }
    }

    fn resolve(
        mut self,
        root_file: &FilePath,
        root_imports: &[ImportDirective],
    ) -> Result<ImportResolution> {
        self.stack.push(root_file.clone());
        for import in root_imports {
            let target = resolve_import_target(root_file, import.module_name.as_str());
            self.resolve_one(root_file, &target, import)?;
        }
        self.stack.pop();
        Ok(ImportResolution {
            functions: self.functions,
            diagnostics: self.diagnostics,
            resolved_dependency_count: self.visited.len(),
        })
    }

    fn resolve_one(
        &mut self,
        source_file: &FilePath,
        target_file: &FilePath,
        import: &ImportDirective,
    ) -> Result<()> {
        if self.stack.contains(target_file) {
            let cycle = self
                .stack
                .iter()
                .map(ToString::to_string)
                .chain(std::iter::once(target_file.to_string()))
                .collect::<Vec<_>>()
                .join(" -> ");
            self.diagnostics.push(
                SourceDiagnostic::new(DiagnosticKind::Typecheck, "import cycle detected")
                    .with_error_code("M3002")
                    .with_hint("break the cycle by removing or restructuring one import")
                    .with_annotated_span(import.span, format!("cycle: {cycle}"))
                    .with_source_excerpt(
                        SourceExcerpt::new(
                            std::fs::read_to_string(source_file.as_str()).unwrap_or_default(),
                            1,
                            0,
                        )
                        .with_source_name(source_file.clone()),
                    ),
            );
            return Ok(());
        }

        if self.visited.contains(target_file) {
            return Ok(());
        }
        self.visited.insert(target_file.clone());
        self.stack.push(target_file.clone());

        let imported_source = match std::fs::read_to_string(target_file.as_str()) {
            Ok(source) => source,
            Err(_) => {
                self.diagnostics.push(
                    SourceDiagnostic::new(
                        DiagnosticKind::Typecheck,
                        format!("import not found `{}`", import.module_name),
                    )
                    .with_error_code("M3001")
                    .with_hint("create the target module file or fix the import name")
                    .with_annotated_span(import.span, "this import target cannot be resolved")
                    .with_source_excerpt(
                        SourceExcerpt::new(
                            std::fs::read_to_string(source_file.as_str()).unwrap_or_default(),
                            1,
                            0,
                        )
                        .with_source_name(source_file.clone()),
                    ),
                );
                self.stack.pop();
                return Ok(());
            }
        };

        let (imports, filtered_source, mut extraction_diagnostics) =
            extract_imports(&imported_source, target_file);
        for diagnostic in &mut extraction_diagnostics {
            for excerpt in &mut diagnostic.source_excerpts {
                if excerpt.source_name.is_none() {
                    excerpt.set_source_name(target_file.clone());
                }
            }
        }
        self.diagnostics.extend(extraction_diagnostics);

        let lexed = self.lexer.lex(&filtered_source);
        let mut lex_diagnostics = lexed.diagnostics;
        for diagnostic in &mut lex_diagnostics {
            for excerpt in &mut diagnostic.source_excerpts {
                if excerpt.source_name.is_none() {
                    excerpt.set_source_name(target_file.clone());
                }
            }
        }
        self.diagnostics.extend(lex_diagnostics);

        let parsed = self.parser.parse_module(&lexed.tokens, &filtered_source);
        let mut parse_diagnostics = parsed.diagnostics;
        for diagnostic in &mut parse_diagnostics {
            for excerpt in &mut diagnostic.source_excerpts {
                if excerpt.source_name.is_none() {
                    excerpt.set_source_name(target_file.clone());
                }
            }
        }
        self.diagnostics.extend(parse_diagnostics);
        self.functions.extend(parsed.module.functions);

        for nested in imports {
            let nested_target = resolve_import_target(target_file, nested.module_name.as_str());
            self.resolve_one(target_file, &nested_target, &nested)?;
        }

        self.stack.pop();
        Ok(())
    }
}

fn resolve_import_target(source_file: &FilePath, module_name: &str) -> FilePath {
    let parent = Path::new(source_file.as_str())
        .parent()
        .unwrap_or_else(|| Path::new("."));
    let relative = format!("{}.holo", module_name.replace('.', "/"));
    normalize_path(parent.join(relative))
}

fn normalize_path(path: PathBuf) -> FilePath {
    path.to_string_lossy().replace('\\', "/").into()
}

fn extract_imports(
    source: &str,
    file_path: &FilePath,
) -> (Vec<ImportDirective>, String, Vec<SourceDiagnostic>) {
    let mut imports = Vec::new();
    let mut filtered = String::with_capacity(source.len());
    let mut diagnostics = Vec::new();
    let mut offset = 0usize;

    for line in source.split_inclusive('\n') {
        let line_without_newline = line.strip_suffix('\n').unwrap_or(line);
        let trimmed = line_without_newline.trim_start();
        if let Some(rest) = trimmed.strip_prefix("import ") {
            let module = rest.trim_end_matches(';').trim();
            let is_valid = rest.trim_end().ends_with(';')
                && !module.is_empty()
                && module
                    .chars()
                    .all(|ch| ch.is_ascii_alphanumeric() || ch == '_' || ch == '.');
            if is_valid {
                let start = offset + line_without_newline.find(trimmed).unwrap_or(0);
                let module_start = start + "import ".len();
                imports.push(ImportDirective {
                    module_name: module.into(),
                    span: Span::new(module_start, module_start + module.len()),
                });
            } else {
                diagnostics.push(
                    SourceDiagnostic::new(DiagnosticKind::Parsing, "invalid import declaration")
                        .with_error_code("M3000")
                        .with_hint("use `import module_name;` syntax")
                        .with_annotated_span(
                            Span::new(offset, offset + line_without_newline.len().max(1)),
                            "this import declaration is malformed",
                        )
                        .with_source_excerpt(
                            SourceExcerpt::new(source, 1, 0).with_source_name(file_path.clone()),
                        ),
                );
            }
            for ch in line.chars() {
                if ch == '\n' {
                    filtered.push('\n');
                } else {
                    filtered.push(' ');
                }
            }
        } else {
            filtered.push_str(line);
        }
        offset += line.len();
    }

    (imports, filtered, diagnostics)
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
    failure_reason: Option<String>,
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
    source_name: Option<String>,
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
                    failure_reason: result.failure_reason.as_ref().map(ToString::to_string),
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
                        failure_reason: result.failure_reason.map(Into::into),
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
                    source_name: excerpt.source_name.as_ref().map(ToString::to_string),
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
            let mut source_excerpt = holo_base::SourceExcerpt::new(
                excerpt.source,
                excerpt.starting_line,
                excerpt.starting_offset,
            );
            if let Some(source_name) = excerpt.source_name {
                source_excerpt.set_source_name(source_name);
            }
            diagnostic.source_excerpts.push(source_excerpt);
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
        file_path: &FilePath,
        content_hash: [u8; 32],
        summary: &CoreCycleSummary,
    ) -> Result<()> {
        let key = CoreArtifactKey(file_path.to_string());
        let persisted = PersistedCycleSummary::from_runtime(summary);
        let record = ArtifactRecord {
            bytes: bitcode::encode(&persisted),
            produced_at: 0,
            content_hash,
            schema_version: 6,
        };
        self.db
            .put_artifact(&CoreArtifactKind::CycleSummary, &key, record)
    }

    fn load_summary(
        &self,
        file_path: &FilePath,
        content_hash: [u8; 32],
    ) -> Result<Option<CoreCycleSummary>> {
        let key = CoreArtifactKey(file_path.to_string());
        let Some(record) = self
            .db
            .get_artifact(&CoreArtifactKind::CycleSummary, &key)?
        else {
            return Ok(None);
        };

        if record.content_hash != content_hash {
            return Ok(None);
        }
        if record.schema_version != 6 {
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
            .process_source(&"smoke.holo".into(), source)
            .expect("pipeline should run");

        assert!(summary.token_count > 0);
        assert_eq!(
            core.query_value(&"smoke.holo".into(), QueryStage::Parse, source),
            Some(&QueryValue::Complete)
        );
        assert_eq!(
            core.query_value(&"smoke.holo".into(), QueryStage::LowerIr, source),
            Some(&QueryValue::Complete)
        );
    }

    #[test]
    fn caches_results_for_unchanged_source_hash() {
        let mut core = CompilerCore::default();
        let source = "#[test] fn smoke() { assert(true); }";

        let first = core
            .process_source(&"smoke.holo".into(), source)
            .expect("first pipeline run should succeed");
        let second = core
            .process_source(&"smoke.holo".into(), source)
            .expect("second pipeline run should hit cache");

        assert_eq!(first, second);
        assert_eq!(
            core.query_value(&"smoke.holo".into(), QueryStage::CollectTests, source),
            Some(&QueryValue::Message("1 collected test(s)".into()))
        );
    }

    #[test]
    fn collects_tests_then_executes_through_interpreter() {
        let mut core = CompilerCore::default();
        let source =
            "#[test] fn pass_case() { assert(true); } #[test] fn fail_case() { assert(false); }";
        let summary = core
            .process_source(&"suite.holo".into(), source)
            .expect("pipeline should run");

        assert_eq!(summary.tests.executed, 2);
        assert_eq!(summary.tests.passed, 1);
        assert_eq!(summary.tests.failed, 1);
        assert_eq!(
            core.query_value(&"suite.holo".into(), QueryStage::CollectTests, source),
            Some(&QueryValue::Message("2 collected test(s)".into()))
        );
    }

    #[test]
    fn collects_diagnostics_and_continues_pipeline() {
        let mut core = CompilerCore::default();
        let source = "#[test] fn good() { assert(true); } @ #[test] fn bad() { assert(); }";
        let summary = core
            .process_source(&"diagnostics.holo".into(), source)
            .expect("pipeline should continue despite diagnostics");

        assert!(summary.tests.executed >= 1);
        assert!(!summary.diagnostics.is_empty());
    }

    #[test]
    fn continues_running_tests_when_non_test_function_has_diagnostics() {
        let mut core = CompilerCore::default();
        let source = "fn helper() -> i64 { missing_value; } #[test] fn smoke() { assert(true); }";
        let summary = core
            .process_source(&"non_test_diagnostic.holo".into(), source)
            .expect("pipeline should continue despite diagnostics in helper function");

        assert_eq!(summary.tests.executed, 1);
        assert_eq!(summary.tests.passed, 1);
        assert!(!summary.diagnostics.is_empty());
    }

    #[test]
    fn invalidates_cache_when_helper_function_changes() {
        let mut core = CompilerCore::default();
        let passing_source =
            "fn helper() -> bool { true; } #[test] fn smoke() { assert(helper()); }";
        let failing_source =
            "fn helper() -> bool { false; } #[test] fn smoke() { assert(helper()); }";

        let first = core
            .process_source(&"helper_change.holo".into(), passing_source)
            .expect("first pipeline run should succeed");
        let second = core
            .process_source(&"helper_change.holo".into(), failing_source)
            .expect("second pipeline run should succeed");

        assert_eq!(first.tests.failed, 0);
        assert_eq!(second.tests.failed, 1);
    }

    #[test]
    fn reuses_unchanged_test_results_when_only_one_test_changes() {
        let mut core = CompilerCore::default();
        let first_source = "\
#[test] fn stable_case() { assert(true); }
#[test] fn changed_case() { assert(true); }";
        let second_source = "\
#[test] fn stable_case() { assert(true); }
#[test] fn changed_case() { assert(false); }";

        let first = core
            .process_source(&"test_reuse.holo".into(), first_source)
            .expect("first run should succeed");
        assert_eq!(first.tests.passed, 2);

        let second = core
            .process_source(&"test_reuse.holo".into(), second_source)
            .expect("second run should succeed");
        assert_eq!(second.tests.executed, 2);
        assert_eq!(second.tests.passed, 1);
        assert_eq!(second.tests.failed, 1);
        assert_eq!(
            core.query_item_value(
                &"test_reuse.holo".into(),
                QueryStage::RunSingleTest,
                "stable_case",
                second_source,
            ),
            Some(&QueryValue::Message("cached result reused".into()))
        );
        assert_eq!(
            core.query_item_value(
                &"test_reuse.holo".into(),
                QueryStage::RunSingleTest,
                "changed_case",
                second_source,
            ),
            Some(&QueryValue::Message("executed".into()))
        );
    }

    #[test]
    fn reuses_persisted_summary_between_core_instances() {
        let root = temp_root_dir("reuses_persisted_summary_between_core_instances");
        let source = "#[test] fn smoke() { assert(true); }";

        let mut first = CompilerCore::with_persistent_cache(&root)
            .expect("first core with persistence should initialize");
        let first_summary = first
            .process_source(&"persisted.holo".into(), source)
            .expect("first run should succeed");
        drop(first);

        let mut second = CompilerCore::with_persistent_cache(&root)
            .expect("second core with persistence should initialize");
        let second_summary = second
            .process_source(&"persisted.holo".into(), source)
            .expect("second run should succeed");

        assert_eq!(first_summary, second_summary);
        assert_eq!(
            second.query_value(&"persisted.holo".into(), QueryStage::Parse, source),
            None
        );
        drop(second);

        fs::remove_dir_all(root).expect("cleanup should succeed");
    }

    #[test]
    fn persists_diagnostic_source_excerpts_between_core_instances() {
        let root = temp_root_dir("persists_diagnostic_source_excerpts_between_core_instances");
        let source = "fn broken(a i64) -> i64 { a + ; }";

        let mut first = CompilerCore::with_persistent_cache(&root)
            .expect("first core with persistence should initialize");
        let first_summary = first
            .process_source(&"broken.holo".into(), source)
            .expect("first run should succeed with diagnostics");
        assert!(!first_summary.diagnostics.is_empty());
        drop(first);

        let mut second = CompilerCore::with_persistent_cache(&root)
            .expect("second core with persistence should initialize");
        let second_summary = second
            .process_source(&"broken.holo".into(), source)
            .expect("second run should succeed with diagnostics");
        let rendered = holo_base::display_source_diagnostics(&second_summary.diagnostics);

        assert!(rendered.contains("fn broken"));
        assert!(rendered.contains("broken.holo:1"));
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

    #[test]
    fn resolves_imported_module_functions() {
        let root = temp_root_dir("resolves_imported_module_functions");
        let helper_path = root.join("helper.holo");
        fs::write(&helper_path, "fn helper() -> bool { true; }").expect("write helper module");

        let main_source = "import helper;\n#[test] fn smoke() { assert(helper()); }";
        let main_path = normalize_file_path(root.join("main.holo"));
        let mut core = CompilerCore::default();
        let summary = core
            .process_source(&main_path, main_source)
            .expect("pipeline should run with imports");

        assert_eq!(summary.tests.executed, 1);
        assert_eq!(summary.tests.failed, 0);
        assert_eq!(
            core.query_value(&main_path, QueryStage::ResolveImports, main_source),
            Some(&QueryValue::Message(
                "1 resolved import dependency file(s)".into()
            ))
        );

        fs::remove_dir_all(root).expect("cleanup should succeed");
    }

    #[test]
    fn reports_missing_import_with_stable_error_code() {
        let root = temp_root_dir("reports_missing_import_with_stable_error_code");
        let main_source = "import missing;\n#[test] fn smoke() { assert(true); }";
        let main_path = normalize_file_path(root.join("main.holo"));
        let mut core = CompilerCore::default();
        let summary = core
            .process_source(&main_path, main_source)
            .expect("pipeline should run with import diagnostic");

        let diagnostic = summary
            .diagnostics
            .iter()
            .find(|diagnostic| diagnostic.error_code.as_deref() == Some("M3001"))
            .expect("expected missing import diagnostic");
        assert!(diagnostic.message.contains("import not found"));

        fs::remove_dir_all(root).expect("cleanup should succeed");
    }

    #[test]
    fn reports_import_cycle_with_stable_error_code() {
        let root = temp_root_dir("reports_import_cycle_with_stable_error_code");
        fs::write(root.join("a.holo"), "import b;\nfn a() -> bool { true; }")
            .expect("write a module");
        fs::write(root.join("b.holo"), "import a;\nfn b() -> bool { true; }")
            .expect("write b module");

        let main_source = "import a;\n#[test] fn smoke() { assert(true); }";
        let main_path = normalize_file_path(root.join("main.holo"));
        let mut core = CompilerCore::default();
        let summary = core
            .process_source(&main_path, main_source)
            .expect("pipeline should run with cycle diagnostic");

        let diagnostic = summary
            .diagnostics
            .iter()
            .find(|diagnostic| diagnostic.error_code.as_deref() == Some("M3002"))
            .expect("expected import cycle diagnostic");
        assert!(diagnostic.message.contains("import cycle"));

        fs::remove_dir_all(root).expect("cleanup should succeed");
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

    fn normalize_file_path(path: std::path::PathBuf) -> holo_base::FilePath {
        path.to_string_lossy().replace('\\', "/").into()
    }
}
