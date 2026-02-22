//! Core orchestration for the minimal compile-and-test pipeline.

pub mod daemon;

use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use holo_ast::{Module, TestItem};
use holo_base::Result;
use holo_interpreter::{BasicInterpreter, Interpreter, TestRunSummary};
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
}

/// Coordinates compiler stages and query cache updates.
#[derive(Debug, Default)]
pub struct CompilerCore {
    lexer: BasicLexer,
    parser: BasicParser,
    typechecker: BasicTypechecker,
    interpreter: BasicInterpreter,
    query_store: InMemoryQueryStore,
    cycle_cache: HashMap<(String, u64), CoreCycleSummary>,
}

impl CompilerCore {
    fn collect_tests(module: &Module) -> Vec<TestItem> {
        module.tests.clone()
    }

    /// Runs lexing, parsing, typechecking, and test execution for one source file.
    #[instrument(skip_all, fields(file_path = %file_path, source_len = source.len()))]
    pub fn process_source(&mut self, file_path: &str, source: &str) -> Result<CoreCycleSummary> {
        info!("starting compile-and-test cycle");
        let content_hash = hash_content(source);
        if !self
            .query_store
            .invalidate_if_hash_changed(file_path, content_hash)
        {
            if let Some(summary) = self
                .cycle_cache
                .get(&(file_path.to_owned(), content_hash))
                .cloned()
            {
                info!("cache hit for unchanged source hash");
                return Ok(summary);
            }
        } else {
            self.cycle_cache.retain(|(path, _), _| path != file_path);
            info!("source changed; invalidated previous cache entries");
        }

        info!("lexing source");
        let tokens = self.lexer.lex(source)?;
        debug!(token_count = tokens.len(), "lexing completed");
        self.query_store.put(
            QueryKey {
                file_path: file_path.to_owned(),
                stage: QueryStage::Lex,
                content_hash,
            },
            QueryValue::Message(format!("{} token(s)", tokens.len())),
        );

        info!("parsing tokens");
        let module = self.parser.parse_module(&tokens)?;
        debug!(test_item_count = module.tests.len(), "parsing completed");
        self.query_store.put(
            QueryKey {
                file_path: file_path.to_owned(),
                stage: QueryStage::Parse,
                content_hash,
            },
            QueryValue::Complete,
        );

        info!("typechecking module");
        let typecheck = self.typechecker.typecheck_module(&module)?;
        debug!(
            typechecked_tests = typecheck.test_count,
            assertions = typecheck.assertion_count,
            "typechecking completed"
        );
        self.query_store.put(
            QueryKey {
                file_path: file_path.to_owned(),
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
                file_path: file_path.to_owned(),
                stage: QueryStage::CollectTests,
                content_hash,
            },
            QueryValue::Message(format!("{} collected test(s)", collected_tests.len())),
        );

        info!("running tests");
        let tests = self.interpreter.run_collected_tests(&collected_tests);
        info!(
            tests_run = tests.executed,
            tests_passed = tests.passed,
            tests_failed = tests.failed,
            "test execution completed"
        );
        self.query_store.put(
            QueryKey {
                file_path: file_path.to_owned(),
                stage: QueryStage::RunTests,
                content_hash,
            },
            QueryValue::Message(format!(
                "{} executed / {} passed / {} failed",
                tests.executed, tests.passed, tests.failed
            )),
        );

        let summary = CoreCycleSummary {
            token_count: tokens.len(),
            typecheck,
            tests,
        };
        self.cycle_cache
            .insert((file_path.to_owned(), content_hash), summary.clone());
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
            file_path: file_path.to_owned(),
            stage,
            content_hash: hash_content(source),
        };
        self.query_store.get(&key)
    }
}

fn hash_content(source: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    source.hash(&mut hasher);
    hasher.finish()
}

#[cfg(test)]
mod tests {
    use super::CompilerCore;
    use holo_query::{QueryStage, QueryValue};

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
            Some(&QueryValue::Message("1 collected test(s)".to_owned()))
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
            Some(&QueryValue::Message("2 collected test(s)".to_owned()))
        );
    }
}
