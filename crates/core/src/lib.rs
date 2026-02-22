//! Core orchestration for the minimal compile-and-test pipeline.

use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use holo_base::Result;
use holo_interpreter::{BasicInterpreter, Interpreter, TestRunSummary};
use holo_lexer::{BasicLexer, Lexer};
use holo_parser::{BasicParser, Parser};
use holo_query::{InMemoryQueryStore, QueryKey, QueryStage, QueryStore, QueryValue};
use holo_typechecker::{BasicTypechecker, TypecheckSummary, Typechecker};

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
}

impl CompilerCore {
    /// Runs lexing, parsing, typechecking, and test execution for one source file.
    pub fn process_source(&mut self, file_path: &str, source: &str) -> Result<CoreCycleSummary> {
        let content_hash = hash_content(source);
        self.query_store.invalidate_file(file_path);

        let tokens = self.lexer.lex(source)?;
        self.query_store.put(
            QueryKey {
                file_path: file_path.to_owned(),
                stage: QueryStage::Lex,
                content_hash,
            },
            QueryValue::Message(format!("{} token(s)", tokens.len())),
        );

        let module = self.parser.parse_module(&tokens)?;
        self.query_store.put(
            QueryKey {
                file_path: file_path.to_owned(),
                stage: QueryStage::Parse,
                content_hash,
            },
            QueryValue::Complete,
        );

        let typecheck = self.typechecker.typecheck_module(&module)?;
        self.query_store.put(
            QueryKey {
                file_path: file_path.to_owned(),
                stage: QueryStage::Typecheck,
                content_hash,
            },
            QueryValue::Complete,
        );

        let tests = self.interpreter.run_tests(&module);
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

        Ok(CoreCycleSummary {
            token_count: tokens.len(),
            typecheck,
            tests,
        })
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
}
