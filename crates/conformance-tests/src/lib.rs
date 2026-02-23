use holo_base::{
    display_source_diagnostics, holo_message_error, DiagnosticKind, Result, SharedString,
    SourceDiagnostic, SourceExcerpt,
};
use holo_core::CompilerCore;
use std::fs;
use std::path::{Path, PathBuf};

pub const DEFAULT_SUITES: &[&str] = &["parser", "typechecker", "interpreter", "end_to_end"];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HoloSuite {
    pub cases: Vec<HoloCase>,
}

impl HoloSuite {
    pub fn new(cases: Vec<HoloCase>) -> Self {
        Self { cases }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HoloCase {
    pub name: SharedString,
    pub blocks: Vec<HoloBlock>,
}

impl HoloCase {
    pub fn new(name: SharedString, blocks: Vec<HoloBlock>) -> Self {
        Self { name, blocks }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HoloBlock {
    pub info: SharedString,
    pub content: SharedString,
}

impl HoloBlock {
    pub fn new(info: SharedString, content: SharedString) -> Self {
        Self { info, content }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaseOutcome {
    pub kind: SharedString,
    pub text: SharedString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaseRecord {
    pub fixture_path: SharedString,
    pub case_name: SharedString,
    pub expected_kind: SharedString,
    pub expected_text: SharedString,
    pub actual_kind: SharedString,
    pub actual_text: SharedString,
    pub passed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SuiteSummary {
    pub suite_name: SharedString,
    pub total: usize,
    pub passed: usize,
    pub failed: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConformanceFailure {
    pub suite_name: SharedString,
    pub fixture_path: SharedString,
    pub case_name: SharedString,
    pub expected_kind: SharedString,
    pub actual_kind: SharedString,
    pub expected_text: SharedString,
    pub actual_text: SharedString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConformanceRunSummary {
    pub total: usize,
    pub passed: usize,
    pub failed: usize,
    pub suite_summaries: Vec<SuiteSummary>,
    pub failures: Vec<ConformanceFailure>,
    pub cases: Vec<CaseRecord>,
}

pub fn parse_holo_suite(source: &str) -> Result<HoloSuite> {
    let mut cases = Vec::new();
    let mut current_case: Option<HoloCase> = None;
    let mut pending_blocks: Vec<HoloBlock> = Vec::new();

    let mut lines = source.lines().enumerate().peekable();
    while let Some((line_no, line)) = lines.next() {
        let trimmed = line.trim_start();
        if let Some(name) = trimmed.strip_prefix("## Case:") {
            if let Some(case) = current_case.take() {
                cases.push(case);
            }
            let name = name.trim();
            if name.is_empty() {
                return Err(holo_message_error!(
                    "case heading missing name at line {}",
                    line_no + 1
                ));
            }
            current_case = Some(HoloCase::new(name.into(), Vec::new()));
            pending_blocks.clear();
            continue;
        }

        if let Some(info) = trimmed.strip_prefix("```") {
            let info = info.trim();
            let Some(case) = current_case.as_mut() else {
                return Err(holo_message_error!(
                    "code block outside of case at line {}",
                    line_no + 1
                ));
            };
            let mut content_lines = Vec::new();
            let mut closed = false;
            while let Some((_, inner)) = lines.next() {
                let inner_trimmed = inner.trim_start();
                if inner_trimmed == "```" {
                    closed = true;
                    break;
                }
                content_lines.push(inner);
            }
            if !closed {
                return Err(holo_message_error!(
                    "unterminated code block starting at line {}",
                    line_no + 1
                ));
            }
            let content = content_lines.join("\n");
            pending_blocks.push(HoloBlock::new(info.into(), content.into()));
            case.blocks.extend(pending_blocks.drain(..));
        }
    }

    if let Some(case) = current_case.take() {
        cases.push(case);
    }

    Ok(HoloSuite::new(cases))
}

pub fn load_holo_suite_from_path(path: &Path) -> Result<HoloSuite> {
    let source = fs::read_to_string(path).map_err(|error| {
        holo_message_error!("failed to read {}", path.display()).with_std_source(error)
    })?;
    parse_holo_suite(&source)
}

pub fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|path| path.parent())
        .expect("workspace root should exist")
        .to_path_buf()
}

pub fn fixture_paths_in_dir(path: &Path) -> Result<Vec<PathBuf>> {
    let mut paths = Vec::new();
    if !path.exists() {
        return Ok(paths);
    }

    for entry in fs::read_dir(path)? {
        let entry = entry?;
        let file_type = entry.file_type()?;
        if !file_type.is_file() {
            continue;
        }
        let file_path = entry.path();
        if file_path
            .extension()
            .is_some_and(|extension| extension == "md")
        {
            paths.push(file_path);
        }
    }

    paths.sort();
    Ok(paths)
}

pub fn all_fixture_paths(fixture_root: &Path, suites: &[&str]) -> Result<Vec<PathBuf>> {
    let mut fixtures = Vec::new();
    for suite_name in suites {
        fixtures.extend(fixture_paths_in_dir(&fixture_root.join(suite_name))?);
    }
    fixtures.sort();
    Ok(fixtures)
}

pub fn normalize_block_content(content: &str) -> SharedString {
    content.replace("\r\n", "\n").trim().into()
}

pub fn strip_ansi_sequences(input: &str) -> SharedString {
    let mut out = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '\u{1b}' && chars.peek() == Some(&'[') {
            chars.next();
            for next in chars.by_ref() {
                if ('@'..='~').contains(&next) {
                    break;
                }
            }
            continue;
        }
        out.push(ch);
    }
    out.into()
}

pub fn normalize_rendered_output(content: &str) -> SharedString {
    strip_ansi_sequences(content).trim().into()
}

pub fn run_conformance_case(core: &mut CompilerCore, source: &str) -> CaseOutcome {
    let file_path = "conformance-case.holo";
    let summary = core
        .process_source(&file_path.into(), source)
        .expect("conformance case should process");

    if let Some(diagnostic) = summary.diagnostics.iter().find(|diagnostic| {
        matches!(
            diagnostic.kind,
            DiagnosticKind::Lexing | DiagnosticKind::Parsing
        )
    }) {
        return CaseOutcome {
            kind: "fails-parse".into(),
            text: normalize_rendered_output(&display_source_diagnostics(std::slice::from_ref(
                diagnostic,
            ))),
        };
    }

    if let Some(diagnostic) = summary
        .diagnostics
        .iter()
        .find(|diagnostic| matches!(diagnostic.kind, DiagnosticKind::Typecheck))
    {
        return CaseOutcome {
            kind: "fails-typecheck".into(),
            text: normalize_rendered_output(&display_source_diagnostics(std::slice::from_ref(
                diagnostic,
            ))),
        };
    }

    if let Some(result) = summary
        .tests
        .results
        .iter()
        .find(|result| result.failure_reason.is_some())
    {
        let failure_reason = result
            .failure_reason
            .as_ref()
            .expect("failure reason should exist");
        let diagnostic = SourceDiagnostic::new(DiagnosticKind::Test, failure_reason.clone())
            .with_source_excerpt(SourceExcerpt::new(source, 1, 0).with_source_name(file_path))
            .with_annotated_span(
                result
                    .failure_span
                    .expect("failure span should exist for test failures"),
                "test failed here",
            );
        return CaseOutcome {
            kind: "fails-interpreter".into(),
            text: normalize_rendered_output(&display_source_diagnostics(std::slice::from_ref(
                &diagnostic,
            ))),
        };
    }

    CaseOutcome {
        kind: "text".into(),
        text: "ok".into(),
    }
}

pub fn run_conformance_fixtures(
    fixture_root: &Path,
    suites: &[&str],
) -> Result<ConformanceRunSummary> {
    let workspace_root = workspace_root();
    let mut core = CompilerCore::default();

    let mut summary = ConformanceRunSummary {
        total: 0,
        passed: 0,
        failed: 0,
        suite_summaries: Vec::new(),
        failures: Vec::new(),
        cases: Vec::new(),
    };

    for suite_name in suites {
        let mut suite_total = 0usize;
        let mut suite_passed = 0usize;
        let mut suite_failed = 0usize;
        let fixture_paths = fixture_paths_in_dir(&fixture_root.join(suite_name))?;

        for fixture_path in fixture_paths {
            let suite = load_holo_suite_from_path(&fixture_path)?;
            let fixture_display: SharedString = fixture_path
                .strip_prefix(&workspace_root)
                .unwrap_or(&fixture_path)
                .display()
                .to_string()
                .into();

            for case in &suite.cases {
                summary.total += 1;
                suite_total += 1;

                let source_block = case
                    .blocks
                    .iter()
                    .find(|block| block.info.as_str() == "holo");
                let expected_block = case.blocks.iter().find(|block| {
                    block.info.as_str() == "text" || block.info.as_str().starts_with("fails-")
                });

                let case_record = match (source_block, expected_block) {
                    (Some(source_block), Some(expected_block)) => {
                        let actual = run_conformance_case(&mut core, source_block.content.as_str());
                        let expected_kind = expected_block.info.clone();
                        let expected_text =
                            normalize_block_content(expected_block.content.as_str());
                        let passed = (expected_kind.as_str() == "text"
                            || expected_kind.as_str() == actual.kind.as_str())
                            && expected_text.as_str() == actual.text.as_str();

                        CaseRecord {
                            fixture_path: fixture_display.clone(),
                            case_name: case.name.clone(),
                            expected_kind,
                            expected_text,
                            actual_kind: actual.kind,
                            actual_text: actual.text,
                            passed,
                        }
                    }
                    (None, _) => CaseRecord {
                        fixture_path: fixture_display.clone(),
                        case_name: case.name.clone(),
                        expected_kind: "text".into(),
                        expected_text: "case should contain one `holo` block".into(),
                        actual_kind: "missing-source-block".into(),
                        actual_text: "no `holo` block found".into(),
                        passed: false,
                    },
                    (_, None) => CaseRecord {
                        fixture_path: fixture_display.clone(),
                        case_name: case.name.clone(),
                        expected_kind: "text|fails-*".into(),
                        expected_text: "case should contain one expected block".into(),
                        actual_kind: "missing-expected-block".into(),
                        actual_text: "no expected block found".into(),
                        passed: false,
                    },
                };

                if case_record.passed {
                    summary.passed += 1;
                    suite_passed += 1;
                } else {
                    summary.failed += 1;
                    suite_failed += 1;
                    summary.failures.push(ConformanceFailure {
                        suite_name: (*suite_name).into(),
                        fixture_path: case_record.fixture_path.clone(),
                        case_name: case_record.case_name.clone(),
                        expected_kind: case_record.expected_kind.clone(),
                        actual_kind: case_record.actual_kind.clone(),
                        expected_text: case_record.expected_text.clone(),
                        actual_text: case_record.actual_text.clone(),
                    });
                }

                summary.cases.push(case_record);
            }
        }

        summary.suite_summaries.push(SuiteSummary {
            suite_name: (*suite_name).into(),
            total: suite_total,
            passed: suite_passed,
            failed: suite_failed,
        });
    }

    Ok(summary)
}

pub fn format_case_report(cases: &[CaseRecord]) -> SharedString {
    let mut report = String::new();
    for case in cases {
        report.push_str("# ");
        report.push_str(case.fixture_path.as_str());
        report.push('\n');
        report.push_str("## Case: ");
        report.push_str(case.case_name.as_str());
        report.push('\n');
        report.push_str(case.actual_text.as_str());
        report.push('\n');
        report.push('\n');
    }
    report.into()
}

#[cfg(test)]
mod tests {
    use super::{
        all_fixture_paths, format_case_report, load_holo_suite_from_path, parse_holo_suite,
        run_conformance_fixtures, workspace_root, DEFAULT_SUITES,
    };
    use expect_test::expect;

    #[test]
    fn parses_cases_and_blocks() {
        let source = r#"
## Case: first

```holo
fn add(a: i64, b: i64) -> i64 { a + b; }
```

```text
ok
```

## Case: second

```fails-typecheck
error: cannot add `i64` and `f64`
```
"#;

        let suite = parse_holo_suite(source).expect("suite should parse");
        assert_eq!(suite.cases.len(), 2);
        assert_eq!(suite.cases[0].name.as_str(), "first");
        assert_eq!(suite.cases[0].blocks.len(), 2);
        assert_eq!(suite.cases[0].blocks[0].info.as_str(), "holo");
        assert!(suite.cases[0]
            .blocks
            .first()
            .expect("first block")
            .content
            .contains("fn add"));
        assert_eq!(suite.cases[1].blocks.len(), 1);
        assert_eq!(suite.cases[1].blocks[0].info.as_str(), "fails-typecheck");
    }

    #[test]
    fn loads_all_fixture_files() {
        let fixture_root = workspace_root().join("tests").join("conformance-tests");
        let fixtures = all_fixture_paths(&fixture_root, DEFAULT_SUITES).expect("fixture discovery");
        for fixture in fixtures {
            let suite = load_holo_suite_from_path(&fixture).expect("fixture should load");
            assert!(
                !suite.cases.is_empty(),
                "fixture should contain at least one case: {}",
                fixture.display()
            );
            for case in suite.cases {
                assert!(
                    case.blocks
                        .iter()
                        .any(|block| block.info.as_str() == "holo"),
                    "case should contain a `holo` block in {} :: {}",
                    fixture.display(),
                    case.name
                );
                assert!(
                    case.blocks.iter().any(|block| {
                        block.info.as_str() == "text" || block.info.as_str().starts_with("fails-")
                    }),
                    "case should contain an expected output block in {} :: {}",
                    fixture.display(),
                    case.name
                );
            }
        }
    }

    #[test]
    fn executes_all_fixture_files() {
        let fixture_root = workspace_root().join("tests").join("conformance-tests");
        let summary = run_conformance_fixtures(&fixture_root, DEFAULT_SUITES)
            .expect("conformance should run");
        assert_eq!(
            summary.failed, 0,
            "unexpected failures: {:?}",
            summary.failures
        );

        let report = format_case_report(&summary.cases);
        expect![[r#"
            # tests\conformance-tests\parser\test-parser.md
            ## Case: parses basic function
            ok

            # tests\conformance-tests\parser\test-parser.md
            ## Case: reports missing close paren
            ⚒️ Parsing: expected `)` after expression

            conformance-case.holo:1
               1 │ fn broken() -> i64 { let value: i64 = (1i64 + 2i64; value; }
                 │                                                   ─ expected `)`, found `;`

            # tests\conformance-tests\typechecker\test-typechecker.md
            ## Case: rejects mixed numeric types
            ⚒️ Typecheck: arithmetic operands must have the same type

            conformance-case.holo:1
               1 │ fn bad() -> i64 { 1i64 + 2.0f64; }
                 │                   ───┬   ────── right operand has type `f64`
                 │                      └─ left operand has type `i64`

            # tests\conformance-tests\typechecker\test-typechecker.md
            ## Case: rejects non-boolean assert
            ⚒️ Typecheck: assert expects a boolean expression

            conformance-case.holo:2
               2 │ fn bad_assert() { assert(1i64); }
                 │                   ───────────── this assertion does not evaluate to `bool`

            # tests\conformance-tests\typechecker\test-typechecker.md
            ## Case: accepts simple numeric function
            ok

            # tests\conformance-tests\interpreter\test-interpreter.md
            ## Case: evaluates arithmetic
            ok

            # tests\conformance-tests\interpreter\test-interpreter.md
            ## Case: reports division by zero
            🧪 Test: division by zero

            conformance-case.holo:1
               1 │ fn boom() -> i64 { 1i64 / 0i64; }
                 │                    ─────────── test failed here

            # tests\conformance-tests\end_to_end\test-end-to-end.md
            ## Case: simple test passes
            ok

            # tests\conformance-tests\end_to_end\test-end-to-end.md
            ## Case: compile error blocks execution
            ⚒️ Typecheck: arithmetic operands must have the same type

            conformance-case.holo:1
               1 │ fn bad() -> i64 { 1i64 + 2.0f64; }
                 │                   ───┬   ────── right operand has type `f64`
                 │                      └─ left operand has type `i64`

            # tests\conformance-tests\end_to_end\test-end-to-end.md
            ## Case: runtime failure reports error
            🧪 Test: division by zero

            conformance-case.holo:1
               1 │ fn boom() -> i64 { 1i64 / 0i64; }
                 │                    ─────────── test failed here

        "#]]
        .assert_eq(report.as_str());
    }
}
