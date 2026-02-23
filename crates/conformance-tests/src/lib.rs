use holo_base::{holo_message_error, Result, SharedString};
use std::fs;
use std::path::Path;

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

#[cfg(test)]
mod tests {
    use super::{load_holo_suite_from_path, parse_holo_suite};
    use expect_test::expect;
    use holo_base::{DiagnosticKind, SourceDiagnostic, SourceExcerpt};
    use holo_core::CompilerCore;
    use std::fs;
    use std::path::{Path, PathBuf};

    const SUITES: &[&str] = &["parser", "typechecker", "interpreter", "end_to_end"];

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
            .get(0)
            .expect("first block")
            .content
            .contains("fn add"));
        assert_eq!(suite.cases[1].blocks.len(), 1);
        assert_eq!(suite.cases[1].blocks[0].info.as_str(), "fails-typecheck");
    }

    #[test]
    fn loads_all_fixture_files() {
        for fixture in all_fixture_paths() {
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
        let root = workspace_root();
        let mut core = CompilerCore::default();
        let mut report = String::new();

        for fixture in all_fixture_paths() {
            let suite = load_holo_suite_from_path(&fixture).expect("fixture should load");
            let fixture_display = fixture
                .strip_prefix(&root)
                .unwrap_or(&fixture)
                .display()
                .to_string();

            for case in &suite.cases {
                let source_block = case
                    .blocks
                    .iter()
                    .find(|block| block.info.as_str() == "holo")
                    .expect("case should contain a `holo` block");
                let expected_block = case
                    .blocks
                    .iter()
                    .find(|block| {
                        block.info.as_str() == "text" || block.info.as_str().starts_with("fails-")
                    })
                    .expect("case should contain an expected-output block");
                let actual = run_conformance_case(&mut core, source_block.content.as_str());
                let expected = normalize_block_content(expected_block.content.as_str());

                assert_eq!(
                    expected, actual,
                    "conformance mismatch for fixture {} case `{}`",
                    fixture_display, case.name
                );

                report.push_str("# ");
                report.push_str(&fixture_display);
                report.push('\n');
                report.push_str("## Case: ");
                report.push_str(case.name.as_str());
                report.push('\n');
                report.push_str(&actual);
                report.push('\n');
                report.push('\n');
            }
        }

        expect![[r#"
# tests\conformance-tests\end_to_end\test-end-to-end.md
## Case: simple test passes
ok

# tests\conformance-tests\end_to_end\test-end-to-end.md
## Case: compile error blocks execution
typecheck error: arithmetic operands must have the same type
--> line 1, column 19
   1 | fn bad() -> i64 { 1i64 + 2.0f64; }
     |                   ^^^^ left operand has type `i64`
--> line 1, column 26
   1 | fn bad() -> i64 { 1i64 + 2.0f64; }
     |                          ^^^^^^ right operand has type `f64`

# tests\conformance-tests\end_to_end\test-end-to-end.md
## Case: runtime failure reports error
test failure: division by zero
--> line 1, column 20
   1 | fn boom() -> i64 { 1i64 / 0i64; }
     |                    ^^^^^^^^^^^ test failed here

# tests\conformance-tests\interpreter\test-interpreter.md
## Case: evaluates arithmetic
ok

# tests\conformance-tests\interpreter\test-interpreter.md
## Case: reports division by zero
test failure: division by zero
--> line 1, column 20
   1 | fn boom() -> i64 { 1i64 / 0i64; }
     |                    ^^^^^^^^^^^ test failed here

# tests\conformance-tests\parser\test-parser.md
## Case: parses basic function
ok

# tests\conformance-tests\parser\test-parser.md
## Case: reports missing close paren
parsing error: expected `)` after expression
--> line 1, column 51
   1 | fn broken() -> i64 { let value: i64 = (1i64 + 2i64; value; }
     |                                                   ^ expected `)`, found `;`

# tests\conformance-tests\typechecker\test-typechecker.md
## Case: rejects mixed numeric types
typecheck error: arithmetic operands must have the same type
--> line 1, column 19
   1 | fn bad() -> i64 { 1i64 + 2.0f64; }
     |                   ^^^^ left operand has type `i64`
--> line 1, column 26
   1 | fn bad() -> i64 { 1i64 + 2.0f64; }
     |                          ^^^^^^ right operand has type `f64`

# tests\conformance-tests\typechecker\test-typechecker.md
## Case: rejects non-boolean assert
typecheck error: assert expects a boolean expression
--> line 2, column 19
   2 | fn bad_assert() { assert(1i64); }
     |                   ^^^^^^^^^^^^^ this assertion does not evaluate to `bool`

# tests\conformance-tests\typechecker\test-typechecker.md
## Case: accepts simple numeric function
ok

"#]]
        .assert_eq(&report);
    }

    fn workspace_root() -> PathBuf {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .and_then(|path| path.parent())
            .expect("workspace root should exist")
            .to_path_buf()
    }

    fn all_fixture_paths() -> Vec<PathBuf> {
        let fixture_root = workspace_root().join("tests").join("conformance-tests");
        let mut fixtures = Vec::new();
        for suite_name in SUITES {
            let suite_dir = fixture_root.join(suite_name);
            if !suite_dir.exists() {
                continue;
            }
            for entry in fs::read_dir(&suite_dir).expect("suite directory should be readable") {
                let entry = entry.expect("fixture directory entry should be readable");
                let path = entry.path();
                if !entry
                    .file_type()
                    .expect("fixture entry type should be readable")
                    .is_file()
                {
                    continue;
                }
                if path.extension().is_some_and(|extension| extension == "md") {
                    fixtures.push(path);
                }
            }
        }
        fixtures.sort();
        fixtures
    }

    fn run_conformance_case(core: &mut CompilerCore, source: &str) -> String {
        let file_path = "conformance-case.holo";
        let summary = core
            .process_source(&file_path.into(), source)
            .expect("conformance case should process");

        if let Some(diagnostic) = summary.diagnostics.iter().find(|diagnostic| {
            matches!(
                diagnostic.kind,
                DiagnosticKind::Lexing | DiagnosticKind::Parsing | DiagnosticKind::Typecheck
            )
        }) {
            return diagnostic.render_annotated().to_string();
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
                .expect("failure reason should be available for failed tests");
            let diagnostic = SourceDiagnostic::new(DiagnosticKind::Test, failure_reason.clone())
                .with_source_excerpt(SourceExcerpt::new(source, 1, 0).with_source_name(file_path))
                .with_annotated_span(
                    result
                        .failure_span
                        .expect("failure span should be available for failed tests"),
                    "test failed here",
                );
            return diagnostic.render_annotated().to_string();
        }

        "ok".to_owned()
    }

    fn normalize_block_content(content: &str) -> String {
        content.replace("\r\n", "\n").trim().to_owned()
    }
}
