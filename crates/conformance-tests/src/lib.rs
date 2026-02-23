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
    use holo_base::DiagnosticKind;
    use holo_core::CompilerCore;
    use std::path::Path;

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
    fn loads_parser_fixture_file() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .and_then(|path| path.parent())
            .expect("workspace root should exist");
        let fixture = root
            .join("tests")
            .join("conformance-tests")
            .join("parser")
            .join("basic.md");
        let suite = load_holo_suite_from_path(&fixture).expect("fixture should load");
        assert_eq!(suite.cases.len(), 2);
        assert_eq!(suite.cases[0].name.as_str(), "parses basic function");
        assert_eq!(suite.cases[1].name.as_str(), "reports missing close paren");
        assert_eq!(suite.cases[0].blocks[0].info.as_str(), "holo");
        assert_eq!(suite.cases[1].blocks[1].info.as_str(), "fails-parse");
    }

    #[test]
    fn loads_typechecker_fixture_file() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .and_then(|path| path.parent())
            .expect("workspace root should exist");
        let fixture = root
            .join("tests")
            .join("conformance-tests")
            .join("typechecker")
            .join("basic.md");
        let suite = load_holo_suite_from_path(&fixture).expect("fixture should load");
        assert_eq!(suite.cases.len(), 3);
        assert_eq!(suite.cases[0].name.as_str(), "rejects mixed numeric types");
        assert_eq!(suite.cases[1].name.as_str(), "rejects non-boolean assert");
        assert_eq!(
            suite.cases[2].name.as_str(),
            "accepts simple numeric function"
        );
    }

    #[test]
    fn loads_interpreter_fixture_file() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .and_then(|path| path.parent())
            .expect("workspace root should exist");
        let fixture = root
            .join("tests")
            .join("conformance-tests")
            .join("interpreter")
            .join("basic.md");
        let suite = load_holo_suite_from_path(&fixture).expect("fixture should load");
        assert_eq!(suite.cases.len(), 2);
        assert_eq!(suite.cases[0].name.as_str(), "evaluates arithmetic");
        assert_eq!(suite.cases[1].name.as_str(), "reports division by zero");
        assert_eq!(suite.cases[1].blocks[1].info.as_str(), "fails-interpreter");
    }

    #[test]
    fn loads_end_to_end_fixture_file() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .and_then(|path| path.parent())
            .expect("workspace root should exist");
        let fixture = root
            .join("tests")
            .join("conformance-tests")
            .join("end_to_end")
            .join("basic.md");
        let suite = load_holo_suite_from_path(&fixture).expect("fixture should load");
        assert_eq!(suite.cases.len(), 3);
        assert_eq!(suite.cases[0].name.as_str(), "simple test passes");
        assert_eq!(
            suite.cases[1].name.as_str(),
            "compile error blocks execution"
        );
        assert_eq!(
            suite.cases[2].name.as_str(),
            "runtime failure reports error"
        );
        assert_eq!(suite.cases[1].blocks[1].info.as_str(), "fails-typecheck");
        assert_eq!(suite.cases[2].blocks[1].info.as_str(), "fails-interpreter");
    }

    #[test]
    fn executes_end_to_end_fixture_file() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .and_then(|path| path.parent())
            .expect("workspace root should exist");
        let fixture = root
            .join("tests")
            .join("conformance-tests")
            .join("end_to_end")
            .join("basic.md");
        let suite = load_holo_suite_from_path(&fixture).expect("fixture should load");
        let mut core = CompilerCore::default();
        let mut report = String::new();

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
            let actual =
                run_end_to_end_case(&mut core, case.name.as_str(), source_block.content.as_str());
            let expected = normalize_block_content(expected_block.content.as_str());

            assert_eq!(
                expected, actual,
                "end-to-end conformance mismatch for case `{}`",
                case.name
            );

            report.push_str("## Case: ");
            report.push_str(case.name.as_str());
            report.push('\n');
            report.push_str(&actual);
            report.push('\n');
            report.push('\n');
        }

        expect![[r#"
## Case: simple test passes
ok

## Case: compile error blocks execution
error: arithmetic operands must have the same type

## Case: runtime failure reports error
error: division by zero

"#]]
        .assert_eq(&report);
    }

    fn run_end_to_end_case(core: &mut CompilerCore, case_name: &str, source: &str) -> String {
        let file_path = format!(
            "conformance-end-to-end-{}.holo",
            case_name.replace(' ', "-")
        );
        let summary = core
            .process_source(&file_path.into(), source)
            .expect("end-to-end case should process");

        if let Some(diagnostic) = summary.diagnostics.iter().find(|diagnostic| {
            matches!(
                diagnostic.kind,
                DiagnosticKind::Lexing | DiagnosticKind::Parsing | DiagnosticKind::Typecheck
            )
        }) {
            return format!("error: {}", diagnostic.message);
        }

        if let Some(result) = summary
            .tests
            .results
            .iter()
            .find(|result| result.failure_reason.is_some())
        {
            return format!(
                "error: {}",
                result
                    .failure_reason
                    .as_ref()
                    .expect("failure reason should be available for failed tests")
            );
        }

        "ok".to_owned()
    }

    fn normalize_block_content(content: &str) -> String {
        content.replace("\r\n", "\n").trim().to_owned()
    }
}
