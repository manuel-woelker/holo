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
                return Err(holo_message_error!("case heading missing name at line {}", line_no + 1));
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
}
