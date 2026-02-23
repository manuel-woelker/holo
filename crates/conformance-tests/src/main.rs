use conformance_tests::load_holo_suite_from_path;
use holo_base::{DiagnosticKind, Result, SharedString, SourceDiagnostic, SourceExcerpt};
use holo_core::CompilerCore;
use std::fs;
use std::path::{Path, PathBuf};

const SUITES: &[&str] = &["parser", "typechecker", "interpreter", "end_to_end"];

fn main() {
    match run_all_conformance_tests() {
        Ok(summary) => {
            print_summary(&summary);
            if summary.failed > 0 {
                std::process::exit(1);
            }
        }
        Err(error) => {
            eprintln!("error: {error}");
            std::process::exit(1);
        }
    }
}

#[derive(Debug)]
struct RunSummary {
    total: usize,
    passed: usize,
    failed: usize,
    suite_summaries: Vec<SuiteSummary>,
    failures: Vec<FailureReport>,
}

#[derive(Debug)]
struct SuiteSummary {
    suite_name: SharedString,
    total: usize,
    passed: usize,
    failed: usize,
}

#[derive(Debug)]
struct FailureReport {
    suite_name: SharedString,
    fixture_path: SharedString,
    case_name: SharedString,
    expected_kind: SharedString,
    actual_kind: SharedString,
    expected_text: SharedString,
    actual_text: SharedString,
}

#[derive(Debug)]
struct CaseOutcome {
    kind: SharedString,
    text: SharedString,
}

fn run_all_conformance_tests() -> Result<RunSummary> {
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|path| path.parent())
        .expect("workspace root should exist");
    let fixture_root = workspace_root.join("tests").join("conformance-tests");
    let mut core = CompilerCore::default();

    let mut summary = RunSummary {
        total: 0,
        passed: 0,
        failed: 0,
        suite_summaries: Vec::new(),
        failures: Vec::new(),
    };

    for suite_name in SUITES {
        let suite_dir = fixture_root.join(suite_name);
        let fixture_paths = fixture_paths_in_dir(&suite_dir)?;
        let mut suite_total = 0usize;
        let mut suite_passed = 0usize;
        let mut suite_failed = 0usize;

        for fixture_path in fixture_paths {
            let suite = load_holo_suite_from_path(&fixture_path)?;
            let fixture_display = fixture_path
                .strip_prefix(workspace_root)
                .unwrap_or(&fixture_path)
                .display()
                .to_string();
            for case in &suite.cases {
                suite_total += 1;
                summary.total += 1;

                let Some(source_block) = case
                    .blocks
                    .iter()
                    .find(|block| block.info.as_str() == "holo")
                else {
                    suite_failed += 1;
                    summary.failed += 1;
                    summary.failures.push(FailureReport {
                        suite_name: (*suite_name).into(),
                        fixture_path: fixture_display.clone().into(),
                        case_name: case.name.clone(),
                        expected_kind: "text".into(),
                        actual_kind: "missing-source-block".into(),
                        expected_text: "case should contain one `holo` block".into(),
                        actual_text: "no `holo` block found".into(),
                    });
                    continue;
                };

                let Some(expected_block) = case.blocks.iter().find(|block| {
                    block.info.as_str() == "text" || block.info.as_str().starts_with("fails-")
                }) else {
                    suite_failed += 1;
                    summary.failed += 1;
                    summary.failures.push(FailureReport {
                        suite_name: (*suite_name).into(),
                        fixture_path: fixture_display.clone().into(),
                        case_name: case.name.clone(),
                        expected_kind: "text|fails-*".into(),
                        actual_kind: "missing-expected-block".into(),
                        expected_text: "case should contain one expected block".into(),
                        actual_text: "no expected block found".into(),
                    });
                    continue;
                };

                let actual = run_case(&mut core, case.name.as_str(), source_block.content.as_str());
                let expected_kind = expected_block.info.as_str();
                let expected_text = normalize_content(expected_block.content.as_str());
                let kind_matches = expected_kind == "text" || expected_kind == actual.kind.as_str();
                let text_matches = expected_text == actual.text.as_str();

                if kind_matches && text_matches {
                    suite_passed += 1;
                    summary.passed += 1;
                    continue;
                }

                suite_failed += 1;
                summary.failed += 1;
                summary.failures.push(FailureReport {
                    suite_name: (*suite_name).into(),
                    fixture_path: fixture_display.clone().into(),
                    case_name: case.name.clone(),
                    expected_kind: expected_kind.into(),
                    actual_kind: actual.kind.clone(),
                    expected_text: expected_text.into(),
                    actual_text: actual.text,
                });
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

fn fixture_paths_in_dir(path: &Path) -> Result<Vec<PathBuf>> {
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

fn run_case(core: &mut CompilerCore, _case_name: &str, source: &str) -> CaseOutcome {
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
            text: diagnostic.render_annotated(),
        };
    }

    if let Some(diagnostic) = summary
        .diagnostics
        .iter()
        .find(|diagnostic| matches!(diagnostic.kind, DiagnosticKind::Typecheck))
    {
        return CaseOutcome {
            kind: "fails-typecheck".into(),
            text: diagnostic.render_annotated(),
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
            text: diagnostic.render_annotated(),
        };
    }

    CaseOutcome {
        kind: "text".into(),
        text: "ok".into(),
    }
}

fn normalize_content(content: &str) -> SharedString {
    content.replace("\r\n", "\n").trim().into()
}

fn print_summary(summary: &RunSummary) {
    for suite in &summary.suite_summaries {
        println!(
            "[{}] passed {}/{} (failed {})",
            suite.suite_name, suite.passed, suite.total, suite.failed
        );
    }

    println!(
        "total: passed {}/{} (failed {})",
        summary.passed, summary.total, summary.failed
    );

    if summary.failures.is_empty() {
        return;
    }

    println!();
    println!("failures:");
    for failure in &summary.failures {
        println!(
            "- [{}] {} :: {}",
            failure.suite_name, failure.fixture_path, failure.case_name
        );
        println!(
            "  expected kind `{}` text `{}`",
            failure.expected_kind, failure.expected_text
        );
        println!(
            "  actual   kind `{}` text `{}`",
            failure.actual_kind, failure.actual_text
        );
    }
}
