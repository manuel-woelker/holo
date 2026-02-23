use conformance_tests::{
    run_conformance_fixtures, workspace_root, ConformanceRunSummary, DEFAULT_SUITES,
};

fn main() {
    let fixture_root = workspace_root().join("tests").join("conformance-tests");
    match run_conformance_fixtures(&fixture_root, DEFAULT_SUITES) {
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

fn print_summary(summary: &ConformanceRunSummary) {
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
