use std::env;
use std::fs;
use std::path::{Path, PathBuf};

use holo_base::{holo_message_error, Result};
use holo_core::daemon::CoreDaemon;
use holo_core::CompilerCore;

fn run_examples(example_dir: &Path) -> Result<String> {
    let sources = collect_example_sources(example_dir)?;
    if sources.is_empty() {
        return Err(holo_message_error!(
            "no .holo files found in {}",
            example_dir.display()
        ));
    }

    let mut daemon = CoreDaemon::new(0);
    let mut core = CompilerCore::default();
    daemon.enqueue_startup_sources(sources, 0);
    let update = daemon.process_ready(&mut core, 0)?;
    Ok(update.to_report())
}

fn collect_example_sources(example_dir: &Path) -> Result<Vec<(String, String)>> {
    let mut paths = fs::read_dir(example_dir)
        .map_err(|error| {
            holo_message_error!(
                "failed to read examples directory {}",
                example_dir.display()
            )
            .with_std_source(error)
        })?
        .filter_map(|entry| entry.ok().map(|value| value.path()))
        .filter(|path| is_holo_file(path))
        .collect::<Vec<_>>();
    paths.sort();

    let mut sources = Vec::new();
    for path in paths {
        let source = fs::read_to_string(&path).map_err(|error| {
            holo_message_error!("failed to read example file {}", path.display())
                .with_std_source(error)
        })?;
        sources.push((path.to_string_lossy().into_owned(), source));
    }
    Ok(sources)
}

fn is_holo_file(path: &Path) -> bool {
    path.extension().is_some_and(|ext| ext == "holo")
}

fn resolve_example_dir(args: &[String]) -> PathBuf {
    if args.len() > 1 {
        PathBuf::from(&args[1])
    } else {
        PathBuf::from("examples")
    }
}

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let example_dir = resolve_example_dir(&args);
    match run_examples(&example_dir) {
        Ok(report) => println!("{report}"),
        Err(error) => {
            eprintln!("{error}");
            std::process::exit(1);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{collect_example_sources, is_holo_file, resolve_example_dir, run_examples};
    use std::fs;
    use std::path::Path;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn detects_holo_extension() {
        assert!(is_holo_file(Path::new("sample.holo")));
        assert!(!is_holo_file(Path::new("sample.txt")));
    }

    #[test]
    fn resolves_default_examples_path() {
        let args = vec!["holo-cli".to_owned()];
        assert_eq!(resolve_example_dir(&args), Path::new("examples"));
    }

    #[test]
    fn collects_and_sorts_holo_sources() {
        let temp = temp_examples_dir("collects_and_sorts_holo_sources");
        fs::write(temp.join("b.holo"), "#[test] fn b() { assert(true); }")
            .expect("should write b.holo");
        fs::write(temp.join("a.holo"), "#[test] fn a() { assert(true); }")
            .expect("should write a.holo");
        fs::write(temp.join("ignore.txt"), "ignored").expect("should write ignore file");

        let sources = collect_example_sources(&temp).expect("source collection should succeed");
        assert_eq!(sources.len(), 2);
        assert!(sources[0].0.ends_with("a.holo"));
        assert!(sources[1].0.ends_with("b.holo"));

        fs::remove_dir_all(temp).expect("cleanup should succeed");
    }

    #[test]
    fn runs_examples_and_returns_report() {
        let temp = temp_examples_dir("runs_examples_and_returns_report");
        fs::write(
            temp.join("sample.holo"),
            "#[test] fn pass() { assert(true); } #[test] fn fail() { assert(false); }",
        )
        .expect("should write sample.holo");

        let report = run_examples(&temp).expect("run should succeed");
        assert!(report.contains("processed_files: 1"));
        assert!(report.contains("tests: run=2 passed=1 failed=1"));
        assert!(report.contains("failing_tests: fail"));

        fs::remove_dir_all(temp).expect("cleanup should succeed");
    }

    fn temp_examples_dir(name: &str) -> std::path::PathBuf {
        let suffix = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock should be after epoch")
            .as_nanos();
        let path = std::env::temp_dir().join(format!("holo-cli-{name}-{suffix}"));
        fs::create_dir_all(&path).expect("temp examples dir should be created");
        path
    }
}
