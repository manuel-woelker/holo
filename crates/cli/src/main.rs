use std::env;
use std::fs;
use std::path::{Path, PathBuf};

use holo_base::{holo_message_error, Result};
use holo_core::daemon::CoreDaemon;
use holo_core::CompilerCore;
use tracing::{debug, info, instrument};

#[instrument(skip_all, fields(root_dir = %root_dir.display()))]
fn run_directory(root_dir: &Path) -> Result<String> {
    info!("starting source discovery");
    let sources = collect_holo_sources(root_dir)?;
    info!(source_count = sources.len(), "completed source discovery");
    if sources.is_empty() {
        return Err(holo_message_error!(
            "no .holo files found in {}",
            root_dir.display()
        ));
    }

    let mut daemon = CoreDaemon::new(0);
    let mut core = CompilerCore::default();
    info!("enqueueing sources for daemon cycle");
    daemon.enqueue_startup_sources(sources, 0);
    info!("processing daemon cycle");
    let update = daemon.process_ready(&mut core, 0)?;
    info!(
        processed_files = update.processed_files.len(),
        tests_run = update.tests_run,
        tests_passed = update.tests_passed,
        tests_failed = update.tests_failed,
        "daemon cycle completed"
    );
    Ok(update.to_report())
}

#[instrument(skip_all, fields(root_dir = %root_dir.display()))]
fn collect_holo_sources(root_dir: &Path) -> Result<Vec<(String, String)>> {
    let mut paths = Vec::new();
    collect_holo_paths_recursive(root_dir, &mut paths)?;
    paths.sort();
    info!(path_count = paths.len(), "collected holo file paths");

    let mut sources = Vec::new();
    for path in paths {
        debug!(path = %path.display(), "reading source file");
        let source = fs::read_to_string(&path).map_err(|error| {
            holo_message_error!("failed to read source file {}", path.display())
                .with_std_source(error)
        })?;
        sources.push((path.to_string_lossy().into_owned(), source));
    }
    Ok(sources)
}

fn collect_holo_paths_recursive(dir: &Path, paths: &mut Vec<PathBuf>) -> Result<()> {
    let entries = fs::read_dir(dir).map_err(|error| {
        holo_message_error!("failed to read directory {}", dir.display()).with_std_source(error)
    })?;

    for entry in entries {
        let entry = entry.map_err(|error| {
            holo_message_error!("failed to read directory entry under {}", dir.display())
                .with_std_source(error)
        })?;
        let path = entry.path();
        if path.is_dir() {
            let name = path
                .file_name()
                .and_then(|value| value.to_str())
                .unwrap_or("");
            if name == ".git" || name == "target" {
                debug!(path = %path.display(), "skipping excluded directory");
                continue;
            }
            collect_holo_paths_recursive(&path, paths)?;
            continue;
        }
        if is_holo_file(&path) {
            paths.push(path);
        }
    }

    Ok(())
}

fn is_holo_file(path: &Path) -> bool {
    path.extension().is_some_and(|ext| ext == "holo")
}

fn resolve_root_dir(args: &[String]) -> PathBuf {
    if args.len() > 1 {
        PathBuf::from(&args[1])
    } else {
        PathBuf::from(".")
    }
}

fn main() {
    if let Err(error) = holo_base::logging::init_logging() {
        eprintln!("{error}");
        std::process::exit(1);
    }

    let args = env::args().collect::<Vec<_>>();
    let root_dir = resolve_root_dir(&args);
    info!(root_dir = %root_dir.display(), "running holo CLI");
    match run_directory(&root_dir) {
        Ok(report) => println!("{report}"),
        Err(error) => {
            eprintln!("{error}");
            std::process::exit(1);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{collect_holo_sources, is_holo_file, resolve_root_dir, run_directory};
    use std::fs;
    use std::path::Path;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn detects_holo_extension() {
        assert!(is_holo_file(Path::new("sample.holo")));
        assert!(!is_holo_file(Path::new("sample.txt")));
    }

    #[test]
    fn resolves_default_root_path() {
        let args = vec!["holo-cli".to_owned()];
        assert_eq!(resolve_root_dir(&args), Path::new("."));
    }

    #[test]
    fn collects_and_sorts_holo_sources() {
        let temp = temp_source_dir("collects_and_sorts_holo_sources");
        fs::write(temp.join("b.holo"), "#[test] fn b() { assert(true); }")
            .expect("should write b.holo");
        fs::create_dir_all(temp.join("nested")).expect("should create nested dir");
        fs::write(
            temp.join("nested").join("a.holo"),
            "#[test] fn a() { assert(true); }",
        )
        .expect("should write a.holo");
        fs::write(temp.join("ignore.txt"), "ignored").expect("should write ignore file");

        let sources = collect_holo_sources(&temp).expect("source collection should succeed");
        assert_eq!(sources.len(), 2);
        let mut names = sources
            .iter()
            .filter_map(|(path, _)| {
                Path::new(path)
                    .file_name()
                    .and_then(|value| value.to_str())
                    .map(|value| value.to_owned())
            })
            .collect::<Vec<_>>();
        names.sort();
        assert_eq!(names, vec!["a.holo".to_owned(), "b.holo".to_owned()]);

        fs::remove_dir_all(temp).expect("cleanup should succeed");
    }

    #[test]
    fn runs_directory_and_returns_report() {
        let temp = temp_source_dir("runs_directory_and_returns_report");
        fs::write(
            temp.join("sample.holo"),
            "#[test] fn pass() { assert(true); } #[test] fn fail() { assert(false); }",
        )
        .expect("should write sample.holo");

        let report = run_directory(&temp).expect("run should succeed");
        assert!(report.contains("processed_files: 1"));
        assert!(report.contains("tests: run=2 passed=1 failed=1"));
        assert!(report.contains("failing_tests: fail"));

        fs::remove_dir_all(temp).expect("cleanup should succeed");
    }

    fn temp_source_dir(name: &str) -> std::path::PathBuf {
        let suffix = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock should be after epoch")
            .as_nanos();
        let path = std::env::temp_dir().join(format!("holo-cli-{name}-{suffix}"));
        fs::create_dir_all(&path).expect("temp examples dir should be created");
        path
    }
}
