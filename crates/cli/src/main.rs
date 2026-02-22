use std::env;
use std::fs;
use std::path::Component;
use std::path::{Path, PathBuf};
use std::sync::mpsc;
use std::time::{SystemTime, UNIX_EPOCH};

use holo_base::{holo_message_error, Result};
use holo_core::daemon::CoreDaemon;
use holo_core::CompilerCore;
use notify::{Config, Event, RecommendedWatcher, RecursiveMode, Watcher};
use tracing::{debug, info, instrument};

#[instrument(skip_all, fields(root_dir = %root_dir.display()))]
fn run_build_once(root_dir: &Path) -> Result<String> {
    info!("starting source discovery");
    let sources = collect_holo_sources(root_dir)?;
    info!(source_count = sources.len(), "completed source discovery");
    if sources.is_empty() {
        return Err(holo_message_error!(
            "no .holo files found in {}",
            root_dir.display()
        ));
    }

    let now = current_time_ms();
    let mut daemon = CoreDaemon::new(0);
    let mut core = CompilerCore::with_persistent_cache(root_dir)?;
    info!("enqueueing sources for daemon cycle");
    daemon.enqueue_startup_sources(sources, now);
    info!("processing daemon cycle");
    let update = daemon.process_ready(&mut core, now)?;
    info!(
        processed_files = update.processed_files.len(),
        tests_run = update.tests_run,
        tests_passed = update.tests_passed,
        tests_failed = update.tests_failed,
        "build cycle completed"
    );
    Ok(update.to_report())
}

#[instrument(skip_all, fields(root_dir = %root_dir.display()))]
fn run_daemon_mode(root_dir: &Path) -> Result<()> {
    let mut daemon = CoreDaemon::new(0);
    let mut core = CompilerCore::with_persistent_cache(root_dir)?;

    let startup_sources = collect_holo_sources(root_dir)?;
    let startup_now = current_time_ms();
    if !startup_sources.is_empty() {
        info!(
            source_count = startup_sources.len(),
            "running initial daemon cycle"
        );
        daemon.enqueue_startup_sources(startup_sources, startup_now);
        let startup_update = daemon.process_ready(&mut core, startup_now)?;
        println!("{}", startup_update.to_report());
    } else {
        info!("no .holo files found during daemon startup");
    }

    let (event_sender, event_receiver) = mpsc::channel();
    let mut watcher = RecommendedWatcher::new(
        move |event_result| {
            let _ = event_sender.send(event_result);
        },
        Config::default(),
    )
    .map_err(|error| {
        holo_message_error!("failed to initialize file watcher").with_std_source(error)
    })?;
    watcher
        .watch(root_dir, RecursiveMode::Recursive)
        .map_err(|error| {
            holo_message_error!("failed to watch directory {}", root_dir.display())
                .with_std_source(error)
        })?;

    info!(
        root_dir = %root_dir.display(),
        "daemon mode started; waiting for file changes"
    );
    loop {
        match event_receiver.recv() {
            Ok(Ok(event)) => {
                let changed_count =
                    record_changed_holo_files(root_dir, &event, &mut daemon, current_time_ms())?;
                if changed_count == 0 {
                    continue;
                }

                let update = daemon.process_ready(&mut core, current_time_ms())?;
                println!("{}", update.to_report());
            }
            Ok(Err(error)) => {
                return Err(holo_message_error!("file watch error").with_std_source(error));
            }
            Err(error) => {
                return Err(
                    holo_message_error!("file watch channel closed unexpectedly")
                        .with_std_source(error),
                );
            }
        }
    }
}

fn record_changed_holo_files(
    root_dir: &Path,
    event: &Event,
    daemon: &mut CoreDaemon,
    now_ms: u64,
) -> Result<usize> {
    let mut changed = 0usize;
    for path in &event.paths {
        if !is_holo_file(path) {
            continue;
        }
        if !path.is_file() {
            debug!(path = %path.display(), "ignoring non-file holo path event");
            continue;
        }

        let source = fs::read_to_string(path).map_err(|error| {
            holo_message_error!("failed to read source file {}", path.display())
                .with_std_source(error)
        })?;
        let source_path = display_source_path(root_dir, path);
        info!(path = %source_path, "detected source change");
        daemon.record_change(source_path, source, now_ms);
        changed += 1;
    }
    Ok(changed)
}

fn display_source_path(root_dir: &Path, path: &Path) -> String {
    if let Ok(relative) = path.strip_prefix(root_dir) {
        if relative.components().next() == Some(Component::CurDir) {
            return relative.to_string_lossy().into_owned();
        }
        return format!(".\\{}", relative.display());
    }
    path.to_string_lossy().into_owned()
}

fn current_time_ms() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock should be after UNIX_EPOCH")
        .as_millis() as u64
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CliMode {
    Build(PathBuf),
    Daemon(PathBuf),
}

fn parse_cli_mode(args: &[String]) -> CliMode {
    if args.get(1).is_some_and(|arg| arg == "build") {
        if let Some(path) = args.get(2) {
            return CliMode::Build(PathBuf::from(path));
        }
        return CliMode::Build(PathBuf::from("."));
    }

    if let Some(path) = args.get(1) {
        return CliMode::Daemon(PathBuf::from(path));
    }

    CliMode::Daemon(PathBuf::from("."))
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
        sources.push((display_source_path(root_dir, &path), source));
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
            if name == ".git" || name == "target" || name == ".holo" {
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

fn main() {
    if let Err(error) = holo_base::logging::init_logging() {
        eprintln!("{error}");
        std::process::exit(1);
    }

    let args = env::args().collect::<Vec<_>>();
    match parse_cli_mode(&args) {
        CliMode::Build(root_dir) => {
            info!(root_dir = %root_dir.display(), "running holo CLI build mode");
            match run_build_once(&root_dir) {
                Ok(report) => println!("{report}"),
                Err(error) => {
                    eprintln!("{error}");
                    std::process::exit(1);
                }
            }
        }
        CliMode::Daemon(root_dir) => {
            info!(root_dir = %root_dir.display(), "running holo CLI daemon mode");
            if let Err(error) = run_daemon_mode(&root_dir) {
                eprintln!("{error}");
                std::process::exit(1);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        collect_holo_sources, display_source_path, is_holo_file, parse_cli_mode, run_build_once,
        CliMode,
    };
    use std::fs;
    use std::path::Path;
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn detects_holo_extension() {
        assert!(is_holo_file(Path::new("sample.holo")));
        assert!(!is_holo_file(Path::new("sample.txt")));
    }

    #[test]
    fn parses_build_mode_with_default_path() {
        let args = vec!["holo-cli".to_owned(), "build".to_owned()];
        assert_eq!(
            parse_cli_mode(&args),
            CliMode::Build(Path::new(".").to_owned())
        );
    }

    #[test]
    fn parses_build_mode_with_custom_path() {
        let args = vec![
            "holo-cli".to_owned(),
            "build".to_owned(),
            "workspace".to_owned(),
        ];
        assert_eq!(
            parse_cli_mode(&args),
            CliMode::Build(Path::new("workspace").to_owned())
        );
    }

    #[test]
    fn parses_daemon_mode_with_default_path() {
        let args = vec!["holo-cli".to_owned()];
        assert_eq!(
            parse_cli_mode(&args),
            CliMode::Daemon(Path::new(".").to_owned())
        );
    }

    #[test]
    fn parses_daemon_mode_with_custom_path() {
        let args = vec!["holo-cli".to_owned(), "project".to_owned()];
        assert_eq!(
            parse_cli_mode(&args),
            CliMode::Daemon(Path::new("project").to_owned())
        );
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
    fn build_mode_runs_single_cycle_and_returns_report() {
        let temp = temp_source_dir("build_mode_runs_single_cycle_and_returns_report");
        fs::write(
            temp.join("sample.holo"),
            "#[test] fn pass() { assert(true); } #[test] fn fail() { assert(false); }",
        )
        .expect("should write sample.holo");

        let report = run_build_once(&temp).expect("build should succeed");
        assert!(report.contains("processed_files: 1"));
        assert!(report.contains("tests: run=2 passed=1 failed=1"));
        assert!(report.contains("failing_tests: fail"));

        fs::remove_dir_all(temp).expect("cleanup should succeed");
    }

    #[test]
    fn renders_relative_source_path() {
        let root = Path::new("repo");
        let source = Path::new("repo/src/main.holo");
        let rendered = display_source_path(root, source);
        assert!(rendered == ".\\src\\main.holo" || rendered == ".\\src/main.holo");
    }

    fn temp_source_dir(name: &str) -> std::path::PathBuf {
        let suffix = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock should be after epoch")
            .as_nanos();
        let path = std::env::temp_dir().join(format!("holo-cli-{name}-{suffix}"));
        fs::create_dir_all(&path).expect("temp source dir should be created");
        path
    }
}
