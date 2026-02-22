use std::collections::hash_map::DefaultHasher;
use std::env;
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::Component;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use holo_ast::Statement;
use holo_base::{holo_message_error, Result, SharedString};
use holo_core::daemon::{CoreDaemon, DaemonStatusUpdate};
use holo_core::CompilerCore;
use holo_deck::{
    run_with_updates as run_deck_with_updates, DaemonState as DeckDaemonState,
    ProjectIssueSeverity as DeckIssueSeverity,
};
use holo_deck::{ProjectIssue as DeckIssue, ProjectIssueKind as DeckIssueKind};
use holo_ipc::{
    DaemonEvent, IpcConnection, IpcServer, ProjectIssue, ProjectIssueKind, ProjectIssueSeverity,
    Request, Response, WireMessage,
};
use holo_lexer::{BasicLexer, Lexer};
use holo_parser::{BasicParser, Parser};
use notify::{Config, Event, RecommendedWatcher, RecursiveMode, Watcher};
use tracing::{debug, error, info, instrument, warn};

#[instrument(skip_all, fields(root_dir = %root_dir.display()))]
fn run_build_once(root_dir: &Path) -> Result<SharedString> {
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
    let endpoint = daemon_endpoint_name(root_dir)?;
    let server = IpcServer::bind(&endpoint)?;

    let state = Arc::new(Mutex::new(DaemonSharedState {
        issues: Vec::new(),
        daemon_state: "Compiling".into(),
        logs: vec!["daemon started".into()],
        dependency_graph: "pipeline:\n  <no .holo files discovered>".into(),
    }));
    let subscribers: Arc<Mutex<Vec<mpsc::Sender<DaemonEvent>>>> = Arc::new(Mutex::new(Vec::new()));

    {
        let state = Arc::clone(&state);
        let subscribers = Arc::clone(&subscribers);
        thread::spawn(move || {
            if let Err(error) = run_ipc_accept_loop(server, state, subscribers) {
                error!(error = %error, "ipc accept loop failed");
            }
        });
    }

    let mut daemon = CoreDaemon::new(0);
    let mut core = CompilerCore::with_persistent_cache(root_dir)?;

    let startup_sources = collect_holo_sources(root_dir)?;
    let startup_now = current_time_ms();
    if !startup_sources.is_empty() {
        info!(
            source_count = startup_sources.len(),
            "running initial daemon cycle"
        );
        append_log_and_broadcast(
            &state,
            &subscribers,
            format!("parsing {} changed file(s)", startup_sources.len()).into(),
        );
        append_log_and_broadcast(
            &state,
            &subscribers,
            format!("typechecking {} changed file(s)", startup_sources.len()).into(),
        );
        broadcast_to_subscribers(&subscribers, DaemonEvent::Lifecycle("Compiling".into()));
        daemon.enqueue_startup_sources(startup_sources, startup_now);
        let startup_update = daemon.process_ready(&mut core, startup_now)?;
        apply_update_to_state_and_subscribers(&startup_update, &state, &subscribers);
        let dependency_graph = build_dependency_graph(root_dir)?;
        set_dependency_graph_and_broadcast(&state, &subscribers, dependency_graph);
        println!("{}", startup_update.to_report());
    } else {
        info!("no .holo files found during daemon startup");
        if let Ok(mut guard) = state.lock() {
            guard.daemon_state = "Green".into();
        }
        broadcast_to_subscribers(&subscribers, DaemonEvent::Lifecycle("Green".into()));
        set_dependency_graph_and_broadcast(
            &state,
            &subscribers,
            "pipeline:\n  <no .holo files discovered>".into(),
        );
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
        endpoint = %endpoint,
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

                broadcast_to_subscribers(&subscribers, DaemonEvent::Lifecycle("Compiling".into()));
                append_log_and_broadcast(
                    &state,
                    &subscribers,
                    format!("parsing {changed_count} changed file(s)").into(),
                );
                append_log_and_broadcast(
                    &state,
                    &subscribers,
                    format!("typechecking {changed_count} changed file(s)").into(),
                );
                let update = daemon.process_ready(&mut core, current_time_ms())?;
                apply_update_to_state_and_subscribers(&update, &state, &subscribers);
                let dependency_graph = build_dependency_graph(root_dir)?;
                set_dependency_graph_and_broadcast(&state, &subscribers, dependency_graph);
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

fn run_ipc_accept_loop(
    server: IpcServer,
    state: Arc<Mutex<DaemonSharedState>>,
    subscribers: Arc<Mutex<Vec<mpsc::Sender<DaemonEvent>>>>,
) -> Result<()> {
    loop {
        let connection = server.accept();
        let Ok(mut connection) = connection else {
            if let Err(error) = connection {
                warn!(error = %error, "failed to accept IPC connection");
            }
            continue;
        };

        let state = Arc::clone(&state);
        let subscribers = Arc::clone(&subscribers);
        thread::spawn(move || {
            if let Err(error) = handle_ipc_connection(&mut connection, state, subscribers) {
                warn!(error = %error, "failed to handle IPC connection");
            }
        });
    }
}

fn handle_ipc_connection(
    connection: &mut IpcConnection,
    state: Arc<Mutex<DaemonSharedState>>,
    subscribers: Arc<Mutex<Vec<mpsc::Sender<DaemonEvent>>>>,
) -> Result<()> {
    let Some(message) = connection.receive()? else {
        return Ok(());
    };

    let (request_id, request) = match message {
        WireMessage::Request {
            request_id,
            request,
        } => (request_id, request),
        _ => {
            return Err(holo_message_error!(
                "expected request wire message when handling IPC client"
            ));
        }
    };

    match request {
        Request::GetIssues => {
            let issues = state
                .lock()
                .map_err(|_| holo_message_error!("daemon state lock poisoned"))?
                .issues
                .clone();
            connection.send(&WireMessage::Response {
                request_id,
                response: Response::IssuesSnapshot(issues),
            })
        }
        Request::SubscribeIssues => {
            connection.send(&WireMessage::Response {
                request_id,
                response: Response::Ok,
            })?;

            let (tx, rx) = mpsc::channel::<DaemonEvent>();
            {
                subscribers
                    .lock()
                    .map_err(|_| holo_message_error!("subscriber lock poisoned"))?
                    .push(tx);
            }

            let (current_issues, current_state, current_logs, current_graph) = {
                let guard = state
                    .lock()
                    .map_err(|_| holo_message_error!("daemon state lock poisoned"))?;
                (
                    guard.issues.clone(),
                    guard.daemon_state.clone(),
                    guard.logs.clone(),
                    guard.dependency_graph.clone(),
                )
            };
            connection.send(&WireMessage::Event {
                event: DaemonEvent::IssuesUpdated(current_issues),
            })?;
            connection.send(&WireMessage::Event {
                event: DaemonEvent::Lifecycle(current_state),
            })?;
            connection.send(&WireMessage::Event {
                event: DaemonEvent::DependencyGraph(current_graph),
            })?;
            for entry in current_logs {
                connection.send(&WireMessage::Event {
                    event: DaemonEvent::CycleReport(entry),
                })?;
            }

            for event in rx {
                if connection.send(&WireMessage::Event { event }).is_err() {
                    break;
                }
            }
            Ok(())
        }
        Request::GetStatus => {
            let issue_count = state
                .lock()
                .map_err(|_| holo_message_error!("daemon state lock poisoned"))?
                .issues
                .len();
            connection.send(&WireMessage::Response {
                request_id,
                response: Response::StatusReport(format!("issues: {issue_count}").into()),
            })
        }
        Request::Build | Request::Shutdown => connection.send(&WireMessage::Response {
            request_id,
            response: Response::Error("request is not supported by this daemon endpoint".into()),
        }),
    }
}

fn apply_update_to_state_and_subscribers(
    update: &DaemonStatusUpdate,
    state: &Arc<Mutex<DaemonSharedState>>,
    subscribers: &Arc<Mutex<Vec<mpsc::Sender<DaemonEvent>>>>,
) {
    let issues = derive_issues(update);
    let daemon_state: SharedString = daemon_state_label_for_update(update).into();
    if let Ok(mut guard) = state.lock() {
        guard.issues = issues.clone();
        guard.daemon_state = daemon_state.clone();
    }

    broadcast_to_subscribers(subscribers, DaemonEvent::IssuesUpdated(issues));
    broadcast_to_subscribers(subscribers, DaemonEvent::Lifecycle(daemon_state));
    append_log_and_broadcast(
        state,
        subscribers,
        format!(
            "cycle complete: files={} errors={} tests failed={}",
            update.processed_files.len(),
            update.errors.len(),
            update.tests_failed
        )
        .into(),
    );
}

fn broadcast_to_subscribers(
    subscribers: &Arc<Mutex<Vec<mpsc::Sender<DaemonEvent>>>>,
    event: DaemonEvent,
) {
    let mut guard = match subscribers.lock() {
        Ok(guard) => guard,
        Err(_) => return,
    };
    guard.retain(|sender| sender.send(event.clone()).is_ok());
}

fn append_log_and_broadcast(
    state: &Arc<Mutex<DaemonSharedState>>,
    subscribers: &Arc<Mutex<Vec<mpsc::Sender<DaemonEvent>>>>,
    entry: SharedString,
) {
    if let Ok(mut guard) = state.lock() {
        guard.logs.push(entry.clone());
        if guard.logs.len() > 500 {
            let remove_count = guard.logs.len() - 500;
            guard.logs.drain(0..remove_count);
        }
    }
    broadcast_to_subscribers(subscribers, DaemonEvent::CycleReport(entry));
}

fn set_dependency_graph_and_broadcast(
    state: &Arc<Mutex<DaemonSharedState>>,
    subscribers: &Arc<Mutex<Vec<mpsc::Sender<DaemonEvent>>>>,
    graph: SharedString,
) {
    if let Ok(mut guard) = state.lock() {
        guard.dependency_graph = graph.clone();
    }
    broadcast_to_subscribers(subscribers, DaemonEvent::DependencyGraph(graph));
}

fn daemon_state_label_for_update(update: &DaemonStatusUpdate) -> &'static str {
    if !update.errors.is_empty() || update.tests_failed > 0 {
        "Failed"
    } else {
        "Green"
    }
}

fn derive_issues(update: &DaemonStatusUpdate) -> Vec<ProjectIssue> {
    let mut issues = Vec::new();

    for error in &update.errors {
        let line = diagnostic_line_number(error).unwrap_or(1);
        let detail = error
            .styled_message
            .as_ref()
            .map(|message| strip_ansi_sequences(message))
            .unwrap_or_else(|| error.message.clone());
        issues.push(ProjectIssue {
            title: format!("Compilation diagnostic at {}:{line}", error.file_path).into(),
            file: error.file_path.clone(),
            line,
            kind: ProjectIssueKind::Compilation,
            severity: ProjectIssueSeverity::Error,
            summary: error.message.clone(),
            detail,
            source_diagnostics: error.source_diagnostic.clone().into_iter().collect(),
        });
    }

    for failure in &update.test_failures {
        let line = diagnostic_line_number(failure).unwrap_or(1);
        let detail = failure
            .styled_message
            .as_ref()
            .map(|message| strip_ansi_sequences(message))
            .unwrap_or_else(|| failure.message.clone());
        let source_diagnostics = failure
            .source_diagnostic
            .clone()
            .into_iter()
            .collect::<Vec<_>>();
        let summary = source_diagnostics
            .first()
            .map(|diagnostic| diagnostic.message.clone())
            .unwrap_or_else(|| failure.message.clone());
        issues.push(ProjectIssue {
            title: format!("Test failure at {}:{line}", failure.file_path).into(),
            file: failure.file_path.clone(),
            line,
            kind: ProjectIssueKind::Test,
            severity: ProjectIssueSeverity::Error,
            summary,
            detail,
            source_diagnostics,
        });
    }

    issues
}

fn diagnostic_line_number(error: &holo_core::daemon::FileDiagnostic) -> Option<usize> {
    let diagnostic = error.source_diagnostic.as_ref()?;
    let span_start = diagnostic.annotated_spans.first()?.span.start;
    diagnostic
        .source_excerpts
        .iter()
        .find_map(|excerpt| line_number_for_offset(excerpt, span_start))
}

#[instrument(skip_all, fields(root_dir = %root_dir.display()))]
fn run_deck_mode(root_dir: &Path) -> Result<()> {
    let endpoint = daemon_endpoint_name(root_dir)?;
    ensure_daemon_running(root_dir, &endpoint)?;

    let initial_issues = request_issues_snapshot(&endpoint)?;
    let (updates_tx, updates_rx) = mpsc::channel::<Vec<DeckIssue>>();
    let (state_tx, state_rx) = mpsc::channel::<DeckDaemonState>();
    let (log_tx, log_rx) = mpsc::channel::<SharedString>();
    let (graph_tx, graph_rx) = mpsc::channel::<SharedString>();
    let _ = state_tx.send(DeckDaemonState::Green);

    let endpoint_for_thread = endpoint.clone();
    thread::spawn(move || {
        if let Err(error) =
            stream_issue_updates(&endpoint_for_thread, updates_tx, state_tx, log_tx, graph_tx)
        {
            warn!(error = %error, "deck issue update stream ended");
        }
    });

    run_deck_with_updates(
        map_ipc_issues_to_deck(initial_issues),
        Some(updates_rx),
        Some(state_rx),
        Some(log_rx),
        Some(graph_rx),
    )
}

fn ensure_daemon_running(root_dir: &Path, endpoint: &str) -> Result<()> {
    if IpcConnection::connect(endpoint).is_ok() {
        return Ok(());
    }

    let executable = env::current_exe().map_err(|error| {
        holo_message_error!("failed to determine current executable").with_std_source(error)
    })?;
    Command::new(executable)
        .arg("daemon")
        .arg(root_dir)
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .map_err(|error| {
            holo_message_error!("failed to spawn daemon process").with_std_source(error)
        })?;

    for _ in 0..50 {
        if IpcConnection::connect(endpoint).is_ok() {
            return Ok(());
        }
        thread::sleep(Duration::from_millis(100));
    }

    Err(holo_message_error!(
        "daemon did not become available on endpoint {endpoint}"
    ))
}

fn request_issues_snapshot(endpoint: &str) -> Result<Vec<ProjectIssue>> {
    let mut connection = IpcConnection::connect(endpoint)?;
    connection.send(&WireMessage::Request {
        request_id: 1,
        request: Request::GetIssues,
    })?;

    let Some(message) = connection.receive()? else {
        return Err(holo_message_error!(
            "daemon closed connection before responding"
        ));
    };

    match message {
        WireMessage::Response {
            request_id: 1,
            response: Response::IssuesSnapshot(issues),
        } => Ok(issues),
        WireMessage::Response {
            request_id: 1,
            response: Response::Error(error),
        } => Err(holo_message_error!("daemon returned error: {error}")),
        other => Err(holo_message_error!(
            "unexpected daemon response when requesting issues: {other:?}"
        )),
    }
}

fn stream_issue_updates(
    endpoint: &str,
    updates_tx: mpsc::Sender<Vec<DeckIssue>>,
    state_tx: mpsc::Sender<DeckDaemonState>,
    log_tx: mpsc::Sender<SharedString>,
    graph_tx: mpsc::Sender<SharedString>,
) -> Result<()> {
    let mut connection = IpcConnection::connect(endpoint)?;
    connection.send(&WireMessage::Request {
        request_id: 2,
        request: Request::SubscribeIssues,
    })?;

    let Some(first_message) = connection.receive()? else {
        return Err(holo_message_error!(
            "daemon closed connection before subscribe acknowledgement"
        ));
    };
    match first_message {
        WireMessage::Response {
            request_id: 2,
            response: Response::Ok,
        } => {
            let _ = state_tx.send(DeckDaemonState::Green);
        }
        WireMessage::Response {
            request_id: 2,
            response: Response::Error(error),
        } => {
            let _ = state_tx.send(DeckDaemonState::Failed);
            return Err(holo_message_error!(
                "daemon rejected issue subscription: {error}"
            ));
        }
        other => {
            return Err(holo_message_error!(
                "unexpected subscribe acknowledgement: {other:?}"
            ));
        }
    }

    while let Some(message) = connection.receive()? {
        match message {
            WireMessage::Event {
                event: DaemonEvent::IssuesUpdated(issues),
            } => {
                if updates_tx.send(map_ipc_issues_to_deck(issues)).is_err() {
                    break;
                }
            }
            WireMessage::Event {
                event: DaemonEvent::Lifecycle(state),
            } => {
                if state_tx.send(map_lifecycle_state_to_deck(&state)).is_err() {
                    break;
                }
            }
            WireMessage::Event {
                event: DaemonEvent::CycleReport(entry),
            } => {
                if log_tx.send(entry).is_err() {
                    break;
                }
            }
            WireMessage::Event {
                event: DaemonEvent::DependencyGraph(graph),
            } => {
                if graph_tx.send(graph).is_err() {
                    break;
                }
            }
            _ => {}
        }
    }

    let _ = state_tx.send(DeckDaemonState::Disconnected);
    Ok(())
}

fn map_lifecycle_state_to_deck(state: &str) -> DeckDaemonState {
    match state {
        "Green" => DeckDaemonState::Green,
        "Compiling" => DeckDaemonState::Compiling,
        "Failed" => DeckDaemonState::Failed,
        _ => DeckDaemonState::Disconnected,
    }
}

fn build_dependency_graph(root_dir: &Path) -> Result<SharedString> {
    let sources = collect_holo_sources(root_dir)?;
    let mut dependencies = Vec::new();
    for (file_path, source) in sources {
        match extract_test_dependencies_from_source(&file_path, &source) {
            Ok(mut file_dependencies) => dependencies.append(&mut file_dependencies),
            Err(error) => {
                warn!(
                    file_path = %file_path,
                    error = %error,
                    "failed to extract dependency graph entries from source"
                );
            }
        }
    }

    dependencies.sort_by(|left, right| {
        left.test_name
            .cmp(&right.test_name)
            .then(left.file_path.cmp(&right.file_path))
    });

    let mut lines = vec!["Tests".into()];
    if dependencies.is_empty() {
        lines.push("  <no discovered tests>".into());
        return Ok(lines.join("\n").into());
    }

    for dependency in dependencies {
        lines.push(format!("  {}", dependency.test_name));
        lines.push(format!("    Input File: {}", dependency.file_path));
        for function in &dependency.used_functions {
            lines.push(format!("    Function: {function}"));
        }
    }

    Ok(lines.join("\n").into())
}

fn extract_test_dependencies_from_source(
    file_path: &str,
    source: &str,
) -> Result<Vec<TestDependency>> {
    let lexer = BasicLexer;
    let parser = BasicParser;
    let lexed = lexer.lex(source);
    if !lexed.diagnostics.is_empty() {
        let rendered = lexed.diagnostics[0].render_annotated();
        return Err(holo_message_error!(
            "failed to extract graph dependencies for {file_path}: {rendered}"
        ));
    }
    let parsed = parser.parse_module(&lexed.tokens, source);
    if !parsed.diagnostics.is_empty() {
        let rendered = parsed.diagnostics[0].render_annotated();
        return Err(holo_message_error!(
            "failed to extract graph dependencies for {file_path}: {rendered}"
        ));
    }
    let module = parsed.module;

    let mut dependencies = Vec::new();
    for test in module.tests {
        let mut used_functions = test
            .statements
            .iter()
            .map(|statement| match statement {
                Statement::Assert(_) => "assert".into(),
            })
            .collect::<Vec<_>>();
        used_functions.sort();
        used_functions.dedup();

        dependencies.push(TestDependency {
            test_name: test.name,
            file_path: file_path.into(),
            used_functions,
        });
    }

    Ok(dependencies)
}

fn map_ipc_issues_to_deck(issues: Vec<ProjectIssue>) -> Vec<DeckIssue> {
    issues
        .into_iter()
        .map(|issue| DeckIssue {
            title: issue.title,
            file: issue.file,
            line: issue.line,
            kind: match issue.kind {
                ProjectIssueKind::Compilation => DeckIssueKind::Compilation,
                ProjectIssueKind::Test => DeckIssueKind::Test,
            },
            severity: match issue.severity {
                ProjectIssueSeverity::Error => DeckIssueSeverity::Error,
                ProjectIssueSeverity::Warning => DeckIssueSeverity::Warning,
            },
            summary: issue.summary,
            detail: issue.detail,
            source_diagnostics: issue.source_diagnostics,
        })
        .collect()
}

fn line_number_for_offset(excerpt: &holo_base::SourceExcerpt, offset: usize) -> Option<usize> {
    if offset < excerpt.starting_offset {
        return None;
    }

    let mut absolute_start = excerpt.starting_offset;
    for (index, raw_line) in excerpt.source.split('\n').enumerate() {
        let line = raw_line.trim_end_matches('\r');
        let line_start = absolute_start;
        let line_end = line_start + line.len();
        if offset >= line_start && offset <= line_end {
            return Some(excerpt.starting_line + index);
        }
        absolute_start += raw_line.len() + 1;
    }

    None
}

fn strip_ansi_sequences(input: &str) -> SharedString {
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

fn daemon_endpoint_name(root_dir: &Path) -> Result<SharedString> {
    let canonical = root_dir.canonicalize().map_err(|error| {
        holo_message_error!(
            "failed to canonicalize root directory {}",
            root_dir.display()
        )
        .with_std_source(error)
    })?;
    let canonical_string = canonical.to_string_lossy();
    let mut hasher = DefaultHasher::new();
    canonical_string.hash(&mut hasher);
    Ok(format!("holo-daemon-{:016x}", hasher.finish()).into())
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
        daemon.record_change(source_path, source.into(), now_ms);
        changed += 1;
    }
    Ok(changed)
}

fn display_source_path(root_dir: &Path, path: &Path) -> SharedString {
    if let Ok(relative) = path.strip_prefix(root_dir) {
        if relative.components().next() == Some(Component::CurDir) {
            return relative.to_string_lossy().into_owned().into();
        }
        return format!(".\\{}", relative.display()).into();
    }
    path.to_string_lossy().into_owned().into()
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
    Deck(PathBuf),
    Daemon(PathBuf),
    Version,
}

fn parse_cli_mode(args: &[String]) -> CliMode {
    match args.get(1).map(String::as_str) {
        Some("build") => CliMode::Build(path_arg_or_default(args, 2)),
        Some("daemon") => CliMode::Daemon(path_arg_or_default(args, 2)),
        Some("deck") => CliMode::Deck(path_arg_or_default(args, 2)),
        Some("version") => CliMode::Version,
        Some(path) => CliMode::Deck(PathBuf::from(path)),
        None => CliMode::Deck(PathBuf::from(".")),
    }
}

fn path_arg_or_default(args: &[String], index: usize) -> PathBuf {
    args.get(index)
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("."))
}

#[instrument(skip_all, fields(root_dir = %root_dir.display()))]
fn collect_holo_sources(root_dir: &Path) -> Result<Vec<(SharedString, SharedString)>> {
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
        sources.push((display_source_path(root_dir, &path), source.into()));
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
        CliMode::Deck(root_dir) => {
            info!(root_dir = %root_dir.display(), "running holo CLI deck mode");
            if let Err(error) = run_deck_mode(&root_dir) {
                eprintln!("{error}");
                std::process::exit(1);
            }
        }
        CliMode::Daemon(root_dir) => {
            info!(root_dir = %root_dir.display(), "running holo CLI daemon mode");
            if let Err(error) = run_daemon_mode(&root_dir) {
                eprintln!("{error}");
                std::process::exit(1);
            }
        }
        CliMode::Version => {
            println!("{}", holo_base::project_revision());
        }
    }
}

#[derive(Debug, Clone)]
struct DaemonSharedState {
    issues: Vec<ProjectIssue>,
    daemon_state: SharedString,
    logs: Vec<SharedString>,
    dependency_graph: SharedString,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TestDependency {
    test_name: SharedString,
    file_path: SharedString,
    used_functions: Vec<SharedString>,
}

#[cfg(test)]
mod tests {
    use super::{
        build_dependency_graph, collect_holo_sources, daemon_endpoint_name, derive_issues,
        display_source_path, is_holo_file, line_number_for_offset, map_lifecycle_state_to_deck,
        parse_cli_mode, path_arg_or_default, run_build_once, strip_ansi_sequences, CliMode,
    };
    use holo_base::{DiagnosticKind, SharedString, SourceDiagnostic, SourceExcerpt, Span};
    use holo_core::daemon::{DaemonStatusUpdate, FileDiagnostic};
    use holo_deck::DaemonState as DeckDaemonState;
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::time::{SystemTime, UNIX_EPOCH};

    #[test]
    fn detects_holo_extension() {
        assert!(is_holo_file(Path::new("sample.holo")));
        assert!(!is_holo_file(Path::new("sample.txt")));
    }

    #[test]
    fn parses_build_mode_with_default_path() {
        let args = vec!["holo-cli".into(), "build".into()];
        assert_eq!(parse_cli_mode(&args), CliMode::Build(Path::new(".").into()));
    }

    #[test]
    fn parses_build_mode_with_custom_path() {
        let args = vec!["holo-cli".into(), "build".into(), "workspace".into()];
        assert_eq!(
            parse_cli_mode(&args),
            CliMode::Build(Path::new("workspace").into())
        );
    }

    #[test]
    fn parses_deck_mode_with_default_path() {
        let args = vec!["holo-cli".into()];
        assert_eq!(parse_cli_mode(&args), CliMode::Deck(Path::new(".").into()));
    }

    #[test]
    fn parses_deck_mode_with_custom_path() {
        let args = vec!["holo-cli".into(), "project".into()];
        assert_eq!(
            parse_cli_mode(&args),
            CliMode::Deck(Path::new("project").into())
        );
    }

    #[test]
    fn parses_deck_subcommand_with_default_path() {
        let args = vec!["holo-cli".into(), "deck".into()];
        assert_eq!(parse_cli_mode(&args), CliMode::Deck(Path::new(".").into()));
    }

    #[test]
    fn parses_deck_subcommand_with_custom_path() {
        let args = vec!["holo-cli".into(), "deck".into(), "repo".into()];
        assert_eq!(
            parse_cli_mode(&args),
            CliMode::Deck(Path::new("repo").into())
        );
    }

    #[test]
    fn parses_version_subcommand() {
        let args = vec!["holo-cli".into(), "version".into()];
        assert_eq!(parse_cli_mode(&args), CliMode::Version);
    }

    #[test]
    fn parses_daemon_subcommand_with_default_path() {
        let args = vec!["holo-cli".into(), "daemon".into()];
        assert_eq!(
            parse_cli_mode(&args),
            CliMode::Daemon(Path::new(".").into())
        );
    }

    #[test]
    fn parses_daemon_subcommand_with_custom_path() {
        let args = vec!["holo-cli".into(), "daemon".into(), "repo".into()];
        assert_eq!(
            parse_cli_mode(&args),
            CliMode::Daemon(Path::new("repo").into())
        );
    }

    #[test]
    fn path_arg_defaults_when_missing() {
        let args = vec!["holo-cli".into(), "daemon".into()];
        assert_eq!(path_arg_or_default(&args, 2), PathBuf::from("."));
    }

    #[test]
    fn maps_daemon_lifecycle_to_deck_state() {
        assert_eq!(map_lifecycle_state_to_deck("Green"), DeckDaemonState::Green);
        assert_eq!(
            map_lifecycle_state_to_deck("Compiling"),
            DeckDaemonState::Compiling
        );
        assert_eq!(
            map_lifecycle_state_to_deck("Failed"),
            DeckDaemonState::Failed
        );
        assert_eq!(
            map_lifecycle_state_to_deck("Unknown"),
            DeckDaemonState::Disconnected
        );
    }

    #[test]
    fn builds_dependency_graph_for_holo_files() {
        let temp = temp_source_dir("builds_dependency_graph_for_holo_files");
        fs::write(temp.join("a.holo"), "#[test] fn a() { assert(true); }").expect("should write");
        fs::create_dir_all(temp.join("nested")).expect("should create nested");
        fs::write(
            temp.join("nested").join("b.holo"),
            "#[test] fn b() { assert(true); }",
        )
        .expect("should write");

        let graph = build_dependency_graph(&temp).expect("graph should build");
        assert!(graph.starts_with("Tests\n"));
        assert!(graph.contains("\n  a\n"));
        assert!(graph.contains("\n  b\n"));
        assert!(graph.contains("Function: assert"));
        assert!(
            graph.contains("Input File: .\\a.holo") || graph.contains("Input File: ./a.holo"),
            "graph missing input file node for a.holo: {graph}"
        );
        assert!(
            graph.contains("Input File: .\\nested\\b.holo")
                || graph.contains("Input File: ./nested/b.holo"),
            "graph missing input file node for nested/b.holo: {graph}"
        );

        fs::remove_dir_all(temp).expect("cleanup should succeed");
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
        let mut names: Vec<SharedString> = sources
            .iter()
            .filter_map(|(path, _)| {
                Path::new(path.as_str())
                    .file_name()
                    .and_then(|value| value.to_str())
                    .map(|value| value.into())
            })
            .collect::<Vec<_>>();
        names.sort();
        assert_eq!(
            names,
            vec![SharedString::from("a.holo"), SharedString::from("b.holo")]
        );

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

    #[test]
    fn endpoint_name_depends_on_root_path() {
        let one = daemon_endpoint_name(Path::new(".")).expect("endpoint should build");
        let two = daemon_endpoint_name(Path::new(".")).expect("endpoint should build");
        assert_eq!(one, two);
        assert!(one.starts_with("holo-daemon-"));
    }

    #[test]
    fn strips_ansi_escape_sequences() {
        let input = "\u{1b}[31merror\u{1b}[0m";
        assert_eq!(strip_ansi_sequences(input), "error");
    }

    #[test]
    fn computes_line_number_for_offset() {
        let excerpt = SourceExcerpt::new("first\nsecond\nthird\n", 10, 100);
        assert_eq!(line_number_for_offset(&excerpt, 100), Some(10));
        assert_eq!(line_number_for_offset(&excerpt, 106), Some(11));
        assert_eq!(line_number_for_offset(&excerpt, 113), Some(12));
        assert_eq!(line_number_for_offset(&excerpt, 99), None);
    }

    #[test]
    fn failing_tests_are_emitted_as_source_diagnostics() {
        let diagnostic =
            SourceDiagnostic::new(DiagnosticKind::Test, "test `sample_fail` assertion failed")
                .with_annotated_span(Span::new(10, 22), "failing assertion")
                .with_source_excerpt(SourceExcerpt::new(
                    "#[test] fn sample_fail() { assert(false); }\n",
                    1,
                    0,
                ));
        let update = DaemonStatusUpdate {
            failing_tests: vec!["sample_fail".into()],
            test_failures: vec![FileDiagnostic {
                file_path: "tests/sample.holo".into(),
                message: diagnostic.render_annotated(),
                styled_message: Some(holo_base::display_source_diagnostics(std::slice::from_ref(
                    &diagnostic,
                ))),
                source_diagnostic: Some(diagnostic),
            }],
            ..DaemonStatusUpdate::default()
        };
        let issues = derive_issues(&update);

        assert_eq!(issues.len(), 1);
        assert_eq!(issues[0].source_diagnostics.len(), 1);
        assert_eq!(issues[0].file, "tests/sample.holo");
        assert_eq!(issues[0].line, 1);
        assert!(issues[0].source_diagnostics[0]
            .message
            .contains("sample_fail"));
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
