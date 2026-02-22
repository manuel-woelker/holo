//! Terminal dashboard for viewing project compilation and test issues.

use std::io;
use std::sync::mpsc::{Receiver, TryRecvError};
use std::time::Duration;

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind};
use crossterm::execute;
use crossterm::terminal::{
    disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
};
use holo_base::{holo_message_error, Result};
use ratatui::backend::CrosstermBackend;
use ratatui::layout::{Alignment, Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, List, ListItem, ListState, Paragraph, Tabs, Wrap};
use ratatui::Terminal;

const MAX_LOG_ENTRIES: usize = 500;

/// Project issue payload shown by the deck TUI.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectIssue {
    /// Short issue title.
    pub title: String,
    /// Source file path.
    pub file: String,
    /// 1-based line number.
    pub line: usize,
    /// Issue kind/category.
    pub kind: ProjectIssueKind,
    /// Severity level.
    pub severity: ProjectIssueSeverity,
    /// Brief summary text.
    pub summary: String,
    /// Detailed explanation text.
    pub detail: String,
}

/// Issue category shown in deck.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProjectIssueKind {
    Compilation,
    Test,
}

impl ProjectIssueKind {
    fn label(self) -> &'static str {
        match self {
            Self::Compilation => "Compilation",
            Self::Test => "Test",
        }
    }

    fn short_tag(self) -> &'static str {
        match self {
            Self::Compilation => "COMP",
            Self::Test => "TEST",
        }
    }
}

/// Issue severity displayed in deck.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProjectIssueSeverity {
    Error,
    Warning,
}

impl ProjectIssueSeverity {
    fn label(self) -> &'static str {
        match self {
            Self::Error => "Error",
            Self::Warning => "Warning",
        }
    }
}

/// Opens deck TUI with static example data.
pub fn run() -> Result<()> {
    run_with_updates(example_issues(), None, None, None, None)
}

/// Opens deck TUI with initial issues and optional live updates.
pub fn run_with_updates(
    initial_issues: Vec<ProjectIssue>,
    issue_updates: Option<Receiver<Vec<ProjectIssue>>>,
    daemon_state_updates: Option<Receiver<DaemonState>>,
    log_updates: Option<Receiver<String>>,
    dependency_graph_updates: Option<Receiver<String>>,
) -> Result<()> {
    enable_raw_mode()
        .map_err(|error| holo_message_error!("failed to enable raw mode").with_std_source(error))?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen).map_err(|error| {
        holo_message_error!("failed to enter alternate screen").with_std_source(error)
    })?;

    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend).map_err(|error| {
        holo_message_error!("failed to initialize terminal backend").with_std_source(error)
    })?;

    let mut app = DeckApp::new(initial_issues);
    let mut issue_updates = issue_updates;
    let mut daemon_state_updates = daemon_state_updates;
    let mut log_updates = log_updates;
    let mut dependency_graph_updates = dependency_graph_updates;
    let result = run_loop(
        &mut terminal,
        &mut app,
        &mut issue_updates,
        &mut daemon_state_updates,
        &mut log_updates,
        &mut dependency_graph_updates,
    );
    restore_terminal(&mut terminal)?;
    result
}

fn run_loop(
    terminal: &mut Terminal<CrosstermBackend<io::Stdout>>,
    app: &mut DeckApp,
    issue_updates: &mut Option<Receiver<Vec<ProjectIssue>>>,
    daemon_state_updates: &mut Option<Receiver<DaemonState>>,
    log_updates: &mut Option<Receiver<String>>,
    dependency_graph_updates: &mut Option<Receiver<String>>,
) -> Result<()> {
    loop {
        if let Some(receiver) = issue_updates.as_ref() {
            loop {
                match receiver.try_recv() {
                    Ok(next_issues) => app.replace_issues(next_issues),
                    Err(TryRecvError::Empty) => break,
                    Err(TryRecvError::Disconnected) => {
                        app.daemon_state = DaemonState::Disconnected;
                        *issue_updates = None;
                        break;
                    }
                }
            }
        }

        if let Some(receiver) = daemon_state_updates.as_ref() {
            loop {
                match receiver.try_recv() {
                    Ok(next_state) => app.daemon_state = next_state,
                    Err(TryRecvError::Empty) => break,
                    Err(TryRecvError::Disconnected) => {
                        app.daemon_state = DaemonState::Disconnected;
                        *daemon_state_updates = None;
                        break;
                    }
                }
            }
        }

        if let Some(receiver) = log_updates.as_ref() {
            loop {
                match receiver.try_recv() {
                    Ok(entry) => app.push_log(entry),
                    Err(TryRecvError::Empty) => break,
                    Err(TryRecvError::Disconnected) => {
                        app.push_log("log stream disconnected".to_owned());
                        *log_updates = None;
                        break;
                    }
                }
            }
        }

        if let Some(receiver) = dependency_graph_updates.as_ref() {
            loop {
                match receiver.try_recv() {
                    Ok(graph) => app.set_dependency_graph(graph),
                    Err(TryRecvError::Empty) => break,
                    Err(TryRecvError::Disconnected) => {
                        *dependency_graph_updates = None;
                        break;
                    }
                }
            }
        }

        terminal
            .draw(|frame| draw(frame.area(), frame, app))
            .map_err(|error| {
                holo_message_error!("failed to draw deck frame").with_std_source(error)
            })?;

        if !event::poll(Duration::from_millis(250)).map_err(|error| {
            holo_message_error!("failed to poll input events").with_std_source(error)
        })? {
            continue;
        }

        let Event::Key(key_event) = event::read().map_err(|error| {
            holo_message_error!("failed to read input event").with_std_source(error)
        })?
        else {
            continue;
        };

        if !is_navigable_key_event(&key_event) {
            continue;
        }

        match key_event.code {
            KeyCode::Esc | KeyCode::Char('q') => return Ok(()),
            KeyCode::Left | KeyCode::Char('h') => app.select_previous_tab(),
            KeyCode::Right | KeyCode::Char('l') => app.select_next_tab(),
            KeyCode::Down | KeyCode::Char('j') => {
                if app.active_tab == DeckTab::Issues {
                    app.select_next();
                }
            }
            KeyCode::Up | KeyCode::Char('k') => {
                if app.active_tab == DeckTab::Issues {
                    app.select_previous();
                }
            }
            _ => {}
        }
    }
}

fn restore_terminal(terminal: &mut Terminal<CrosstermBackend<io::Stdout>>) -> Result<()> {
    disable_raw_mode().map_err(|error| {
        holo_message_error!("failed to disable raw mode").with_std_source(error)
    })?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen).map_err(|error| {
        holo_message_error!("failed to leave alternate screen").with_std_source(error)
    })?;
    terminal
        .show_cursor()
        .map_err(|error| holo_message_error!("failed to show cursor").with_std_source(error))
}

fn draw(area: Rect, frame: &mut ratatui::Frame<'_>, app: &mut DeckApp) {
    let rows = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(1),
            Constraint::Min(1),
            Constraint::Length(1),
        ])
        .split(area);

    draw_tabs(rows[0], frame, app);
    match app.active_tab {
        DeckTab::Issues => draw_issues(rows[1], frame, app),
        DeckTab::Log => draw_log(rows[1], frame, app),
        DeckTab::DependencyGraph => draw_dependency_graph(rows[1], frame, app),
    }
    draw_daemon_status(rows[2], frame, app);
}

fn draw_tabs(area: Rect, frame: &mut ratatui::Frame<'_>, app: &DeckApp) {
    let titles = DeckTab::titles()
        .iter()
        .map(|title| Line::from(*title))
        .collect::<Vec<_>>();
    let tabs = Tabs::new(titles)
        .highlight_style(
            Style::default()
                .fg(Color::Cyan)
                .add_modifier(Modifier::BOLD),
        )
        .select(app.active_tab.as_index());
    frame.render_widget(tabs, area);
}

fn draw_issues(area: Rect, frame: &mut ratatui::Frame<'_>, app: &mut DeckApp) {
    let chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(45), Constraint::Percentage(55)])
        .split(area);

    draw_master(chunks[0], frame, app);
    draw_detail(chunks[1], frame, app);
}

fn draw_master(area: Rect, frame: &mut ratatui::Frame<'_>, app: &mut DeckApp) {
    let items = app
        .issues
        .iter()
        .map(|issue| {
            let severity_style = match issue.severity {
                ProjectIssueSeverity::Error => {
                    Style::default().fg(Color::Red).add_modifier(Modifier::BOLD)
                }
                ProjectIssueSeverity::Warning => Style::default().fg(Color::Yellow),
            };
            ListItem::new(vec![Line::from(vec![
                Span::styled(
                    format!("[{}] ", issue.kind.short_tag()),
                    Style::default().fg(Color::Cyan),
                ),
                Span::styled(format!("[{}] ", issue.severity.label()), severity_style),
                Span::raw(&issue.title),
            ])])
        })
        .collect::<Vec<_>>();

    let list = List::new(items)
        .block(
            Block::default()
                .title("Issues (Master)")
                .borders(Borders::ALL),
        )
        .highlight_style(
            Style::default()
                .fg(Color::Black)
                .bg(Color::White)
                .add_modifier(Modifier::BOLD),
        );

    let mut state = ListState::default();
    state.select(Some(app.selected));
    frame.render_stateful_widget(list, area, &mut state);
}

fn draw_detail(area: Rect, frame: &mut ratatui::Frame<'_>, app: &DeckApp) {
    let Some(issue) = app.current_issue() else {
        let empty = Paragraph::new("No issue selected")
            .block(Block::default().title("Details").borders(Borders::ALL));
        frame.render_widget(empty, area);
        return;
    };

    let body = vec![
        Line::from(vec![
            Span::styled("Title: ", Style::default().add_modifier(Modifier::BOLD)),
            Span::raw(&issue.title),
        ]),
        Line::from(vec![
            Span::styled("Kind: ", Style::default().add_modifier(Modifier::BOLD)),
            Span::raw(issue.kind.label()),
        ]),
        Line::from(vec![
            Span::styled("Severity: ", Style::default().add_modifier(Modifier::BOLD)),
            Span::raw(issue.severity.label()),
        ]),
        Line::from(vec![
            Span::styled("Location: ", Style::default().add_modifier(Modifier::BOLD)),
            Span::raw(format!("{}:{}", issue.file, issue.line)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("Summary: ", Style::default().add_modifier(Modifier::BOLD)),
            Span::raw(&issue.summary),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("Detail: ", Style::default().add_modifier(Modifier::BOLD)),
            Span::raw(&issue.detail),
        ]),
        Line::from(""),
        Line::from(Span::styled(
            "Keys: left/right tab, up/down issue, q/esc quit",
            Style::default().fg(Color::DarkGray),
        )),
    ];

    let detail = Paragraph::new(body)
        .block(
            Block::default()
                .title("Issue Detail (Detail)")
                .borders(Borders::ALL),
        )
        .wrap(Wrap { trim: false });
    frame.render_widget(detail, area);
}

fn draw_log(area: Rect, frame: &mut ratatui::Frame<'_>, app: &DeckApp) {
    let items = app
        .logs
        .iter()
        .rev()
        .take(area.height.saturating_sub(2) as usize)
        .rev()
        .map(|entry| ListItem::new(entry.as_str()))
        .collect::<Vec<_>>();
    let list = List::new(items).block(Block::default().title("Daemon Log").borders(Borders::ALL));
    frame.render_widget(list, area);
}

fn draw_dependency_graph(area: Rect, frame: &mut ratatui::Frame<'_>, app: &DeckApp) {
    let graph = Paragraph::new(app.dependency_graph.as_str())
        .block(
            Block::default()
                .title("Dependency Graph")
                .borders(Borders::ALL),
        )
        .wrap(Wrap { trim: false });
    frame.render_widget(graph, area);
}

fn is_navigable_key_event(key_event: &KeyEvent) -> bool {
    key_event.kind == KeyEventKind::Press
}

fn draw_daemon_status(area: Rect, frame: &mut ratatui::Frame<'_>, app: &DeckApp) {
    let (icon, label, color) = app.daemon_state.display_parts();
    let (compilation_failures, test_failures) = app.failure_counts();
    let status = Paragraph::new(Line::from(vec![
        Span::styled(icon, Style::default().fg(color)),
        Span::raw(" "),
        Span::styled(label, Style::default().fg(color)),
        Span::raw(" "),
        Span::styled(
            format!("C:{compilation_failures} T:{test_failures}"),
            Style::default().fg(Color::DarkGray),
        ),
    ]))
    .alignment(Alignment::Right);
    frame.render_widget(status, area);
}

fn example_issues() -> Vec<ProjectIssue> {
    vec![
        ProjectIssue {
            title: "Unknown token in test declaration".to_owned(),
            file: "src/compiler/lexer.holo".to_owned(),
            line: 12,
            kind: ProjectIssueKind::Compilation,
            severity: ProjectIssueSeverity::Error,
            summary: "Lexer hit an unsupported symbol while parsing #[test] item.".to_owned(),
            detail: "The parser expected `fn` after `#[test]`, but found `fna`. Fix the typo and rerun.".to_owned(),
        },
        ProjectIssue {
            title: "Duplicate test name".to_owned(),
            file: "tests/smoke.holo".to_owned(),
            line: 34,
            kind: ProjectIssueKind::Compilation,
            severity: ProjectIssueSeverity::Error,
            summary: "Two tests share the same function name.".to_owned(),
            detail: "Rename either `fn startup_checks()` definition so each test item has a unique name.".to_owned(),
        },
        ProjectIssue {
            title: "Assertion failed in login flow".to_owned(),
            file: "tests/auth.holo".to_owned(),
            line: 21,
            kind: ProjectIssueKind::Test,
            severity: ProjectIssueSeverity::Error,
            summary: "Test `login_valid_credentials` evaluated to false.".to_owned(),
            detail: "The test body executed `assert(false)`. Replace with expected boolean expression.".to_owned(),
        },
        ProjectIssue {
            title: "Flaky startup timing test".to_owned(),
            file: "tests/startup.holo".to_owned(),
            line: 9,
            kind: ProjectIssueKind::Test,
            severity: ProjectIssueSeverity::Warning,
            summary: "Intermittent failures detected over last 20 runs.".to_owned(),
            detail:
                "This warning is example data to demonstrate non-fatal test diagnostics in deck."
                    .to_owned(),
        },
    ]
}

fn example_logs() -> Vec<String> {
    vec![
        "daemon started".to_owned(),
        "parsing startup sources".to_owned(),
        "typechecking startup sources".to_owned(),
        "running tests".to_owned(),
    ]
}

fn example_dependency_graph() -> String {
    [
        "pipeline:",
        "  source.holo -> lexer -> parser -> typechecker -> interpreter",
    ]
    .join("\n")
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DeckApp {
    issues: Vec<ProjectIssue>,
    selected: usize,
    daemon_state: DaemonState,
    logs: Vec<String>,
    dependency_graph: String,
    active_tab: DeckTab,
}

impl DeckApp {
    fn new(issues: Vec<ProjectIssue>) -> Self {
        Self {
            issues,
            selected: 0,
            daemon_state: DaemonState::Disconnected,
            logs: example_logs(),
            dependency_graph: example_dependency_graph(),
            active_tab: DeckTab::Issues,
        }
    }

    #[cfg(test)]
    fn with_example_data() -> Self {
        Self::new(example_issues())
    }

    fn replace_issues(&mut self, issues: Vec<ProjectIssue>) {
        self.issues = issues;
        if self.issues.is_empty() {
            self.selected = 0;
        } else if self.selected >= self.issues.len() {
            self.selected = self.issues.len() - 1;
        }
    }

    fn select_next(&mut self) {
        if self.issues.is_empty() {
            self.selected = 0;
            return;
        }
        self.selected = (self.selected + 1) % self.issues.len();
    }

    fn select_previous(&mut self) {
        if self.issues.is_empty() {
            self.selected = 0;
            return;
        }
        if self.selected == 0 {
            self.selected = self.issues.len() - 1;
        } else {
            self.selected -= 1;
        }
    }

    fn select_next_tab(&mut self) {
        self.active_tab = self.active_tab.next();
    }

    fn select_previous_tab(&mut self) {
        self.active_tab = self.active_tab.previous();
    }

    fn current_issue(&self) -> Option<&ProjectIssue> {
        self.issues.get(self.selected)
    }

    fn push_log(&mut self, entry: String) {
        self.logs.push(entry);
        if self.logs.len() > MAX_LOG_ENTRIES {
            let remove_count = self.logs.len() - MAX_LOG_ENTRIES;
            self.logs.drain(0..remove_count);
        }
    }

    fn set_dependency_graph(&mut self, graph: String) {
        self.dependency_graph = graph;
    }

    fn failure_counts(&self) -> (usize, usize) {
        let mut compilation_failures = 0usize;
        let mut test_failures = 0usize;
        for issue in &self.issues {
            if issue.severity != ProjectIssueSeverity::Error {
                continue;
            }
            match issue.kind {
                ProjectIssueKind::Compilation => compilation_failures += 1,
                ProjectIssueKind::Test => test_failures += 1,
            }
        }
        (compilation_failures, test_failures)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DeckTab {
    Issues,
    Log,
    DependencyGraph,
}

impl DeckTab {
    fn titles() -> [&'static str; 3] {
        ["Issues", "Log", "Dependency Graph"]
    }

    fn as_index(self) -> usize {
        match self {
            Self::Issues => 0,
            Self::Log => 1,
            Self::DependencyGraph => 2,
        }
    }

    fn next(self) -> Self {
        match self {
            Self::Issues => Self::Log,
            Self::Log => Self::DependencyGraph,
            Self::DependencyGraph => Self::Issues,
        }
    }

    fn previous(self) -> Self {
        match self {
            Self::Issues => Self::DependencyGraph,
            Self::Log => Self::Issues,
            Self::DependencyGraph => Self::Log,
        }
    }
}

/// Daemon connection/activity state shown in deck.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DaemonState {
    Disconnected,
    Green,
    Compiling,
    Failed,
}

impl DaemonState {
    fn display_parts(self) -> (&'static str, &'static str, Color) {
        match self {
            Self::Disconnected => ("●", "Disconnected", Color::DarkGray),
            Self::Green => ("●", "Green", Color::Green),
            Self::Compiling => ("●", "Compiling", Color::Yellow),
            Self::Failed => ("●", "Failed", Color::Red),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        example_issues, is_navigable_key_event, DaemonState, DeckApp, DeckTab, ProjectIssueKind,
        ProjectIssueSeverity, MAX_LOG_ENTRIES,
    };
    use crossterm::event::{KeyCode, KeyEvent, KeyEventKind, KeyEventState, KeyModifiers};

    #[test]
    fn example_data_contains_compilation_and_test_issues() {
        let app = DeckApp::with_example_data();
        assert!(app
            .issues
            .iter()
            .any(|issue| issue.kind == ProjectIssueKind::Compilation));
        assert!(app
            .issues
            .iter()
            .any(|issue| issue.kind == ProjectIssueKind::Test));
    }

    #[test]
    fn navigation_wraps_in_both_directions() {
        let mut app = DeckApp::with_example_data();
        app.selected = app.issues.len() - 1;
        app.select_next();
        assert_eq!(app.selected, 0);

        app.select_previous();
        assert_eq!(app.selected, app.issues.len() - 1);
    }

    #[test]
    fn example_data_includes_warning_and_error() {
        let app = DeckApp::with_example_data();
        assert!(app
            .issues
            .iter()
            .any(|issue| issue.severity == ProjectIssueSeverity::Error));
        assert!(app
            .issues
            .iter()
            .any(|issue| issue.severity == ProjectIssueSeverity::Warning));
    }

    #[test]
    fn only_press_key_events_trigger_navigation() {
        let press = KeyEvent {
            code: KeyCode::Down,
            modifiers: KeyModifiers::empty(),
            kind: KeyEventKind::Press,
            state: KeyEventState::empty(),
        };
        let release = KeyEvent {
            code: KeyCode::Down,
            modifiers: KeyModifiers::empty(),
            kind: KeyEventKind::Release,
            state: KeyEventState::empty(),
        };
        let repeat = KeyEvent {
            code: KeyCode::Down,
            modifiers: KeyModifiers::empty(),
            kind: KeyEventKind::Repeat,
            state: KeyEventState::empty(),
        };

        assert!(is_navigable_key_event(&press));
        assert!(!is_navigable_key_event(&release));
        assert!(!is_navigable_key_event(&repeat));
    }

    #[test]
    fn replacing_issues_clamps_selection() {
        let mut app = DeckApp::with_example_data();
        app.selected = 3;
        app.replace_issues(vec![example_issues()[0].clone()]);
        assert_eq!(app.selected, 0);
    }

    #[test]
    fn daemon_state_has_expected_labels() {
        assert_eq!(DaemonState::Disconnected.display_parts().1, "Disconnected");
        assert_eq!(DaemonState::Green.display_parts().1, "Green");
        assert_eq!(DaemonState::Compiling.display_parts().1, "Compiling");
        assert_eq!(DaemonState::Failed.display_parts().1, "Failed");
    }

    #[test]
    fn failure_counts_include_only_error_issues() {
        let app = DeckApp::with_example_data();
        let (compilation, test) = app.failure_counts();
        assert_eq!(compilation, 2);
        assert_eq!(test, 1);
    }

    #[test]
    fn tab_navigation_wraps() {
        let mut app = DeckApp::with_example_data();
        assert_eq!(app.active_tab, DeckTab::Issues);

        app.select_next_tab();
        assert_eq!(app.active_tab, DeckTab::Log);
        app.select_next_tab();
        assert_eq!(app.active_tab, DeckTab::DependencyGraph);
        app.select_next_tab();
        assert_eq!(app.active_tab, DeckTab::Issues);

        app.select_previous_tab();
        assert_eq!(app.active_tab, DeckTab::DependencyGraph);
    }

    #[test]
    fn log_entries_are_capped() {
        let mut app = DeckApp::with_example_data();
        app.logs.clear();
        for index in 0..(MAX_LOG_ENTRIES + 5) {
            app.push_log(format!("entry-{index}"));
        }
        assert_eq!(app.logs.len(), MAX_LOG_ENTRIES);
        assert_eq!(app.logs.first().map(String::as_str), Some("entry-5"));
    }
}
