//! Terminal dashboard for viewing project compilation and test issues.

use std::io;
use std::sync::mpsc::{Receiver, TryRecvError};
use std::time::Duration;

use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyEventKind};
use crossterm::execute;
use crossterm::terminal::{
    disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
};
use holo_base::{
    display_source_diagnostics, holo_message_error, Result, SharedString, SourceDiagnostic,
};
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
    pub title: SharedString,
    /// Source file path.
    pub file: SharedString,
    /// 1-based line number.
    pub line: usize,
    /// Issue kind/category.
    pub kind: ProjectIssueKind,
    /// Severity level.
    pub severity: ProjectIssueSeverity,
    /// Brief summary text.
    pub summary: SharedString,
    /// Detailed explanation text.
    pub detail: SharedString,
    /// Structured source diagnostics associated with this issue.
    pub source_diagnostics: Vec<SourceDiagnostic>,
}

/// Issue category shown in deck.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProjectIssueKind {
    Compilation,
    Test,
}

/// Issue severity displayed in deck.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProjectIssueSeverity {
    Error,
    Warning,
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
    log_updates: Option<Receiver<SharedString>>,
    dependency_graph_updates: Option<Receiver<SharedString>>,
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
    log_updates: &mut Option<Receiver<SharedString>>,
    dependency_graph_updates: &mut Option<Receiver<SharedString>>,
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
                        app.push_log("log stream disconnected".into());
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
                } else if app.active_tab == DeckTab::DependencyGraph {
                    app.select_next_dependency_node();
                }
            }
            KeyCode::Up | KeyCode::Char('k') => {
                if app.active_tab == DeckTab::Issues {
                    app.select_previous();
                } else if app.active_tab == DeckTab::DependencyGraph {
                    app.select_previous_dependency_node();
                }
            }
            KeyCode::Enter => {
                if app.active_tab == DeckTab::DependencyGraph {
                    app.toggle_selected_dependency_node();
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
        .map(|issue| ListItem::new(vec![Line::from(Span::raw(issue.title.as_str()))]))
        .collect::<Vec<_>>();

    let list = List::new(items)
        .block(Block::default().title("Issues").borders(Borders::ALL))
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

    let rendered_diagnostics = if issue.source_diagnostics.is_empty() {
        issue.detail.clone()
    } else {
        strip_ansi_sequences(&display_source_diagnostics(&issue.source_diagnostics))
    };

    let mut body = Vec::new();
    for line in rendered_diagnostics.lines() {
        body.push(Line::from(line.to_owned()));
    }
    body.push(Line::from(""));
    body.push(Line::from(Span::styled(
        "Keys: left/right tab, up/down navigate, enter expand tree, q/esc quit",
        Style::default().fg(Color::DarkGray),
    )));

    let detail = Paragraph::new(body)
        .block(Block::default().title("Diagnostics").borders(Borders::ALL))
        .wrap(Wrap { trim: false });
    frame.render_widget(detail, area);
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
    let visible_nodes = app.visible_dependency_nodes();
    if visible_nodes.is_empty() {
        let empty = Paragraph::new("No dependency graph available").block(
            Block::default()
                .title("Dependency Graph")
                .borders(Borders::ALL),
        );
        frame.render_widget(empty, area);
        return;
    }

    let items = visible_nodes
        .iter()
        .enumerate()
        .map(|(index, node)| {
            let pointer = if index == app.dependency_selected {
                "▶"
            } else {
                " "
            };
            let branch = match (node.connector, node.has_children, node.expanded) {
                (Some(connector), true, true) => format!("{connector}▾"),
                (Some(connector), true, false) => format!("{connector}▸"),
                (Some(connector), false, _) => format!("{connector}─"),
                (None, true, true) => "▾".into(),
                (None, true, false) => "▸".into(),
                (None, false, _) => " ".into(),
            };
            let content = format!("{pointer}{}{} {}", node.prefix, branch, node.label);
            let mut style = Style::default();
            if node.depth <= 1 {
                style = style.add_modifier(Modifier::BOLD);
            }
            ListItem::new(Line::from(Span::styled(content, style)))
        })
        .collect::<Vec<_>>();
    let list = List::new(items)
        .block(
            Block::default()
                .title("Dependency Graph")
                .borders(Borders::ALL),
        )
        .highlight_style(
            Style::default()
                .fg(Color::Black)
                .bg(Color::White)
                .add_modifier(Modifier::BOLD),
        );
    let mut state = ListState::default();
    state.select(Some(app.dependency_selected));
    frame.render_stateful_widget(list, area, &mut state);
}

fn is_navigable_key_event(key_event: &KeyEvent) -> bool {
    key_event.kind == KeyEventKind::Press
}

fn draw_daemon_status(area: Rect, frame: &mut ratatui::Frame<'_>, app: &DeckApp) {
    let version = format!("Version: {}", holo_base::project_revision());
    let version_widget = Paragraph::new(version)
        .style(Style::default().fg(Color::DarkGray))
        .alignment(Alignment::Left);
    frame.render_widget(version_widget, area);

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
            title: "Unknown token in test declaration".into(),
            file: "src/compiler/lexer.holo".into(),
            line: 12,
            kind: ProjectIssueKind::Compilation,
            severity: ProjectIssueSeverity::Error,
            summary: "Lexer hit an unsupported symbol while parsing #[test] item.".into(),
            detail: "The parser expected `fn` after `#[test]`, but found `fna`. Fix the typo and rerun.".into(),
            source_diagnostics: Vec::new(),
        },
        ProjectIssue {
            title: "Duplicate test name".into(),
            file: "tests/smoke.holo".into(),
            line: 34,
            kind: ProjectIssueKind::Compilation,
            severity: ProjectIssueSeverity::Error,
            summary: "Two tests share the same function name.".into(),
            detail: "Rename either `fn startup_checks()` definition so each test item has a unique name.".into(),
            source_diagnostics: Vec::new(),
        },
        ProjectIssue {
            title: "Assertion failed in login flow".into(),
            file: "tests/auth.holo".into(),
            line: 21,
            kind: ProjectIssueKind::Test,
            severity: ProjectIssueSeverity::Error,
            summary: "Test `login_valid_credentials` evaluated to false.".into(),
            detail: "The test body executed `assert(false)`. Replace with expected boolean expression.".into(),
            source_diagnostics: Vec::new(),
        },
        ProjectIssue {
            title: "Flaky startup timing test".into(),
            file: "tests/startup.holo".into(),
            line: 9,
            kind: ProjectIssueKind::Test,
            severity: ProjectIssueSeverity::Warning,
            summary: "Intermittent failures detected over last 20 runs.".into(),
            detail:
                "This warning is example data to demonstrate non-fatal test diagnostics in deck."
                    .into(),
            source_diagnostics: Vec::new(),
        },
    ]
}

fn example_logs() -> Vec<SharedString> {
    vec![
        "daemon started".into(),
        "parsing startup sources".into(),
        "typechecking startup sources".into(),
        "running tests".into(),
    ]
}

fn example_dependency_graph() -> SharedString {
    [
        "Tests",
        "  login_valid_credentials",
        "    Input File: ./tests/auth.holo",
        "    Function: assert",
        "  startup_checks",
        "    Input File: ./tests/startup.holo",
        "    Function: assert",
    ]
    .join("\n")
    .into()
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DeckApp {
    issues: Vec<ProjectIssue>,
    selected: usize,
    daemon_state: DaemonState,
    logs: Vec<SharedString>,
    dependency_tree: Vec<DependencyNode>,
    dependency_selected: usize,
    active_tab: DeckTab,
}

impl DeckApp {
    fn new(issues: Vec<ProjectIssue>) -> Self {
        Self {
            issues,
            selected: 0,
            daemon_state: DaemonState::Disconnected,
            logs: example_logs(),
            dependency_tree: parse_dependency_tree(&example_dependency_graph()),
            dependency_selected: 0,
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

    fn push_log(&mut self, entry: SharedString) {
        self.logs.push(entry);
        if self.logs.len() > MAX_LOG_ENTRIES {
            let remove_count = self.logs.len() - MAX_LOG_ENTRIES;
            self.logs.drain(0..remove_count);
        }
    }

    fn set_dependency_graph(&mut self, graph: SharedString) {
        self.dependency_tree = parse_dependency_tree(&graph);
        let visible_len = self.visible_dependency_nodes().len();
        if visible_len == 0 {
            self.dependency_selected = 0;
        } else if self.dependency_selected >= visible_len {
            self.dependency_selected = visible_len - 1;
        }
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

    fn visible_dependency_nodes(&self) -> Vec<VisibleDependencyNode> {
        let mut visible = Vec::new();
        collect_visible_dependency_nodes(&self.dependency_tree, 0, &[], &mut visible);
        visible
    }

    fn select_next_dependency_node(&mut self) {
        let visible = self.visible_dependency_nodes();
        if visible.is_empty() {
            self.dependency_selected = 0;
            return;
        }
        self.dependency_selected = (self.dependency_selected + 1) % visible.len();
    }

    fn select_previous_dependency_node(&mut self) {
        let visible = self.visible_dependency_nodes();
        if visible.is_empty() {
            self.dependency_selected = 0;
            return;
        }
        if self.dependency_selected == 0 {
            self.dependency_selected = visible.len() - 1;
        } else {
            self.dependency_selected -= 1;
        }
    }

    fn toggle_selected_dependency_node(&mut self) {
        let visible = self.visible_dependency_nodes();
        let Some(selected) = visible.get(self.dependency_selected) else {
            return;
        };
        if !selected.has_children || selected.depth == 0 {
            return;
        }
        if let Some(node) = dependency_node_mut(&mut self.dependency_tree, &selected.path) {
            node.expanded = !node.expanded;
        }
        let visible_len = self.visible_dependency_nodes().len();
        if visible_len == 0 {
            self.dependency_selected = 0;
        } else if self.dependency_selected >= visible_len {
            self.dependency_selected = visible_len - 1;
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct DependencyNode {
    label: SharedString,
    children: Vec<DependencyNode>,
    expanded: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct VisibleDependencyNode {
    path: Vec<usize>,
    label: SharedString,
    depth: usize,
    prefix: SharedString,
    connector: Option<char>,
    has_children: bool,
    expanded: bool,
}

fn parse_dependency_tree(graph: &str) -> Vec<DependencyNode> {
    let mut roots = Vec::<DependencyNode>::new();
    let mut path_stack = Vec::<usize>::new();

    for line in graph.lines() {
        if line.trim().is_empty() {
            continue;
        }
        let indentation = line.chars().take_while(|ch| *ch == ' ').count();
        let depth = indentation / 2;
        let label = line.trim().into();
        let node = DependencyNode {
            label,
            children: Vec::new(),
            expanded: depth == 0,
        };

        while path_stack.len() > depth {
            path_stack.pop();
        }

        if path_stack.is_empty() {
            roots.push(node);
            path_stack.clear();
            path_stack.push(roots.len() - 1);
            continue;
        }

        if let Some(parent) = dependency_node_mut(&mut roots, &path_stack) {
            parent.children.push(node);
            path_stack.push(parent.children.len() - 1);
        } else {
            roots.push(node);
            path_stack.clear();
            path_stack.push(roots.len() - 1);
        }
    }

    roots
}

fn dependency_node_mut<'a>(
    roots: &'a mut [DependencyNode],
    path: &[usize],
) -> Option<&'a mut DependencyNode> {
    let (first, rest) = path.split_first()?;
    let mut node = roots.get_mut(*first)?;
    for index in rest {
        node = node.children.get_mut(*index)?;
    }
    Some(node)
}

fn collect_visible_dependency_nodes(
    nodes: &[DependencyNode],
    depth: usize,
    ancestor_has_more: &[bool],
    output: &mut Vec<VisibleDependencyNode>,
) {
    for (index, node) in nodes.iter().enumerate() {
        let is_last = index + 1 == nodes.len();
        collect_visible_dependency_node(
            node,
            vec![index],
            depth,
            is_last,
            ancestor_has_more,
            output,
        );
    }
}

fn collect_visible_dependency_node(
    node: &DependencyNode,
    path: Vec<usize>,
    depth: usize,
    is_last: bool,
    ancestor_has_more: &[bool],
    output: &mut Vec<VisibleDependencyNode>,
) {
    let mut prefix = SharedString::new();
    if depth > 0 {
        for has_more in ancestor_has_more {
            prefix.push_str(if *has_more { "│ " } else { "  " });
        }
    }

    output.push(VisibleDependencyNode {
        path: path.clone(),
        label: node.label.clone(),
        depth,
        prefix,
        connector: if depth == 0 {
            None
        } else if is_last {
            Some('└')
        } else {
            Some('├')
        },
        has_children: !node.children.is_empty(),
        expanded: node.expanded,
    });

    if !node.expanded {
        return;
    }

    let mut child_ancestor_has_more = ancestor_has_more.to_vec();
    if depth > 0 {
        child_ancestor_has_more.push(!is_last);
    }

    for (index, child) in node.children.iter().enumerate() {
        let child_is_last = index + 1 == node.children.len();
        let mut child_path = path.clone();
        child_path.push(index);
        collect_visible_dependency_node(
            child,
            child_path,
            depth + 1,
            child_is_last,
            &child_ancestor_has_more,
            output,
        );
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
        example_issues, is_navigable_key_event, parse_dependency_tree, DaemonState, DeckApp,
        DeckTab, ProjectIssueKind, ProjectIssueSeverity, MAX_LOG_ENTRIES,
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
            app.push_log(format!("entry-{index}").into());
        }
        assert_eq!(app.logs.len(), MAX_LOG_ENTRIES);
        assert_eq!(
            app.logs.first().map(|entry| entry.as_str()),
            Some("entry-5")
        );
    }

    #[test]
    fn dependency_tree_initially_expands_only_first_two_layers() {
        let graph = "Tests\n  sample_test\n    Input File: ./sample.holo\n    Function: assert";
        let tree = parse_dependency_tree(graph);
        let mut app = DeckApp::with_example_data();
        app.dependency_tree = tree;
        app.dependency_selected = 0;

        let visible = app.visible_dependency_nodes();
        assert_eq!(visible.len(), 2);
        assert_eq!(visible[0].label, "Tests");
        assert_eq!(visible[1].label, "sample_test");
    }

    #[test]
    fn dependency_tree_expands_children_with_enter_toggle() {
        let graph = "Tests\n  sample_test\n    Input File: ./sample.holo\n    Function: assert";
        let tree = parse_dependency_tree(graph);
        let mut app = DeckApp::with_example_data();
        app.dependency_tree = tree;
        app.dependency_selected = 1;

        app.toggle_selected_dependency_node();
        let visible = app.visible_dependency_nodes();
        assert!(visible
            .iter()
            .any(|node| node.label == "Input File: ./sample.holo"));
        assert!(visible.iter().any(|node| node.label == "Function: assert"));
    }
}
