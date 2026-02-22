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
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, List, ListItem, ListState, Paragraph, Wrap};
use ratatui::Terminal;

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
    run_with_updates(example_issues(), None)
}

/// Opens deck TUI with initial issues and optional live updates.
pub fn run_with_updates(
    initial_issues: Vec<ProjectIssue>,
    updates: Option<Receiver<Vec<ProjectIssue>>>,
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
    let mut updates = updates;
    let result = run_loop(&mut terminal, &mut app, &mut updates);
    restore_terminal(&mut terminal)?;
    result
}

fn run_loop(
    terminal: &mut Terminal<CrosstermBackend<io::Stdout>>,
    app: &mut DeckApp,
    updates: &mut Option<Receiver<Vec<ProjectIssue>>>,
) -> Result<()> {
    loop {
        if let Some(receiver) = updates.as_ref() {
            loop {
                match receiver.try_recv() {
                    Ok(next_issues) => app.replace_issues(next_issues),
                    Err(TryRecvError::Empty) => break,
                    Err(TryRecvError::Disconnected) => {
                        *updates = None;
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
            KeyCode::Down | KeyCode::Char('j') => app.select_next(),
            KeyCode::Up | KeyCode::Char('k') => app.select_previous(),
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
            "Keys: ↑/k previous, ↓/j next, q/esc quit",
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

fn is_navigable_key_event(key_event: &KeyEvent) -> bool {
    key_event.kind == KeyEventKind::Press
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

#[derive(Debug, Clone, PartialEq, Eq)]
struct DeckApp {
    issues: Vec<ProjectIssue>,
    selected: usize,
}

impl DeckApp {
    fn new(issues: Vec<ProjectIssue>) -> Self {
        Self {
            issues,
            selected: 0,
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

    fn current_issue(&self) -> Option<&ProjectIssue> {
        self.issues.get(self.selected)
    }
}

#[cfg(test)]
mod tests {
    use super::{
        example_issues, is_navigable_key_event, DeckApp, ProjectIssueKind, ProjectIssueSeverity,
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
}
