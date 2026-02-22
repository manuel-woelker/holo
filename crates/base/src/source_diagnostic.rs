use crate::{SharedString, Span};
use serde::{Deserialize, Serialize};

/// Compiler stage associated with a source diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum DiagnosticKind {
    /// Produced while lexing source text into tokens.
    Lexing,
    /// Produced while parsing tokens into syntax nodes.
    Parsing,
    /// Produced while checking semantic/type constraints.
    Typecheck,
    /// Produced while running tests.
    Test,
}

/// Span annotation attached to a source diagnostic.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct AnnotatedSpan {
    /// Source range this annotation points to.
    pub span: Span,
    /// Annotation message for the span.
    pub message: SharedString,
}

impl AnnotatedSpan {
    /// Creates a new annotated span.
    pub fn new(span: Span, message: impl Into<SharedString>) -> Self {
        Self {
            span,
            message: message.into(),
        }
    }
}

/// Source snippet metadata used to render diagnostics with context.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SourceExcerpt {
    /// Source text content used for rendering context lines.
    pub source: SharedString,
    /// 1-based line number corresponding to byte offset `starting_offset`.
    pub starting_line: usize,
    /// Absolute byte offset represented by the beginning of `source`.
    pub starting_offset: usize,
}

impl SourceExcerpt {
    /// Creates a new source excerpt.
    pub fn new(
        source: impl Into<SharedString>,
        starting_line: usize,
        starting_offset: usize,
    ) -> Self {
        Self {
            source: source.into(),
            starting_line,
            starting_offset,
        }
    }
}

/// Structured source-level compilation diagnostic.
///
/// Use this type to describe compilation errors (lexing/parsing/typecheck)
/// with precise span annotations.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceDiagnostic {
    /// Human-readable primary message for the diagnostic.
    pub message: SharedString,
    /// Compilation stage that produced this diagnostic.
    pub kind: DiagnosticKind,
    /// Source span annotations associated with this diagnostic.
    pub annotated_spans: Vec<AnnotatedSpan>,
    /// Source excerpts used to render annotated diagnostics.
    pub source_excerpts: Vec<SourceExcerpt>,
}

impl SourceDiagnostic {
    /// Creates a new source diagnostic without annotations.
    pub fn new(kind: DiagnosticKind, message: impl Into<SharedString>) -> Self {
        Self {
            message: message.into(),
            kind,
            annotated_spans: Vec::new(),
            source_excerpts: Vec::new(),
        }
    }

    /// Appends one annotated span and returns the updated diagnostic.
    pub fn with_annotated_span(mut self, span: Span, message: impl Into<SharedString>) -> Self {
        self.annotated_spans.push(AnnotatedSpan::new(span, message));
        self
    }

    /// Attaches a source excerpt for contextual rendering.
    pub fn with_source_excerpt(mut self, excerpt: SourceExcerpt) -> Self {
        self.source_excerpts.push(excerpt);
        self
    }

    /// Renders the diagnostic as a human-readable annotated message.
    pub fn render_annotated(&self) -> SharedString {
        let mut output = String::new();
        output.push_str(&format!("{}: {}", self.kind.label(), self.message));

        if self.annotated_spans.is_empty() {
            return output.into();
        }

        for annotation in &self.annotated_spans {
            output.push('\n');
            if let Some(rendered) = self
                .source_excerpts
                .iter()
                .find_map(|excerpt| render_annotation_with_excerpt(annotation, excerpt))
            {
                output.push_str(&rendered);
                continue;
            }
            output.push_str(&format!(
                "at bytes {}..{}: {}",
                annotation.span.start, annotation.span.end, annotation.message
            ));
        }

        output.into()
    }
}

/// Renders diagnostics with ANSI colors and Unicode drawing characters.
///
/// This output is intended for terminal UIs and CLI output.
pub fn display_source_diagnostics(diagnostics: &[SourceDiagnostic]) -> SharedString {
    let mut output = String::new();

    for (index, diagnostic) in diagnostics.iter().enumerate() {
        if index > 0 {
            output.push('\n');
        }

        let (kind_color, kind_label) = diagnostic.kind.ansi_label();
        let marker = diagnostic.kind.marker();
        output.push_str(&format!(
            "{bold}{color}{marker} {label}{reset}{bold}: {reset}{message}\n",
            bold = ANSI_BOLD,
            color = kind_color,
            marker = marker,
            label = kind_label,
            reset = ANSI_RESET,
            message = diagnostic.message,
        ));

        if diagnostic.annotated_spans.is_empty() {
            continue;
        }

        for annotation in &diagnostic.annotated_spans {
            if let Some((line_no, line_text, caret_offset, caret_len)) = diagnostic
                .source_excerpts
                .iter()
                .find_map(|excerpt| render_info_with_excerpt(annotation, excerpt))
            {
                output.push('\n');
                output.push_str(&format!(
                    "{blue}{line:>4} │{reset} {source}\n",
                    blue = ANSI_BLUE,
                    reset = ANSI_RESET,
                    line = line_no,
                    source = line_text,
                ));
                output.push_str(&format!(
                    "     │ {red}{spacing}{carets}{reset} {annotation}\n",
                    red = ANSI_RED,
                    reset = ANSI_RESET,
                    spacing = " ".repeat(caret_offset),
                    carets = "┄".repeat(caret_len),
                    annotation = annotation.message,
                ));
                continue;
            }

            output.push_str(&format!(
                "{dim}└─{reset} at bytes {start}..{end}: {annotation}\n",
                dim = ANSI_DIM,
                reset = ANSI_RESET,
                start = annotation.span.start,
                end = annotation.span.end,
                annotation = annotation.message,
            ));
        }
    }

    output.into()
}

impl DiagnosticKind {
    fn marker(self) -> &'static str {
        match self {
            Self::Lexing | Self::Parsing | Self::Typecheck => "⚒️",
            Self::Test => "🧪",
        }
    }

    fn label(self) -> &'static str {
        match self {
            Self::Lexing => "lexing error",
            Self::Parsing => "parsing error",
            Self::Typecheck => "typecheck error",
            Self::Test => "test failure",
        }
    }

    fn ansi_label(self) -> (&'static str, &'static str) {
        match self {
            Self::Lexing => (ANSI_YELLOW, "Lexing"),
            Self::Parsing => (ANSI_RED, "Parsing"),
            Self::Typecheck => (ANSI_MAGENTA, "Typecheck"),
            Self::Test => (ANSI_RED, "Test"),
        }
    }
}

fn render_annotation_with_excerpt(
    annotation: &AnnotatedSpan,
    excerpt: &SourceExcerpt,
) -> Option<String> {
    let (line_no, line_start, line_text) = find_line_for_offset(excerpt, annotation.span.start)?;
    let start = annotation.span.start.max(line_start);
    let end = annotation.span.end.max(start + 1);
    let line_end = line_start + line_text.len();
    let caret_start = start.saturating_sub(line_start);
    let caret_end = end.min(line_end).max(start + 1);
    let caret_len = caret_end.saturating_sub(start).max(1);

    let spacing = " ".repeat(caret_start);
    let carets = "^".repeat(caret_len);

    Some(format!(
        "--> line {}, column {}\n{:>4} | {}\n     | {}{} {}",
        line_no,
        caret_start + 1,
        line_no,
        line_text,
        spacing,
        carets,
        annotation.message
    ))
}

fn render_info_with_excerpt(
    annotation: &AnnotatedSpan,
    excerpt: &SourceExcerpt,
) -> Option<(usize, String, usize, usize)> {
    let (line_no, line_start, line_text) = find_line_for_offset(excerpt, annotation.span.start)?;
    let start = annotation.span.start.max(line_start);
    let end = annotation.span.end.max(start + 1);
    let line_end = line_start + line_text.len();
    let caret_start = start.saturating_sub(line_start);
    let caret_end = end.min(line_end).max(start + 1);
    let caret_len = caret_end.saturating_sub(start).max(1);
    Some((line_no, line_text, caret_start, caret_len))
}

const ANSI_RESET: &str = "\x1b[0m";
const ANSI_BOLD: &str = "\x1b[1m";
const ANSI_DIM: &str = "\x1b[2m";
const ANSI_RED: &str = "\x1b[31m";
const ANSI_YELLOW: &str = "\x1b[33m";
const ANSI_BLUE: &str = "\x1b[34m";
const ANSI_MAGENTA: &str = "\x1b[35m";

fn find_line_for_offset(excerpt: &SourceExcerpt, offset: usize) -> Option<(usize, usize, String)> {
    if offset < excerpt.starting_offset {
        return None;
    }

    let mut absolute_start = excerpt.starting_offset;
    for (index, raw_line) in excerpt.source.split('\n').enumerate() {
        let line = raw_line.trim_end_matches('\r');
        let line_start = absolute_start;
        let line_end = line_start + line.len();
        if offset >= line_start && offset <= line_end {
            return Some((excerpt.starting_line + index, line_start, line.to_owned()));
        }
        absolute_start += raw_line.len() + 1;
    }

    None
}

#[cfg(test)]
mod tests {
    use super::{display_source_diagnostics, DiagnosticKind, SourceDiagnostic, SourceExcerpt};
    use crate::Span;

    #[test]
    fn creates_source_diagnostic_with_annotations() {
        let diagnostic = SourceDiagnostic::new(DiagnosticKind::Parsing, "expected ')'")
            .with_annotated_span(Span::new(4, 5), "missing closing parenthesis");

        assert_eq!(diagnostic.kind, DiagnosticKind::Parsing);
        assert_eq!(diagnostic.message, "expected ')'");
        assert_eq!(diagnostic.annotated_spans.len(), 1);
        assert_eq!(
            diagnostic.annotated_spans[0].message,
            "missing closing parenthesis"
        );
        assert_eq!(diagnostic.annotated_spans[0].span, Span::new(4, 5));
    }

    #[test]
    fn renders_annotation_with_source_excerpt() {
        let diagnostic = SourceDiagnostic::new(DiagnosticKind::Parsing, "expected ')'")
            .with_source_excerpt(SourceExcerpt::new("assert(true;\n", 10, 120))
            .with_annotated_span(Span::new(126, 127), "missing closing parenthesis");
        let rendered = diagnostic.render_annotated();

        assert!(rendered.contains("parsing error: expected ')'"));
        assert!(rendered.contains("--> line 10, column 7"));
        assert!(rendered.contains("10 | assert(true;"));
        assert!(rendered.contains("missing closing parenthesis"));
    }

    #[test]
    fn displays_diagnostics_with_unicode_and_ansi() {
        let diagnostics =
            vec![
                SourceDiagnostic::new(DiagnosticKind::Parsing, "expected expression")
                    .with_source_excerpt(SourceExcerpt::new("assert();\n", 20, 300))
                    .with_annotated_span(Span::new(307, 308), "missing expression"),
            ];
        let rendered = display_source_diagnostics(&diagnostics);

        assert!(rendered.contains("⚒️ Parsing"));
        assert!(!rendered.contains("--> line"));
        assert!(!rendered.contains("├─"));
        assert!(rendered.contains(" │ "));
        assert!(rendered.contains("assert();"));
        assert!(rendered.contains("┄"));
        assert!(rendered.contains("\u{1b}[31m"));
        assert!(rendered.contains("\u{1b}[1m"));
    }
}
