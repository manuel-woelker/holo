use crate::{FilePath, SharedString, Span};
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
    /// Optional source file name/path for this excerpt.
    #[serde(default)]
    pub source_name: Option<FilePath>,
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
            source_name: None,
            source: source.into(),
            starting_line,
            starting_offset,
        }
    }

    /// Sets a source file name/path for this excerpt.
    pub fn with_source_name(mut self, source_name: impl Into<FilePath>) -> Self {
        self.source_name = Some(source_name.into());
        self
    }

    /// Sets or replaces source file name/path for this excerpt in place.
    pub fn set_source_name(&mut self, source_name: impl Into<FilePath>) {
        self.source_name = Some(source_name.into());
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

    /// Attaches a single source excerpt and multiple annotations associated with it.
    pub fn with_source_excerpt_annotations<I, M>(
        mut self,
        excerpt: SourceExcerpt,
        annotations: I,
    ) -> Self
    where
        I: IntoIterator<Item = (Span, M)>,
        M: Into<SharedString>,
    {
        self.source_excerpts.push(excerpt);
        self.annotated_spans.extend(
            annotations
                .into_iter()
                .map(|(span, message)| AnnotatedSpan::new(span, message)),
        );
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

        let mut unmatched_annotations: Vec<&AnnotatedSpan> = Vec::new();
        let mut rendered_by_line: Vec<DisplayRenderedAnnotation> = Vec::new();
        for annotation in &diagnostic.annotated_spans {
            if let Some((line_no, line_text, caret_offset, caret_len, source_name)) = diagnostic
                .source_excerpts
                .iter()
                .find_map(|excerpt| render_info_with_excerpt(annotation, excerpt))
            {
                rendered_by_line.push(DisplayRenderedAnnotation {
                    message: annotation.message.clone(),
                    line_no,
                    line_text,
                    caret_offset,
                    caret_len,
                    anchor_offset: caret_offset + caret_len.saturating_sub(1),
                    source_name,
                });
            } else {
                unmatched_annotations.push(annotation);
            }
        }

        rendered_by_line.sort_by_key(|annotation| (annotation.line_no, annotation.caret_offset));

        let mut current_group: Option<(usize, String, Option<FilePath>)> = None;
        let mut current_group_annotations: Vec<DisplayRenderedAnnotation> = Vec::new();
        for annotation in rendered_by_line {
            let group_key = (
                annotation.line_no,
                annotation.line_text.clone(),
                annotation.source_name.clone(),
            );
            if current_group.as_ref() != Some(&group_key) && !current_group_annotations.is_empty() {
                render_annotation_group(&mut output, &current_group_annotations);
                current_group_annotations.clear();
            }
            current_group = Some(group_key);
            current_group_annotations.push(annotation);
        }

        if !current_group_annotations.is_empty() {
            render_annotation_group(&mut output, &current_group_annotations);
        }

        for annotation in unmatched_annotations {
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
) -> Option<(usize, String, usize, usize, Option<FilePath>)> {
    let (line_no, line_start, line_text) = find_line_for_offset(excerpt, annotation.span.start)?;
    let start = annotation.span.start.max(line_start);
    let end = annotation.span.end.max(start + 1);
    let line_end = line_start + line_text.len();
    let caret_start = start.saturating_sub(line_start);
    let caret_end = end.min(line_end).max(start + 1);
    let caret_len = caret_end.saturating_sub(start).max(1);
    Some((
        line_no,
        line_text,
        caret_start,
        caret_len,
        excerpt.source_name.clone(),
    ))
}

fn render_annotation_group(output: &mut String, annotations: &[DisplayRenderedAnnotation]) {
    let first = &annotations[0];
    output.push('\n');
    if let Some(source_name) = &first.source_name {
        output.push_str(&format!(
            "{dim}{file}:{line}{reset}\n",
            dim = ANSI_DIM,
            file = source_name,
            line = first.line_no,
            reset = ANSI_RESET,
        ));
    }
    output.push_str(&format!(
        "{blue}{line:>4} │{reset} {source}\n",
        blue = ANSI_BLUE,
        reset = ANSI_RESET,
        line = first.line_no,
        source = &first.line_text,
    ));

    let line_width = first.line_text.chars().count();
    let mut underline = vec![' '; line_width.max(1)];
    for annotation in annotations {
        let start = annotation
            .caret_offset
            .min(underline.len().saturating_sub(1));
        let end = (annotation.caret_offset + annotation.caret_len).min(underline.len());
        for cell in underline.iter_mut().take(end).skip(start) {
            *cell = '─';
        }
        if end > 0 {
            underline[end - 1] = '┬';
        }
    }
    let mut ordered: Vec<DisplayRenderedAnnotation> = annotations.to_vec();
    ordered.sort_by_key(|info| std::cmp::Reverse(info.anchor_offset));

    let underline_text: String = underline.into_iter().collect();
    if let Some(first) = ordered.first() {
        let connector_column = first.anchor_offset.min(underline_text.chars().count());
        let leading_underline: String = underline_text.chars().take(connector_column).collect();
        output.push_str(&format!(
            "     │ {red}{leading}─{reset} {message}\n",
            red = ANSI_RED,
            reset = ANSI_RESET,
            leading = leading_underline,
            message = first.message,
        ));
    } else {
        output.push_str(&format!(
            "     │ {red}{underline}{reset}\n",
            red = ANSI_RED,
            underline = underline_text,
            reset = ANSI_RESET,
        ));
    }

    for index in 1..ordered.len() {
        let info = &ordered[index];
        let mut prefix_cells = vec![' '; info.anchor_offset];
        for later in ordered.iter().skip(index + 1) {
            if later.anchor_offset < prefix_cells.len() {
                prefix_cells[later.anchor_offset] = '│';
            }
        }
        let prefix: String = prefix_cells.into_iter().collect();
        output.push_str(&format!(
            "     │ {red}{prefix}└─{reset} {message}\n",
            red = ANSI_RED,
            reset = ANSI_RESET,
            prefix = prefix,
            message = info.message,
        ));
    }
}

#[derive(Debug, Clone)]
struct DisplayRenderedAnnotation {
    message: SharedString,
    line_no: usize,
    line_text: String,
    caret_offset: usize,
    caret_len: usize,
    anchor_offset: usize,
    source_name: Option<FilePath>,
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
    use expect_test::expect;

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
        expect![[r#"
            parsing error: expected ')'
            --> line 10, column 7
              10 | assert(true;
                 |       ^ missing closing parenthesis"#]]
        .assert_eq(rendered.as_str());
    }

    #[test]
    fn displays_diagnostics_with_unicode_and_ansi() {
        let diagnostics =
            vec![
                SourceDiagnostic::new(DiagnosticKind::Parsing, "expected expression")
                    .with_source_excerpt(
                        SourceExcerpt::new("assert();\n", 20, 300).with_source_name("sample.holo"),
                    )
                    .with_annotated_span(Span::new(307, 308), "missing expression"),
            ];
        let rendered = display_source_diagnostics(&diagnostics);
        let rendered = strip_ansi_sequences(&rendered);
        expect![[r#"
            ⚒️ Parsing: expected expression

            sample.holo:20
              20 │ assert();
                 │        ─ missing expression
"#]]
        .assert_eq(&rendered);
    }

    #[test]
    fn supports_multiple_annotations_for_single_source_excerpt() {
        let diagnostic = SourceDiagnostic::new(
            DiagnosticKind::Typecheck,
            "arithmetic operands must have the same type",
        )
        .with_source_excerpt_annotations(
            SourceExcerpt::new("1i64 + 2.0f64\n", 1, 0),
            [
                (Span::new(0, 4), "left operand has type `i64`"),
                (Span::new(7, 13), "right operand has type `f64`"),
            ],
        );

        assert_eq!(diagnostic.source_excerpts.len(), 1);
        assert_eq!(diagnostic.annotated_spans.len(), 2);
        assert_eq!(
            diagnostic.annotated_spans[0].message,
            "left operand has type `i64`"
        );
        assert_eq!(
            diagnostic.annotated_spans[1].message,
            "right operand has type `f64`"
        );
    }

    #[test]
    fn displays_single_excerpt_once_for_multiple_annotations() {
        let diagnostics = vec![SourceDiagnostic::new(
            DiagnosticKind::Typecheck,
            "arithmetic operands must have the same type",
        )
        .with_source_excerpt_annotations(
            SourceExcerpt::new("assert(1i64 + 2.0f64);\n", 1, 0),
            [
                (Span::new(7, 11), "left operand has type `i64`"),
                (Span::new(14, 20), "right operand has type `f64`"),
            ],
        )];
        let rendered = display_source_diagnostics(&diagnostics);
        let rendered = strip_ansi_sequences(&rendered);
        expect![[r#"
            ⚒️ Typecheck: arithmetic operands must have the same type

               1 │ assert(1i64 + 2.0f64);
                 │        ───┬   ────── right operand has type `f64`
                 │           └─ left operand has type `i64`
"#]]
        .assert_eq(&rendered);
    }

    fn strip_ansi_sequences(input: &str) -> String {
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
        out
    }
}
