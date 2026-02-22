use crate::{SharedString, Span};

/// Compiler stage associated with a source diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DiagnosticKind {
    /// Produced while lexing source text into tokens.
    Lexing,
    /// Produced while parsing tokens into syntax nodes.
    Parsing,
    /// Produced while checking semantic/type constraints.
    Typecheck,
}

/// Span annotation attached to a source diagnostic.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

/// Structured source-level compilation diagnostic.
///
/// Use this type to describe compilation errors (lexing/parsing/typecheck)
/// with precise span annotations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceDiagnostic {
    /// Human-readable primary message for the diagnostic.
    pub message: SharedString,
    /// Compilation stage that produced this diagnostic.
    pub kind: DiagnosticKind,
    /// Source span annotations associated with this diagnostic.
    pub annotated_spans: Vec<AnnotatedSpan>,
}

impl SourceDiagnostic {
    /// Creates a new source diagnostic without annotations.
    pub fn new(kind: DiagnosticKind, message: impl Into<SharedString>) -> Self {
        Self {
            message: message.into(),
            kind,
            annotated_spans: Vec::new(),
        }
    }

    /// Appends one annotated span and returns the updated diagnostic.
    pub fn with_annotated_span(mut self, span: Span, message: impl Into<SharedString>) -> Self {
        self.annotated_spans.push(AnnotatedSpan::new(span, message));
        self
    }
}

#[cfg(test)]
mod tests {
    use super::{DiagnosticKind, SourceDiagnostic};
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
}
