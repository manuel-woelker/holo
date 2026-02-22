//! Abstract syntax tree types for the minimal holo language.

use holo_base::{SharedString, Span};

/// Full file-level syntax tree.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Module {
    /// Test items declared in this source file.
    pub tests: Vec<TestItem>,
}

/// A `#[test] fn name() { ... }` item.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestItem {
    /// Test function name.
    pub name: SharedString,
    /// Ordered statements in the test body.
    pub statements: Vec<Statement>,
    /// Byte span for the whole test item.
    pub span: Span,
}

/// Supported statement forms in the minimal language.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    /// `assert(<expr>);`
    Assert(AssertStatement),
}

/// Assertion statement node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssertStatement {
    /// Expression that must evaluate to `true`.
    pub expression: Expr,
    /// Byte span for the assertion statement.
    pub span: Span,
}

/// Expression node with span metadata.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expr {
    /// Expression variant payload.
    pub kind: ExprKind,
    /// Byte span for this expression node.
    pub span: Span,
}

/// Supported expression kinds in the minimal language.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprKind {
    /// Boolean literal expression.
    BoolLiteral(bool),
    /// Unary boolean negation (`!expr`).
    Negation(Box<Expr>),
}

impl Expr {
    /// Creates a boolean literal expression.
    pub fn bool_literal(value: bool, span: Span) -> Self {
        Self {
            kind: ExprKind::BoolLiteral(value),
            span,
        }
    }

    /// Creates a negation expression.
    pub fn negation(expression: Expr, span: Span) -> Self {
        Self {
            kind: ExprKind::Negation(Box::new(expression)),
            span,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Expr, ExprKind, Module};
    use holo_base::Span;

    #[test]
    fn module_defaults_to_empty_tests() {
        let module = Module::default();
        assert!(module.tests.is_empty());
    }

    #[test]
    fn creates_negation_expression() {
        let literal = Expr::bool_literal(true, Span::new(0, 4));
        let negation = Expr::negation(literal, Span::new(0, 5));
        assert!(matches!(negation.kind, ExprKind::Negation(_)));
    }
}
