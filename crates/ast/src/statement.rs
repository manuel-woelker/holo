use super::expression::Expr;
use super::types::TypeRef;
use holo_base::{SharedString, Span};
use speedy::{Readable, Writable};

/// Supported statement forms in the minimal language.
#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub enum Statement {
    /// `assert(<expr>);`
    Assert(AssertStatement),
    /// `let name[: type] = <expr>;`
    Let(LetStatement),
    /// `<expr>;`
    Expr(ExprStatement),
}

/// Assertion statement node.
#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub struct AssertStatement {
    /// Expression that must evaluate to `true`.
    pub expression: Expr,
    /// Byte span for the assertion statement.
    pub span: Span,
}

/// Let-binding statement node.
#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub struct LetStatement {
    /// Bound identifier.
    pub name: SharedString,
    /// Optional declared type.
    pub ty: Option<TypeRef>,
    /// Initializer expression.
    pub value: Expr,
    /// Byte span for the let statement.
    pub span: Span,
}

/// Expression statement node.
#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub struct ExprStatement {
    /// Evaluated expression.
    pub expression: Expr,
    /// Byte span for the expression statement.
    pub span: Span,
}
