use super::expression::Expr;
use super::types::TypeRef;
use holo_base::{SharedString, Span};
use speedy::{Readable, Writable};

/// Full file-level syntax tree.
#[derive(Debug, Clone, PartialEq, Eq, Default, Readable, Writable)]
pub struct Module {
    /// Function items declared in this source file.
    pub functions: Vec<FunctionItem>,
    /// Test items declared in this source file.
    pub tests: Vec<TestItem>,
}

/// A `fn name(...) -> type { ... }` item.
#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub struct FunctionItem {
    /// Function name.
    pub name: SharedString,
    /// Ordered parameter list.
    pub parameters: Vec<FunctionParameter>,
    /// Declared return type.
    pub return_type: TypeRef,
    /// Ordered statements in the function body.
    pub statements: Vec<Statement>,
    /// Whether this function was annotated with `#[test]`.
    pub is_test: bool,
    /// Byte span for the whole function item.
    pub span: Span,
}

/// Function parameter declaration.
#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub struct FunctionParameter {
    /// Parameter name.
    pub name: SharedString,
    /// Parameter type annotation.
    pub ty: TypeRef,
    /// Byte span for the whole parameter.
    pub span: Span,
}

/// A `#[test] fn name() { ... }` item.
#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub struct TestItem {
    /// Test function name.
    pub name: SharedString,
    /// Ordered statements in the test body.
    pub statements: Vec<Statement>,
    /// Byte span for the whole test item.
    pub span: Span,
}

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
