//! Abstract syntax tree types for the minimal holo language.

use holo_base::{SharedString, Span};

/// Full file-level syntax tree.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Module {
    /// Function items declared in this source file.
    pub functions: Vec<FunctionItem>,
    /// Test items declared in this source file.
    pub tests: Vec<TestItem>,
}

/// A `fn name(...) -> type { ... }` item.
#[derive(Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionParameter {
    /// Parameter name.
    pub name: SharedString,
    /// Parameter type annotation.
    pub ty: TypeRef,
    /// Byte span for the whole parameter.
    pub span: Span,
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
    /// `let name[: type] = <expr>;`
    Let(LetStatement),
    /// `<expr>;`
    Expr(ExprStatement),
}

/// Assertion statement node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssertStatement {
    /// Expression that must evaluate to `true`.
    pub expression: Expr,
    /// Byte span for the assertion statement.
    pub span: Span,
}

/// Let-binding statement node.
#[derive(Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprStatement {
    /// Evaluated expression.
    pub expression: Expr,
    /// Byte span for the expression statement.
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
    /// Numeric literal expression.
    NumberLiteral(SharedString),
    /// String literal expression.
    StringLiteral(SharedString),
    /// Identifier reference.
    Identifier(SharedString),
    /// Unary boolean negation (`!expr`).
    Negation(Box<Expr>),
    /// Unary arithmetic negation (`-expr`).
    UnaryMinus(Box<Expr>),
    /// Binary operator application.
    Binary(BinaryExpr),
    /// Function call.
    Call(CallExpr),
    /// If expression with block branches.
    If(IfExpr),
    /// While loop expression.
    While(WhileExpr),
    /// Block expression.
    Block(BlockExpr),
}

/// Binary expression payload.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryExpr {
    /// Binary operator.
    pub operator: BinaryOperator,
    /// Left operand.
    pub left: Box<Expr>,
    /// Right operand.
    pub right: Box<Expr>,
}

/// Function call payload.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallExpr {
    /// Called function expression.
    pub callee: Box<Expr>,
    /// Ordered call arguments.
    pub arguments: Vec<Expr>,
}

/// If expression payload.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfExpr {
    /// Condition expression.
    pub condition: Box<Expr>,
    /// Then branch block.
    pub then_branch: Box<Expr>,
    /// Optional else branch block.
    pub else_branch: Option<Box<Expr>>,
}

/// While expression payload.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhileExpr {
    /// Loop condition expression.
    pub condition: Box<Expr>,
    /// Loop body block.
    pub body: Box<Expr>,
}

/// Block expression payload.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BlockExpr {
    /// Statements evaluated in order.
    pub statements: Vec<Statement>,
    /// Optional trailing expression result.
    pub result: Option<Box<Expr>>,
}

/// Arithmetic binary operators supported by the parser.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

/// Type annotation reference.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeRef {
    Bool,
    U32,
    U64,
    I32,
    I64,
    F32,
    F64,
    String,
    Unit,
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

    /// Creates a numeric literal expression from source text.
    pub fn number_literal(value: impl Into<SharedString>, span: Span) -> Self {
        Self {
            kind: ExprKind::NumberLiteral(value.into()),
            span,
        }
    }

    /// Creates a string literal expression from source text.
    pub fn string_literal(value: impl Into<SharedString>, span: Span) -> Self {
        Self {
            kind: ExprKind::StringLiteral(value.into()),
            span,
        }
    }

    /// Creates an identifier expression.
    pub fn identifier(name: impl Into<SharedString>, span: Span) -> Self {
        Self {
            kind: ExprKind::Identifier(name.into()),
            span,
        }
    }

    /// Creates a unary minus expression.
    pub fn unary_minus(expression: Expr, span: Span) -> Self {
        Self {
            kind: ExprKind::UnaryMinus(Box::new(expression)),
            span,
        }
    }

    /// Creates a binary expression.
    pub fn binary(operator: BinaryOperator, left: Expr, right: Expr, span: Span) -> Self {
        Self {
            kind: ExprKind::Binary(BinaryExpr {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            }),
            span,
        }
    }

    /// Creates a function call expression.
    pub fn call(callee: Expr, arguments: Vec<Expr>, span: Span) -> Self {
        Self {
            kind: ExprKind::Call(CallExpr {
                callee: Box::new(callee),
                arguments,
            }),
            span,
        }
    }

    /// Creates an if expression.
    pub fn if_expression(
        condition: Expr,
        then_branch: Expr,
        else_branch: Option<Expr>,
        span: Span,
    ) -> Self {
        Self {
            kind: ExprKind::If(IfExpr {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: else_branch.map(Box::new),
            }),
            span,
        }
    }

    /// Creates a while expression.
    pub fn while_expression(condition: Expr, body: Expr, span: Span) -> Self {
        Self {
            kind: ExprKind::While(WhileExpr {
                condition: Box::new(condition),
                body: Box::new(body),
            }),
            span,
        }
    }

    /// Creates a block expression.
    pub fn block(statements: Vec<Statement>, result: Option<Expr>, span: Span) -> Self {
        Self {
            kind: ExprKind::Block(BlockExpr {
                statements,
                result: result.map(Box::new),
            }),
            span,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{BinaryOperator, Expr, ExprKind, Module};
    use holo_base::Span;

    #[test]
    fn module_defaults_to_empty_tests() {
        let module = Module::default();
        assert!(module.functions.is_empty());
        assert!(module.tests.is_empty());
    }

    #[test]
    fn creates_negation_expression() {
        let literal = Expr::bool_literal(true, Span::new(0, 4));
        let negation = Expr::negation(literal, Span::new(0, 5));
        assert!(matches!(negation.kind, ExprKind::Negation(_)));
    }

    #[test]
    fn creates_binary_expression() {
        let left = Expr::number_literal("1", Span::new(0, 1));
        let right = Expr::number_literal("2", Span::new(4, 5));
        let binary = Expr::binary(BinaryOperator::Add, left, right, Span::new(0, 5));
        assert!(matches!(binary.kind, ExprKind::Binary(_)));
    }
}
