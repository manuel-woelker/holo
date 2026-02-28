use super::statement::Statement;
use super::types::BinaryOperator;
use holo_base::{QualifiedName, SharedString, Span};
use speedy::{Readable, Writable};

/// Expression node with span metadata.
#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub struct Expr {
    /// Expression variant payload.
    pub kind: ExprKind,
    /// Byte span for this expression node.
    pub span: Span,
}

/// Supported expression kinds in the minimal language.
#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub enum ExprKind {
    /// Boolean literal expression.
    BoolLiteral(bool),
    /// Numeric literal expression.
    NumberLiteral(SharedString),
    /// String literal expression.
    StringLiteral(SharedString),
    /// Template string expression with interpolation.
    TemplateString(Vec<TemplatePart>),
    /// Simple identifier reference.
    Identifier(SharedString),
    /// Qualified identifier reference (e.g., `module::name`).
    QualifiedIdentifier(QualifiedName),
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

/// A part of a template string - either literal text or an expression to interpolate.
#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub enum TemplatePart {
    /// Literal text content.
    Literal(SharedString),
    /// Expression to interpolate.
    Expression(Expr),
}

/// Binary expression payload.
#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub struct BinaryExpr {
    /// Binary operator.
    pub operator: BinaryOperator,
    /// Left operand.
    pub left: Box<Expr>,
    /// Right operand.
    pub right: Box<Expr>,
}

/// Function call payload.
#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub struct CallExpr {
    /// Called function expression.
    pub callee: Box<Expr>,
    /// Ordered call arguments.
    pub arguments: Vec<Expr>,
}

/// If expression payload.
#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub struct IfExpr {
    /// Condition expression.
    pub condition: Box<Expr>,
    /// Then branch block.
    pub then_branch: Box<Expr>,
    /// Optional else branch block.
    pub else_branch: Option<Box<Expr>>,
}

/// While expression payload.
#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub struct WhileExpr {
    /// Loop condition expression.
    pub condition: Box<Expr>,
    /// Loop body block.
    pub body: Box<Expr>,
}

/// Block expression payload.
#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub struct BlockExpr {
    /// Statements evaluated in order.
    pub statements: Vec<Statement>,
    /// Optional trailing expression result.
    pub result: Option<Box<Expr>>,
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

    /// Creates a template string expression.
    pub fn template_string(parts: Vec<TemplatePart>, span: Span) -> Self {
        Self {
            kind: ExprKind::TemplateString(parts),
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

    /// Creates a qualified identifier expression.
    pub fn qualified_identifier(name: QualifiedName, span: Span) -> Self {
        Self {
            kind: ExprKind::QualifiedIdentifier(name),
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
