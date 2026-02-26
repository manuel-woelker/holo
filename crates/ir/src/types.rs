use holo_base::{SharedString, Span};

/// Full file-level typed IR module.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Module {
    /// Function items declared in this source file.
    pub functions: Vec<FunctionItem>,
    /// Test items declared in this source file.
    pub tests: Vec<TestItem>,
}

/// A typed function item.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionItem {
    /// Function name.
    pub name: SharedString,
    /// Ordered parameter list.
    pub parameters: Vec<FunctionParameter>,
    /// Typed function return.
    pub return_type: Type,
    /// Ordered statements in function body.
    pub statements: Vec<Statement>,
    /// Whether this function was annotated with `#[test]`.
    pub is_test: bool,
    /// Byte span for the whole function item.
    pub span: Span,
}

/// Typed function parameter declaration.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionParameter {
    /// Parameter name.
    pub name: SharedString,
    /// Parameter type.
    pub ty: Type,
    /// Byte span for the whole parameter.
    pub span: Span,
}

/// A typed test item.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TestItem {
    /// Test function name.
    pub name: SharedString,
    /// Ordered statements in the test body.
    pub statements: Vec<Statement>,
    /// Byte span for the whole test item.
    pub span: Span,
}

/// Typed statement forms.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Statement {
    /// `assert(<expr>);`
    Assert(AssertStatement),
    /// `let name[: type] = <expr>;`
    Let(LetStatement),
    /// `<expr>;`
    Expr(ExprStatement),
}

/// Typed assertion statement node.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssertStatement {
    /// Expression that must evaluate to `true`.
    pub expression: Expr,
    /// Byte span for the assertion statement.
    pub span: Span,
}

/// Typed let-binding statement node.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LetStatement {
    /// Bound identifier.
    pub name: SharedString,
    /// Optional declared type.
    pub ty: Option<Type>,
    /// Initializer expression.
    pub value: Expr,
    /// Byte span for the let statement.
    pub span: Span,
}

/// Typed expression statement node.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprStatement {
    /// Evaluated expression.
    pub expression: Expr,
    /// Byte span for the expression statement.
    pub span: Span,
}

/// Typed expression node with span and computed/static type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Expr {
    /// Expression variant payload.
    pub kind: ExprKind,
    /// Static/inferred type at IR-lowering time.
    pub ty: Type,
    /// Byte span for this expression node.
    pub span: Span,
}

/// Typed expression kinds in IR.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
    BoolLiteral(bool),
    NumberLiteral(SharedString),
    StringLiteral(SharedString),
    TemplateString(Vec<IrTemplatePart>),
    Identifier(SharedString),
    Negation(Box<Expr>),
    UnaryMinus(Box<Expr>),
    Binary(BinaryExpr),
    Call(CallExpr),
    If(IfExpr),
    While(WhileExpr),
    Block(BlockExpr),
}

/// A part of a template string in IR - either literal text or an expression to interpolate.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IrTemplatePart {
    /// Literal text content.
    Literal(SharedString),
    /// Expression to interpolate.
    Expression(Box<Expr>),
}

/// Typed binary expression payload.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BinaryExpr {
    pub operator: BinaryOperator,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

/// Typed function call payload.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub arguments: Vec<Expr>,
}

/// Typed if expression payload.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub then_branch: Box<Expr>,
    pub else_branch: Option<Box<Expr>>,
}

/// Typed while expression payload.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WhileExpr {
    pub condition: Box<Expr>,
    pub body: Box<Expr>,
}

/// Typed block expression payload.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BlockExpr {
    pub statements: Vec<Statement>,
    pub result: Option<Box<Expr>>,
}

/// IR type for execution/backend boundaries.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Bool,
    U32,
    U64,
    I32,
    I64,
    F32,
    F64,
    String,
    Unit,
    Unknown,
    /// Native function type with signature.
    NativeFunction {
        param_types: Vec<Type>,
        return_type: Box<Type>,
    },
}

/// Arithmetic operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

impl Module {
    pub(crate) fn function(
        name: SharedString,
        parameters: Vec<FunctionParameter>,
        return_type: Type,
        statements: Vec<Statement>,
        is_test: bool,
        span: Span,
    ) -> FunctionItem {
        FunctionItem {
            name,
            parameters,
            return_type,
            statements,
            is_test,
            span,
        }
    }
}

impl Expr {
    pub fn bool_literal(value: bool, span: Span) -> Self {
        Self {
            kind: ExprKind::BoolLiteral(value),
            ty: Type::Bool,
            span,
        }
    }

    pub fn number_literal(value: impl Into<SharedString>, span: Span) -> Self {
        let value = value.into();
        Self {
            ty: infer_number_literal_type(value.as_str()),
            kind: ExprKind::NumberLiteral(value),
            span,
        }
    }

    pub fn string_literal(value: impl Into<SharedString>, span: Span) -> Self {
        let value = value.into();
        Self {
            ty: Type::String,
            kind: ExprKind::StringLiteral(value),
            span,
        }
    }

    pub fn template_string(parts: Vec<IrTemplatePart>, span: Span) -> Self {
        Self {
            ty: Type::String,
            kind: ExprKind::TemplateString(parts),
            span,
        }
    }

    pub fn identifier(name: impl Into<SharedString>, span: Span) -> Self {
        Self {
            kind: ExprKind::Identifier(name.into()),
            ty: Type::Unknown,
            span,
        }
    }

    pub fn negation(expression: Expr, span: Span) -> Self {
        Self {
            kind: ExprKind::Negation(Box::new(expression)),
            ty: Type::Bool,
            span,
        }
    }

    pub fn unary_minus(expression: Expr, span: Span) -> Self {
        let ty = expression.ty.clone();
        Self {
            ty,
            kind: ExprKind::UnaryMinus(Box::new(expression)),
            span,
        }
    }

    pub fn binary(operator: BinaryOperator, left: Expr, right: Expr, span: Span) -> Self {
        let ty = if left.ty == right.ty {
            left.ty.clone()
        } else {
            Type::Unknown
        };
        Self {
            kind: ExprKind::Binary(BinaryExpr {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            }),
            ty,
            span,
        }
    }

    pub fn call(callee: Expr, arguments: Vec<Expr>, span: Span) -> Self {
        Self::call_typed(callee, arguments, Type::Unknown, span)
    }

    pub fn call_typed(callee: Expr, arguments: Vec<Expr>, ty: Type, span: Span) -> Self {
        Self {
            kind: ExprKind::Call(CallExpr {
                callee: Box::new(callee),
                arguments,
            }),
            ty,
            span,
        }
    }

    pub fn if_expression(
        condition: Expr,
        then_branch: Expr,
        else_branch: Option<Expr>,
        span: Span,
    ) -> Self {
        let ty = else_branch
            .as_ref()
            .map(|branch| {
                if then_branch.ty == branch.ty {
                    then_branch.ty.clone()
                } else {
                    Type::Unknown
                }
            })
            .unwrap_or(Type::Unit);
        Self {
            kind: ExprKind::If(IfExpr {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: else_branch.map(Box::new),
            }),
            ty,
            span,
        }
    }

    pub fn while_expression(condition: Expr, body: Expr, span: Span) -> Self {
        Self {
            kind: ExprKind::While(WhileExpr {
                condition: Box::new(condition),
                body: Box::new(body),
            }),
            ty: Type::Unit,
            span,
        }
    }

    pub fn block(statements: Vec<Statement>, result: Option<Expr>, span: Span) -> Self {
        let ty = result
            .as_ref()
            .map(|expr| expr.ty.clone())
            .unwrap_or(Type::Unit);
        Self {
            kind: ExprKind::Block(BlockExpr {
                statements,
                result: result.map(Box::new),
            }),
            ty,
            span,
        }
    }

    /// Creates a native function type from parameter and return types.
    pub fn native_function_type(param_types: Vec<Type>, return_type: Type) -> Type {
        Type::NativeFunction {
            param_types,
            return_type: Box::new(return_type),
        }
    }
}

fn infer_number_literal_type(literal: &str) -> Type {
    if literal.ends_with("u32") {
        Type::U32
    } else if literal.ends_with("u64") {
        Type::U64
    } else if literal.ends_with("i32") {
        Type::I32
    } else if literal.ends_with("i64") {
        Type::I64
    } else if literal.ends_with("f32") {
        Type::F32
    } else if literal.ends_with("f64") || literal.contains('.') {
        Type::F64
    } else {
        Type::I64
    }
}

/// Backward-compatible alias for type annotations in existing execution code/tests.
pub type TypeRef = Type;
