//! Typed intermediate representation for holo execution/backends.

use std::collections::HashMap;

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
    Identifier(SharedString),
    Negation(Box<Expr>),
    UnaryMinus(Box<Expr>),
    Binary(BinaryExpr),
    Call(CallExpr),
    If(IfExpr),
    While(WhileExpr),
    Block(BlockExpr),
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    Bool,
    U32,
    U64,
    I32,
    I64,
    F32,
    F64,
    Unit,
    Unknown,
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
        Self {
            ty: expression.ty,
            kind: ExprKind::UnaryMinus(Box::new(expression)),
            span,
        }
    }

    pub fn binary(operator: BinaryOperator, left: Expr, right: Expr, span: Span) -> Self {
        let ty = if left.ty == right.ty {
            left.ty
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
                    then_branch.ty
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
        let ty = result.as_ref().map(|expr| expr.ty).unwrap_or(Type::Unit);
        Self {
            kind: ExprKind::Block(BlockExpr {
                statements,
                result: result.map(Box::new),
            }),
            ty,
            span,
        }
    }
}

/// Lowers parser AST into typed IR for execution/backend consumers.
pub fn lower_module(module: &holo_ast::Module) -> Module {
    let function_types: HashMap<SharedString, Type> = module
        .functions
        .iter()
        .map(|function| (function.name.clone(), type_from_ref(function.return_type)))
        .collect();

    Module {
        functions: module
            .functions
            .iter()
            .map(|function| {
                let mut scopes = vec![HashMap::new()];
                for parameter in &function.parameters {
                    scopes[0].insert(parameter.name.clone(), type_from_ref(parameter.ty));
                }
                scopes.push(HashMap::new());
                let statements = function
                    .statements
                    .iter()
                    .map(|statement| lower_statement(statement, &function_types, &mut scopes))
                    .collect();
                Module::function(
                    function.name.clone(),
                    function
                        .parameters
                        .iter()
                        .map(|parameter| FunctionParameter {
                            name: parameter.name.clone(),
                            ty: type_from_ref(parameter.ty),
                            span: parameter.span,
                        })
                        .collect(),
                    type_from_ref(function.return_type),
                    statements,
                    function.is_test,
                    function.span,
                )
            })
            .collect(),
        tests: module
            .tests
            .iter()
            .map(|test| {
                let mut scopes = vec![HashMap::new()];
                let statements = test
                    .statements
                    .iter()
                    .map(|statement| lower_statement(statement, &function_types, &mut scopes))
                    .collect();
                TestItem {
                    name: test.name.clone(),
                    statements,
                    span: test.span,
                }
            })
            .collect(),
    }
}

impl Module {
    fn function(
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

fn lower_statement(
    statement: &holo_ast::Statement,
    function_types: &HashMap<SharedString, Type>,
    scopes: &mut Vec<HashMap<SharedString, Type>>,
) -> Statement {
    match statement {
        holo_ast::Statement::Assert(assertion) => Statement::Assert(AssertStatement {
            expression: lower_expression(&assertion.expression, function_types, scopes),
            span: assertion.span,
        }),
        holo_ast::Statement::Let(let_statement) => {
            let value = lower_expression(&let_statement.value, function_types, scopes);
            let declared = let_statement.ty.map(type_from_ref);
            let final_ty = declared.unwrap_or(value.ty);
            if let Some(scope) = scopes.last_mut() {
                scope.insert(let_statement.name.clone(), final_ty);
            }
            Statement::Let(LetStatement {
                name: let_statement.name.clone(),
                ty: declared,
                value,
                span: let_statement.span,
            })
        }
        holo_ast::Statement::Expr(expr_statement) => Statement::Expr(ExprStatement {
            expression: lower_expression(&expr_statement.expression, function_types, scopes),
            span: expr_statement.span,
        }),
    }
}

fn lower_expression(
    expression: &holo_ast::Expr,
    function_types: &HashMap<SharedString, Type>,
    scopes: &mut Vec<HashMap<SharedString, Type>>,
) -> Expr {
    match &expression.kind {
        holo_ast::ExprKind::BoolLiteral(value) => Expr::bool_literal(*value, expression.span),
        holo_ast::ExprKind::NumberLiteral(value) => {
            Expr::number_literal(value.clone(), expression.span)
        }
        holo_ast::ExprKind::Identifier(name) => {
            let ty = lookup_scope(scopes, name).unwrap_or(Type::Unknown);
            Expr {
                kind: ExprKind::Identifier(name.clone()),
                ty,
                span: expression.span,
            }
        }
        holo_ast::ExprKind::Negation(inner) => Expr::negation(
            lower_expression(inner, function_types, scopes),
            expression.span,
        ),
        holo_ast::ExprKind::UnaryMinus(inner) => Expr::unary_minus(
            lower_expression(inner, function_types, scopes),
            expression.span,
        ),
        holo_ast::ExprKind::Binary(binary) => Expr::binary(
            lower_binary_operator(binary.operator),
            lower_expression(&binary.left, function_types, scopes),
            lower_expression(&binary.right, function_types, scopes),
            expression.span,
        ),
        holo_ast::ExprKind::Call(call) => {
            let callee = lower_expression(&call.callee, function_types, scopes);
            let arguments = call
                .arguments
                .iter()
                .map(|argument| lower_expression(argument, function_types, scopes))
                .collect();
            let return_ty = match &call.callee.kind {
                holo_ast::ExprKind::Identifier(name) => {
                    function_types.get(name).copied().unwrap_or(Type::Unknown)
                }
                _ => Type::Unknown,
            };
            Expr::call_typed(callee, arguments, return_ty, expression.span)
        }
        holo_ast::ExprKind::If(if_expression) => Expr::if_expression(
            lower_expression(&if_expression.condition, function_types, scopes),
            lower_expression(&if_expression.then_branch, function_types, scopes),
            if_expression
                .else_branch
                .as_ref()
                .map(|branch| lower_expression(branch, function_types, scopes)),
            expression.span,
        ),
        holo_ast::ExprKind::While(while_expression) => Expr::while_expression(
            lower_expression(&while_expression.condition, function_types, scopes),
            lower_expression(&while_expression.body, function_types, scopes),
            expression.span,
        ),
        holo_ast::ExprKind::Block(block_expression) => {
            scopes.push(HashMap::new());
            let statements = block_expression
                .statements
                .iter()
                .map(|statement| lower_statement(statement, function_types, scopes))
                .collect();
            let result = block_expression
                .result
                .as_ref()
                .map(|result| lower_expression(result, function_types, scopes));
            let _ = scopes.pop();
            Expr::block(statements, result, expression.span)
        }
    }
}

fn lookup_scope(scopes: &[HashMap<SharedString, Type>], name: &SharedString) -> Option<Type> {
    for scope in scopes.iter().rev() {
        if let Some(ty) = scope.get(name) {
            return Some(*ty);
        }
    }
    None
}

fn lower_binary_operator(operator: holo_ast::BinaryOperator) -> BinaryOperator {
    match operator {
        holo_ast::BinaryOperator::Add => BinaryOperator::Add,
        holo_ast::BinaryOperator::Subtract => BinaryOperator::Subtract,
        holo_ast::BinaryOperator::Multiply => BinaryOperator::Multiply,
        holo_ast::BinaryOperator::Divide => BinaryOperator::Divide,
        holo_ast::BinaryOperator::Modulo => BinaryOperator::Modulo,
        holo_ast::BinaryOperator::Equals => BinaryOperator::Equals,
        holo_ast::BinaryOperator::NotEquals => BinaryOperator::NotEquals,
        holo_ast::BinaryOperator::LessThan => BinaryOperator::LessThan,
        holo_ast::BinaryOperator::GreaterThan => BinaryOperator::GreaterThan,
        holo_ast::BinaryOperator::LessThanOrEqual => BinaryOperator::LessThanOrEqual,
        holo_ast::BinaryOperator::GreaterThanOrEqual => BinaryOperator::GreaterThanOrEqual,
    }
}

fn type_from_ref(type_ref: holo_ast::TypeRef) -> Type {
    match type_ref {
        holo_ast::TypeRef::Bool => Type::Bool,
        holo_ast::TypeRef::U32 => Type::U32,
        holo_ast::TypeRef::U64 => Type::U64,
        holo_ast::TypeRef::I32 => Type::I32,
        holo_ast::TypeRef::I64 => Type::I64,
        holo_ast::TypeRef::F32 => Type::F32,
        holo_ast::TypeRef::F64 => Type::F64,
        holo_ast::TypeRef::Unit => Type::Unit,
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

#[cfg(test)]
mod tests {
    use super::{lower_module, ExprKind, Type};
    use holo_ast::{
        Expr, ExprStatement, FunctionItem, Module as AstModule, Statement as AstStatement, TypeRef,
    };
    use holo_base::Span;

    #[test]
    fn lowers_function_signatures_and_expression_types() {
        let module = AstModule {
            functions: vec![FunctionItem {
                name: "entry".into(),
                parameters: vec![holo_ast::FunctionParameter {
                    name: "v".into(),
                    ty: TypeRef::I64,
                    span: Span::new(0, 1),
                }],
                return_type: TypeRef::I64,
                statements: vec![AstStatement::Expr(ExprStatement {
                    expression: Expr::identifier("v", Span::new(0, 1)),
                    span: Span::new(0, 2),
                })],
                is_test: false,
                span: Span::new(0, 2),
            }],
            tests: Vec::new(),
        };

        let lowered = lower_module(&module);
        assert_eq!(lowered.functions.len(), 1);
        assert_eq!(lowered.functions[0].return_type, Type::I64);
        let super::Statement::Expr(expr_statement) = &lowered.functions[0].statements[0] else {
            panic!("expected expression statement");
        };
        assert!(matches!(
            expr_statement.expression.kind,
            ExprKind::Identifier(_)
        ));
        assert_eq!(expr_statement.expression.ty, Type::I64);
    }
}
