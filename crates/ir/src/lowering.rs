use crate::types::*;
use holo_base::SharedString;
use std::collections::HashMap;

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
            let final_ty = declared.clone().unwrap_or(value.ty.clone());
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
        holo_ast::ExprKind::StringLiteral(value) => {
            Expr::string_literal(value.clone(), expression.span)
        }
        holo_ast::ExprKind::TemplateString(parts) => {
            let ir_parts: Vec<IrTemplatePart> = parts
                .iter()
                .map(|part| match part {
                    holo_ast::TemplatePart::Literal(s) => IrTemplatePart::Literal(s.clone()),
                    holo_ast::TemplatePart::Expression(e) => IrTemplatePart::Expression(Box::new(
                        lower_expression(e, function_types, scopes),
                    )),
                })
                .collect();
            Expr::template_string(ir_parts, expression.span)
        }
        holo_ast::ExprKind::Identifier(name) => {
            let ty = lookup_scope(scopes, name.ident()).unwrap_or(Type::Unknown);
            Expr {
                kind: ExprKind::Identifier(name.ident().clone()),
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
                holo_ast::ExprKind::Identifier(name) => function_types
                    .get(name.ident())
                    .cloned()
                    .unwrap_or(Type::Unknown),
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
            return Some(ty.clone());
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
        holo_ast::TypeRef::String => Type::String,
        holo_ast::TypeRef::Unit => Type::Unit,
    }
}

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
