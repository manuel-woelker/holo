//! Abstract syntax tree types for the minimal holo language.

pub mod expression;
pub mod statement;
pub mod types;

pub use expression::{Expr, ExprKind, TemplatePart};
pub use statement::{
    AssertStatement, ExprStatement, FunctionItem, FunctionParameter, LetStatement, Module,
    Statement, TestItem,
};
pub use types::{BinaryOperator, TypeRef};

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
