//! Abstract syntax tree types for the minimal holo language.

pub mod expression;
pub mod module;
pub mod statement;
pub mod types;

pub use expression::{Expr, ExprKind, QualifiedName, TemplatePart};
pub use module::{FunctionItem, FunctionParameter, Module, ModuleItem};
pub use statement::{AssertStatement, ExprStatement, LetStatement, Statement};
pub use types::{BinaryOperator, TypeRef};

#[cfg(test)]
mod tests {
    use super::{BinaryOperator, Expr, ExprKind, Module, ModuleItem};
    use holo_base::Span;
    use speedy::{Readable, Writable};

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

    #[test]
    fn test_module_speedy_serialization() {
        let mut module = Module::empty("test_module");

        let func = super::FunctionItem {
            name: "test_func".into(),
            parameters: vec![],
            return_type: super::TypeRef::Unit,
            statements: vec![],
            is_test: false,
            span: Span::new(0, 10),
        };
        module.items.push(ModuleItem::Function(func));

        let buffer = module.write_to_vec().unwrap();
        assert!(!buffer.is_empty());

        let deserialized = Module::read_from_buffer(&buffer).unwrap();
        assert_eq!(module, deserialized);
        assert_eq!(deserialized.items.len(), 1);
        let ModuleItem::Function(func) = &deserialized.items[0];
        assert_eq!(func.name.to_string(), "test_func");
    }
}
