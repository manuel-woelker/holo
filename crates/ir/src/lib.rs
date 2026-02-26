//! Typed intermediate representation for holo execution/backends.

pub mod lowering;
pub mod types;

pub use lowering::lower_module;
pub use types::{
    AssertStatement, BinaryExpr, BinaryOperator, BlockExpr, CallExpr, Expr, ExprKind, FunctionItem,
    FunctionParameter, IfExpr, IrTemplatePart, LetStatement, Module, Statement, TestItem, Type,
    TypeRef, WhileExpr,
};
