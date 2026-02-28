//! Arithmetic binary operators and type annotations for the minimal holo language.

use speedy::{Readable, Writable};

/// Arithmetic binary operators supported by the parser.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Readable, Writable)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Readable, Writable)]
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
