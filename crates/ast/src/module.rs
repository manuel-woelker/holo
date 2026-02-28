use super::types::TypeRef;
use holo_base::{SharedString, Span};
use speedy::{Readable, Writable};

/// Full file-level syntax tree.
#[derive(Debug, Clone, PartialEq, Eq, Default, Readable, Writable)]
pub struct Module {
    /// Items declared in this source file.
    pub items: Vec<ModuleItem>,
}

/// A function item in a module.
#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub enum ModuleItem {
    /// A `fn name(...) -> type { ... }` item.
    Function(FunctionItem),
}

impl ModuleItem {
    pub fn span(&self) -> Span {
        match self {
            ModuleItem::Function(f) => f.span,
        }
    }
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
    pub statements: Vec<super::Statement>,
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
