//! Typechecking interfaces and the minimal boolean typechecker.

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use holo_ast::{BinaryOperator, Expr, ExprKind, Module, Statement, TypeRef};
use holo_base::{
    DiagnosticKind, SharedString, SourceDiagnostic, SourceExcerpt, Span, TaskTimer, TaskTiming,
};
use holo_interpreter::NativeFunctionRegistry;
use tracing::info;

/// Type representation for the minimal language.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
}

/// Function type representation used during typechecking.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    /// Parameter types in declaration order.
    pub parameter_types: Vec<Type>,
    /// Return type.
    pub return_type: Type,
}

/// Summary result returned by module typechecking.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypecheckSummary {
    /// Number of tests that were typechecked.
    pub test_count: usize,
    /// Number of assertion statements validated.
    pub assertion_count: usize,
}

/// Result payload produced by typechecking.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypecheckResult {
    /// Typechecking summary for the module.
    pub summary: TypecheckSummary,
    /// Typechecking diagnostics encountered while validating the module.
    pub diagnostics: Vec<SourceDiagnostic>,
    /// Per-test typechecking timings.
    pub timings: Vec<TaskTiming>,
    /// Function signatures discovered in the module.
    pub function_types: HashMap<holo_base::SharedString, FunctionType>,
}

/// Typechecking abstraction used by the core compiler crate.
pub trait Typechecker {
    /// Validates semantic and type rules for a module.
    fn typecheck_module(&self, module: &Module, source: &str) -> TypecheckResult;
}

/// Minimal typechecker for boolean expressions and assert statements.
#[derive(Debug)]
pub struct BasicTypechecker {
    native_functions: Arc<NativeFunctionRegistry>,
}

impl Default for BasicTypechecker {
    fn default() -> Self {
        Self::new(Arc::new(NativeFunctionRegistry::default()))
    }
}

impl BasicTypechecker {
    /// Creates a new typechecker with the given native function registry.
    pub fn new(native_functions: Arc<NativeFunctionRegistry>) -> Self {
        Self { native_functions }
    }

    /// Creates a new typechecker with an empty native function registry.
    pub fn with_empty_registry() -> Self {
        Self::new(Arc::new(NativeFunctionRegistry::default()))
    }
}

/// Category of symbol stored in lexical scopes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SymbolKind {
    Parameter,
    Local,
}

/// One symbol table entry.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SymbolEntry {
    ty: Type,
    kind: SymbolKind,
    declaration_span: Span,
}

/// Lexical scope stack used for identifier resolution.
#[derive(Debug, Default)]
struct ScopeStack {
    scopes: Vec<HashMap<SharedString, SymbolEntry>>,
}

impl ScopeStack {
    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        let _ = self.scopes.pop();
    }

    fn lookup(&self, name: &SharedString) -> Option<SymbolEntry> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(*symbol);
            }
        }
        None
    }

    fn lookup_current_scope(&self, name: &SharedString) -> Option<SymbolEntry> {
        self.scopes
            .last()
            .and_then(|scope| scope.get(name))
            .copied()
    }

    fn insert_current_scope(&mut self, name: SharedString, entry: SymbolEntry) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, entry);
        }
    }
}

impl BasicTypechecker {
    fn has_explicit_numeric_suffix(literal: &str) -> bool {
        ["u32", "u64", "i32", "i64", "f32", "f64"]
            .iter()
            .any(|suffix| literal.ends_with(suffix))
    }

    fn type_name(ty: Type) -> &'static str {
        match ty {
            Type::Bool => "bool",
            Type::U32 => "u32",
            Type::U64 => "u64",
            Type::I32 => "i32",
            Type::I64 => "i64",
            Type::F32 => "f32",
            Type::F64 => "f64",
            Type::String => "string",
            Type::Unit => "()",
            Type::Unknown => "<unknown>",
        }
    }

    fn type_from_ref(type_ref: TypeRef) -> Type {
        match type_ref {
            TypeRef::Bool => Type::Bool,
            TypeRef::U32 => Type::U32,
            TypeRef::U64 => Type::U64,
            TypeRef::I32 => Type::I32,
            TypeRef::I64 => Type::I64,
            TypeRef::F32 => Type::F32,
            TypeRef::F64 => Type::F64,
            TypeRef::String => Type::String,
            TypeRef::Unit => Type::Unit,
        }
    }

    fn type_from_interpreter_type(ir_type: holo_interpreter::Type) -> Type {
        match ir_type {
            holo_interpreter::Type::Bool => Type::Bool,
            holo_interpreter::Type::U32 => Type::U32,
            holo_interpreter::Type::U64 => Type::U64,
            holo_interpreter::Type::I32 => Type::I32,
            holo_interpreter::Type::I64 => Type::I64,
            holo_interpreter::Type::F32 => Type::F32,
            holo_interpreter::Type::F64 => Type::F64,
            holo_interpreter::Type::String => Type::String,
            holo_interpreter::Type::Unit => Type::Unit,
            holo_interpreter::Type::Unknown => Type::Unknown,
            holo_interpreter::Type::NativeFunction { .. } => Type::Unknown,
        }
    }

    fn infer_number_literal_type(literal: &str, expected_type: Option<Type>) -> Type {
        if !Self::has_explicit_numeric_suffix(literal) {
            if let Some(expected_type) = expected_type {
                if Self::is_numeric_type(expected_type) {
                    return expected_type;
                }
            }
        }

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

    fn is_integer_type(ty: Type) -> bool {
        matches!(ty, Type::U32 | Type::U64 | Type::I32 | Type::I64)
    }

    fn is_numeric_type(ty: Type) -> bool {
        Self::is_integer_type(ty) || matches!(ty, Type::F32 | Type::F64)
    }

    fn check_same_type(
        diagnostics: &mut Vec<SourceDiagnostic>,
        source: &str,
        left: Type,
        left_span: holo_base::Span,
        right: Type,
        right_span: holo_base::Span,
        message: &'static str,
    ) -> bool {
        if left == Type::Unknown || right == Type::Unknown {
            return false;
        }
        if left == right {
            return true;
        }
        diagnostics.push(
            SourceDiagnostic::new(DiagnosticKind::Typecheck, message)
                .with_error_code("T1100")
                .with_annotated_span(
                    left_span,
                    format!("left operand has type `{}`", Self::type_name(left)),
                )
                .with_annotated_span(
                    right_span,
                    format!("right operand has type `{}`", Self::type_name(right)),
                )
                .with_annotated_span(
                    right_span,
                    "implicit numeric conversions are not allowed; use explicit literal suffixes",
                )
                .with_hint("align operand types with explicit literal suffixes")
                .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
        );
        false
    }

    fn check_same_arithmetic_operand_type(
        diagnostics: &mut Vec<SourceDiagnostic>,
        source: &str,
        left: Type,
        left_span: holo_base::Span,
        right: Type,
        right_span: holo_base::Span,
    ) -> bool {
        if left == Type::Unknown || right == Type::Unknown {
            return false;
        }
        if left == right {
            return true;
        }

        diagnostics.push(
            SourceDiagnostic::new(
                DiagnosticKind::Typecheck,
                "arithmetic operands must have the same type",
            )
            .with_error_code("T1101")
            .with_hint("use matching numeric types on both operands")
            .with_source_excerpt_annotations(
                SourceExcerpt::new(source, 1, 0),
                [
                    (
                        left_span,
                        format!("left operand has type `{}`", Self::type_name(left)),
                    ),
                    (
                        right_span,
                        format!("right operand has type `{}`", Self::type_name(right)),
                    ),
                ],
            ),
        );
        false
    }

    fn typecheck_statement(
        statement: &Statement,
        diagnostics: &mut Vec<SourceDiagnostic>,
        source: &str,
        function_types: &HashMap<SharedString, FunctionType>,
        function_spans: &HashMap<SharedString, holo_base::Span>,
        scopes: &mut ScopeStack,
        assertion_count: &mut usize,
    ) {
        match statement {
            Statement::Assert(assertion) => {
                let expression_type = Self::typecheck_expression(
                    &assertion.expression,
                    diagnostics,
                    source,
                    function_types,
                    function_spans,
                    scopes,
                    assertion_count,
                    None,
                );
                if expression_type != Type::Bool {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            "assert expects a boolean expression",
                        )
                        .with_error_code("T1001")
                        .with_hint("change the asserted expression to produce `bool`")
                        .with_annotated_span(
                            assertion.span,
                            "this assertion does not evaluate to `bool`",
                        )
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                    return;
                }

                *assertion_count += 1;
            }
            Statement::Let(let_statement) => {
                let value_type = Self::typecheck_expression(
                    &let_statement.value,
                    diagnostics,
                    source,
                    function_types,
                    function_spans,
                    scopes,
                    assertion_count,
                    let_statement.ty.map(Self::type_from_ref),
                );
                if scopes.lookup_current_scope(&let_statement.name).is_some() {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            format!("duplicate local binding `{}`", let_statement.name),
                        )
                        .with_error_code("T1002")
                        .with_annotated_span(
                            let_statement.span,
                            "this binding name is already defined in this scope",
                        )
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                    return;
                }
                let declared_type = let_statement.ty.map(Self::type_from_ref);
                let final_type = if let Some(declared_type) = declared_type {
                    if value_type != Type::Unknown
                        && !Self::check_same_type(
                            diagnostics,
                            source,
                            declared_type,
                            let_statement.span,
                            value_type,
                            let_statement.value.span,
                            "let binding type does not match initializer type",
                        )
                    {
                        Type::Unknown
                    } else {
                        declared_type
                    }
                } else {
                    value_type
                };
                scopes.insert_current_scope(
                    let_statement.name.clone(),
                    SymbolEntry {
                        ty: final_type,
                        kind: SymbolKind::Local,
                        declaration_span: let_statement.span,
                    },
                );
            }
            Statement::Expr(expr_statement) => {
                let _ = Self::typecheck_expression(
                    &expr_statement.expression,
                    diagnostics,
                    source,
                    function_types,
                    function_spans,
                    scopes,
                    assertion_count,
                    None,
                );
            }
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn typecheck_expression(
        expression: &Expr,
        diagnostics: &mut Vec<SourceDiagnostic>,
        source: &str,
        function_types: &HashMap<SharedString, FunctionType>,
        function_spans: &HashMap<SharedString, holo_base::Span>,
        scopes: &mut ScopeStack,
        assertion_count: &mut usize,
        expected_type: Option<Type>,
    ) -> Type {
        match &expression.kind {
            ExprKind::BoolLiteral(_) => Type::Bool,
            ExprKind::NumberLiteral(literal) => {
                Self::infer_number_literal_type(literal, expected_type)
            }
            ExprKind::StringLiteral(_) => Type::String,
            ExprKind::TemplateString(parts) => {
                for part in parts {
                    if let holo_ast::TemplatePart::Expression(expr) = part {
                        Self::typecheck_expression(
                            expr,
                            diagnostics,
                            source,
                            function_types,
                            function_spans,
                            scopes,
                            assertion_count,
                            None,
                        );
                    }
                }
                Type::String
            }
            ExprKind::Identifier(name) => {
                if let Some(symbol) = scopes.lookup(name) {
                    return symbol.ty;
                }
                diagnostics.push(
                    SourceDiagnostic::new(
                        DiagnosticKind::Typecheck,
                        format!("unknown identifier `{name}`"),
                    )
                    .with_error_code("T1003")
                    .with_annotated_span(expression.span, "this name is not defined in scope")
                    .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                );
                Type::Unknown
            }
            ExprKind::Negation(inner) => {
                let inner_type = Self::typecheck_expression(
                    inner,
                    diagnostics,
                    source,
                    function_types,
                    function_spans,
                    scopes,
                    assertion_count,
                    None,
                );
                if inner_type != Type::Bool && inner_type != Type::Unknown {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            "operator `!` expects a boolean operand",
                        )
                        .with_error_code("T1004")
                        .with_hint("apply `!` only to expressions of type `bool`")
                        .with_annotated_span(inner.span, "this operand is not `bool`")
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                }
                Type::Bool
            }
            ExprKind::UnaryMinus(inner) => {
                let inner_type = Self::typecheck_expression(
                    inner,
                    diagnostics,
                    source,
                    function_types,
                    function_spans,
                    scopes,
                    assertion_count,
                    None,
                );
                if matches!(inner_type, Type::I32 | Type::I64 | Type::F32 | Type::F64) {
                    inner_type
                } else {
                    if inner_type != Type::Unknown {
                        diagnostics.push(
                            SourceDiagnostic::new(
                                DiagnosticKind::Typecheck,
                                "operator `-` expects a signed numeric operand",
                            )
                            .with_error_code("T1005")
                            .with_hint("use i32/i64/f32/f64 or adjust the operand type")
                            .with_annotated_span(inner.span, "this operand is not signed numeric")
                            .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                        );
                    }
                    Type::Unknown
                }
            }
            ExprKind::Binary(binary) => {
                let left_type = Self::typecheck_expression(
                    &binary.left,
                    diagnostics,
                    source,
                    function_types,
                    function_spans,
                    scopes,
                    assertion_count,
                    None,
                );
                let right_type = Self::typecheck_expression(
                    &binary.right,
                    diagnostics,
                    source,
                    function_types,
                    function_spans,
                    scopes,
                    assertion_count,
                    Some(left_type),
                );
                if left_type == Type::Unknown || right_type == Type::Unknown {
                    return Type::Unknown;
                }

                let is_comparison = matches!(
                    binary.operator,
                    BinaryOperator::Equals
                        | BinaryOperator::NotEquals
                        | BinaryOperator::LessThan
                        | BinaryOperator::GreaterThan
                        | BinaryOperator::LessThanOrEqual
                        | BinaryOperator::GreaterThanOrEqual
                );
                let is_equality = matches!(
                    binary.operator,
                    BinaryOperator::Equals | BinaryOperator::NotEquals
                );

                if is_comparison {
                    if is_equality {
                        if left_type != right_type {
                            diagnostics.push(
                                SourceDiagnostic::new(
                                    DiagnosticKind::Typecheck,
                                    "equality operators require operands of the same type",
                                )
                                .with_error_code("T1010")
                                .with_hint("ensure both operands have the same type")
                                .with_annotated_span(
                                    binary.left.span,
                                    format!(
                                        "left operand has type `{}`",
                                        Self::type_name(left_type)
                                    ),
                                )
                                .with_annotated_span(
                                    binary.right.span,
                                    format!(
                                        "right operand has type `{}`",
                                        Self::type_name(right_type)
                                    ),
                                )
                                .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                            );
                            return Type::Unknown;
                        }
                    } else {
                        if !Self::is_numeric_type(left_type) || !Self::is_numeric_type(right_type) {
                            diagnostics.push(
                                SourceDiagnostic::new(
                                    DiagnosticKind::Typecheck,
                                    "ordering operators require numeric operands",
                                )
                                .with_error_code("T1011")
                                .with_hint("use numeric operand types for ordering operations")
                                .with_annotated_span(
                                    binary.left.span,
                                    format!(
                                        "left operand has type `{}`",
                                        Self::type_name(left_type)
                                    ),
                                )
                                .with_annotated_span(
                                    binary.right.span,
                                    format!(
                                        "right operand has type `{}`",
                                        Self::type_name(right_type)
                                    ),
                                )
                                .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                            );
                            return Type::Unknown;
                        }

                        if left_type != right_type {
                            diagnostics.push(
                                SourceDiagnostic::new(
                                    DiagnosticKind::Typecheck,
                                    "ordering operators require operands of the same numeric type",
                                )
                                .with_error_code("T1012")
                                .with_hint("ensure both operands have the same numeric type")
                                .with_annotated_span(
                                    binary.left.span,
                                    format!(
                                        "left operand has type `{}`",
                                        Self::type_name(left_type)
                                    ),
                                )
                                .with_annotated_span(
                                    binary.right.span,
                                    format!(
                                        "right operand has type `{}`",
                                        Self::type_name(right_type)
                                    ),
                                )
                                .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                            );
                            return Type::Unknown;
                        }
                    }
                    return Type::Bool;
                }

                if !Self::is_numeric_type(left_type) || !Self::is_numeric_type(right_type) {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            "arithmetic operators require numeric operands",
                        )
                        .with_error_code("T1006")
                        .with_hint("use numeric operand types for arithmetic operations")
                        .with_annotated_span(
                            binary.left.span,
                            format!("left operand has type `{}`", Self::type_name(left_type)),
                        )
                        .with_annotated_span(
                            binary.right.span,
                            format!("right operand has type `{}`", Self::type_name(right_type)),
                        )
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                    return Type::Unknown;
                }

                if !Self::check_same_arithmetic_operand_type(
                    diagnostics,
                    source,
                    left_type,
                    binary.left.span,
                    right_type,
                    binary.right.span,
                ) {
                    return Type::Unknown;
                }

                if binary.operator == BinaryOperator::Modulo && !Self::is_integer_type(left_type) {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            "operator `%` is only valid for integer types",
                        )
                        .with_error_code("T1007")
                        .with_hint("use integer operands or replace `%` with another operator")
                        .with_annotated_span(
                            expression.span,
                            format!(
                                "operands have type `{}` but `%` requires integer types",
                                Self::type_name(left_type)
                            ),
                        )
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                    return Type::Unknown;
                }

                left_type
            }
            ExprKind::Call(call) => {
                let ExprKind::Identifier(callee_name) = &call.callee.kind else {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            "call target must be a function name",
                        )
                        .with_error_code("T1008")
                        .with_hint("call a named function identifier")
                        .with_annotated_span(call.callee.span, "this expression is not callable")
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                    return Type::Unknown;
                };

                let Some(function_type) = function_types.get(callee_name) else {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            format!("unknown function `{callee_name}`"),
                        )
                        .with_error_code("T1009")
                        .with_annotated_span(call.callee.span, "this function is not defined")
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                    for argument in &call.arguments {
                        let _ = Self::typecheck_expression(
                            argument,
                            diagnostics,
                            source,
                            function_types,
                            function_spans,
                            scopes,
                            assertion_count,
                            None,
                        );
                    }
                    return Type::Unknown;
                };

                if call.arguments.len() != function_type.parameter_types.len() {
                    let mut diagnostic = SourceDiagnostic::new(
                        DiagnosticKind::Typecheck,
                        format!(
                            "function `{callee_name}` expects {} argument(s) but got {}",
                            function_type.parameter_types.len(),
                            call.arguments.len()
                        ),
                    )
                    .with_error_code("T1010")
                    .with_hint("pass the exact number of arguments required by the function")
                    .with_annotated_span(
                        expression.span,
                        "call argument count does not match function signature",
                    );
                    if let Some(definition_span) = function_spans.get(callee_name) {
                        diagnostic = diagnostic.with_annotated_span(
                            *definition_span,
                            format!("function `{callee_name}` is defined here"),
                        );
                    }
                    diagnostics
                        .push(diagnostic.with_source_excerpt(SourceExcerpt::new(source, 1, 0)));
                    for argument in &call.arguments {
                        let _ = Self::typecheck_expression(
                            argument,
                            diagnostics,
                            source,
                            function_types,
                            function_spans,
                            scopes,
                            assertion_count,
                            None,
                        );
                    }
                    return function_type.return_type;
                }

                for (index, argument) in call.arguments.iter().enumerate() {
                    let argument_type = Self::typecheck_expression(
                        argument,
                        diagnostics,
                        source,
                        function_types,
                        function_spans,
                        scopes,
                        assertion_count,
                        Some(function_type.parameter_types[index]),
                    );
                    let expected_type = function_type.parameter_types[index];
                    if argument_type == Type::Unknown {
                        continue;
                    }
                    let _ = Self::check_same_type(
                        diagnostics,
                        source,
                        expected_type,
                        argument.span,
                        argument_type,
                        argument.span,
                        "call argument type does not match parameter type",
                    );
                }

                function_type.return_type
            }
            ExprKind::If(if_expression) => {
                let condition_type = Self::typecheck_expression(
                    &if_expression.condition,
                    diagnostics,
                    source,
                    function_types,
                    function_spans,
                    scopes,
                    assertion_count,
                    None,
                );
                if condition_type != Type::Bool && condition_type != Type::Unknown {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            "if condition must evaluate to `bool`",
                        )
                        .with_error_code("T1011")
                        .with_hint("make the if condition evaluate to `bool`")
                        .with_annotated_span(
                            if_expression.condition.span,
                            "this condition is not `bool`",
                        )
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                }

                let then_type = Self::typecheck_expression(
                    &if_expression.then_branch,
                    diagnostics,
                    source,
                    function_types,
                    function_spans,
                    scopes,
                    assertion_count,
                    expected_type,
                );
                let else_type = if let Some(else_branch) = &if_expression.else_branch {
                    Self::typecheck_expression(
                        else_branch,
                        diagnostics,
                        source,
                        function_types,
                        function_spans,
                        scopes,
                        assertion_count,
                        expected_type,
                    )
                } else {
                    Type::Unit
                };

                if !Self::check_same_type(
                    diagnostics,
                    source,
                    then_type,
                    if_expression.then_branch.span,
                    else_type,
                    if_expression
                        .else_branch
                        .as_ref()
                        .map(|branch| branch.span)
                        .unwrap_or(expression.span),
                    "if branches must evaluate to the same type",
                ) {
                    return Type::Unknown;
                }
                then_type
            }
            ExprKind::While(while_expression) => {
                let condition_type = Self::typecheck_expression(
                    &while_expression.condition,
                    diagnostics,
                    source,
                    function_types,
                    function_spans,
                    scopes,
                    assertion_count,
                    None,
                );
                if condition_type != Type::Bool && condition_type != Type::Unknown {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            "while condition must evaluate to `bool`",
                        )
                        .with_error_code("T1012")
                        .with_hint("make the while condition evaluate to `bool`")
                        .with_annotated_span(
                            while_expression.condition.span,
                            "this condition is not `bool`",
                        )
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                }

                let body_type = Self::typecheck_expression(
                    &while_expression.body,
                    diagnostics,
                    source,
                    function_types,
                    function_spans,
                    scopes,
                    assertion_count,
                    None,
                );
                if body_type != Type::Unit && body_type != Type::Unknown {
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            "while body must evaluate to `()`",
                        )
                        .with_error_code("T1013")
                        .with_hint("remove trailing values from the loop body")
                        .with_annotated_span(
                            while_expression.body.span,
                            format!(
                                "this body evaluates to `{}` but loops require `()`",
                                Self::type_name(body_type)
                            ),
                        )
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                }
                Type::Unit
            }
            ExprKind::Block(block_expression) => {
                scopes.push_scope();
                for statement in &block_expression.statements {
                    Self::typecheck_statement(
                        statement,
                        diagnostics,
                        source,
                        function_types,
                        function_spans,
                        scopes,
                        assertion_count,
                    );
                }
                let result_type = if let Some(result) = &block_expression.result {
                    Self::typecheck_expression(
                        result,
                        diagnostics,
                        source,
                        function_types,
                        function_spans,
                        scopes,
                        assertion_count,
                        expected_type,
                    )
                } else {
                    Type::Unit
                };
                scopes.pop_scope();
                result_type
            }
        }
    }
}

impl Typechecker for BasicTypechecker {
    fn typecheck_module(&self, module: &Module, source: &str) -> TypecheckResult {
        let mut seen_test_names = HashMap::new();
        let mut assertion_count = 0usize;
        let mut diagnostics = Vec::new();
        let mut timings = Vec::new();
        let mut function_types = HashMap::new();
        let mut function_spans = HashMap::new();
        let mut seen_function_names = HashMap::new();
        for function in &module.functions {
            if let Some(first_span) = seen_function_names.get(function.name.as_str()).copied() {
                diagnostics.push(
                    SourceDiagnostic::new(
                        DiagnosticKind::Typecheck,
                        format!("duplicate function name `{}` in module", function.name),
                    )
                    .with_error_code("T1014")
                    .with_annotated_span(first_span, "first declaration")
                    .with_annotated_span(function.span, "duplicate declaration")
                    .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                );
            } else {
                seen_function_names.insert(function.name.clone(), function.span);
            }

            function_types.insert(
                function.name.clone(),
                FunctionType {
                    parameter_types: function
                        .parameters
                        .iter()
                        .map(|parameter| Self::type_from_ref(parameter.ty))
                        .collect(),
                    return_type: Self::type_from_ref(function.return_type),
                },
            );
            function_spans.insert(function.name.clone(), function.span);
        }

        // Add native functions to function_types for type checking
        for (name, native_fn) in self.native_functions.iter() {
            function_types.insert(
                name.clone(),
                FunctionType {
                    parameter_types: native_fn
                        .param_types()
                        .iter()
                        .cloned()
                        .map(Self::type_from_interpreter_type)
                        .collect(),
                    return_type: Self::type_from_interpreter_type(native_fn.return_type()),
                },
            );
        }

        for function in &module.functions {
            let mut parameter_names = HashSet::new();
            let mut scopes = ScopeStack::default();
            scopes.push_scope();
            for parameter in &function.parameters {
                if !parameter_names.insert(parameter.name.clone()) {
                    let first_declaration_span = scopes
                        .lookup_current_scope(&parameter.name)
                        .map(|entry| entry.declaration_span)
                        .unwrap_or(parameter.span);
                    diagnostics.push(
                        SourceDiagnostic::new(
                            DiagnosticKind::Typecheck,
                            format!(
                                "duplicate parameter name `{}` in function `{}`",
                                parameter.name, function.name
                            ),
                        )
                        .with_error_code("T1015")
                        .with_annotated_span(first_declaration_span, "first declaration")
                        .with_annotated_span(parameter.span, "duplicate declaration")
                        .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                    );
                    continue;
                }
                scopes.insert_current_scope(
                    parameter.name.clone(),
                    SymbolEntry {
                        ty: Self::type_from_ref(parameter.ty),
                        kind: SymbolKind::Parameter,
                        declaration_span: parameter.span,
                    },
                );
            }

            // Function bodies are an inner lexical scope, so locals may shadow parameters.
            scopes.push_scope();
            for statement in &function.statements {
                Self::typecheck_statement(
                    statement,
                    &mut diagnostics,
                    source,
                    &function_types,
                    &function_spans,
                    &mut scopes,
                    &mut assertion_count,
                );
            }
            scopes.pop_scope();
            scopes.pop_scope();
        }

        for test in &module.tests {
            let timer = TaskTimer::start(format!("typecheck test `{}`", test.name));
            if let Some(first_span) = seen_test_names.get(test.name.as_str()).copied() {
                diagnostics.push(
                    SourceDiagnostic::new(
                        DiagnosticKind::Typecheck,
                        format!("duplicate test function name `{}` in module", test.name),
                    )
                    .with_error_code("T1016")
                    .with_annotated_span(first_span, "first declaration of this test name")
                    .with_annotated_span(test.span, "duplicate declaration")
                    .with_source_excerpt(SourceExcerpt::new(source, 1, 0)),
                );
            } else {
                seen_test_names.insert(test.name.clone(), test.span);
            }

            let mut scopes = ScopeStack::default();
            scopes.push_scope();
            for statement in &test.statements {
                Self::typecheck_statement(
                    statement,
                    &mut diagnostics,
                    source,
                    &function_types,
                    &function_spans,
                    &mut scopes,
                    &mut assertion_count,
                );
            }
            scopes.pop_scope();

            let timing = timer.finish();
            timings.push(timing.clone());
            info!(
                test_name = %test.name,
                stage = "typecheck_test",
                elapsed_ms = timing.elapsed.as_secs_f64() * 1000.0,
                "test timing"
            );
        }

        TypecheckResult {
            summary: TypecheckSummary {
                test_count: module.tests.len(),
                assertion_count,
            },
            diagnostics,
            timings,
            function_types,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{BasicTypechecker, Typechecker};
    use holo_ast::{
        AssertStatement, BinaryOperator, Expr, FunctionItem, FunctionParameter, Module, Statement,
        TestItem, TypeRef,
    };
    use holo_base::Span;

    #[test]
    fn typechecks_assertion_count_for_module() {
        let module = Module {
            functions: Vec::new(),
            tests: vec![TestItem {
                name: "sample".into(),
                statements: vec![Statement::Assert(AssertStatement {
                    expression: Expr::bool_literal(true, Span::new(20, 24)),
                    span: Span::new(13, 25),
                })],
                span: Span::new(0, 26),
            }],
        };

        let result =
            BasicTypechecker::default().typecheck_module(&module, "#[test] fn sample() { ... }");
        assert_eq!(result.summary.test_count, 1);
        assert_eq!(result.summary.assertion_count, 1);
        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn reports_duplicate_test_names_and_continues() {
        let module = Module {
            functions: Vec::new(),
            tests: vec![
                TestItem {
                    name: "same_name".into(),
                    statements: vec![Statement::Assert(AssertStatement {
                        expression: Expr::bool_literal(true, Span::new(20, 24)),
                        span: Span::new(13, 25),
                    })],
                    span: Span::new(0, 26),
                },
                TestItem {
                    name: "same_name".into(),
                    statements: vec![Statement::Assert(AssertStatement {
                        expression: Expr::bool_literal(true, Span::new(50, 54)),
                        span: Span::new(43, 55),
                    })],
                    span: Span::new(30, 56),
                },
            ],
        };

        let result =
            BasicTypechecker::default().typecheck_module(&module, "#[test] fn same_name() { ... }");
        assert_eq!(result.summary.test_count, 2);
        assert_eq!(result.summary.assertion_count, 2);
        assert_eq!(result.diagnostics.len(), 1);
        assert!(result.diagnostics[0]
            .message
            .contains("duplicate test function name"));
    }

    #[test]
    fn infers_default_numeric_literal_types() {
        let module = Module {
            functions: Vec::new(),
            tests: vec![TestItem {
                name: "numeric".into(),
                statements: vec![Statement::Assert(AssertStatement {
                    expression: Expr::binary(
                        BinaryOperator::Add,
                        Expr::number_literal("1", Span::new(0, 1)),
                        Expr::number_literal("2", Span::new(4, 5)),
                        Span::new(0, 5),
                    ),
                    span: Span::new(0, 6),
                })],
                span: Span::new(0, 6),
            }],
        };

        let result = BasicTypechecker::default().typecheck_module(&module, "assert(1 + 2);");
        assert!(!result.diagnostics.is_empty());
        assert!(result.diagnostics.iter().any(|diagnostic| diagnostic
            .message
            .contains("assert expects a boolean expression")));
    }

    #[test]
    fn rejects_mixed_numeric_types_in_binary_ops() {
        let module = Module {
            functions: Vec::new(),
            tests: vec![TestItem {
                name: "mixed".into(),
                statements: vec![Statement::Assert(AssertStatement {
                    expression: Expr::binary(
                        BinaryOperator::Add,
                        Expr::number_literal("1i64", Span::new(0, 4)),
                        Expr::number_literal("2.0f64", Span::new(7, 13)),
                        Span::new(0, 13),
                    ),
                    span: Span::new(0, 14),
                })],
                span: Span::new(0, 14),
            }],
        };

        let result =
            BasicTypechecker::default().typecheck_module(&module, "assert(1i64 + 2.0f64);");
        let mismatch = result
            .diagnostics
            .iter()
            .find(|diagnostic| diagnostic.message.contains("same type"))
            .expect("expected arithmetic same-type diagnostic");
        assert_eq!(mismatch.source_excerpts.len(), 1);
        assert_eq!(mismatch.annotated_spans.len(), 2);
        let rendered = holo_base::display_source_diagnostics(&result.diagnostics);
        assert!(
            rendered.contains("left operand has type `i64`"),
            "{rendered}"
        );
        assert!(
            rendered.contains("right operand has type `f64`"),
            "{rendered}"
        );
    }

    #[test]
    fn reports_unknown_identifier() {
        let module = Module {
            functions: vec![FunctionItem {
                name: "entry".into(),
                parameters: Vec::new(),
                return_type: TypeRef::Unit,
                statements: vec![Statement::Expr(holo_ast::ExprStatement {
                    expression: Expr::identifier("missing", Span::new(0, 7)),
                    span: Span::new(0, 8),
                })],
                is_test: false,
                span: Span::new(0, 8),
            }],
            tests: Vec::new(),
        };

        let result = BasicTypechecker::default().typecheck_module(&module, "missing;");
        assert!(result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("unknown identifier")));
    }

    #[test]
    fn allows_local_to_shadow_parameter_in_function_body_scope() {
        let module = Module {
            functions: vec![FunctionItem {
                name: "shadow".into(),
                parameters: vec![FunctionParameter {
                    name: "value".into(),
                    ty: TypeRef::I64,
                    span: Span::new(10, 20),
                }],
                return_type: TypeRef::I64,
                statements: vec![
                    Statement::Let(holo_ast::LetStatement {
                        name: "value".into(),
                        ty: Some(TypeRef::I64),
                        value: Expr::number_literal("1i64", Span::new(35, 39)),
                        span: Span::new(24, 40),
                    }),
                    Statement::Expr(holo_ast::ExprStatement {
                        expression: Expr::identifier("value", Span::new(45, 50)),
                        span: Span::new(45, 51),
                    }),
                ],
                is_test: false,
                span: Span::new(0, 53),
            }],
            tests: Vec::new(),
        };

        let result = BasicTypechecker::default().typecheck_module(
            &module,
            "fn shadow(value: i64) -> i64 { let value: i64 = 1i64; value; }",
        );
        assert!(
            result
                .diagnostics
                .iter()
                .all(|diagnostic| !diagnostic.message.contains("duplicate local binding")),
            "unexpected diagnostics: {:?}",
            result.diagnostics
        );
    }

    #[test]
    fn reports_unknown_function_call() {
        let module = Module {
            functions: vec![FunctionItem {
                name: "entry".into(),
                parameters: Vec::new(),
                return_type: TypeRef::Unit,
                statements: vec![Statement::Expr(holo_ast::ExprStatement {
                    expression: Expr::call(
                        Expr::identifier("unknown_fn", Span::new(0, 10)),
                        vec![],
                        Span::new(0, 12),
                    ),
                    span: Span::new(0, 13),
                })],
                is_test: false,
                span: Span::new(0, 13),
            }],
            tests: Vec::new(),
        };

        let result = BasicTypechecker::default().typecheck_module(&module, "unknown_fn();");
        assert!(result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("unknown function")));
    }

    #[test]
    fn reports_call_arity_mismatch() {
        let module = Module {
            functions: vec![
                FunctionItem {
                    name: "sum".into(),
                    parameters: vec![
                        FunctionParameter {
                            name: "a".into(),
                            ty: TypeRef::I64,
                            span: Span::new(0, 1),
                        },
                        FunctionParameter {
                            name: "b".into(),
                            ty: TypeRef::I64,
                            span: Span::new(3, 4),
                        },
                    ],
                    return_type: TypeRef::I64,
                    statements: Vec::new(),
                    is_test: false,
                    span: Span::new(0, 10),
                },
                FunctionItem {
                    name: "entry".into(),
                    parameters: Vec::new(),
                    return_type: TypeRef::Unit,
                    statements: vec![Statement::Expr(holo_ast::ExprStatement {
                        expression: Expr::call(
                            Expr::identifier("sum", Span::new(11, 14)),
                            vec![Expr::number_literal("1", Span::new(15, 16))],
                            Span::new(11, 17),
                        ),
                        span: Span::new(11, 18),
                    })],
                    is_test: false,
                    span: Span::new(11, 18),
                },
            ],
            tests: Vec::new(),
        };

        let result = BasicTypechecker::default().typecheck_module(&module, "sum(1);");
        assert!(result.diagnostics.iter().any(|diagnostic| diagnostic
            .message
            .contains("expects 2 argument(s) but got 1")));
        let rendered = holo_base::display_source_diagnostics(&result.diagnostics);
        assert!(
            rendered.contains("function `sum` is defined here"),
            "{rendered}"
        );
    }

    #[test]
    fn rejects_non_numeric_arithmetic_operands() {
        let module = Module {
            functions: Vec::new(),
            tests: vec![TestItem {
                name: "non_numeric".into(),
                statements: vec![Statement::Assert(AssertStatement {
                    expression: Expr::binary(
                        BinaryOperator::Add,
                        Expr::bool_literal(true, Span::new(0, 4)),
                        Expr::bool_literal(false, Span::new(7, 12)),
                        Span::new(0, 12),
                    ),
                    span: Span::new(0, 13),
                })],
                span: Span::new(0, 13),
            }],
        };

        let result = BasicTypechecker::default().typecheck_module(&module, "assert(true + false);");
        assert!(result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("require numeric operands")));
        let rendered = holo_base::display_source_diagnostics(&result.diagnostics);
        assert!(
            rendered.contains("left operand has type `bool`"),
            "{rendered}"
        );
        assert!(
            rendered.contains("right operand has type `bool`"),
            "{rendered}"
        );
    }

    #[test]
    fn rejects_float_modulo() {
        let module = Module {
            functions: Vec::new(),
            tests: vec![TestItem {
                name: "float_modulo".into(),
                statements: vec![Statement::Assert(AssertStatement {
                    expression: Expr::binary(
                        BinaryOperator::Modulo,
                        Expr::number_literal("1.0f64", Span::new(0, 6)),
                        Expr::number_literal("2.0f64", Span::new(9, 15)),
                        Span::new(0, 15),
                    ),
                    span: Span::new(0, 16),
                })],
                span: Span::new(0, 16),
            }],
        };

        let result =
            BasicTypechecker::default().typecheck_module(&module, "assert(1.0f64 % 2.0f64);");
        assert!(result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("only valid for integer types")));
        let rendered = holo_base::display_source_diagnostics(&result.diagnostics);
        assert!(rendered.contains("operands have type `f64`"), "{rendered}");
    }

    #[test]
    fn reports_duplicate_parameter_with_both_spans() {
        let module = Module {
            functions: vec![FunctionItem {
                name: "dup_params".into(),
                parameters: vec![
                    FunctionParameter {
                        name: "value".into(),
                        ty: TypeRef::I64,
                        span: Span::new(14, 24),
                    },
                    FunctionParameter {
                        name: "value".into(),
                        ty: TypeRef::I64,
                        span: Span::new(26, 36),
                    },
                ],
                return_type: TypeRef::I64,
                statements: vec![Statement::Expr(holo_ast::ExprStatement {
                    expression: Expr::identifier("value", Span::new(40, 45)),
                    span: Span::new(40, 46),
                })],
                is_test: false,
                span: Span::new(0, 48),
            }],
            tests: Vec::new(),
        };

        let result = BasicTypechecker::default().typecheck_module(
            &module,
            "fn dup_params(value: i64, value: i64) -> i64 { value; }",
        );
        let diagnostic = result
            .diagnostics
            .iter()
            .find(|diagnostic| diagnostic.message.contains("duplicate parameter name"))
            .expect("expected duplicate parameter diagnostic");
        assert_eq!(diagnostic.annotated_spans.len(), 2);
        let rendered = holo_base::display_source_diagnostics(std::slice::from_ref(diagnostic));
        assert!(rendered.contains("first declaration"), "{rendered}");
        assert!(rendered.contains("duplicate declaration"), "{rendered}");
    }

    #[test]
    fn rejects_if_branch_type_mismatch() {
        let module = Module {
            functions: vec![FunctionItem {
                name: "branch".into(),
                parameters: Vec::new(),
                return_type: TypeRef::Unit,
                statements: vec![Statement::Expr(holo_ast::ExprStatement {
                    expression: Expr::if_expression(
                        Expr::bool_literal(true, Span::new(0, 4)),
                        Expr::block(
                            Vec::new(),
                            Some(Expr::number_literal("1i64", Span::new(10, 14))),
                            Span::new(8, 16),
                        ),
                        Some(Expr::block(
                            Vec::new(),
                            Some(Expr::bool_literal(false, Span::new(24, 29))),
                            Span::new(22, 31),
                        )),
                        Span::new(0, 31),
                    ),
                    span: Span::new(0, 32),
                })],
                is_test: false,
                span: Span::new(0, 32),
            }],
            tests: Vec::new(),
        };

        let result = BasicTypechecker::default()
            .typecheck_module(&module, "if true { 1i64 } else { false };");
        assert!(result.diagnostics.iter().any(|diagnostic| diagnostic
            .message
            .contains("if branches must evaluate to the same type")));
    }

    #[test]
    fn rejects_non_bool_while_condition() {
        let module = Module {
            functions: vec![FunctionItem {
                name: "loop_bad".into(),
                parameters: Vec::new(),
                return_type: TypeRef::Unit,
                statements: vec![Statement::Expr(holo_ast::ExprStatement {
                    expression: Expr::while_expression(
                        Expr::number_literal("1i64", Span::new(0, 4)),
                        Expr::block(Vec::new(), None, Span::new(6, 8)),
                        Span::new(0, 8),
                    ),
                    span: Span::new(0, 9),
                })],
                is_test: false,
                span: Span::new(0, 9),
            }],
            tests: Vec::new(),
        };

        let result = BasicTypechecker::default().typecheck_module(&module, "while 1i64 { };");
        assert!(result.diagnostics.iter().any(|diagnostic| diagnostic
            .message
            .contains("while condition must evaluate to `bool`")));
    }

    #[test]
    fn block_expression_scopes_locals() {
        let module = Module {
            functions: vec![FunctionItem {
                name: "block_scope".into(),
                parameters: Vec::new(),
                return_type: TypeRef::Unit,
                statements: vec![
                    Statement::Expr(holo_ast::ExprStatement {
                        expression: Expr::block(
                            vec![Statement::Let(holo_ast::LetStatement {
                                name: "inner".into(),
                                ty: Some(TypeRef::I64),
                                value: Expr::number_literal("1i64", Span::new(8, 12)),
                                span: Span::new(0, 13),
                            })],
                            Some(Expr::identifier("inner", Span::new(14, 19))),
                            Span::new(0, 20),
                        ),
                        span: Span::new(0, 21),
                    }),
                    Statement::Expr(holo_ast::ExprStatement {
                        expression: Expr::identifier("inner", Span::new(22, 27)),
                        span: Span::new(22, 28),
                    }),
                ],
                is_test: false,
                span: Span::new(0, 28),
            }],
            tests: Vec::new(),
        };

        let result = BasicTypechecker::default()
            .typecheck_module(&module, "{ let inner: i64 = 1i64; inner }; inner;");
        assert!(result
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("unknown identifier `inner`")));
    }

    #[test]
    fn infers_unsuffixed_literal_from_let_annotation() {
        let module = Module {
            functions: vec![FunctionItem {
                name: "typed_let".into(),
                parameters: Vec::new(),
                return_type: TypeRef::Unit,
                statements: vec![Statement::Let(holo_ast::LetStatement {
                    name: "value".into(),
                    ty: Some(TypeRef::U32),
                    value: Expr::number_literal("1", Span::new(0, 1)),
                    span: Span::new(0, 2),
                })],
                is_test: false,
                span: Span::new(0, 2),
            }],
            tests: Vec::new(),
        };

        let result = BasicTypechecker::default().typecheck_module(&module, "let value: u32 = 1;");
        assert!(
            result.diagnostics.iter().all(|diagnostic| !diagnostic
                .message
                .contains("let binding type does not match")),
            "unexpected diagnostics: {:?}",
            result.diagnostics
        );
    }

    #[test]
    fn infers_unsuffixed_literal_from_call_parameter_type() {
        let module = Module {
            functions: vec![
                FunctionItem {
                    name: "takes_u32".into(),
                    parameters: vec![FunctionParameter {
                        name: "x".into(),
                        ty: TypeRef::U32,
                        span: Span::new(0, 1),
                    }],
                    return_type: TypeRef::Unit,
                    statements: Vec::new(),
                    is_test: false,
                    span: Span::new(0, 1),
                },
                FunctionItem {
                    name: "entry".into(),
                    parameters: Vec::new(),
                    return_type: TypeRef::Unit,
                    statements: vec![Statement::Expr(holo_ast::ExprStatement {
                        expression: Expr::call(
                            Expr::identifier("takes_u32", Span::new(0, 9)),
                            vec![Expr::number_literal("1", Span::new(10, 11))],
                            Span::new(0, 12),
                        ),
                        span: Span::new(0, 13),
                    })],
                    is_test: false,
                    span: Span::new(0, 13),
                },
            ],
            tests: Vec::new(),
        };

        let result = BasicTypechecker::default().typecheck_module(&module, "takes_u32(1);");
        assert!(result.diagnostics.iter().all(|diagnostic| !diagnostic
            .message
            .contains("call argument type does not match")));
    }

    #[test]
    fn keeps_no_implicit_coercion_for_non_literal_values() {
        let module = Module {
            functions: vec![
                FunctionItem {
                    name: "takes_u32".into(),
                    parameters: vec![FunctionParameter {
                        name: "x".into(),
                        ty: TypeRef::U32,
                        span: Span::new(0, 1),
                    }],
                    return_type: TypeRef::Unit,
                    statements: Vec::new(),
                    is_test: false,
                    span: Span::new(0, 1),
                },
                FunctionItem {
                    name: "entry".into(),
                    parameters: Vec::new(),
                    return_type: TypeRef::Unit,
                    statements: vec![
                        Statement::Let(holo_ast::LetStatement {
                            name: "value".into(),
                            ty: Some(TypeRef::I64),
                            value: Expr::number_literal("1i64", Span::new(0, 4)),
                            span: Span::new(0, 5),
                        }),
                        Statement::Expr(holo_ast::ExprStatement {
                            expression: Expr::call(
                                Expr::identifier("takes_u32", Span::new(6, 15)),
                                vec![Expr::identifier("value", Span::new(16, 21))],
                                Span::new(6, 22),
                            ),
                            span: Span::new(6, 23),
                        }),
                    ],
                    is_test: false,
                    span: Span::new(0, 23),
                },
            ],
            tests: Vec::new(),
        };

        let result = BasicTypechecker::default()
            .typecheck_module(&module, "let value: i64 = 1i64; takes_u32(value);");
        let diagnostic = result
            .diagnostics
            .iter()
            .find(|diagnostic| {
                diagnostic
                    .message
                    .contains("call argument type does not match")
            })
            .expect("expected no-implicit-coercion diagnostic");
        let rendered = holo_base::display_source_diagnostics(std::slice::from_ref(diagnostic));
        assert!(
            rendered.contains("implicit numeric conversions are not allowed"),
            "{rendered}"
        );
        assert_eq!(diagnostic.error_code.as_deref(), Some("T1100"));
        assert!(diagnostic.hint.is_some());
    }
}
