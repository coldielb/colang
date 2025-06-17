/// Type checking and inference for COLANG
/// 
/// This module performs static type analysis, type inference, and semantic validation
/// on the COLANG AST. It tracks types through expressions, validates assignments,
/// and ensures type safety.

use std::collections::HashMap;
use crate::ast::*;
use crate::ast::expr::{BinaryOp, UnaryOp, Literal, Pattern};
use crate::ast::stmt::Statement;
use crate::ast::decl::{Declaration, Parameter};
use crate::ast::types::Type;
use crate::semantics::symbol_table::{SymbolTable, Symbol, SymbolType, TypeInfo, ScopeType, SymbolError};
use crate::util::Span;

/// Type checker that validates types and performs inference
pub struct TypeChecker {
    symbol_table: SymbolTable,
    errors: Vec<TypeError>,
    current_function_return_type: Option<TypeNode>,
}

/// Type checking errors
#[derive(Debug, Clone)]
pub enum TypeError {
    TypeMismatch {
        expected: String,
        found: String,
        span: Span,
    },
    UndefinedVariable {
        name: String,
        span: Span,
    },
    UndefinedFunction {
        name: String,
        span: Span,
    },
    ArgumentCountMismatch {
        expected: usize,
        found: usize,
        function_name: String,
        span: Span,
    },
    ArgumentTypeMismatch {
        parameter_index: usize,
        expected: String,
        found: String,
        function_name: String,
        span: Span,
    },
    ReturnTypeMismatch {
        expected: String,
        found: String,
        span: Span,
    },
    CannotInferType {
        expression: String,
        span: Span,
    },
    InvalidBinaryOperation {
        operator: String,
        left_type: String,
        right_type: String,
        span: Span,
    },
    InvalidUnaryOperation {
        operator: String,
        operand_type: String,
        span: Span,
    },
    DuplicateSymbol {
        name: String,
        original_span: Span,
        duplicate_span: Span,
    },
    AssignmentToImmutable {
        name: String,
        span: Span,
    },
    ReturnOutsideFunction {
        span: Span,
    },
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::TypeMismatch { expected, found, span } => {
                write!(f, "error: type mismatch at {}\n  expected: {}\n  found: {}", span, expected, found)
            },
            TypeError::UndefinedVariable { name, span } => {
                write!(f, "error: undefined variable '{}' at {}", name, span)
            },
            TypeError::UndefinedFunction { name, span } => {
                write!(f, "error: undefined function '{}' at {}", name, span)
            },
            TypeError::ArgumentCountMismatch { expected, found, function_name, span } => {
                write!(f, "error: function '{}' expects {} arguments, found {} at {}", 
                       function_name, expected, found, span)
            },
            TypeError::ArgumentTypeMismatch { parameter_index, expected, found, function_name, span } => {
                write!(f, "error: argument {} to function '{}' has type {}, expected {} at {}", 
                       parameter_index + 1, function_name, found, expected, span)
            },
            TypeError::ReturnTypeMismatch { expected, found, span } => {
                write!(f, "error: return type mismatch at {}\n  expected: {}\n  found: {}", span, expected, found)
            },
            TypeError::CannotInferType { expression, span } => {
                write!(f, "error: cannot infer type for expression '{}' at {}", expression, span)
            },
            TypeError::InvalidBinaryOperation { operator, left_type, right_type, span } => {
                write!(f, "error: invalid binary operation {} {} {} at {}", 
                       left_type, operator, right_type, span)
            },
            TypeError::InvalidUnaryOperation { operator, operand_type, span } => {
                write!(f, "error: invalid unary operation {}{} at {}", operator, operand_type, span)
            },
            TypeError::DuplicateSymbol { name, original_span, duplicate_span } => {
                write!(f, "error: duplicate symbol '{}' at {}, originally declared at {}", 
                       name, duplicate_span, original_span)
            },
            TypeError::AssignmentToImmutable { name, span } => {
                write!(f, "error: cannot assign to immutable variable '{}' at {}", name, span)
            },
            TypeError::ReturnOutsideFunction { span } => {
                write!(f, "error: return statement outside function at {}", span)
            },
        }
    }
}

impl std::error::Error for TypeError {}

impl TypeChecker {
    /// Create a new type checker
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),
            current_function_return_type: None,
        }
    }
    
    /// Type check a complete program
    pub fn check_program(&mut self, program: &Program) -> Result<(), Vec<TypeError>> {
        // First pass: collect all function signatures and type definitions
        for item in &program.items {
            if let Item::Declaration(ref decl) = item.inner {
                self.collect_declaration_signature(decl);
            }
        }
        
        // Second pass: type check function bodies and validate expressions
        for item in &program.items {
            if let Item::Declaration(ref decl) = item.inner {
                self.check_declaration(decl);
            }
        }
        
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }
    
    /// Collect function and type signatures without checking bodies
    fn collect_declaration_signature(&mut self, decl: &DeclarationNode) {
        match &decl.inner {
            Declaration::Function { name, parameters, return_type, .. } => {
                let param_types: Vec<TypeNode> = parameters.iter()
                    .map(|p| p.param_type.clone())
                    .collect();
                
                let ret_type = return_type.as_ref()
                    .cloned()
                    .unwrap_or_else(|| self.create_unit_type(decl.span));
                
                let symbol = Symbol::function(
                    name.clone(),
                    param_types,
                    ret_type,
                    decl.span,
                    self.symbol_table.current_scope_id(),
                );
                
                if let Err(SymbolError::DuplicateSymbol { name, original_span, duplicate_span }) = 
                   self.symbol_table.add_symbol(symbol) {
                    self.errors.push(TypeError::DuplicateSymbol {
                        name,
                        original_span,
                        duplicate_span,
                    });
                }
            },
            Declaration::Struct { name, .. } => {
                // TODO: Collect struct type definitions
            },
            Declaration::Enum { name, .. } => {
                // TODO: Collect enum type definitions
            },
            _ => {
                // Other declarations handled in second pass
            },
        }
    }
    
    /// Type check a declaration
    fn check_declaration(&mut self, decl: &DeclarationNode) {
        match &decl.inner {
            Declaration::Function { name, parameters, return_type, body, .. } => {
                self.check_function(name, parameters, return_type.as_ref(), body.as_ref(), decl.span);
            },
            Declaration::Struct { .. } => {
                // TODO: Check struct definition
            },
            Declaration::Enum { .. } => {
                // TODO: Check enum definition
            },
            _ => {
                // TODO: Handle other declaration types
            },
        }
    }
    
    /// Type check a function
    fn check_function(
        &mut self, 
        name: &str, 
        parameters: &[Parameter], 
        return_type: Option<&TypeNode>,
        body: Option<&Block>,
        span: Span,
    ) {
        // Enter function scope
        let scope_id = self.symbol_table.enter_scope(
            ScopeType::Function { name: name.to_string() },
            Some(span),
        );
        
        // Set current function return type for return statement checking
        self.current_function_return_type = return_type.cloned()
            .or_else(|| Some(self.create_unit_type(span)));
        
        // Add parameters to symbol table
        for param in parameters {
            let symbol = Symbol::parameter(
                self.get_pattern_identifier(&param.pattern).unwrap_or_else(|| "unknown".to_string()),
                TypeInfo::Known(param.param_type.clone()),
                param.span,
                false, // TODO: Handle mutable parameters
                scope_id,
            );
            
            if let Err(SymbolError::DuplicateSymbol { name, original_span, duplicate_span }) = 
               self.symbol_table.add_symbol(symbol) {
                self.errors.push(TypeError::DuplicateSymbol {
                    name,
                    original_span,
                    duplicate_span,
                });
            }
        }
        
        // Type check function body
        if let Some(body) = body {
            self.check_block(body);
        }
        
        // Exit function scope
        if let Err(_) = self.symbol_table.exit_scope() {
            // Error handling for scope exit failure
        }
        
        self.current_function_return_type = None;
    }
    
    /// Type check a block
    fn check_block(&mut self, block: &Block) {
        for stmt in &block.statements {
            self.check_statement(stmt);
        }
    }
    
    /// Type check a statement
    fn check_statement(&mut self, stmt: &StmtNode) {
        match &stmt.inner {
            Statement::Let { pattern, type_annotation, initializer, mutable } => {
                self.check_let_statement(pattern, type_annotation.as_ref(), initializer.as_ref(), *mutable, stmt.span);
            },
            Statement::Return(expr_opt) => {
                self.check_return_statement(expr_opt.as_ref(), stmt.span);
            },
            Statement::Expression(expr) => {
                self.check_expression(expr);
            },
            _ => {
                // TODO: Handle other statement types
            },
        }
    }
    
    /// Type check a let statement
    fn check_let_statement(
        &mut self,
        pattern: &Pattern,
        type_annotation: Option<&TypeNode>,
        initializer: Option<&ExprNode>,
        is_mutable: bool,
        span: Span,
    ) {
        let variable_name = if let Some(name) = self.get_pattern_identifier(pattern) {
            name
        } else {
            self.errors.push(TypeError::CannotInferType {
                expression: "complex pattern".to_string(),
                span,
            });
            return;
        };
        
        let variable_type = if let Some(init_expr) = initializer {
            // Type inference from initializer
            let inferred_type = self.check_expression(init_expr);
            
            if let Some(annotation) = type_annotation {
                // Check that annotation matches inferred type
                if !self.types_compatible(&inferred_type, annotation) {
                    self.errors.push(TypeError::TypeMismatch {
                        expected: self.type_to_string(annotation),
                        found: self.type_to_string(&inferred_type),
                        span,
                    });
                }
                annotation.clone()
            } else {
                inferred_type
            }
        } else if let Some(annotation) = type_annotation {
            // Use explicit type annotation
            annotation.clone()
        } else {
            // Cannot infer type without initializer or annotation
            self.errors.push(TypeError::CannotInferType {
                expression: variable_name.clone(),
                span,
            });
            return;
        };
        
        // Add variable to symbol table
        let symbol = Symbol::variable(
            variable_name,
            TypeInfo::Known(variable_type),
            span,
            is_mutable,
            self.symbol_table.current_scope_id(),
        );
        
        if let Err(SymbolError::DuplicateSymbol { name, original_span, duplicate_span }) = 
           self.symbol_table.add_symbol(symbol) {
            self.errors.push(TypeError::DuplicateSymbol {
                name,
                original_span,
                duplicate_span,
            });
        }
    }
    
    /// Type check a return statement
    fn check_return_statement(&mut self, expr_opt: Option<&ExprNode>, span: Span) {
        if !self.symbol_table.is_in_function() {
            self.errors.push(TypeError::ReturnOutsideFunction { span });
            return;
        }
        
        let return_type = if let Some(expr) = expr_opt {
            self.check_expression(expr)
        } else {
            self.create_unit_type(span)
        };
        
        if let Some(ref expected_type) = self.current_function_return_type {
            if !self.types_compatible(&return_type, expected_type) {
                self.errors.push(TypeError::ReturnTypeMismatch {
                    expected: self.type_to_string(expected_type),
                    found: self.type_to_string(&return_type),
                    span,
                });
            }
        }
    }
    
    /// Type check an expression and return its type
    fn check_expression(&mut self, expr: &ExprNode) -> TypeNode {
        match &expr.inner {
            Expr::Literal(literal) => self.check_literal(literal, expr.span),
            Expr::Identifier(name) => self.check_identifier(name, expr.span),
            Expr::Binary { left, operator, right } => {
                self.check_binary_expression(left, *operator, right, expr.span)
            },
            Expr::Unary { operator, operand } => {
                self.check_unary_expression(*operator, operand, expr.span)
            },
            Expr::Call { function, args } => {
                self.check_call_expression(function, args, expr.span)
            },
            Expr::FieldAccess { object, field } => {
                self.check_field_access(object, field, expr.span)
            },
            Expr::Index { array, index } => {
                self.check_index_expression(array, index, expr.span)
            },
            Expr::Block(statements) => {
                self.check_block_expression(statements, expr.span)
            },
            Expr::If { condition, then_expr, else_expr } => {
                self.check_if_expression(condition, then_expr, else_expr.as_deref(), expr.span)
            },
            _ => {
                // TODO: Handle other expression types
                self.create_error_type(expr.span)
            },
        }
    }
    
    /// Type check a literal expression
    fn check_literal(&mut self, literal: &Literal, span: Span) -> TypeNode {
        let ty = match literal {
            Literal::Integer(_) => Type::I32, // Default integer type
            Literal::Float(_) => Type::F64,   // Default float type
            Literal::String(_) => Type::Str,
            Literal::Character(_) => Type::Char,
            Literal::Boolean(_) => Type::Bool,
            Literal::Null => Type::Unit, // TODO: Handle null properly
        };
        
        self.create_type_node(ty, span)
    }
    
    /// Type check an identifier expression
    fn check_identifier(&mut self, name: &str, span: Span) -> TypeNode {
        // First check if symbol exists and get its type
        let symbol_type = if let Some(symbol) = self.symbol_table.lookup_symbol(name) {
            symbol.get_type().cloned()
        } else {
            None
        };
        
        // Mark symbol as used if it exists
        let _ = self.symbol_table.mark_symbol_used(name);
        
        match symbol_type {
            Some(type_node) => type_node,
            None => {
                self.errors.push(TypeError::UndefinedVariable {
                    name: name.to_string(),
                    span,
                });
                self.create_error_type(span)
            }
        }
    }
    
    /// Type check a binary expression
    fn check_binary_expression(
        &mut self,
        left: &ExprNode,
        operator: BinaryOp,
        right: &ExprNode,
        span: Span,
    ) -> TypeNode {
        let left_type = self.check_expression(left);
        let right_type = self.check_expression(right);
        
        // Check if the operation is valid for these types
        if let Some(result_type) = self.check_binary_operation_type(&left_type, operator, &right_type) {
            result_type
        } else {
            self.errors.push(TypeError::InvalidBinaryOperation {
                operator: format!("{:?}", operator),
                left_type: self.type_to_string(&left_type),
                right_type: self.type_to_string(&right_type),
                span,
            });
            self.create_error_type(span)
        }
    }
    
    /// Type check a unary expression
    fn check_unary_expression(&mut self, operator: UnaryOp, operand: &ExprNode, span: Span) -> TypeNode {
        let operand_type = self.check_expression(operand);
        
        if let Some(result_type) = self.check_unary_operation_type(operator, &operand_type) {
            result_type
        } else {
            self.errors.push(TypeError::InvalidUnaryOperation {
                operator: format!("{:?}", operator),
                operand_type: self.type_to_string(&operand_type),
                span,
            });
            self.create_error_type(span)
        }
    }
    
    /// Type check a function call expression
    fn check_call_expression(&mut self, function: &ExprNode, args: &[ExprNode], span: Span) -> TypeNode {
        // Check if function expression is an identifier
        if let Expr::Identifier(func_name) = &function.inner {
            // First, get function signature if it exists
            let function_info = if let Some(symbol) = self.symbol_table.lookup_symbol(func_name) {
                symbol.get_function_signature().map(|(params, ret)| (params.clone(), ret.clone()))
            } else {
                None
            };
            
            match function_info {
                Some((param_types, return_type)) => {
                    // Check argument count
                    if args.len() != param_types.len() {
                        self.errors.push(TypeError::ArgumentCountMismatch {
                            expected: param_types.len(),
                            found: args.len(),
                            function_name: func_name.clone(),
                            span,
                        });
                        return self.create_error_type(span);
                    }
                    
                    // Check argument types
                    for (i, (arg, expected_type)) in args.iter().zip(param_types.iter()).enumerate() {
                        let arg_type = self.check_expression(arg);
                        if !self.types_compatible(&arg_type, expected_type) {
                            self.errors.push(TypeError::ArgumentTypeMismatch {
                                parameter_index: i,
                                expected: self.type_to_string(expected_type),
                                found: self.type_to_string(&arg_type),
                                function_name: func_name.clone(),
                                span: arg.span,
                            });
                        }
                    }
                    
                    return return_type;
                },
                None => {
                    self.errors.push(TypeError::UndefinedFunction {
                        name: func_name.clone(),
                        span,
                    });
                }
            }
        } else {
            // TODO: Handle function pointer calls
        }
        
        self.create_error_type(span)
    }
    
    /// Type check a field access expression
    fn check_field_access(&mut self, _object: &ExprNode, _field: &str, span: Span) -> TypeNode {
        // TODO: Implement field access type checking
        self.create_error_type(span)
    }
    
    /// Type check an index expression
    fn check_index_expression(&mut self, _array: &ExprNode, _index: &ExprNode, span: Span) -> TypeNode {
        // TODO: Implement array indexing type checking
        self.create_error_type(span)
    }
    
    /// Type check a block expression
    fn check_block_expression(&mut self, statements: &[StmtNode], span: Span) -> TypeNode {
        // Enter new scope for block
        let _scope_id = self.symbol_table.enter_scope(ScopeType::Block, Some(span));
        
        // Check all statements
        for stmt in statements {
            self.check_statement(stmt);
        }
        
        // TODO: Determine block return type from last expression
        let block_type = self.create_unit_type(span);
        
        // Exit block scope
        let _ = self.symbol_table.exit_scope();
        
        block_type
    }
    
    /// Type check an if expression
    fn check_if_expression(
        &mut self,
        condition: &ExprNode,
        then_expr: &ExprNode,
        else_expr: Option<&ExprNode>,
        span: Span,
    ) -> TypeNode {
        let condition_type = self.check_expression(condition);
        
        // Condition must be boolean
        if !self.is_boolean_type(&condition_type) {
            self.errors.push(TypeError::TypeMismatch {
                expected: "bool".to_string(),
                found: self.type_to_string(&condition_type),
                span: condition.span,
            });
        }
        
        let then_type = self.check_expression(then_expr);
        
        if let Some(else_expr) = else_expr {
            let else_type = self.check_expression(else_expr);
            
            // Both branches must have compatible types
            if self.types_compatible(&then_type, &else_type) {
                then_type
            } else {
                self.errors.push(TypeError::TypeMismatch {
                    expected: self.type_to_string(&then_type),
                    found: self.type_to_string(&else_type),
                    span: else_expr.span,
                });
                self.create_error_type(span)
            }
        } else {
            // If without else always returns unit
            self.create_unit_type(span)
        }
    }
    
    // Helper methods
    
    /// Check if a binary operation is valid for the given types
    fn check_binary_operation_type(
        &self,
        left_type: &TypeNode,
        operator: BinaryOp,
        right_type: &TypeNode,
    ) -> Option<TypeNode> {
        use BinaryOp::*;
        
        match (&left_type.inner, operator, &right_type.inner) {
            // Arithmetic operations
            (Type::I32, Add | Subtract | Multiply | Divide | Modulo, Type::I32) => {
                Some(self.create_type_node(Type::I32, left_type.span))
            },
            (Type::I64, Add | Subtract | Multiply | Divide | Modulo, Type::I64) => {
                Some(self.create_type_node(Type::I64, left_type.span))
            },
            (Type::F32, Add | Subtract | Multiply | Divide, Type::F32) => {
                Some(self.create_type_node(Type::F32, left_type.span))
            },
            (Type::F64, Add | Subtract | Multiply | Divide, Type::F64) => {
                Some(self.create_type_node(Type::F64, left_type.span))
            },
            
            // Comparison operations
            (Type::I32, Equal | NotEqual | Less | Greater | LessEqual | GreaterEqual, Type::I32) |
            (Type::I64, Equal | NotEqual | Less | Greater | LessEqual | GreaterEqual, Type::I64) |
            (Type::F32, Equal | NotEqual | Less | Greater | LessEqual | GreaterEqual, Type::F32) |
            (Type::F64, Equal | NotEqual | Less | Greater | LessEqual | GreaterEqual, Type::F64) |
            (Type::Bool, Equal | NotEqual, Type::Bool) |
            (Type::Char, Equal | NotEqual | Less | Greater | LessEqual | GreaterEqual, Type::Char) => {
                Some(self.create_type_node(Type::Bool, left_type.span))
            },
            
            // Logical operations
            (Type::Bool, And | Or, Type::Bool) => {
                Some(self.create_type_node(Type::Bool, left_type.span))
            },
            
            // String concatenation
            (Type::Str, Add, Type::Str) => {
                Some(self.create_type_node(Type::Str, left_type.span))
            },
            
            _ => None,
        }
    }
    
    /// Check if a unary operation is valid for the given type
    fn check_unary_operation_type(&self, operator: UnaryOp, operand_type: &TypeNode) -> Option<TypeNode> {
        use UnaryOp::*;
        
        match (operator, &operand_type.inner) {
            (Plus | Minus, Type::I32) => Some(self.create_type_node(Type::I32, operand_type.span)),
            (Plus | Minus, Type::I64) => Some(self.create_type_node(Type::I64, operand_type.span)),
            (Plus | Minus, Type::F32) => Some(self.create_type_node(Type::F32, operand_type.span)),
            (Plus | Minus, Type::F64) => Some(self.create_type_node(Type::F64, operand_type.span)),
            (Not, Type::Bool) => Some(self.create_type_node(Type::Bool, operand_type.span)),
            _ => None,
        }
    }
    
    /// Check if two types are compatible
    fn types_compatible(&self, actual: &TypeNode, expected: &TypeNode) -> bool {
        // For now, exact type matching
        // TODO: Implement more sophisticated type compatibility (coercion, etc.)
        std::mem::discriminant(&actual.inner) == std::mem::discriminant(&expected.inner)
    }
    
    /// Check if a type is boolean
    fn is_boolean_type(&self, ty: &TypeNode) -> bool {
        matches!(ty.inner, Type::Bool)
    }
    
    /// Convert a type to a string representation
    fn type_to_string(&self, ty: &TypeNode) -> String {
        match &ty.inner {
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::I128 => "i128".to_string(),
            Type::U8 => "u8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::U128 => "u128".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Char => "char".to_string(),
            Type::Str => "str".to_string(),
            Type::Unit => "()".to_string(),
            Type::Array(elem_type, size) => {
                if let Some(size) = size {
                    format!("[{}; {}]", self.type_to_string(elem_type), size)
                } else {
                    format!("[{}]", self.type_to_string(elem_type))
                }
            },
            Type::Tuple(types) => {
                let type_strs: Vec<String> = types.iter().map(|t| self.type_to_string(t)).collect();
                format!("({})", type_strs.join(", "))
            },
            Type::Named { name, .. } => name.clone(),
            _ => "unknown".to_string(),
        }
    }
    
    /// Extract identifier name from a pattern (simplified)
    fn get_pattern_identifier(&self, pattern: &Pattern) -> Option<String> {
        match pattern {
            Pattern::Identifier(name) => Some(name.clone()),
            _ => None, // TODO: Handle more complex patterns
        }
    }
    
    /// Create a type node with the given type and span
    fn create_type_node(&self, ty: Type, span: Span) -> TypeNode {
        use crate::ast::NodeIdGenerator;
        let mut gen = NodeIdGenerator::new();
        Node::new(ty, span, gen.next())
    }
    
    /// Create a unit type node
    fn create_unit_type(&self, span: Span) -> TypeNode {
        self.create_type_node(Type::Unit, span)
    }
    
    /// Create an error type node (for error recovery)
    fn create_error_type(&self, span: Span) -> TypeNode {
        self.create_type_node(Type::Unit, span) // Use unit as error type for now
    }
    
    /// Get the symbol table (for debugging)
    pub fn symbol_table(&self) -> &SymbolTable {
        &self.symbol_table
    }
    
    /// Get type checking errors
    pub fn errors(&self) -> &[TypeError] {
        &self.errors
    }
    
    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use crate::lexer::Lexer;
    
    fn parse_and_check(source: &str) -> Result<(), Vec<TypeError>> {
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer).unwrap();
        let program = parser.parse_program().unwrap();
        
        let mut type_checker = TypeChecker::new();
        type_checker.check_program(&program)
    }
    
    #[test]
    fn test_simple_function() {
        let source = r#"
            fn main() {
                let x := 42;
                let y: i32 = 10;
                return x + y;
            }
        "#;
        
        let result = parse_and_check(source);
        assert!(result.is_ok(), "Type checking should succeed: {:?}", result);
    }
    
    #[test]
    fn test_type_mismatch() {
        let source = r#"
            fn main() {
                let x: i32 = "hello";
            }
        "#;
        
        let result = parse_and_check(source);
        assert!(result.is_err());
        if let Err(errors) = result {
            assert!(errors.iter().any(|e| matches!(e, TypeError::TypeMismatch { .. })));
        }
    }
    
    #[test]
    fn test_undefined_variable() {
        let source = r#"
            fn main() {
                let x = y + 1;
            }
        "#;
        
        let result = parse_and_check(source);
        assert!(result.is_err());
        if let Err(errors) = result {
            assert!(errors.iter().any(|e| matches!(e, TypeError::UndefinedVariable { .. })));
        }
    }
    
    #[test]
    fn test_function_call() {
        let source = r#"
            fn add(a: i32, b: i32) -> i32 {
                return a + b;
            }
            
            fn main() {
                let result = add(5, 10);
            }
        "#;
        
        let result = parse_and_check(source);
        assert!(result.is_ok(), "Type checking should succeed: {:?}", result);
    }
}