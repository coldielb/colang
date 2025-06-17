/// Symbol table for scope and name resolution in COLANG
/// 
/// This module provides a hierarchical symbol table system that tracks variables,
/// functions, types, and other identifiers across nested scopes.

use std::collections::HashMap;
use crate::ast::{TypeNode, DeclarationNode};
use crate::util::Span;

/// Unique identifier for scopes
pub type ScopeId = usize;

/// Symbol table managing nested scopes and symbol resolution
#[derive(Debug, Clone)]
pub struct SymbolTable {
    scopes: Vec<Scope>,
    current_scope: ScopeId,
    next_scope_id: ScopeId,
}

/// Individual scope containing symbols
#[derive(Debug, Clone)]
pub struct Scope {
    id: ScopeId,
    parent: Option<ScopeId>,
    symbols: HashMap<String, Symbol>,
    scope_type: ScopeType,
    span: Option<Span>,
}

/// Types of scopes in COLANG
#[derive(Debug, Clone, PartialEq)]
pub enum ScopeType {
    Global,
    Function { name: String },
    Block,
    Module { name: String },
    Struct { name: String },
    Enum { name: String },
    Trait { name: String },
    Impl { target: String },
}

/// Symbol information stored in the symbol table
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub symbol_type: SymbolType,
    pub type_info: TypeInfo,
    pub span: Span,
    pub is_mutable: bool,
    pub is_used: bool,
    pub declaration_scope: ScopeId,
}

/// Different kinds of symbols that can be stored
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolType {
    Variable,
    Function,
    Parameter,
    Type,
    Module,
    Trait,
    Field,
    Constant,
    Static,
}

/// Type information for symbols
#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo {
    Known(TypeNode),
    Inferred(TypeNode),
    Unknown,
    Function {
        parameters: Vec<TypeNode>,
        return_type: Box<TypeNode>,
    },
    Type {
        definition: TypeDefinition,
    },
}

/// Type definitions for user-defined types
#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    Struct {
        fields: Vec<FieldInfo>,
        generic_params: Vec<String>,
    },
    Enum {
        variants: Vec<VariantInfo>,
        generic_params: Vec<String>,
    },
    TypeAlias {
        target: TypeNode,
        generic_params: Vec<String>,
    },
}

/// Field information for struct types
#[derive(Debug, Clone, PartialEq)]
pub struct FieldInfo {
    pub name: String,
    pub field_type: TypeNode,
    pub is_public: bool,
    pub span: Span,
}

/// Variant information for enum types
#[derive(Debug, Clone, PartialEq)]
pub struct VariantInfo {
    pub name: String,
    pub fields: Vec<TypeNode>,
    pub discriminant: Option<i64>,
    pub span: Span,
}

/// Errors that can occur during symbol table operations
#[derive(Debug, Clone)]
pub enum SymbolError {
    DuplicateSymbol {
        name: String,
        original_span: Span,
        duplicate_span: Span,
    },
    UndefinedSymbol {
        name: String,
        span: Span,
    },
    InvalidScope {
        scope_id: ScopeId,
    },
    TypeMismatch {
        expected: String,
        found: String,
        span: Span,
    },
}

impl std::fmt::Display for SymbolError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolError::DuplicateSymbol { name, original_span, duplicate_span } => {
                write!(f, "Duplicate symbol '{}' at {}, originally declared at {}", 
                       name, duplicate_span, original_span)
            },
            SymbolError::UndefinedSymbol { name, span } => {
                write!(f, "Undefined symbol '{}' at {}", name, span)
            },
            SymbolError::InvalidScope { scope_id } => {
                write!(f, "Invalid scope ID: {}", scope_id)
            },
            SymbolError::TypeMismatch { expected, found, span } => {
                write!(f, "Type mismatch at {}: expected {}, found {}", span, expected, found)
            },
        }
    }
}

impl std::error::Error for SymbolError {}

impl SymbolTable {
    /// Create a new symbol table with a global scope
    pub fn new() -> Self {
        let global_scope = Scope {
            id: 0,
            parent: None,
            symbols: HashMap::new(),
            scope_type: ScopeType::Global,
            span: None,
        };
        
        Self {
            scopes: vec![global_scope],
            current_scope: 0,
            next_scope_id: 1,
        }
    }
    
    /// Enter a new scope
    pub fn enter_scope(&mut self, scope_type: ScopeType, span: Option<Span>) -> ScopeId {
        let scope_id = self.next_scope_id;
        self.next_scope_id += 1;
        
        let scope = Scope {
            id: scope_id,
            parent: Some(self.current_scope),
            symbols: HashMap::new(),
            scope_type,
            span,
        };
        
        self.scopes.push(scope);
        self.current_scope = scope_id;
        scope_id
    }
    
    /// Exit the current scope and return to parent
    pub fn exit_scope(&mut self) -> Result<(), SymbolError> {
        let current = self.get_current_scope()?;
        if let Some(parent_id) = current.parent {
            self.current_scope = parent_id;
            Ok(())
        } else {
            Err(SymbolError::InvalidScope { scope_id: self.current_scope })
        }
    }
    
    /// Add a symbol to the current scope
    pub fn add_symbol(&mut self, symbol: Symbol) -> Result<(), SymbolError> {
        let current_scope = self.current_scope;
        
        // Check for duplicate symbols in current scope
        if let Some(current) = self.scopes.iter().find(|s| s.id == current_scope) {
            if let Some(existing) = current.symbols.get(&symbol.name) {
                return Err(SymbolError::DuplicateSymbol {
                    name: symbol.name.clone(),
                    original_span: existing.span,
                    duplicate_span: symbol.span,
                });
            }
        }
        
        // Add symbol to current scope
        if let Some(scope) = self.scopes.iter_mut().find(|s| s.id == current_scope) {
            scope.symbols.insert(symbol.name.clone(), symbol);
            Ok(())
        } else {
            Err(SymbolError::InvalidScope { scope_id: current_scope })
        }
    }
    
    /// Look up a symbol by name, searching current scope and parent scopes
    pub fn lookup_symbol(&self, name: &str) -> Option<&Symbol> {
        let mut current_scope = self.current_scope;
        
        loop {
            if let Some(scope) = self.scopes.iter().find(|s| s.id == current_scope) {
                if let Some(symbol) = scope.symbols.get(name) {
                    return Some(symbol);
                }
                
                if let Some(parent_id) = scope.parent {
                    current_scope = parent_id;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        
        None
    }
    
    /// Look up a symbol by name in the current scope only
    pub fn lookup_symbol_current_scope(&self, name: &str) -> Option<&Symbol> {
        self.scopes
            .iter()
            .find(|s| s.id == self.current_scope)
            .and_then(|scope| scope.symbols.get(name))
    }
    
    /// Mark a symbol as used
    pub fn mark_symbol_used(&mut self, name: &str) -> Result<(), SymbolError> {
        let mut current_scope = self.current_scope;
        
        loop {
            if let Some(scope) = self.scopes.iter_mut().find(|s| s.id == current_scope) {
                if let Some(symbol) = scope.symbols.get_mut(name) {
                    symbol.is_used = true;
                    return Ok(());
                }
                
                if let Some(parent_id) = scope.parent {
                    current_scope = parent_id;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        
        Err(SymbolError::UndefinedSymbol {
            name: name.to_string(),
            span: Span::new(0, 0, 0, 0), // TODO: Better span handling
        })
    }
    
    /// Get the current scope
    pub fn get_current_scope(&self) -> Result<&Scope, SymbolError> {
        self.scopes
            .iter()
            .find(|s| s.id == self.current_scope)
            .ok_or(SymbolError::InvalidScope { scope_id: self.current_scope })
    }
    
    /// Get the current scope ID
    pub fn current_scope_id(&self) -> ScopeId {
        self.current_scope
    }
    
    /// Get the scope type of the current scope
    pub fn current_scope_type(&self) -> Result<&ScopeType, SymbolError> {
        Ok(&self.get_current_scope()?.scope_type)
    }
    
    /// Check if we're in a function scope
    pub fn is_in_function(&self) -> bool {
        let mut current_scope = self.current_scope;
        
        loop {
            if let Some(scope) = self.scopes.iter().find(|s| s.id == current_scope) {
                if matches!(scope.scope_type, ScopeType::Function { .. }) {
                    return true;
                }
                
                if let Some(parent_id) = scope.parent {
                    current_scope = parent_id;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        
        false
    }
    
    /// Get all symbols in the current scope
    pub fn current_scope_symbols(&self) -> Result<&HashMap<String, Symbol>, SymbolError> {
        Ok(&self.get_current_scope()?.symbols)
    }
    
    /// Get all unused symbols in all scopes
    pub fn get_unused_symbols(&self) -> Vec<&Symbol> {
        self.scopes
            .iter()
            .flat_map(|scope| scope.symbols.values())
            .filter(|symbol| !symbol.is_used && symbol.symbol_type != SymbolType::Function)
            .collect()
    }
    
    /// Debug: Print the current symbol table structure
    pub fn debug_print(&self) -> String {
        let mut output = String::new();
        output.push_str("Symbol Table:\n");
        
        for scope in &self.scopes {
            let indent = "  ".repeat(self.get_scope_depth(scope.id));
            output.push_str(&format!("{}Scope {} ({:?}):\n", indent, scope.id, scope.scope_type));
            
            for (name, symbol) in &scope.symbols {
                output.push_str(&format!("{}  {} ({:?}): {:?}\n", 
                    indent, name, symbol.symbol_type, symbol.type_info));
            }
        }
        
        output
    }
    
    /// Get the depth of a scope (0 for global, 1 for children of global, etc.)
    fn get_scope_depth(&self, scope_id: ScopeId) -> usize {
        let mut depth = 0;
        let mut current = scope_id;
        
        while let Some(scope) = self.scopes.iter().find(|s| s.id == current) {
            if let Some(parent_id) = scope.parent {
                depth += 1;
                current = parent_id;
            } else {
                break;
            }
        }
        
        depth
    }
}

impl Symbol {
    /// Create a new variable symbol
    pub fn variable(
        name: String, 
        type_info: TypeInfo, 
        span: Span, 
        is_mutable: bool,
        declaration_scope: ScopeId,
    ) -> Self {
        Self {
            name,
            symbol_type: SymbolType::Variable,
            type_info,
            span,
            is_mutable,
            is_used: false,
            declaration_scope,
        }
    }
    
    /// Create a new function symbol
    pub fn function(
        name: String,
        parameters: Vec<TypeNode>,
        return_type: TypeNode,
        span: Span,
        declaration_scope: ScopeId,
    ) -> Self {
        Self {
            name,
            symbol_type: SymbolType::Function,
            type_info: TypeInfo::Function {
                parameters,
                return_type: Box::new(return_type),
            },
            span,
            is_mutable: false,
            is_used: false,
            declaration_scope,
        }
    }
    
    /// Create a new parameter symbol
    pub fn parameter(
        name: String,
        type_info: TypeInfo,
        span: Span,
        is_mutable: bool,
        declaration_scope: ScopeId,
    ) -> Self {
        Self {
            name,
            symbol_type: SymbolType::Parameter,
            type_info,
            span,
            is_mutable,
            is_used: false,
            declaration_scope,
        }
    }
    
    /// Create a new type symbol
    pub fn type_symbol(
        name: String,
        definition: TypeDefinition,
        span: Span,
        declaration_scope: ScopeId,
    ) -> Self {
        Self {
            name,
            symbol_type: SymbolType::Type,
            type_info: TypeInfo::Type { definition },
            span,
            is_mutable: false,
            is_used: false,
            declaration_scope,
        }
    }
    
    /// Check if this symbol is a function
    pub fn is_function(&self) -> bool {
        self.symbol_type == SymbolType::Function
    }
    
    /// Check if this symbol is a variable
    pub fn is_variable(&self) -> bool {
        matches!(self.symbol_type, SymbolType::Variable | SymbolType::Parameter)
    }
    
    /// Check if this symbol is a type
    pub fn is_type(&self) -> bool {
        self.symbol_type == SymbolType::Type
    }
    
    /// Get the type of this symbol if it's a variable or parameter
    pub fn get_type(&self) -> Option<&TypeNode> {
        match &self.type_info {
            TypeInfo::Known(ty) | TypeInfo::Inferred(ty) => Some(ty),
            _ => None,
        }
    }
    
    /// Get function signature if this is a function symbol
    pub fn get_function_signature(&self) -> Option<(&Vec<TypeNode>, &TypeNode)> {
        match &self.type_info {
            TypeInfo::Function { parameters, return_type } => Some((parameters, return_type)),
            _ => None,
        }
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Type, Node};
    use crate::util::NodeIdGenerator;
    
    fn create_test_type_node(ty: Type) -> TypeNode {
        let mut gen = NodeIdGenerator::new();
        Node::new(ty, Span::new(0, 0, 0, 0), gen.next())
    }
    
    #[test]
    fn test_symbol_table_creation() {
        let table = SymbolTable::new();
        assert_eq!(table.current_scope, 0);
        assert_eq!(table.scopes.len(), 1);
    }
    
    #[test]
    fn test_scope_management() {
        let mut table = SymbolTable::new();
        
        // Enter function scope
        let func_scope = table.enter_scope(
            ScopeType::Function { name: "test".to_string() }, 
            None
        );
        assert_eq!(func_scope, 1);
        assert_eq!(table.current_scope, 1);
        
        // Enter block scope
        let block_scope = table.enter_scope(ScopeType::Block, None);
        assert_eq!(block_scope, 2);
        assert_eq!(table.current_scope, 2);
        
        // Exit block scope
        table.exit_scope().unwrap();
        assert_eq!(table.current_scope, 1);
        
        // Exit function scope
        table.exit_scope().unwrap();
        assert_eq!(table.current_scope, 0);
    }
    
    #[test]
    fn test_symbol_addition_and_lookup() {
        let mut table = SymbolTable::new();
        
        let symbol = Symbol::variable(
            "x".to_string(),
            TypeInfo::Known(create_test_type_node(Type::I32)),
            Span::new(0, 0, 0, 0),
            false,
            0,
        );
        
        table.add_symbol(symbol).unwrap();
        
        let found = table.lookup_symbol("x");
        assert!(found.is_some());
        assert_eq!(found.unwrap().name, "x");
    }
    
    #[test]
    fn test_duplicate_symbol_detection() {
        let mut table = SymbolTable::new();
        
        let symbol1 = Symbol::variable(
            "x".to_string(),
            TypeInfo::Known(create_test_type_node(Type::I32)),
            Span::new(0, 0, 0, 0),
            false,
            0,
        );
        
        let symbol2 = Symbol::variable(
            "x".to_string(),
            TypeInfo::Known(create_test_type_node(Type::F32)),
            Span::new(1, 1, 1, 1),
            false,
            0,
        );
        
        table.add_symbol(symbol1).unwrap();
        
        let result = table.add_symbol(symbol2);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), SymbolError::DuplicateSymbol { .. }));
    }
    
    #[test]
    fn test_symbol_shadowing() {
        let mut table = SymbolTable::new();
        
        // Add symbol in global scope
        let global_symbol = Symbol::variable(
            "x".to_string(),
            TypeInfo::Known(create_test_type_node(Type::I32)),
            Span::new(0, 0, 0, 0),
            false,
            0,
        );
        table.add_symbol(global_symbol).unwrap();
        
        // Enter new scope
        table.enter_scope(ScopeType::Block, None);
        
        // Add symbol with same name in inner scope
        let inner_symbol = Symbol::variable(
            "x".to_string(),
            TypeInfo::Known(create_test_type_node(Type::Str)),
            Span::new(1, 1, 1, 1),
            false,
            1,
        );
        table.add_symbol(inner_symbol).unwrap();
        
        // Should find inner symbol
        let found = table.lookup_symbol("x");
        assert!(found.is_some());
        // The inner symbol should have Str type
        if let Some(TypeInfo::Known(type_node)) = found.map(|s| &s.type_info) {
            assert!(matches!(type_node.inner, Type::Str));
        } else {
            panic!("Expected to find Str type");
        }
        
        // Exit scope
        table.exit_scope().unwrap();
        
        // Should now find global symbol again
        let found = table.lookup_symbol("x");
        assert!(found.is_some());
        if let Some(TypeInfo::Known(type_node)) = found.map(|s| &s.type_info) {
            assert!(matches!(type_node.inner, Type::I32));
        } else {
            panic!("Expected to find I32 type");
        }
    }
}