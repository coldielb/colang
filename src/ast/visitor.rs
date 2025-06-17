/// Visitor pattern implementation for traversing the AST
/// 
/// This module provides a generic visitor pattern that allows for easy traversal
/// and transformation of the Abstract Syntax Tree. It supports both mutable and
/// immutable visitation patterns.

use crate::ast::*;
use crate::ast::expr::*;
use crate::ast::stmt::*;
use crate::ast::decl::*;

/// Trait for AST visitors that can walk the tree immutably
pub trait Visitor<T = ()> {
    /// Visit an item
    fn visit_item(&mut self, item: &ItemNode) -> T {
        self.walk_item(item)
    }
    
    /// Visit a declaration
    fn visit_declaration(&mut self, decl: &DeclarationNode) -> T {
        self.walk_declaration(decl)
    }
    
    /// Visit a statement
    fn visit_statement(&mut self, stmt: &StatementNode) -> T {
        self.walk_statement(stmt)
    }
    
    /// Visit an expression
    fn visit_expression(&mut self, expr: &ExprNode) -> T {
        self.walk_expression(expr)
    }
    
    /// Visit a type
    fn visit_type(&mut self, ty: &TypeNode) -> T {
        self.walk_type(ty)
    }
    
    /// Visit a pattern
    fn visit_pattern(&mut self, pat: &Pattern) -> T {
        self.walk_pattern(pat)
    }
    
    /// Visit a block
    fn visit_block(&mut self, block: &Block) -> T {
        self.walk_block(block)
    }
    
    // Default walking implementations
    
    fn walk_item(&mut self, item: &ItemNode) -> T {
        match &item.inner {
            Item::Declaration(decl) => self.visit_declaration(decl),
        }
    }
    
    fn walk_declaration(&mut self, decl: &DeclarationNode) -> T {
        match &decl.inner {
            Declaration::Function { parameters, return_type, body, .. } => {
                for param in parameters {
                    self.visit_pattern(&param.pattern);
                    self.visit_type(&param.param_type);
                    if let Some(default) = &param.default {
                        self.visit_expression(default);
                    }
                }
                if let Some(ret_ty) = return_type {
                    self.visit_type(ret_ty);
                }
                if let Some(body) = body {
                    self.visit_block(body);
                }
            },
            Declaration::Struct { fields, .. } => {
                for field in fields {
                    self.visit_type(&field.field_type);
                }
            },
            Declaration::Enum { variants, .. } => {
                for variant in variants {
                    match &variant.fields {
                        VariantFields::Unit => {},
                        VariantFields::Tuple(types) => {
                            for ty in types {
                                self.visit_type(ty);
                            }
                        },
                        VariantFields::Struct(fields) => {
                            for field in fields {
                                self.visit_type(&field.field_type);
                            }
                        },
                    }
                    if let Some(discriminant) = &variant.discriminant {
                        self.visit_expression(discriminant);
                    }
                }
            },
            Declaration::Trait { items, .. } => {
                for item in items {
                    match item {
                        TraitItem::Function { parameters, return_type, default_body, .. } => {
                            for param in parameters {
                                self.visit_pattern(&param.pattern);
                                self.visit_type(&param.param_type);
                            }
                            if let Some(ret_ty) = return_type {
                                self.visit_type(ret_ty);
                            }
                            if let Some(body) = default_body {
                                self.visit_block(body);
                            }
                        },
                        TraitItem::Type { bounds, default, .. } => {
                            for bound in bounds {
                                self.visit_type(bound);
                            }
                            if let Some(default_ty) = default {
                                self.visit_type(default_ty);
                            }
                        },
                        TraitItem::Const { const_type, default, .. } => {
                            self.visit_type(const_type);
                            if let Some(default_val) = default {
                                self.visit_expression(default_val);
                            }
                        },
                    }
                }
            },
            Declaration::Impl { self_type, items, .. } => {
                self.visit_type(self_type);
                for item in items {
                    match item {
                        ImplItem::Function { parameters, return_type, body, .. } => {
                            for param in parameters {
                                self.visit_pattern(&param.pattern);
                                self.visit_type(&param.param_type);
                            }
                            if let Some(ret_ty) = return_type {
                                self.visit_type(ret_ty);
                            }
                            self.visit_block(body);
                        },
                        ImplItem::Type { type_def, .. } => {
                            self.visit_type(type_def);
                        },
                        ImplItem::Const { const_type, value, .. } => {
                            self.visit_type(const_type);
                            self.visit_expression(value);
                        },
                    }
                }
            },
            Declaration::TypeAlias { type_def, .. } => {
                self.visit_type(type_def);
            },
            Declaration::Const { type_annotation, value, .. } => {
                self.visit_type(type_annotation);
                self.visit_expression(value);
            },
            Declaration::Static { type_annotation, value, .. } => {
                self.visit_type(type_annotation);
                self.visit_expression(value);
            },
            Declaration::Module { items, .. } => {
                for item in items {
                    match item {
                        Item::Declaration(decl) => self.visit_declaration(decl),
                    };
                }
            },
            Declaration::Use { .. } => {
                // Use declarations don't contain expressions/types to visit
            },
            Declaration::ExternBlock { items, .. } => {
                for item in items {
                    match item {
                        ExternItem::Function { parameters, return_type, .. } => {
                            for param in parameters {
                                self.visit_pattern(&param.pattern);
                                self.visit_type(&param.param_type);
                            }
                            if let Some(ret_ty) = return_type {
                                self.visit_type(ret_ty);
                            }
                        },
                        ExternItem::Static { static_type, .. } => {
                            self.visit_type(static_type);
                        },
                        ExternItem::Type { .. } => {
                            // External types don't have definitions to visit
                        },
                    }
                }
            },
        }
        
        // Return default value for the visitor type
        self.default_return()
    }
    
    fn walk_statement(&mut self, stmt: &StatementNode) -> T {
        match &stmt.inner {
            Statement::Expression(expr) => {
                self.visit_expression(expr);
            },
            Statement::Let { pattern, type_annotation, initializer, .. } => {
                self.visit_pattern(pattern);
                if let Some(ty) = type_annotation {
                    self.visit_type(ty);
                }
                if let Some(init) = initializer {
                    self.visit_expression(init);
                }
            },
            Statement::Const { type_annotation, initializer, .. } => {
                if let Some(ty) = type_annotation {
                    self.visit_type(ty);
                }
                self.visit_expression(initializer);
            },
            Statement::Static { type_annotation, initializer, .. } => {
                self.visit_type(type_annotation);
                self.visit_expression(initializer);
            },
            Statement::Assign { target, value } => {
                self.visit_expression(target);
                self.visit_expression(value);
            },
            Statement::CompoundAssign { target, value, .. } => {
                self.visit_expression(target);
                self.visit_expression(value);
            },
            Statement::If { condition, then_block, else_block } => {
                self.visit_expression(condition);
                self.visit_block(then_block);
                if let Some(else_blk) = else_block {
                    self.visit_block(else_blk);
                }
            },
            Statement::While { condition, body } => {
                self.visit_expression(condition);
                self.visit_block(body);
            },
            Statement::For { pattern, iterator, body } => {
                self.visit_pattern(pattern);
                self.visit_expression(iterator);
                self.visit_block(body);
            },
            Statement::Match { expr, arms } => {
                self.visit_expression(expr);
                for arm in arms {
                    self.visit_pattern(&arm.pattern);
                    if let Some(guard) = &arm.guard {
                        self.visit_expression(guard);
                    }
                    self.visit_expression(&arm.body);
                }
            },
            Statement::Break(expr) => {
                if let Some(e) = expr {
                    self.visit_expression(e);
                }
            },
            Statement::Continue => {},
            Statement::Return(expr) => {
                if let Some(e) = expr {
                    self.visit_expression(e);
                }
            },
            Statement::Block(block) => {
                self.visit_block(block);
            },
            Statement::Empty => {},
        }
        
        self.default_return()
    }
    
    fn walk_expression(&mut self, expr: &ExprNode) -> T {
        match &expr.inner {
            Expr::Literal(_) => {},
            Expr::Identifier(_) => {},
            Expr::Binary { left, right, .. } => {
                self.visit_expression(left);
                self.visit_expression(right);
            },
            Expr::Unary { operand, .. } => {
                self.visit_expression(operand);
            },
            Expr::Call { function, args } => {
                self.visit_expression(function);
                for arg in args {
                    self.visit_expression(arg);
                }
            },
            Expr::MethodCall { receiver, args, .. } => {
                self.visit_expression(receiver);
                for arg in args {
                    self.visit_expression(arg);
                }
            },
            Expr::FieldAccess { object, .. } => {
                self.visit_expression(object);
            },
            Expr::Index { array, index } => {
                self.visit_expression(array);
                self.visit_expression(index);
            },
            Expr::Array(elements) => {
                for element in elements {
                    self.visit_expression(element);
                }
            },
            Expr::Tuple(elements) => {
                for element in elements {
                    self.visit_expression(element);
                }
            },
            Expr::Struct { fields, .. } => {
                for field in fields {
                    self.visit_expression(&field.value);
                }
            },
            Expr::If { condition, then_expr, else_expr } => {
                self.visit_expression(condition);
                self.visit_expression(then_expr);
                if let Some(else_e) = else_expr {
                    self.visit_expression(else_e);
                }
            },
            Expr::Match { expr, arms } => {
                self.visit_expression(expr);
                for arm in arms {
                    self.visit_pattern(&arm.pattern);
                    if let Some(guard) = &arm.guard {
                        self.visit_expression(guard);
                    }
                    self.visit_expression(&arm.body);
                }
            },
            Expr::Block(statements) => {
                for stmt in statements {
                    self.visit_statement(stmt);
                }
            },
            Expr::Assign { target, value } => {
                self.visit_expression(target);
                self.visit_expression(value);
            },
            Expr::CompoundAssign { target, value, .. } => {
                self.visit_expression(target);
                self.visit_expression(value);
            },
            Expr::Cast { expr, target_type } => {
                self.visit_expression(expr);
                self.visit_type(target_type);
            },
            Expr::Reference { expr, .. } => {
                self.visit_expression(expr);
            },
            Expr::Dereference(expr) => {
                self.visit_expression(expr);
            },
            Expr::Move(expr) => {
                self.visit_expression(expr);
            },
            Expr::Try(expr) => {
                self.visit_expression(expr);
            },
            Expr::Lambda { params, return_type, body } => {
                for param in params {
                    self.visit_pattern(&param.pattern);
                    if let Some(ty) = &param.param_type {
                        self.visit_type(ty);
                    }
                    if let Some(default) = &param.default {
                        self.visit_expression(default);
                    }
                }
                if let Some(ret_ty) = return_type {
                    self.visit_type(ret_ty);
                }
                self.visit_expression(body);
            },
            Expr::Range { start, end, .. } => {
                if let Some(s) = start {
                    self.visit_expression(s);
                }
                if let Some(e) = end {
                    self.visit_expression(e);
                }
            },
            Expr::Error => {},
        }
        
        self.default_return()
    }
    
    fn walk_type(&mut self, ty: &TypeNode) -> T {
        match &ty.inner {
            Type::Own(inner) | Type::Ref(inner) | Type::MutRef(inner) | Type::Shared(inner) => {
                self.visit_type(inner);
            },
            Type::Array(inner, _) => {
                self.visit_type(inner);
            },
            Type::Tuple(types) => {
                for t in types {
                    self.visit_type(t);
                }
            },
            Type::Function { params, return_type } => {
                for param in params {
                    self.visit_type(param);
                }
                self.visit_type(return_type);
            },
            Type::Named { generic_args, .. } => {
                for arg in generic_args {
                    self.visit_type(arg);
                }
            },
            _ => {
                // Primitive types and others don't have nested types
            },
        }
        
        self.default_return()
    }
    
    fn walk_pattern(&mut self, pat: &Pattern) -> T {
        match pat {
            Pattern::Tuple(patterns) | Pattern::Array(patterns) | Pattern::Or(patterns) => {
                for p in patterns {
                    self.visit_pattern(p);
                }
            },
            Pattern::Struct { fields, .. } => {
                for field in fields {
                    self.visit_pattern(&field.pattern);
                }
            },
            Pattern::Enum { fields, .. } => {
                for field in fields {
                    self.visit_pattern(field);
                }
            },
            Pattern::Reference { pattern, .. } => {
                self.visit_pattern(pattern);
            },
            Pattern::Range { start, end, .. } => {
                self.visit_pattern(start);
                self.visit_pattern(end);
            },
            _ => {
                // Other patterns don't have nested patterns
            },
        }
        
        self.default_return()
    }
    
    fn walk_block(&mut self, block: &Block) -> T {
        for stmt in &block.statements {
            self.visit_statement(stmt);
        }
        if let Some(expr) = &block.expression {
            self.visit_expression(expr);
        }
        
        self.default_return()
    }
    
    /// Provide default return value for visitor methods
    fn default_return(&self) -> T;
}

/// Trait for AST visitors that can mutate the tree
pub trait VisitorMut<T = ()> {
    // Similar structure to Visitor but with mutable references
    // This would be a large implementation, so including just the signature for now
    
    fn visit_item_mut(&mut self, item: &mut ItemNode) -> T;
    fn visit_declaration_mut(&mut self, decl: &mut DeclarationNode) -> T;
    fn visit_statement_mut(&mut self, stmt: &mut StatementNode) -> T;
    fn visit_expression_mut(&mut self, expr: &mut ExprNode) -> T;
    fn visit_type_mut(&mut self, ty: &mut TypeNode) -> T;
    fn visit_pattern_mut(&mut self, pat: &mut Pattern) -> T;
    fn visit_block_mut(&mut self, block: &mut Block) -> T;
    
    fn default_return(&self) -> T;
}

/// A concrete visitor that collects all identifiers in the AST
#[derive(Debug, Default)]
pub struct IdentifierCollector {
    pub identifiers: Vec<String>,
}

impl Visitor<()> for IdentifierCollector {
    fn visit_expression(&mut self, expr: &ExprNode) -> () {
        if let Expr::Identifier(name) = &expr.inner {
            self.identifiers.push(name.clone());
        }
        self.walk_expression(expr)
    }
    
    fn visit_pattern(&mut self, pat: &Pattern) -> () {
        if let Pattern::Identifier(name) = pat {
            self.identifiers.push(name.clone());
        }
        self.walk_pattern(pat)
    }
    
    fn default_return(&self) -> () {
        ()
    }
}