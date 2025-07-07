/// IR Builder for COLANG
/// 
/// This module provides the IR builder that converts COLANG AST into SSA-form IR.
/// The builder handles control flow construction, SSA generation, and basic optimizations.

use std::collections::HashMap;
use crate::ast::*;
use crate::ast::expr::{BinaryOp, UnaryOp, Literal};
use crate::ast::stmt::Statement;
use crate::ast::decl::Declaration;
use crate::ir::instruction::{Instruction, InstructionKind, IrType, ValueId, BlockId, FunctionId, IdGenerator};
use crate::ir::basic_block::{BasicBlock, ControlFlowGraph};
use crate::semantics::symbol_table::SymbolTable;
use crate::util::Span;

/// IR Builder that converts AST to IR
pub struct IrBuilder {
    id_gen: IdGenerator,
    modules: Vec<IrModule>,
    current_function: Option<FunctionId>,
    current_block: Option<BlockId>,
    cfgs: HashMap<FunctionId, ControlFlowGraph>,
    
    // Variable mappings for SSA construction
    variable_stack: HashMap<String, Vec<ValueId>>,
    sealed_blocks: HashMap<BlockId, bool>,
    incomplete_phis: HashMap<BlockId, Vec<(String, ValueId)>>,
    
    // Type mappings
    type_cache: HashMap<String, IrType>,
}

/// A complete IR module containing all functions and global data
#[derive(Debug, Clone)]
pub struct IrModule {
    pub name: String,
    pub functions: HashMap<FunctionId, ControlFlowGraph>,
    pub globals: Vec<GlobalValue>,
    pub type_definitions: HashMap<String, IrType>,
}

/// Global values (constants, static variables, etc.)
#[derive(Debug, Clone)]
pub struct GlobalValue {
    pub name: String,
    pub ty: IrType,
    pub initializer: Option<ConstantValue>,
    pub is_mutable: bool,
}

/// Constant values for global initializers
#[derive(Debug, Clone)]
pub enum ConstantValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Unit,
    Array(Vec<ConstantValue>),
    Struct(Vec<ConstantValue>),
}

/// Errors that can occur during IR generation
#[derive(Debug, Clone)]
pub enum IrBuildError {
    UndefinedVariable { name: String, span: Span },
    UndefinedFunction { name: String, span: Span },
    TypeMismatch { expected: String, found: String, span: Span },
    InvalidOperation { message: String, span: Span },
    InternalError { message: String },
}

impl std::fmt::Display for IrBuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IrBuildError::UndefinedVariable { name, span } => {
                write!(f, "Undefined variable '{}' at {}", name, span)
            },
            IrBuildError::UndefinedFunction { name, span } => {
                write!(f, "Undefined function '{}' at {}", name, span)
            },
            IrBuildError::TypeMismatch { expected, found, span } => {
                write!(f, "Type mismatch at {}: expected {}, found {}", span, expected, found)
            },
            IrBuildError::InvalidOperation { message, span } => {
                write!(f, "Invalid operation at {}: {}", span, message)
            },
            IrBuildError::InternalError { message } => {
                write!(f, "Internal error: {}", message)
            },
        }
    }
}

impl std::error::Error for IrBuildError {}

impl IrBuilder {
    /// Create a new IR builder
    pub fn new() -> Self {
        Self {
            id_gen: IdGenerator::new(),
            modules: Vec::new(),
            current_function: None,
            current_block: None,
            cfgs: HashMap::new(),
            variable_stack: HashMap::new(),
            sealed_blocks: HashMap::new(),
            incomplete_phis: HashMap::new(),
            type_cache: HashMap::new(),
        }
    }
    
    /// Build IR from a program AST
    pub fn build_program(&mut self, program: &Program, symbol_table: &SymbolTable) -> Result<IrModule, IrBuildError> {
        let module_name = "main".to_string();
        let mut module = IrModule {
            name: module_name.clone(),
            functions: HashMap::new(),
            globals: Vec::new(),
            type_definitions: HashMap::new(),
        };
        
        // First pass: collect function signatures
        for item in &program.items {
            if let Item::Declaration(decl) = &item.inner {
                if let Declaration::Function { name, parameters, return_type, .. } = &decl.inner {
                    let func_id = self.id_gen.next_function();
                    let entry_block = self.id_gen.next_block();
                    
                    let mut cfg = ControlFlowGraph::new(func_id, name.clone(), entry_block);
                    
                    // Add parameters
                    for param in parameters {
                        let param_value = self.id_gen.next_value();
                        let param_name = self.extract_pattern_name(&param.pattern)?;
                        cfg.add_parameter(param_name, param_value);
                    }
                    
                    self.cfgs.insert(func_id, cfg);
                }
            }
        }
        
        // Second pass: build function bodies
        for item in &program.items {
            if let Item::Declaration(decl) = &item.inner {
                if let Declaration::Function { name, body, .. } = &decl.inner {
                    // Find the function ID
                    let func_id = self.cfgs.iter()
                        .find_map(|(id, cfg)| if cfg.name == *name { Some(*id) } else { None })
                        .ok_or_else(|| IrBuildError::InternalError {
                            message: format!("Function {} not found in CFG map", name),
                        })?;
                    
                    self.build_function(func_id, body.as_ref(), &decl.span)?;
                }
            }
        }
        
        // Move CFGs to module
        module.functions = self.cfgs.clone();
        
        Ok(module)
    }
    
    /// Build IR for a function
    fn build_function(&mut self, func_id: FunctionId, body: Option<&Block>, span: &Span) -> Result<(), IrBuildError> {
        self.current_function = Some(func_id);
        
        let cfg = self.cfgs.get(&func_id).cloned().ok_or_else(|| IrBuildError::InternalError {
            message: format!("CFG for function {} not found", func_id),
        })?;
        
        let entry_block = cfg.entry_block;
        self.current_block = Some(entry_block);
        
        // Initialize variable mappings for parameters
        for (param_name, param_value) in cfg.get_parameters() {
            self.write_variable(param_name.clone(), *param_value, entry_block);
        }
        
        // Build function body
        if let Some(body) = body {
            self.build_block(body)?;
        }
        
        // Ensure function has a return if it doesn't already
        let current_cfg = self.cfgs.get_mut(&func_id).unwrap();
        if let Some(current_block_id) = self.current_block {
            if let Some(current_block) = current_cfg.get_block_mut(current_block_id) {
                if !current_block.has_terminator() {
                    // Add implicit unit return
                    let ret_inst = Instruction::new(
                        self.id_gen.next_value(),
                        InstructionKind::Return { value: None },
                        IrType::Unit,
                        Some(*span),
                    );
                    current_block.add_instruction(ret_inst);
                }
            }
        }
        
        // Seal all blocks and resolve phi nodes
        self.seal_all_blocks(func_id)?;
        
        // Update control flow edges
        let current_cfg = self.cfgs.get_mut(&func_id).unwrap();
        current_cfg.update_control_flow();
        
        self.current_function = None;
        self.current_block = None;
        
        Ok(())
    }
    
    /// Build IR for a block of statements
    fn build_block(&mut self, block: &Block) -> Result<Option<ValueId>, IrBuildError> {
        let mut last_value = None;
        
        for stmt in &block.statements {
            last_value = self.build_statement(stmt)?;
        }
        
        Ok(last_value)
    }
    
    /// Build IR for a statement
    fn build_statement(&mut self, stmt: &StmtNode) -> Result<Option<ValueId>, IrBuildError> {
        match &stmt.inner {
            Statement::Let { pattern, initializer, .. } => {
                if let Some(init_expr) = initializer {
                    let value = self.build_expression(init_expr)?;
                    let var_name = self.extract_pattern_name(pattern)?;
                    
                    if let Some(current_block) = self.current_block {
                        self.write_variable(var_name, value, current_block);
                    }
                    
                    Ok(Some(value))
                } else {
                    Ok(None)
                }
            },
            Statement::Return(expr_opt) => {
                let return_value = if let Some(expr) = expr_opt {
                    Some(self.build_expression(expr)?)
                } else {
                    None
                };
                
                let ret_inst = Instruction::new(
                    self.id_gen.next_value(),
                    InstructionKind::Return { value: return_value },
                    IrType::Unit,
                    Some(stmt.span),
                );
                
                self.add_instruction(ret_inst)?;
                
                Ok(None)
            },
            Statement::Expression(expr) => {
                let value = self.build_expression(expr)?;
                Ok(Some(value))
            },
            _ => {
                // TODO: Handle other statement types
                Ok(None)
            }
        }
    }
    
    /// Build IR for an expression
    fn build_expression(&mut self, expr: &ExprNode) -> Result<ValueId, IrBuildError> {
        match &expr.inner {
            Expr::Literal(literal) => self.build_literal(literal, expr.span),
            Expr::Identifier(name) => self.build_identifier(name, expr.span),
            Expr::Binary { left, operator, right } => {
                self.build_binary_expression(left, *operator, right, expr.span)
            },
            Expr::Unary { operator, operand } => {
                self.build_unary_expression(*operator, operand, expr.span)
            },
            Expr::Call { function, args } => {
                self.build_call_expression(function, args, expr.span)
            },
            Expr::Block(statements) => {
                // Create a new block for the expression block
                let block_id = self.id_gen.next_block();
                let current_func = self.current_function.unwrap();
                
                let cfg = self.cfgs.get_mut(&current_func).unwrap();
                cfg.add_block(block_id);
                
                // Connect current block to new block
                if let Some(current_block_id) = self.current_block {
                    let jump_inst = Instruction::new(
                        self.id_gen.next_value(),
                        InstructionKind::Jump { target: block_id },
                        IrType::Unit,
                        Some(expr.span),
                    );
                    
                    if let Some(current_block) = cfg.get_block_mut(current_block_id) {
                        current_block.add_instruction(jump_inst);
                    }
                    
                    cfg.connect_blocks(current_block_id, block_id);
                }
                
                self.current_block = Some(block_id);
                
                // Build block contents
                let mut last_value = None;
                for stmt in statements {
                    last_value = self.build_statement(stmt)?;
                }
                
                // Return the last expression value or unit
                Ok(last_value.unwrap_or_else(|| {
                    // Create unit constant
                    let unit_inst = Instruction::new(
                        self.id_gen.next_value(),
                        InstructionKind::ConstUnit,
                        IrType::Unit,
                        Some(expr.span),
                    );
                    let value_id = unit_inst.id;
                    self.add_instruction(unit_inst).unwrap();
                    value_id
                }))
            },
            _ => {
                // TODO: Handle other expression types
                Err(IrBuildError::InvalidOperation {
                    message: "Unsupported expression type".to_string(),
                    span: expr.span,
                })
            }
        }
    }
    
    /// Build IR for a literal
    fn build_literal(&mut self, literal: &Literal, span: Span) -> Result<ValueId, IrBuildError> {
        let (kind, ty) = match literal {
            Literal::Integer(value) => {
                (InstructionKind::ConstInt { value: *value, ty: IrType::I32 }, IrType::I32)
            },
            Literal::Float(value) => {
                (InstructionKind::ConstFloat { value: *value, ty: IrType::F64 }, IrType::F64)
            },
            Literal::Boolean(value) => {
                (InstructionKind::ConstBool { value: *value }, IrType::Bool)
            },
            Literal::String(value) => {
                (InstructionKind::ConstString { value: value.clone() }, IrType::String)
            },
            Literal::Character(value) => {
                (InstructionKind::ConstInt { value: *value as i64, ty: IrType::Char }, IrType::Char)
            },
            Literal::Null => {
                (InstructionKind::ConstUnit, IrType::Unit)
            },
        };
        
        let inst = Instruction::new(self.id_gen.next_value(), kind, ty, Some(span));
        let value_id = inst.id;
        self.add_instruction(inst)?;
        Ok(value_id)
    }
    
    /// Build IR for an identifier
    fn build_identifier(&mut self, name: &str, span: Span) -> Result<ValueId, IrBuildError> {
        if let Some(current_block) = self.current_block {
            self.read_variable(name.to_string(), current_block)
                .ok_or_else(|| IrBuildError::UndefinedVariable {
                    name: name.to_string(),
                    span,
                })
        } else {
            Err(IrBuildError::InternalError {
                message: "No current block when building identifier".to_string(),
            })
        }
    }
    
    /// Build IR for a binary expression
    fn build_binary_expression(&mut self, left: &ExprNode, op: BinaryOp, right: &ExprNode, span: Span) -> Result<ValueId, IrBuildError> {
        let left_val = self.build_expression(left)?;
        let right_val = self.build_expression(right)?;
        
        let kind = match op {
            BinaryOp::Add => InstructionKind::Add { lhs: left_val, rhs: right_val },
            BinaryOp::Subtract => InstructionKind::Sub { lhs: left_val, rhs: right_val },
            BinaryOp::Multiply => InstructionKind::Mul { lhs: left_val, rhs: right_val },
            BinaryOp::Divide => InstructionKind::Div { lhs: left_val, rhs: right_val },
            BinaryOp::Modulo => InstructionKind::Mod { lhs: left_val, rhs: right_val },
            BinaryOp::Equal => InstructionKind::Eq { lhs: left_val, rhs: right_val },
            BinaryOp::NotEqual => InstructionKind::Ne { lhs: left_val, rhs: right_val },
            BinaryOp::Less => InstructionKind::Lt { lhs: left_val, rhs: right_val },
            BinaryOp::LessEqual => InstructionKind::Le { lhs: left_val, rhs: right_val },
            BinaryOp::Greater => InstructionKind::Gt { lhs: left_val, rhs: right_val },
            BinaryOp::GreaterEqual => InstructionKind::Ge { lhs: left_val, rhs: right_val },
            BinaryOp::And => InstructionKind::And { lhs: left_val, rhs: right_val },
            BinaryOp::Or => InstructionKind::Or { lhs: left_val, rhs: right_val },
            _ => {
                return Err(IrBuildError::InvalidOperation {
                    message: format!("Unsupported binary operator: {:?}", op),
                    span,
                });
            }
        };
        
        // Determine result type based on operation
        let result_type = match op {
            BinaryOp::Equal | BinaryOp::NotEqual | BinaryOp::Less | BinaryOp::LessEqual |
            BinaryOp::Greater | BinaryOp::GreaterEqual | BinaryOp::And | BinaryOp::Or => IrType::Bool,
            _ => IrType::I32, // TODO: Get actual type from type checker
        };
        
        let inst = Instruction::new(self.id_gen.next_value(), kind, result_type, Some(span));
        let value_id = inst.id;
        self.add_instruction(inst)?;
        Ok(value_id)
    }
    
    /// Build IR for a unary expression
    fn build_unary_expression(&mut self, op: UnaryOp, operand: &ExprNode, span: Span) -> Result<ValueId, IrBuildError> {
        let operand_val = self.build_expression(operand)?;
        
        let kind = match op {
            UnaryOp::Minus => InstructionKind::Neg { operand: operand_val },
            UnaryOp::Plus => {
                // Plus is a no-op, just return the operand
                return Ok(operand_val);
            },
            UnaryOp::Not => InstructionKind::Not { operand: operand_val },
            UnaryOp::BitNot => {
                // Bitwise NOT - use XOR with all 1s mask
                return Err(IrBuildError::InvalidOperation {
                    message: "Bitwise NOT not yet implemented".to_string(),
                    span,
                });
            },
        };
        
        let result_type = match op {
            UnaryOp::Not => IrType::Bool,
            UnaryOp::BitNot => IrType::I32, // TODO: Get actual type from type checker
            _ => IrType::I32, // TODO: Get actual type from type checker
        };
        
        let inst = Instruction::new(self.id_gen.next_value(), kind, result_type, Some(span));
        let value_id = inst.id;
        self.add_instruction(inst)?;
        Ok(value_id)
    }
    
    /// Build IR for a function call
    fn build_call_expression(&mut self, function: &ExprNode, args: &[ExprNode], span: Span) -> Result<ValueId, IrBuildError> {
        // For now, only support direct function calls
        if let Expr::Identifier(func_name) = &function.inner {
            // Find function ID
            let func_id = self.cfgs.iter()
                .find_map(|(id, cfg)| if cfg.name == *func_name { Some(*id) } else { None })
                .ok_or_else(|| IrBuildError::UndefinedFunction {
                    name: func_name.clone(),
                    span,
                })?;
            
            // Build argument values
            let mut arg_values = Vec::new();
            for arg in args {
                arg_values.push(self.build_expression(arg)?);
            }
            
            let call_inst = Instruction::new(
                self.id_gen.next_value(),
                InstructionKind::Call { function: func_id, args: arg_values },
                IrType::I32, // TODO: Get actual return type
                Some(span),
            );
            let value_id = call_inst.id;
            self.add_instruction(call_inst)?;
            Ok(value_id)
        } else {
            Err(IrBuildError::InvalidOperation {
                message: "Indirect function calls not yet supported".to_string(),
                span,
            })
        }
    }
    
    /// Add an instruction to the current block
    fn add_instruction(&mut self, instruction: Instruction) -> Result<(), IrBuildError> {
        if let (Some(func_id), Some(block_id)) = (self.current_function, self.current_block) {
            let cfg = self.cfgs.get_mut(&func_id).unwrap();
            if let Some(block) = cfg.get_block_mut(block_id) {
                block.add_instruction(instruction);
                Ok(())
            } else {
                Err(IrBuildError::InternalError {
                    message: format!("Block {} not found", block_id),
                })
            }
        } else {
            Err(IrBuildError::InternalError {
                message: "No current function or block".to_string(),
            })
        }
    }
    
    /// SSA construction: Write a variable in a block
    fn write_variable(&mut self, var: String, value: ValueId, block: BlockId) {
        self.variable_stack.entry(var).or_insert_with(Vec::new).push(value);
    }
    
    /// SSA construction: Read a variable in a block
    fn read_variable(&mut self, var: String, block: BlockId) -> Option<ValueId> {
        if let Some(stack) = self.variable_stack.get(&var) {
            if let Some(&value) = stack.last() {
                Some(value)
            } else {
                self.read_variable_recursive(var, block)
            }
        } else {
            self.read_variable_recursive(var, block)
        }
    }
    
    /// SSA construction: Recursive variable reading for phi node creation
    fn read_variable_recursive(&mut self, var: String, block: BlockId) -> Option<ValueId> {
        let value_id = self.id_gen.next_value();
        
        // Check if block is sealed
        if self.sealed_blocks.get(&block).copied().unwrap_or(false) {
            // Block is sealed, add phi if needed
            let current_function = self.current_function?;
            let predecessors = {
                let cfg = self.cfgs.get(&current_function)?;
                let block_obj = cfg.get_block(block)?;
                block_obj.predecessors.clone()
            };
            
            if predecessors.len() == 1 {
                // Single predecessor, just read from there
                let pred = *predecessors.iter().next()?;
                return self.read_variable(var, pred);
            } else if predecessors.is_empty() {
                // No predecessors, undefined variable
                return None;
            } else {
                // Multiple predecessors, need phi
                let mut incoming = Vec::new();
                for pred in predecessors {
                    if let Some(val) = self.read_variable(var.clone(), pred) {
                        incoming.push((val, pred));
                    }
                }
                
                if !incoming.is_empty() {
                    let phi_inst = Instruction::new(
                        value_id,
                        InstructionKind::Phi { incoming },
                        IrType::I32, // TODO: Get proper type
                        None,
                    );
                    
                    // Add phi to beginning of block
                    let cfg = self.cfgs.get_mut(&current_function)?;
                    if let Some(block_obj) = cfg.get_block_mut(block) {
                        block_obj.instructions.insert(0, phi_inst);
                    }
                    
                    self.write_variable(var, value_id, block);
                    return Some(value_id);
                }
            }
        } else {
            // Block not sealed, record incomplete phi
            self.incomplete_phis.entry(block).or_insert_with(Vec::new)
                .push((var.clone(), value_id));
        }
        
        self.write_variable(var, value_id, block);
        Some(value_id)
    }
    
    /// Seal all blocks in a function
    fn seal_all_blocks(&mut self, func_id: FunctionId) -> Result<(), IrBuildError> {
        let block_ids: Vec<_> = self.cfgs.get(&func_id)
            .ok_or_else(|| IrBuildError::InternalError {
                message: format!("Function {} not found", func_id),
            })?
            .blocks()
            .map(|(id, _)| *id)
            .collect();
        
        for block_id in block_ids {
            self.sealed_blocks.insert(block_id, true);
        }
        
        Ok(())
    }
    
    /// Extract pattern name (simplified for now)
    fn extract_pattern_name(&self, pattern: &crate::ast::expr::Pattern) -> Result<String, IrBuildError> {
        match pattern {
            crate::ast::expr::Pattern::Identifier(name) => Ok(name.clone()),
            _ => Err(IrBuildError::InvalidOperation {
                message: "Complex patterns not yet supported".to_string(),
                span: Span::new(0, 0, 0, 0), // TODO: Better span handling
            }),
        }
    }
    
    /// Get the built modules
    pub fn modules(&self) -> &[IrModule] {
        &self.modules
    }
    
    /// Get a specific CFG
    pub fn get_cfg(&self, func_id: FunctionId) -> Option<&ControlFlowGraph> {
        self.cfgs.get(&func_id)
    }
}

impl Default for IrBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl IrModule {
    /// Create a new empty module
    pub fn new(name: String) -> Self {
        Self {
            name,
            functions: HashMap::new(),
            globals: Vec::new(),
            type_definitions: HashMap::new(),
        }
    }
    
    /// Add a function to the module
    pub fn add_function(&mut self, cfg: ControlFlowGraph) {
        self.functions.insert(cfg.function_id, cfg);
    }
    
    /// Get a function by ID
    pub fn get_function(&self, func_id: FunctionId) -> Option<&ControlFlowGraph> {
        self.functions.get(&func_id)
    }
    
    /// Print the entire module
    pub fn display(&self) -> String {
        let mut output = String::new();
        output.push_str(&format!("module {} {{\n", self.name));
        
        for (_, cfg) in &self.functions {
            output.push_str(&format!("{}\n", cfg));
        }
        
        output.push_str("}\n");
        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use crate::lexer::Lexer;
    use crate::semantics::TypeChecker;
    
    fn build_ir_from_source(source: &str) -> Result<IrModule, Box<dyn std::error::Error>> {
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer)?;
        let program = parser.parse_program()?;
        
        let mut type_checker = TypeChecker::new();
        type_checker.check_program(&program)?;
        
        let mut builder = IrBuilder::new();
        let module = builder.build_program(&program, type_checker.symbol_table())?;
        
        Ok(module)
    }
    
    #[test]
    fn test_simple_function() {
        let source = r#"
            fn main() {
                let x := 42;
                return x;
            }
        "#;
        
        let module = build_ir_from_source(source).unwrap();
        assert_eq!(module.functions.len(), 1);
        
        let cfg = module.functions.values().next().unwrap();
        assert_eq!(cfg.name, "main");
        assert!(!cfg.blocks.is_empty());
    }
    
    #[test]
    fn test_arithmetic_expression() {
        let source = r#"
            fn main() {
                let x := 5;
                let y := 10;
                let result := x + y;
                return result;
            }
        "#;
        
        let module = build_ir_from_source(source).unwrap();
        let cfg = module.functions.values().next().unwrap();
        
        // Check that we have the expected instructions
        let entry_block = cfg.get_block(cfg.entry_block).unwrap();
        assert!(!entry_block.instructions.is_empty());
        
        // Look for add instruction
        let has_add = entry_block.instructions.iter()
            .any(|inst| matches!(inst.kind, InstructionKind::Add { .. }));
        assert!(has_add);
    }
    
    #[test]
    fn test_function_call() {
        let source = r#"
            fn add(a: i32, b: i32) -> i32 {
                return a + b;
            }
            
            fn main() {
                let result := add(5, 10);
                return result;
            }
        "#;
        
        let module = build_ir_from_source(source).unwrap();
        assert_eq!(module.functions.len(), 2);
        
        // Check that main function has a call instruction
        let main_cfg = module.functions.values()
            .find(|cfg| cfg.name == "main")
            .unwrap();
        
        let entry_block = main_cfg.get_block(main_cfg.entry_block).unwrap();
        let has_call = entry_block.instructions.iter()
            .any(|inst| matches!(inst.kind, InstructionKind::Call { .. }));
        assert!(has_call);
    }
}