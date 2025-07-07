/// LLVM IR Code Generation for COLANG
/// 
/// This module generates LLVM IR from COLANG's internal IR representation.
/// It provides a backend-agnostic way to target different architectures via LLVM.

use std::collections::HashMap;
use std::fmt::Write;
use crate::ir::instruction::{Instruction, InstructionKind, IrType, ValueId, BlockId, FunctionId};
use crate::ir::basic_block::{ControlFlowGraph, BasicBlock};
use crate::ir::builder::IrModule;

/// LLVM IR code generator
pub struct LlvmCodegen {
    /// Mapping from COLANG value IDs to LLVM register names
    value_map: HashMap<ValueId, String>,
    /// Mapping from COLANG block IDs to LLVM block labels
    block_map: HashMap<BlockId, String>,
    /// Mapping from COLANG function IDs to LLVM function names
    function_map: HashMap<FunctionId, String>,
    /// Counter for generating unique temporary names
    temp_counter: usize,
    /// Counter for generating sequential LLVM value names
    llvm_value_counter: usize,
    /// Target triple for the generated LLVM IR
    target_triple: String,
    /// Generated LLVM IR code
    output: String,
}

/// Errors that can occur during LLVM code generation
#[derive(Debug, Clone)]
pub enum LlvmCodegenError {
    UnsupportedInstruction { instruction: String },
    UnsupportedType { ir_type: String },
    UnresolvedValue { value_id: ValueId },
    UnresolvedBlock { block_id: BlockId },
    UnresolvedFunction { func_id: FunctionId },
    InvalidOperation { message: String },
}

impl std::fmt::Display for LlvmCodegenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LlvmCodegenError::UnsupportedInstruction { instruction } => {
                write!(f, "Unsupported instruction: {}", instruction)
            },
            LlvmCodegenError::UnsupportedType { ir_type } => {
                write!(f, "Unsupported type: {}", ir_type)
            },
            LlvmCodegenError::UnresolvedValue { value_id } => {
                write!(f, "Unresolved value: %{}", value_id)
            },
            LlvmCodegenError::UnresolvedBlock { block_id } => {
                write!(f, "Unresolved block: bb{}", block_id)
            },
            LlvmCodegenError::UnresolvedFunction { func_id } => {
                write!(f, "Unresolved function: @{}", func_id)
            },
            LlvmCodegenError::InvalidOperation { message } => {
                write!(f, "Invalid operation: {}", message)
            },
        }
    }
}

impl std::error::Error for LlvmCodegenError {}

impl LlvmCodegen {
    /// Create a new LLVM code generator
    pub fn new() -> Self {
        Self {
            value_map: HashMap::new(),
            block_map: HashMap::new(),
            function_map: HashMap::new(),
            temp_counter: 0,
            llvm_value_counter: 0,
            target_triple: crate::codegen::target::default_target_triple(),
            output: String::new(),
        }
    }
    
    /// Create a new LLVM code generator with a specific target triple
    pub fn with_target(mut self, target_triple: String) -> Self {
        self.target_triple = target_triple;
        self
    }
    
    /// Generate LLVM IR for a complete module
    pub fn generate_module(&mut self, module: &IrModule) -> Result<String, LlvmCodegenError> {
        self.output.clear();
        
        // Module header
        writeln!(self.output, "; COLANG Module: {}", module.name).unwrap();
        
        // Generate appropriate data layout for the target
        let data_layout = self.get_data_layout_for_target();
        writeln!(self.output, "target datalayout = \"{}\"", data_layout).unwrap();
        writeln!(self.output, "target triple = \"{}\"", self.target_triple).unwrap();
        writeln!(self.output).unwrap();
        
        // Generate type definitions
        self.generate_type_definitions(module)?;
        
        // Generate global variables
        self.generate_globals(module)?;
        
        // Generate function declarations for external functions
        self.generate_external_declarations()?;
        
        // Pre-populate function mappings for cross-references
        for (func_id, cfg) in &module.functions {
            self.function_map.insert(*func_id, cfg.name.clone());
        }
        
        // Generate functions
        for (func_id, cfg) in &module.functions {
            self.generate_function(*func_id, cfg)?;
            writeln!(self.output).unwrap();
        }
        
        Ok(self.output.clone())
    }
    
    /// Generate type definitions
    fn generate_type_definitions(&mut self, _module: &IrModule) -> Result<(), LlvmCodegenError> {
        // TODO: Generate struct and other type definitions
        Ok(())
    }
    
    /// Generate global variables
    fn generate_globals(&mut self, module: &IrModule) -> Result<(), LlvmCodegenError> {
        for global in &module.globals {
            let llvm_type = self.ir_type_to_llvm(&global.ty)?;
            
            if let Some(ref initializer) = global.initializer {
                let init_value = self.constant_to_llvm(initializer, &global.ty)?;
                writeln!(self.output, "@{} = global {} {}", 
                    global.name, llvm_type, init_value).unwrap();
            } else {
                writeln!(self.output, "@{} = external global {}", 
                    global.name, llvm_type).unwrap();
            }
        }
        
        if !module.globals.is_empty() {
            writeln!(self.output).unwrap();
        }
        
        Ok(())
    }
    
    /// Generate external function declarations
    fn generate_external_declarations(&mut self) -> Result<(), LlvmCodegenError> {
        // Common runtime functions that might be needed
        writeln!(self.output, "declare i32 @printf(i8*, ...)").unwrap();
        writeln!(self.output, "declare void @exit(i32)").unwrap();
        writeln!(self.output, "declare i8* @malloc(i64)").unwrap();
        writeln!(self.output, "declare void @free(i8*)").unwrap();
        writeln!(self.output).unwrap();
        
        Ok(())
    }
    
    /// Generate LLVM IR for a function
    fn generate_function(&mut self, func_id: FunctionId, cfg: &ControlFlowGraph) -> Result<(), LlvmCodegenError> {
        // Clear value and block mappings for this function (but keep function mappings)
        self.value_map.clear();
        self.block_map.clear();
        self.llvm_value_counter = 1; // Reset LLVM value counter for each function
        
        // Map function ID
        self.function_map.insert(func_id, cfg.name.clone());
        
        // Generate function signature
        let return_type = "i32"; // TODO: Get actual return type
        let func_name = &cfg.name;
        
        write!(self.output, "define {} @{}(", return_type, func_name).unwrap();
        
        // Generate parameters
        for (i, (param_name, param_value)) in cfg.get_parameters().iter().enumerate() {
            if i > 0 {
                write!(self.output, ", ").unwrap();
            }
            let param_type = "i32"; // TODO: Get actual parameter type
            write!(self.output, "{} %{}", param_type, param_value).unwrap();
            
            // Map parameter value
            self.value_map.insert(*param_value, format!("%{}", param_value));
        }
        
        writeln!(self.output, ") {{").unwrap();
        
        // Generate block labels
        for (block_id, _) in cfg.blocks() {
            self.block_map.insert(*block_id, format!("bb{}", block_id));
        }
        
        // Generate blocks in reverse post-order
        let block_order = cfg.reverse_postorder();
        for (i, block_id) in block_order.iter().enumerate() {
            if let Some(block) = cfg.get_block(*block_id) {
                self.generate_basic_block(*block_id, block, i == 0)?;
            }
        }
        
        writeln!(self.output, "}}").unwrap();
        
        Ok(())
    }
    
    /// Generate LLVM IR for a basic block
    fn generate_basic_block(&mut self, block_id: BlockId, block: &BasicBlock, is_entry: bool) -> Result<(), LlvmCodegenError> {
        // Generate instructions
        for instruction in &block.instructions {
            self.generate_instruction(instruction)?;
        }
        
        Ok(())
    }
    
    /// Generate LLVM IR for an instruction
    fn generate_instruction(&mut self, instruction: &Instruction) -> Result<(), LlvmCodegenError> {
        match &instruction.kind {
            InstructionKind::ConstInt { value, ty } => {
                // Constants don't need instructions, just store them for reference
                self.value_map.insert(instruction.id, value.to_string());
                // Don't emit any LLVM instruction for constants
                return Ok(());
            },
            InstructionKind::ConstFloat { value, ty } => {
                self.value_map.insert(instruction.id, value.to_string());
                return Ok(());
            },
            InstructionKind::ConstBool { value } => {
                let bool_val = if *value { "1" } else { "0" };
                self.value_map.insert(instruction.id, bool_val.to_string());
                return Ok(());
            },
            _ => {
                // For non-constant instructions, generate the value name first
                let value_name = format!("%{}", self.llvm_value_counter);
                self.llvm_value_counter += 1;
                self.value_map.insert(instruction.id, value_name.clone());
            }
        }
        
        let value_name = self.value_map[&instruction.id].clone();
        
        match &instruction.kind {
            InstructionKind::ConstString { value } => {
                // Create a global string constant
                let string_name = format!("@.str.{}", self.temp_counter);
                self.temp_counter += 1;
                
                writeln!(self.output, "{} = private unnamed_addr constant [{}x i8] c\"{}\\00\"", 
                    string_name, value.len() + 1, value).unwrap();
                writeln!(self.output, "  {} = getelementptr [{}x i8], [{}x i8]* {}, i32 0, i32 0", 
                    value_name, value.len() + 1, value.len() + 1, string_name).unwrap();
            },
            InstructionKind::ConstUnit => {
                // Unit type is represented as void, but we need a dummy value
                writeln!(self.output, "  {} = add i8 0, 0", value_name).unwrap();
            },
            
            InstructionKind::Add { lhs, rhs } => {
                let lhs_name = self.get_value_name(*lhs)?;
                let rhs_name = self.get_value_name(*rhs)?;
                let result_type = self.ir_type_to_llvm(&instruction.ty)?;
                writeln!(self.output, "  {} = add {} {}, {}", 
                    value_name, result_type, lhs_name, rhs_name).unwrap();
            },
            InstructionKind::Sub { lhs, rhs } => {
                let lhs_name = self.get_value_name(*lhs)?;
                let rhs_name = self.get_value_name(*rhs)?;
                let result_type = self.ir_type_to_llvm(&instruction.ty)?;
                writeln!(self.output, "  {} = sub {} {}, {}", 
                    value_name, result_type, lhs_name, rhs_name).unwrap();
            },
            InstructionKind::Mul { lhs, rhs } => {
                let lhs_name = self.get_value_name(*lhs)?;
                let rhs_name = self.get_value_name(*rhs)?;
                let result_type = self.ir_type_to_llvm(&instruction.ty)?;
                writeln!(self.output, "  {} = mul {} {}, {}", 
                    value_name, result_type, lhs_name, rhs_name).unwrap();
            },
            InstructionKind::Div { lhs, rhs } => {
                let lhs_name = self.get_value_name(*lhs)?;
                let rhs_name = self.get_value_name(*rhs)?;
                let result_type = self.ir_type_to_llvm(&instruction.ty)?;
                
                if instruction.ty.is_signed_int() {
                    writeln!(self.output, "  {} = sdiv {} {}, {}", 
                        value_name, result_type, lhs_name, rhs_name).unwrap();
                } else if instruction.ty.is_unsigned_int() {
                    writeln!(self.output, "  {} = udiv {} {}, {}", 
                        value_name, result_type, lhs_name, rhs_name).unwrap();
                } else if instruction.ty.is_float() {
                    writeln!(self.output, "  {} = fdiv {} {}, {}", 
                        value_name, result_type, lhs_name, rhs_name).unwrap();
                } else {
                    return Err(LlvmCodegenError::UnsupportedType { 
                        ir_type: format!("{}", instruction.ty) 
                    });
                }
            },
            InstructionKind::Mod { lhs, rhs } => {
                let lhs_name = self.get_value_name(*lhs)?;
                let rhs_name = self.get_value_name(*rhs)?;
                let result_type = self.ir_type_to_llvm(&instruction.ty)?;
                
                if instruction.ty.is_signed_int() {
                    writeln!(self.output, "  {} = srem {} {}, {}", 
                        value_name, result_type, lhs_name, rhs_name).unwrap();
                } else if instruction.ty.is_unsigned_int() {
                    writeln!(self.output, "  {} = urem {} {}, {}", 
                        value_name, result_type, lhs_name, rhs_name).unwrap();
                } else if instruction.ty.is_float() {
                    writeln!(self.output, "  {} = frem {} {}, {}", 
                        value_name, result_type, lhs_name, rhs_name).unwrap();
                } else {
                    return Err(LlvmCodegenError::UnsupportedType { 
                        ir_type: format!("{}", instruction.ty) 
                    });
                }
            },
            
            InstructionKind::Eq { lhs, rhs } => {
                let lhs_name = self.get_value_name(*lhs)?;
                let rhs_name = self.get_value_name(*rhs)?;
                // TODO: Get operand type instead of assuming i32
                writeln!(self.output, "  {} = icmp eq i32 {}, {}", 
                    value_name, lhs_name, rhs_name).unwrap();
            },
            InstructionKind::Ne { lhs, rhs } => {
                let lhs_name = self.get_value_name(*lhs)?;
                let rhs_name = self.get_value_name(*rhs)?;
                writeln!(self.output, "  {} = icmp ne i32 {}, {}", 
                    value_name, lhs_name, rhs_name).unwrap();
            },
            InstructionKind::Lt { lhs, rhs } => {
                let lhs_name = self.get_value_name(*lhs)?;
                let rhs_name = self.get_value_name(*rhs)?;
                writeln!(self.output, "  {} = icmp slt i32 {}, {}", 
                    value_name, lhs_name, rhs_name).unwrap();
            },
            InstructionKind::Le { lhs, rhs } => {
                let lhs_name = self.get_value_name(*lhs)?;
                let rhs_name = self.get_value_name(*rhs)?;
                writeln!(self.output, "  {} = icmp sle i32 {}, {}", 
                    value_name, lhs_name, rhs_name).unwrap();
            },
            InstructionKind::Gt { lhs, rhs } => {
                let lhs_name = self.get_value_name(*lhs)?;
                let rhs_name = self.get_value_name(*rhs)?;
                writeln!(self.output, "  {} = icmp sgt i32 {}, {}", 
                    value_name, lhs_name, rhs_name).unwrap();
            },
            InstructionKind::Ge { lhs, rhs } => {
                let lhs_name = self.get_value_name(*lhs)?;
                let rhs_name = self.get_value_name(*rhs)?;
                writeln!(self.output, "  {} = icmp sge i32 {}, {}", 
                    value_name, lhs_name, rhs_name).unwrap();
            },
            
            InstructionKind::And { lhs, rhs } => {
                let lhs_name = self.get_value_name(*lhs)?;
                let rhs_name = self.get_value_name(*rhs)?;
                writeln!(self.output, "  {} = and i1 {}, {}", 
                    value_name, lhs_name, rhs_name).unwrap();
            },
            InstructionKind::Or { lhs, rhs } => {
                let lhs_name = self.get_value_name(*lhs)?;
                let rhs_name = self.get_value_name(*rhs)?;
                writeln!(self.output, "  {} = or i1 {}, {}", 
                    value_name, lhs_name, rhs_name).unwrap();
            },
            InstructionKind::Not { operand } => {
                let operand_name = self.get_value_name(*operand)?;
                writeln!(self.output, "  {} = xor i1 {}, true", 
                    value_name, operand_name).unwrap();
            },
            InstructionKind::Neg { operand } => {
                let operand_name = self.get_value_name(*operand)?;
                let result_type = self.ir_type_to_llvm(&instruction.ty)?;
                writeln!(self.output, "  {} = sub {} 0, {}", 
                    value_name, result_type, operand_name).unwrap();
            },
            
            InstructionKind::Load { address } => {
                let address_name = self.get_value_name(*address)?;
                let result_type = self.ir_type_to_llvm(&instruction.ty)?;
                writeln!(self.output, "  {} = load {}, {}* {}", 
                    value_name, result_type, result_type, address_name).unwrap();
            },
            InstructionKind::Store { address, value } => {
                let address_name = self.get_value_name(*address)?;
                let value_name_ref = self.get_value_name(*value)?;
                let value_type = self.ir_type_to_llvm(&instruction.ty)?;
                writeln!(self.output, "  store {} {}, {}* {}", 
                    value_type, value_name_ref, value_type, address_name).unwrap();
            },
            InstructionKind::Alloca { ty } => {
                let alloca_type = self.ir_type_to_llvm(ty)?;
                writeln!(self.output, "  {} = alloca {}", value_name, alloca_type).unwrap();
            },
            
            InstructionKind::Call { function, args } => {
                let func_name = self.get_function_name(*function)?;
                let return_type = self.ir_type_to_llvm(&instruction.ty)?;
                
                write!(self.output, "  {} = call {} @{}(", value_name, return_type, func_name).unwrap();
                for (i, &arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(self.output, ", ").unwrap();
                    }
                    let arg_name = self.get_value_name(arg)?;
                    let arg_type = "i32"; // TODO: Get actual argument type
                    write!(self.output, "{} {}", arg_type, arg_name).unwrap();
                }
                writeln!(self.output, ")").unwrap();
            },
            
            InstructionKind::Branch { condition, then_block, else_block } => {
                let condition_name = self.get_value_name(*condition)?;
                let then_label = self.get_block_label(*then_block)?;
                let else_label = self.get_block_label(*else_block)?;
                writeln!(self.output, "  br i1 {}, label %{}, label %{}", 
                    condition_name, then_label, else_label).unwrap();
            },
            InstructionKind::Jump { target } => {
                let target_label = self.get_block_label(*target)?;
                writeln!(self.output, "  br label %{}", target_label).unwrap();
            },
            InstructionKind::Return { value: Some(val) } => {
                let return_value = self.get_value_name(*val)?;
                let return_type = "i32"; // TODO: Get actual return type
                writeln!(self.output, "  ret {} {}", return_type, return_value).unwrap();
            },
            InstructionKind::Return { value: None } => {
                writeln!(self.output, "  ret void").unwrap();
            },
            
            InstructionKind::Cast { value, target_ty } => {
                let value_name_ref = self.get_value_name(*value)?;
                let source_type = "i32"; // TODO: Get actual source type
                let target_type = self.ir_type_to_llvm(target_ty)?;
                
                // Choose appropriate cast instruction
                if target_ty.is_integer() {
                    writeln!(self.output, "  {} = bitcast {} {} to {}", 
                        value_name, source_type, value_name_ref, target_type).unwrap();
                } else if target_ty.is_float() {
                    writeln!(self.output, "  {} = sitofp {} {} to {}", 
                        value_name, source_type, value_name_ref, target_type).unwrap();
                } else {
                    writeln!(self.output, "  {} = bitcast {} {} to {}", 
                        value_name, source_type, value_name_ref, target_type).unwrap();
                }
            },
            
            InstructionKind::Phi { incoming } => {
                let result_type = self.ir_type_to_llvm(&instruction.ty)?;
                write!(self.output, "  {} = phi {} ", value_name, result_type).unwrap();
                
                for (i, (value, block)) in incoming.iter().enumerate() {
                    if i > 0 {
                        write!(self.output, ", ").unwrap();
                    }
                    let value_name_ref = self.get_value_name(*value)?;
                    let block_label = self.get_block_label(*block)?;
                    write!(self.output, "[ {}, %{} ]", value_name_ref, block_label).unwrap();
                }
                writeln!(self.output).unwrap();
            },
            
            _ => {
                return Err(LlvmCodegenError::UnsupportedInstruction {
                    instruction: format!("{:?}", instruction.kind),
                });
            }
        }
        
        Ok(())
    }
    
    /// Convert IR type to LLVM type string
    fn ir_type_to_llvm(&self, ir_type: &IrType) -> Result<String, LlvmCodegenError> {
        match ir_type {
            IrType::I8 => Ok("i8".to_string()),
            IrType::I16 => Ok("i16".to_string()),
            IrType::I32 => Ok("i32".to_string()),
            IrType::I64 => Ok("i64".to_string()),
            IrType::I128 => Ok("i128".to_string()),
            IrType::U8 => Ok("i8".to_string()),  // LLVM doesn't distinguish signed/unsigned
            IrType::U16 => Ok("i16".to_string()),
            IrType::U32 => Ok("i32".to_string()),
            IrType::U64 => Ok("i64".to_string()),
            IrType::U128 => Ok("i128".to_string()),
            IrType::F32 => Ok("float".to_string()),
            IrType::F64 => Ok("double".to_string()),
            IrType::Bool => Ok("i1".to_string()),
            IrType::Char => Ok("i32".to_string()), // Unicode code point
            IrType::Unit => Ok("void".to_string()),
            IrType::String => Ok("i8*".to_string()), // Null-terminated string
            IrType::Ptr(pointee) => {
                let pointee_type = self.ir_type_to_llvm(pointee)?;
                Ok(format!("{}*", pointee_type))
            },
            IrType::Array(elem_type, Some(size)) => {
                let elem_llvm = self.ir_type_to_llvm(elem_type)?;
                Ok(format!("[{} x {}]", size, elem_llvm))
            },
            IrType::Array(elem_type, None) => {
                let elem_llvm = self.ir_type_to_llvm(elem_type)?;
                Ok(format!("{}*", elem_llvm)) // Dynamic array as pointer
            },
            IrType::Tuple(types) => {
                let mut field_types = Vec::new();
                for ty in types {
                    field_types.push(self.ir_type_to_llvm(ty)?);
                }
                Ok(format!("{{ {} }}", field_types.join(", ")))
            },
            IrType::Struct { name, fields } => {
                if fields.is_empty() {
                    // Opaque struct
                    Ok(format!("%{}", name))
                } else {
                    let mut field_types = Vec::new();
                    for field_ty in fields {
                        field_types.push(self.ir_type_to_llvm(field_ty)?);
                    }
                    Ok(format!("{{ {} }}", field_types.join(", ")))
                }
            },
            IrType::Function { params, ret } => {
                let mut param_types = Vec::new();
                for param_ty in params {
                    param_types.push(self.ir_type_to_llvm(param_ty)?);
                }
                let ret_type = self.ir_type_to_llvm(ret)?;
                Ok(format!("{} ({})*", ret_type, param_types.join(", ")))
            },
        }
    }
    
    /// Convert constant value to LLVM representation
    fn constant_to_llvm(&self, constant: &crate::ir::builder::ConstantValue, ty: &IrType) -> Result<String, LlvmCodegenError> {
        use crate::ir::builder::ConstantValue;
        
        match constant {
            ConstantValue::Int(value) => Ok(value.to_string()),
            ConstantValue::Float(value) => Ok(value.to_string()),
            ConstantValue::Bool(value) => Ok(if *value { "1".to_string() } else { "0".to_string() }),
            ConstantValue::String(value) => Ok(format!("c\"{}\\00\"", value)),
            ConstantValue::Unit => Ok("undef".to_string()),
            ConstantValue::Array(elements) => {
                if let IrType::Array(elem_ty, _) = ty {
                    let mut elem_values = Vec::new();
                    for elem in elements {
                        elem_values.push(self.constant_to_llvm(elem, elem_ty)?);
                    }
                    Ok(format!("[ {} ]", elem_values.join(", ")))
                } else {
                    Err(LlvmCodegenError::UnsupportedType {
                        ir_type: format!("{}", ty),
                    })
                }
            },
            ConstantValue::Struct(fields) => {
                let mut field_values = Vec::new();
                for field in fields {
                    // TODO: Get proper field types
                    field_values.push(self.constant_to_llvm(field, &IrType::I32)?);
                }
                Ok(format!("{{ {} }}", field_values.join(", ")))
            },
        }
    }
    
    /// Get LLVM register name for a value
    fn get_value_name(&self, value_id: ValueId) -> Result<String, LlvmCodegenError> {
        self.value_map.get(&value_id)
            .cloned()
            .ok_or(LlvmCodegenError::UnresolvedValue { value_id })
    }
    
    /// Get LLVM block label for a block
    fn get_block_label(&self, block_id: BlockId) -> Result<String, LlvmCodegenError> {
        self.block_map.get(&block_id)
            .cloned()
            .ok_or(LlvmCodegenError::UnresolvedBlock { block_id })
    }
    
    /// Get LLVM function name for a function
    fn get_function_name(&self, func_id: FunctionId) -> Result<String, LlvmCodegenError> {
        self.function_map.get(&func_id)
            .cloned()
            .ok_or(LlvmCodegenError::UnresolvedFunction { func_id })
    }
    
    /// Get the generated LLVM IR
    pub fn output(&self) -> &str {
        &self.output
    }
    
    /// Get the appropriate data layout for the target triple
    fn get_data_layout_for_target(&self) -> &'static str {
        if self.target_triple.starts_with("aarch64-apple-darwin") {
            // ARM64 macOS data layout
            "e-m:o-i64:64-i128:128-n32:64-S128"
        } else if self.target_triple.starts_with("x86_64-apple-darwin") {
            // x86_64 macOS data layout  
            "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
        } else if self.target_triple.starts_with("x86_64") && self.target_triple.contains("linux") {
            // x86_64 Linux data layout
            "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
        } else {
            // Default fallback - x86_64 Linux
            "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
        }
    }

    /// Clear the generator state
    pub fn clear(&mut self) {
        self.value_map.clear();
        self.block_map.clear();
        self.function_map.clear();
        self.temp_counter = 0;
        self.llvm_value_counter = 0;
        self.output.clear();
    }
}

impl Default for LlvmCodegen {
    fn default() -> Self {
        Self::new()
    }
}

/// Helper function to generate LLVM IR from an IR module
pub fn generate_llvm_ir(module: &IrModule) -> Result<String, LlvmCodegenError> {
    let mut codegen = LlvmCodegen::new();
    codegen.generate_module(module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::instruction::{IdGenerator, Instruction, InstructionKind, IrType};
    use crate::ir::basic_block::ControlFlowGraph;
    use std::collections::HashMap;
    
    #[test]
    fn test_simple_function_generation() {
        let mut module = IrModule::new("test".to_string());
        let mut id_gen = IdGenerator::new();
        
        // Create a simple function: fn test() -> i32 { return 42; }
        let func_id = id_gen.next_function();
        let entry_block = id_gen.next_block();
        let mut cfg = ControlFlowGraph::new(func_id, "test".to_string(), entry_block);
        
        // Add constant and return instructions
        let const_inst = Instruction::new(
            id_gen.next_value(),
            InstructionKind::ConstInt { value: 42, ty: IrType::I32 },
            IrType::I32,
            None,
        );
        let const_value = const_inst.id;
        
        let ret_inst = Instruction::new(
            id_gen.next_value(),
            InstructionKind::Return { value: Some(const_value) },
            IrType::Unit,
            None,
        );
        
        let block = cfg.get_block_mut(entry_block).unwrap();
        block.add_instruction(const_inst);
        block.add_instruction(ret_inst);
        
        module.add_function(cfg);
        
        // Generate LLVM IR
        let mut codegen = LlvmCodegen::new();
        let llvm_ir = codegen.generate_module(&module).unwrap();
        
        // Check that the output contains expected elements
        assert!(llvm_ir.contains("define i32 @test()"));
        assert!(llvm_ir.contains("ret i32"));
    }
    
    #[test]
    fn test_arithmetic_operations() {
        let mut module = IrModule::new("test".to_string());
        let mut id_gen = IdGenerator::new();
        
        // Create function: fn add_test() -> i32 { return 5 + 10; }
        let func_id = id_gen.next_function();
        let entry_block = id_gen.next_block();
        let mut cfg = ControlFlowGraph::new(func_id, "add_test".to_string(), entry_block);
        
        // 5
        let const5 = Instruction::new(
            id_gen.next_value(),
            InstructionKind::ConstInt { value: 5, ty: IrType::I32 },
            IrType::I32,
            None,
        );
        let val5 = const5.id;
        
        // 10
        let const10 = Instruction::new(
            id_gen.next_value(),
            InstructionKind::ConstInt { value: 10, ty: IrType::I32 },
            IrType::I32,
            None,
        );
        let val10 = const10.id;
        
        // 5 + 10
        let add_inst = Instruction::new(
            id_gen.next_value(),
            InstructionKind::Add { lhs: val5, rhs: val10 },
            IrType::I32,
            None,
        );
        let add_result = add_inst.id;
        
        // return
        let ret_inst = Instruction::new(
            id_gen.next_value(),
            InstructionKind::Return { value: Some(add_result) },
            IrType::Unit,
            None,
        );
        
        let block = cfg.get_block_mut(entry_block).unwrap();
        block.add_instruction(const5);
        block.add_instruction(const10);
        block.add_instruction(add_inst);
        block.add_instruction(ret_inst);
        
        module.add_function(cfg);
        
        // Generate LLVM IR
        let mut codegen = LlvmCodegen::new();
        let llvm_ir = codegen.generate_module(&module).unwrap();
        
        // Check for add instruction
        assert!(llvm_ir.contains("add i32"));
        assert!(llvm_ir.contains("define i32 @add_test()"));
    }
    
    #[test]
    fn test_type_conversion() {
        let codegen = LlvmCodegen::new();
        
        assert_eq!(codegen.ir_type_to_llvm(&IrType::I32).unwrap(), "i32");
        assert_eq!(codegen.ir_type_to_llvm(&IrType::F64).unwrap(), "double");
        assert_eq!(codegen.ir_type_to_llvm(&IrType::Bool).unwrap(), "i1");
        assert_eq!(codegen.ir_type_to_llvm(&IrType::String).unwrap(), "i8*");
        
        let ptr_type = IrType::Ptr(Box::new(IrType::I32));
        assert_eq!(codegen.ir_type_to_llvm(&ptr_type).unwrap(), "i32*");
        
        let array_type = IrType::Array(Box::new(IrType::I32), Some(10));
        assert_eq!(codegen.ir_type_to_llvm(&array_type).unwrap(), "[10 x i32]");
    }
}