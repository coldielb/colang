/// COLANG Intermediate Representation Instructions
/// 
/// This module defines the instruction set for COLANG's SSA-form IR.
/// The IR is designed to be target-independent and optimize-friendly.

use std::fmt;
use crate::ast::TypeNode;
use crate::util::Span;

/// Unique identifier for IR values (SSA registers)
pub type ValueId = usize;

/// Unique identifier for basic blocks
pub type BlockId = usize;

/// Unique identifier for functions
pub type FunctionId = usize;

/// IR instruction representing a single operation in SSA form
#[derive(Debug, Clone, PartialEq)]
pub struct Instruction {
    pub id: ValueId,
    pub kind: InstructionKind,
    pub ty: IrType,
    pub span: Option<Span>,
}

/// Different kinds of IR instructions
#[derive(Debug, Clone, PartialEq)]
pub enum InstructionKind {
    // Arithmetic operations
    Add { lhs: ValueId, rhs: ValueId },
    Sub { lhs: ValueId, rhs: ValueId },
    Mul { lhs: ValueId, rhs: ValueId },
    Div { lhs: ValueId, rhs: ValueId },
    Mod { lhs: ValueId, rhs: ValueId },
    
    // Comparison operations
    Eq { lhs: ValueId, rhs: ValueId },
    Ne { lhs: ValueId, rhs: ValueId },
    Lt { lhs: ValueId, rhs: ValueId },
    Le { lhs: ValueId, rhs: ValueId },
    Gt { lhs: ValueId, rhs: ValueId },
    Ge { lhs: ValueId, rhs: ValueId },
    
    // Logical operations
    And { lhs: ValueId, rhs: ValueId },
    Or { lhs: ValueId, rhs: ValueId },
    Not { operand: ValueId },
    
    // Unary operations
    Neg { operand: ValueId },
    
    // Memory operations
    Load { address: ValueId },
    Store { address: ValueId, value: ValueId },
    Alloca { ty: IrType },
    
    // Constants
    ConstInt { value: i64, ty: IrType },
    ConstFloat { value: f64, ty: IrType },
    ConstBool { value: bool },
    ConstString { value: String },
    ConstUnit,
    
    // Control flow
    Branch { condition: ValueId, then_block: BlockId, else_block: BlockId },
    Jump { target: BlockId },
    Return { value: Option<ValueId> },
    
    // Function calls
    Call { function: FunctionId, args: Vec<ValueId> },
    CallIndirect { function: ValueId, args: Vec<ValueId> },
    
    // Type operations
    Cast { value: ValueId, target_ty: IrType },
    
    // Phi nodes for SSA
    Phi { incoming: Vec<(ValueId, BlockId)> },
    
    // Variable access (before SSA transformation)
    GetLocal { name: String },
    SetLocal { name: String, value: ValueId },
    
    // Debugging
    DebugLocation { line: u32, column: u32, file: String },
}

/// Type system for IR values
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IrType {
    // Primitive types
    I8, I16, I32, I64, I128,
    U8, U16, U32, U64, U128,
    F32, F64,
    Bool,
    Char,
    Unit,
    
    // Pointer types
    Ptr(Box<IrType>),
    
    // Aggregate types
    Array(Box<IrType>, Option<usize>),
    Tuple(Vec<IrType>),
    Struct { name: String, fields: Vec<IrType> },
    
    // Function types
    Function {
        params: Vec<IrType>,
        ret: Box<IrType>,
    },
    
    // String type (special handling)
    String,
}

/// Context for generating unique IDs
#[derive(Debug)]
pub struct IdGenerator {
    next_value_id: ValueId,
    next_block_id: BlockId,
    next_function_id: FunctionId,
}

impl IdGenerator {
    pub fn new() -> Self {
        Self {
            next_value_id: 0,
            next_block_id: 0,
            next_function_id: 0,
        }
    }
    
    pub fn next_value(&mut self) -> ValueId {
        let id = self.next_value_id;
        self.next_value_id += 1;
        id
    }
    
    pub fn next_block(&mut self) -> BlockId {
        let id = self.next_block_id;
        self.next_block_id += 1;
        id
    }
    
    pub fn next_function(&mut self) -> FunctionId {
        let id = self.next_function_id;
        self.next_function_id += 1;
        id
    }
}

impl Instruction {
    pub fn new(id: ValueId, kind: InstructionKind, ty: IrType, span: Option<Span>) -> Self {
        Self { id, kind, ty, span }
    }
    
    /// Get the result type of this instruction
    pub fn result_type(&self) -> &IrType {
        &self.ty
    }
    
    /// Check if this instruction is a terminator (ends a basic block)
    pub fn is_terminator(&self) -> bool {
        matches!(
            self.kind,
            InstructionKind::Branch { .. } 
                | InstructionKind::Jump { .. } 
                | InstructionKind::Return { .. }
        )
    }
    
    /// Check if this instruction has side effects
    pub fn has_side_effects(&self) -> bool {
        matches!(
            self.kind,
            InstructionKind::Store { .. }
                | InstructionKind::Call { .. }
                | InstructionKind::CallIndirect { .. }
                | InstructionKind::SetLocal { .. }
                | InstructionKind::Return { .. }
        )
    }
    
    /// Get the values this instruction uses
    pub fn used_values(&self) -> Vec<ValueId> {
        match &self.kind {
            InstructionKind::Add { lhs, rhs } 
            | InstructionKind::Sub { lhs, rhs }
            | InstructionKind::Mul { lhs, rhs }
            | InstructionKind::Div { lhs, rhs }
            | InstructionKind::Mod { lhs, rhs }
            | InstructionKind::Eq { lhs, rhs }
            | InstructionKind::Ne { lhs, rhs }
            | InstructionKind::Lt { lhs, rhs }
            | InstructionKind::Le { lhs, rhs }
            | InstructionKind::Gt { lhs, rhs }
            | InstructionKind::Ge { lhs, rhs }
            | InstructionKind::And { lhs, rhs }
            | InstructionKind::Or { lhs, rhs } => vec![*lhs, *rhs],
            
            InstructionKind::Not { operand }
            | InstructionKind::Neg { operand }
            | InstructionKind::Load { address: operand }
            | InstructionKind::Cast { value: operand, .. } => vec![*operand],
            
            InstructionKind::Store { address, value } => vec![*address, *value],
            InstructionKind::Branch { condition, .. } => vec![*condition],
            InstructionKind::Return { value: Some(v) } => vec![*v],
            InstructionKind::Call { args, .. } | InstructionKind::CallIndirect { args, .. } => args.clone(),
            InstructionKind::SetLocal { value, .. } => vec![*value],
            InstructionKind::Phi { incoming } => incoming.iter().map(|(v, _)| *v).collect(),
            
            _ => vec![],
        }
    }
}

impl IrType {
    /// Check if this is a signed integer type
    pub fn is_signed_int(&self) -> bool {
        matches!(self, IrType::I8 | IrType::I16 | IrType::I32 | IrType::I64 | IrType::I128)
    }
    
    /// Check if this is an unsigned integer type
    pub fn is_unsigned_int(&self) -> bool {
        matches!(self, IrType::U8 | IrType::U16 | IrType::U32 | IrType::U64 | IrType::U128)
    }
    
    /// Check if this is an integer type
    pub fn is_integer(&self) -> bool {
        self.is_signed_int() || self.is_unsigned_int()
    }
    
    /// Check if this is a floating point type
    pub fn is_float(&self) -> bool {
        matches!(self, IrType::F32 | IrType::F64)
    }
    
    /// Check if this is a numeric type
    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }
    
    /// Check if this is a pointer type
    pub fn is_pointer(&self) -> bool {
        matches!(self, IrType::Ptr(_))
    }
    
    /// Get the size in bytes for this type (if known at compile time)
    pub fn size_bytes(&self) -> Option<usize> {
        match self {
            IrType::I8 | IrType::U8 | IrType::Bool => Some(1),
            IrType::I16 | IrType::U16 => Some(2),
            IrType::I32 | IrType::U32 | IrType::F32 | IrType::Char => Some(4),
            IrType::I64 | IrType::U64 | IrType::F64 => Some(8),
            IrType::I128 | IrType::U128 => Some(16),
            IrType::Unit => Some(0),
            IrType::Ptr(_) => Some(8), // Assume 64-bit pointers
            IrType::Array(elem_ty, Some(size)) => {
                elem_ty.size_bytes().map(|elem_size| elem_size * size)
            },
            IrType::Tuple(types) => {
                let mut total = 0;
                for ty in types {
                    total += ty.size_bytes()?;
                }
                Some(total)
            },
            _ => None, // Dynamic size or unknown
        }
    }
    
    /// Create a pointer to this type
    pub fn pointer_to(self) -> IrType {
        IrType::Ptr(Box::new(self))
    }
    
    /// Get the pointed-to type if this is a pointer
    pub fn pointee(&self) -> Option<&IrType> {
        match self {
            IrType::Ptr(pointee) => Some(pointee),
            _ => None,
        }
    }
}

impl fmt::Display for IrType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrType::I8 => write!(f, "i8"),
            IrType::I16 => write!(f, "i16"),
            IrType::I32 => write!(f, "i32"),
            IrType::I64 => write!(f, "i64"),
            IrType::I128 => write!(f, "i128"),
            IrType::U8 => write!(f, "u8"),
            IrType::U16 => write!(f, "u16"),
            IrType::U32 => write!(f, "u32"),
            IrType::U64 => write!(f, "u64"),
            IrType::U128 => write!(f, "u128"),
            IrType::F32 => write!(f, "f32"),
            IrType::F64 => write!(f, "f64"),
            IrType::Bool => write!(f, "bool"),
            IrType::Char => write!(f, "char"),
            IrType::Unit => write!(f, "()"),
            IrType::String => write!(f, "str"),
            IrType::Ptr(pointee) => write!(f, "*{}", pointee),
            IrType::Array(elem, Some(size)) => write!(f, "[{}; {}]", elem, size),
            IrType::Array(elem, None) => write!(f, "[{}]", elem),
            IrType::Tuple(types) => {
                write!(f, "(")?;
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", ty)?;
                }
                write!(f, ")")
            },
            IrType::Struct { name, .. } => write!(f, "{}", name),
            IrType::Function { params, ret } => {
                write!(f, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", param)?;
                }
                write!(f, ") -> {}", ret)
            },
        }
    }
}

impl fmt::Display for InstructionKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InstructionKind::Add { lhs, rhs } => write!(f, "add %{}, %{}", lhs, rhs),
            InstructionKind::Sub { lhs, rhs } => write!(f, "sub %{}, %{}", lhs, rhs),
            InstructionKind::Mul { lhs, rhs } => write!(f, "mul %{}, %{}", lhs, rhs),
            InstructionKind::Div { lhs, rhs } => write!(f, "div %{}, %{}", lhs, rhs),
            InstructionKind::Mod { lhs, rhs } => write!(f, "mod %{}, %{}", lhs, rhs),
            
            InstructionKind::Eq { lhs, rhs } => write!(f, "eq %{}, %{}", lhs, rhs),
            InstructionKind::Ne { lhs, rhs } => write!(f, "ne %{}, %{}", lhs, rhs),
            InstructionKind::Lt { lhs, rhs } => write!(f, "lt %{}, %{}", lhs, rhs),
            InstructionKind::Le { lhs, rhs } => write!(f, "le %{}, %{}", lhs, rhs),
            InstructionKind::Gt { lhs, rhs } => write!(f, "gt %{}, %{}", lhs, rhs),
            InstructionKind::Ge { lhs, rhs } => write!(f, "ge %{}, %{}", lhs, rhs),
            
            InstructionKind::And { lhs, rhs } => write!(f, "and %{}, %{}", lhs, rhs),
            InstructionKind::Or { lhs, rhs } => write!(f, "or %{}, %{}", lhs, rhs),
            InstructionKind::Not { operand } => write!(f, "not %{}", operand),
            InstructionKind::Neg { operand } => write!(f, "neg %{}", operand),
            
            InstructionKind::Load { address } => write!(f, "load %{}", address),
            InstructionKind::Store { address, value } => write!(f, "store %{}, %{}", value, address),
            InstructionKind::Alloca { ty } => write!(f, "alloca {}", ty),
            
            InstructionKind::ConstInt { value, ty } => write!(f, "const {} {}", ty, value),
            InstructionKind::ConstFloat { value, ty } => write!(f, "const {} {}", ty, value),
            InstructionKind::ConstBool { value } => write!(f, "const bool {}", value),
            InstructionKind::ConstString { value } => write!(f, "const str \"{}\"", value),
            InstructionKind::ConstUnit => write!(f, "const ()"),
            
            InstructionKind::Branch { condition, then_block, else_block } => {
                write!(f, "br %{}, bb{}, bb{}", condition, then_block, else_block)
            },
            InstructionKind::Jump { target } => write!(f, "jmp bb{}", target),
            InstructionKind::Return { value: Some(v) } => write!(f, "ret %{}", v),
            InstructionKind::Return { value: None } => write!(f, "ret"),
            
            InstructionKind::Call { function, args } => {
                write!(f, "call @{}", function)?;
                if !args.is_empty() {
                    write!(f, "(")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 { write!(f, ", ")?; }
                        write!(f, "%{}", arg)?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            },
            InstructionKind::CallIndirect { function, args } => {
                write!(f, "call %{}", function)?;
                if !args.is_empty() {
                    write!(f, "(")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 { write!(f, ", ")?; }
                        write!(f, "%{}", arg)?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            },
            
            InstructionKind::Cast { value, target_ty } => write!(f, "cast %{} to {}", value, target_ty),
            
            InstructionKind::Phi { incoming } => {
                write!(f, "phi ")?;
                for (i, (value, block)) in incoming.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "[%{}, bb{}]", value, block)?;
                }
                Ok(())
            },
            
            InstructionKind::GetLocal { name } => write!(f, "get_local {}", name),
            InstructionKind::SetLocal { name, value } => write!(f, "set_local {}, %{}", name, value),
            
            InstructionKind::DebugLocation { line, column, file } => {
                write!(f, "debug_loc {}:{}:{}", file, line, column)
            },
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%{}: {} = {}", self.id, self.ty, self.kind)
    }
}

impl Default for IdGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Convert from AST type to IR type
impl From<&crate::ast::types::Type> for IrType {
    fn from(ast_type: &crate::ast::types::Type) -> Self {
        use crate::ast::types::Type;
        
        match ast_type {
            Type::I8 => IrType::I8,
            Type::I16 => IrType::I16,
            Type::I32 => IrType::I32,
            Type::I64 => IrType::I64,
            Type::I128 => IrType::I128,
            Type::U8 => IrType::U8,
            Type::U16 => IrType::U16,
            Type::U32 => IrType::U32,
            Type::U64 => IrType::U64,
            Type::U128 => IrType::U128,
            Type::F32 => IrType::F32,
            Type::F64 => IrType::F64,
            Type::Bool => IrType::Bool,
            Type::Char => IrType::Char,
            Type::Str => IrType::String,
            Type::Unit => IrType::Unit,
            Type::Array(elem_type, size) => {
                IrType::Array(Box::new(IrType::from(&elem_type.inner)), size.map(|s| s as usize))
            },
            Type::Tuple(types) => {
                IrType::Tuple(types.iter().map(|t| IrType::from(&t.inner)).collect())
            },
            Type::Named { name, .. } => {
                // For now, treat named types as opaque structs
                IrType::Struct { name: name.clone(), fields: vec![] }
            },
            _ => IrType::Unit, // Fallback for unhandled types
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_id_generator() {
        let mut gen = IdGenerator::new();
        assert_eq!(gen.next_value(), 0);
        assert_eq!(gen.next_value(), 1);
        assert_eq!(gen.next_block(), 0);
        assert_eq!(gen.next_function(), 0);
    }
    
    #[test]
    fn test_ir_type_properties() {
        assert!(IrType::I32.is_signed_int());
        assert!(IrType::U32.is_unsigned_int());
        assert!(IrType::F64.is_float());
        assert!(IrType::I32.is_integer());
        assert!(IrType::F32.is_numeric());
        assert_eq!(IrType::I32.size_bytes(), Some(4));
        assert_eq!(IrType::Unit.size_bytes(), Some(0));
    }
    
    #[test]
    fn test_instruction_terminator() {
        let mut gen = IdGenerator::new();
        let ret_inst = Instruction::new(
            gen.next_value(),
            InstructionKind::Return { value: None },
            IrType::Unit,
            None,
        );
        assert!(ret_inst.is_terminator());
        
        let add_inst = Instruction::new(
            gen.next_value(),
            InstructionKind::Add { lhs: 0, rhs: 1 },
            IrType::I32,
            None,
        );
        assert!(!add_inst.is_terminator());
    }
    
    #[test]
    fn test_instruction_used_values() {
        let mut gen = IdGenerator::new();
        let add_inst = Instruction::new(
            gen.next_value(),
            InstructionKind::Add { lhs: 5, rhs: 10 },
            IrType::I32,
            None,
        );
        assert_eq!(add_inst.used_values(), vec![5, 10]);
    }
}