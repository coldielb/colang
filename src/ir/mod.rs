/// Intermediate Representation for COLANG
/// 
/// This module defines a target-independent intermediate representation (IR)
/// that serves as the interface between the frontend and backend. The IR is
/// designed for easy optimization and multiple backend targets.
/// 
/// Features:
/// - SSA (Static Single Assignment) form for optimization
/// - Type information preservation for safety checks
/// - Explicit ownership transfer operations
/// - Control flow graph representation
/// - Built-in optimization passes

pub mod instruction;
pub mod basic_block;
pub mod function;
pub mod module;
pub mod builder;
pub mod optimizer;

pub use instruction::*;
pub use basic_block::*;
pub use function::*;
pub use module::*;
pub use builder::*;
pub use optimizer::*;