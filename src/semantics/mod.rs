/// Semantic analysis module for COLANG
/// 
/// This module performs type checking, ownership analysis, and semantic validation
/// of COLANG programs. It ensures memory safety, type correctness, and proper
/// ownership semantics before code generation.
/// 
/// Analysis phases:
/// - Symbol resolution and scope checking
/// - Type inference and checking
/// - Ownership and borrow checking
/// - Trait resolution and coherence checking
/// - Dead code analysis and optimization hints

pub mod type_checker;
pub mod ownership;
pub mod symbol_table;
pub mod error;

pub use type_checker::*;
pub use ownership::*;
pub use symbol_table::*;
pub use error::*;