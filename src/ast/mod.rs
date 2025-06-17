/// Abstract Syntax Tree definitions for COLANG
/// 
/// This module defines the complete AST node hierarchy for COLANG programs.
/// Each node type includes position information for error reporting and
/// contains all semantic information needed for later compiler phases.
/// 
/// The AST represents:
/// - Expressions: literals, variables, function calls, binary operations
/// - Statements: variable declarations, assignments, control flow
/// - Declarations: functions, structs, enums, traits, implementations
/// - Types: primitive types, user-defined types, generic parameters
/// - Ownership annotations: own, ref, mut ref, shared

pub mod node;
pub mod visitor;
pub mod expr;
pub mod stmt;
pub mod decl;
pub mod types;

pub use node::*;
pub use visitor::*;
pub use expr::*;
pub use stmt::*;
pub use decl::*;
pub use types::*;