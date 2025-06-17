/// Parser module for COLANG
/// 
/// This module implements a recursive descent parser that builds an Abstract Syntax Tree (AST)
/// from the token stream produced by the lexer. The parser includes error recovery mechanisms
/// to continue parsing after encountering syntax errors, providing comprehensive diagnostics.
/// 
/// Features:
/// - Recursive descent parsing with operator precedence
/// - Error recovery and synchronization points
/// - Position tracking for accurate error reporting
/// - Support for COLANG's ownership syntax and type inference

pub mod parser;
pub mod error;

pub use parser::*;
pub use error::*;