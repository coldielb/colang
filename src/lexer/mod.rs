/// Lexical analysis module for COLANG
/// 
/// This module provides tokenization capabilities for the COLANG source code.
/// It converts raw source text into a stream of tokens with position information
/// for accurate error reporting and IDE integration.
/// 
/// The lexer supports:
/// - Keywords: own, ref, mut, shared, or, return, if, else, while, for, fn, struct, enum, trait, impl, use, pub
/// - Operators: +, -, *, /, %, ==, !=, <, >, <=, >=, &&, ||, !, &, |, ^, <<, >>
/// - Delimiters: (, ), {, }, [, ], ;, :, ::, ,, ., ->
/// - Literals: integers, floats, strings, characters, booleans
/// - Identifiers and type inference operator (:=)

pub mod token;
pub mod lexer;

pub use token::*;
pub use lexer::*;