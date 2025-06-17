/// Recursive descent parser for COLANG
/// 
/// This module implements the main parser logic for converting tokens
/// into an Abstract Syntax Tree.

use crate::lexer::{Lexer, Token, TokenType};
use crate::ast::*;
use crate::util::Span;

/// Parser state and implementation
pub struct Parser<'source> {
    lexer: Lexer<'source>,
    current_token: Option<Token>,
}

impl<'source> Parser<'source> {
    pub fn new(mut lexer: Lexer<'source>) -> Result<Self, crate::lexer::LexError> {
        let current_token = lexer.next_token().ok();
        Ok(Self {
            lexer,
            current_token,
        })
    }
    
    /// Parse a complete program
    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut items = Vec::new();
        
        while !self.is_at_end() {
            // TODO: Parse top-level items
            break; // Temporary to avoid infinite loop
        }
        
        Ok(Program::new(items))
    }
    
    fn is_at_end(&self) -> bool {
        self.current_token.as_ref().map_or(true, |t| t.is_eof())
    }
}

/// Parse errors
#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedToken {
        found: String,
        expected: String,
        span: Span,
    },
    UnexpectedEof {
        expected: String,
    },
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken { found, expected, span } => {
                write!(f, "Unexpected token '{}' at {}, expected {}", found, span, expected)
            },
            ParseError::UnexpectedEof { expected } => {
                write!(f, "Unexpected end of file, expected {}", expected)
            },
        }
    }
}

impl std::error::Error for ParseError {}