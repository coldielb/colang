use logos::Logos;
use std::fmt;
use thiserror::Error;

use crate::lexer::token::{Token, TokenType, Span};

#[derive(Error, Debug, Clone)]
pub enum LexError {
    #[error("Unexpected character '{character}' at line {line}, column {column}")]
    UnexpectedCharacter {
        character: char,
        line: u32,
        column: u32,
    },
    
    #[error("Unterminated string literal at line {line}, column {column}")]
    UnterminatedString {
        line: u32,
        column: u32,
    },
    
    #[error("Invalid number format at line {line}, column {column}")]
    InvalidNumber {
        line: u32,
        column: u32,
    },
    
    #[error("Invalid character literal at line {line}, column {column}")]
    InvalidCharacter {
        line: u32,
        column: u32,
    },
}

/// COLANG lexer that converts source code into tokens
pub struct Lexer<'source> {
    source: &'source str,
    lexer: logos::Lexer<'source, TokenType>,
    current_line: u32,
    current_column: u32,
    line_starts: Vec<usize>,
}

impl<'source> Lexer<'source> {
    /// Create a new lexer for the given source code
    pub fn new(source: &'source str) -> Self {
        let mut lexer = Self {
            source,
            lexer: TokenType::lexer(source),
            current_line: 1,
            current_column: 1,
            line_starts: vec![0],
        };
        
        lexer.build_line_table();
        lexer
    }
    
    /// Build a table of line start positions for efficient span calculation
    fn build_line_table(&mut self) {
        for (i, ch) in self.source.char_indices() {
            if ch == '\n' {
                self.line_starts.push(i + 1);
            }
        }
    }
    
    /// Convert a byte position to line and column numbers
    fn position_to_line_col(&self, position: usize) -> (u32, u32) {
        match self.line_starts.binary_search(&position) {
            Ok(line) => (line as u32 + 1, 1),
            Err(line) => {
                let line_num = line as u32;
                let line_start = self.line_starts.get(line - 1).copied().unwrap_or(0);
                let column = position - line_start + 1;
                (line_num, column as u32)
            }
        }
    }
    
    /// Create a span from start and end positions
    fn make_span(&self, start: usize, end: usize) -> Span {
        let (start_line, start_col) = self.position_to_line_col(start);
        Span::new(start, end, start_line, start_col)
    }
    
    /// Get the next token from the source
    pub fn next_token(&mut self) -> Result<Token, LexError> {
        match self.lexer.next() {
            Some(Ok(token_type)) => {
                let span_range = self.lexer.span();
                let lexeme = self.lexer.slice().to_owned();
                let span = self.make_span(span_range.start, span_range.end);
                
                // Validate certain token types
                match &token_type {
                    TokenType::StringLiteral(ref s) => {
                        if !s.starts_with('"') || !s.ends_with('"') {
                            return Err(LexError::UnterminatedString {
                                line: span.line,
                                column: span.column,
                            });
                        }
                    },
                    TokenType::CharLiteral(_) => {
                        if lexeme.len() < 3 || !lexeme.starts_with('\'') || !lexeme.ends_with('\'') {
                            return Err(LexError::InvalidCharacter {
                                line: span.line,
                                column: span.column,
                            });
                        }
                    },
                    _ => {}
                }
                
                Ok(Token::new(token_type, span, lexeme))
            },
            Some(Err(_)) => {
                let span_range = self.lexer.span();
                let span = self.make_span(span_range.start, span_range.end);
                let lexeme = self.lexer.slice();
                
                if let Some(first_char) = lexeme.chars().next() {
                    Err(LexError::UnexpectedCharacter {
                        character: first_char,
                        line: span.line,
                        column: span.column,
                    })
                } else {
                    Ok(Token::eof(self.source.len()))
                }
            },
            None => Ok(Token::eof(self.source.len())),
        }
    }
    
    /// Tokenize the entire source and return all tokens
    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        
        loop {
            let token = self.next_token()?;
            let is_eof = token.is_eof();
            tokens.push(token);
            
            if is_eof {
                break;
            }
        }
        
        Ok(tokens)
    }
    
    /// Peek at the current token without consuming it
    pub fn peek(&self) -> Result<Token, LexError> {
        let mut cloned = self.clone();
        cloned.next_token()
    }
    
    /// Get the current position in the source
    pub fn position(&self) -> usize {
        self.lexer.span().start
    }
    
    /// Get the remaining source code
    pub fn remainder(&self) -> &str {
        self.lexer.remainder()
    }
    
    /// Check if we're at the end of the source
    pub fn is_at_end(&self) -> bool {
        self.lexer.remainder().is_empty()
    }
}

impl<'source> Clone for Lexer<'source> {
    fn clone(&self) -> Self {
        Self {
            source: self.source,
            lexer: TokenType::lexer(self.source).spanned(self.lexer.span()),
            current_line: self.current_line,
            current_column: self.current_column,
            line_starts: self.line_starts.clone(),
        }
    }
}

impl fmt::Debug for Lexer<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Lexer")
            .field("position", &self.position())
            .field("line", &self.current_line)
            .field("column", &self.current_column)
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_basic_tokens() {
        let source = "fn main() { let x := 42; }";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        
        assert_eq!(tokens.len(), 11); // Including EOF
        assert!(matches!(tokens[0].token_type, TokenType::Fn));
        assert!(matches!(tokens[1].token_type, TokenType::Identifier(_)));
        assert!(matches!(tokens[2].token_type, TokenType::LeftParen));
        assert!(matches!(tokens[3].token_type, TokenType::RightParen));
        assert!(matches!(tokens[4].token_type, TokenType::LeftBrace));
        assert!(matches!(tokens[5].token_type, TokenType::Let));
        assert!(matches!(tokens[6].token_type, TokenType::Identifier(_)));
        assert!(matches!(tokens[7].token_type, TokenType::ColonAssign));
        assert!(matches!(tokens[8].token_type, TokenType::IntegerLiteral(42)));
        assert!(matches!(tokens[9].token_type, TokenType::Semicolon));
        assert!(matches!(tokens[10].token_type, TokenType::Eof));
    }
    
    #[test]
    fn test_ownership_keywords() {
        let source = "own ref mut shared or return";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        
        assert!(matches!(tokens[0].token_type, TokenType::Own));
        assert!(matches!(tokens[1].token_type, TokenType::Ref));
        assert!(matches!(tokens[2].token_type, TokenType::Mut));
        assert!(matches!(tokens[3].token_type, TokenType::Shared));
        assert!(matches!(tokens[4].token_type, TokenType::Or));
        assert!(matches!(tokens[5].token_type, TokenType::Return));
    }
    
    #[test]
    fn test_literals() {
        let source = r#"42 3.14 "hello" 'c' true false null"#;
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        
        assert!(matches!(tokens[0].token_type, TokenType::IntegerLiteral(42)));
        assert!(matches!(tokens[1].token_type, TokenType::FloatLiteral(_)));
        assert!(matches!(tokens[2].token_type, TokenType::StringLiteral(_)));
        assert!(matches!(tokens[3].token_type, TokenType::CharLiteral('c')));
        assert!(matches!(tokens[4].token_type, TokenType::True));
        assert!(matches!(tokens[5].token_type, TokenType::False));
        assert!(matches!(tokens[6].token_type, TokenType::Null));
    }
    
    #[test]
    fn test_operators() {
        let source = "+ - * / % == != < > <= >= && || !";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        
        assert!(matches!(tokens[0].token_type, TokenType::Plus));
        assert!(matches!(tokens[1].token_type, TokenType::Minus));
        assert!(matches!(tokens[2].token_type, TokenType::Star));
        assert!(matches!(tokens[3].token_type, TokenType::Slash));
        assert!(matches!(tokens[4].token_type, TokenType::Percent));
        assert!(matches!(tokens[5].token_type, TokenType::Equal));
        assert!(matches!(tokens[6].token_type, TokenType::NotEqual));
        assert!(matches!(tokens[7].token_type, TokenType::Less));
        assert!(matches!(tokens[8].token_type, TokenType::Greater));
        assert!(matches!(tokens[9].token_type, TokenType::LessEqual));
        assert!(matches!(tokens[10].token_type, TokenType::GreaterEqual));
        assert!(matches!(tokens[11].token_type, TokenType::And));
        assert!(matches!(tokens[12].token_type, TokenType::OrOr));
        assert!(matches!(tokens[13].token_type, TokenType::Not));
    }
    
    #[test]
    fn test_comments_ignored() {
        let source = r#"
            // This is a line comment
            fn test() {
                /* This is a block comment */
                let x := 1;
            }
        "#;
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        
        // Should not contain any comment tokens
        for token in &tokens {
            assert!(!matches!(token.token_type, TokenType::Error));
        }
    }
    
    #[test]
    fn test_position_tracking() {
        let source = "fn\nmain() {\n  x := 1;\n}";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        
        // First token should be on line 1
        assert_eq!(tokens[0].span.line, 1);
        
        // Find the identifier token and check its position
        for token in &tokens {
            if let TokenType::Identifier(ref name) = token.token_type {
                if name == "main" {
                    assert_eq!(token.span.line, 2);
                    break;
                }
            }
        }
    }
}