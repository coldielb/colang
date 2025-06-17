/// Recursive descent parser for COLANG
/// 
/// This module implements the main parser logic for converting tokens
/// into an Abstract Syntax Tree using recursive descent with operator precedence climbing.

use crate::lexer::{Lexer, Token, TokenType, LexError};
use crate::ast::*;
use crate::ast::decl::Parameter; // Use the declaration Parameter type
use crate::util::Span;

/// Parser state and implementation
pub struct Parser<'source> {
    lexer: Lexer<'source>,
    current_token: Token,
    node_id_gen: NodeIdGenerator,
}

impl<'source> Parser<'source> {
    pub fn new(mut lexer: Lexer<'source>) -> Result<Self, ParseError> {
        let current_token = lexer.next_token()
            .map_err(|e| ParseError::LexError(e))?;
        
        Ok(Self {
            lexer,
            current_token,
            node_id_gen: NodeIdGenerator::new(),
        })
    }
    
    /// Parse a complete program
    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut items = Vec::new();
        
        while !self.is_at_end() {
            // Skip any empty statements or extra semicolons
            if self.check(&TokenType::Semicolon) {
                self.advance()?;
                continue;
            }
            
            let item = self.parse_item()?;
            items.push(item);
        }
        
        Ok(Program::new(items))
    }
    
    /// Parse a top-level item (declaration)
    fn parse_item(&mut self) -> Result<ItemNode, ParseError> {
        let decl = self.parse_declaration()?;
        let span = decl.span();
        let id = self.node_id_gen.next();
        
        Ok(Node::new(Item::Declaration(decl), span, id))
    }
    
    /// Parse a declaration
    fn parse_declaration(&mut self) -> Result<DeclarationNode, ParseError> {
        let visibility = self.parse_visibility()?;
        
        match &self.current_token.token_type {
            TokenType::Fn => self.parse_function(visibility),
            TokenType::Struct => self.parse_struct(visibility),
            TokenType::Enum => self.parse_enum(visibility),
            TokenType::Trait => self.parse_trait(visibility),
            TokenType::Impl => self.parse_impl(),
            TokenType::Type => self.parse_type_alias(visibility),
            TokenType::Const => self.parse_const(visibility),
            TokenType::Static => self.parse_static(visibility),
            TokenType::Mod => self.parse_module(visibility),
            TokenType::Use => self.parse_use(visibility),
            TokenType::Extern => self.parse_extern_block(),
            _ => Err(ParseError::UnexpectedToken {
                found: self.current_token.lexeme.clone(),
                expected: "declaration".to_string(),
                span: self.current_token.span,
            }),
        }
    }
    
    /// Parse a function declaration
    fn parse_function(&mut self, visibility: Visibility) -> Result<DeclarationNode, ParseError> {
        let start_span = self.current_token.span;
        self.expect(&TokenType::Fn)?;
        
        let name = self.expect_identifier("function name")?;
        let generic_params = self.parse_generic_params()?;
        
        self.expect(&TokenType::LeftParen)?;
        let parameters = self.parse_parameter_list()?;
        self.expect(&TokenType::RightParen)?;
        
        let return_type = if self.check(&TokenType::Arrow) {
            self.advance()?;
            Some(self.parse_type()?)
        } else {
            None
        };
        
        let body = if self.check(&TokenType::LeftBrace) {
            Some(self.parse_block()?)
        } else {
            self.expect(&TokenType::Semicolon)?;
            None
        };
        
        let end_span = self.previous_span();
        let span = start_span.to(end_span);
        let id = self.node_id_gen.next();
        
        let decl = Declaration::Function {
            name,
            generic_params,
            parameters,
            return_type,
            body,
            visibility,
            is_unsafe: false, // TODO: Handle unsafe functions
            is_extern: false,
            abi: None,
        };
        
        Ok(Node::new(decl, span, id))
    }
    
    /// Parse a struct declaration
    fn parse_struct(&mut self, visibility: Visibility) -> Result<DeclarationNode, ParseError> {
        let start_span = self.current_token.span;
        self.expect(&TokenType::Struct)?;
        
        let name = self.expect_identifier("struct name")?;
        let generic_params = self.parse_generic_params()?;
        
        self.expect(&TokenType::LeftBrace)?;
        let mut fields = Vec::new();
        
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            let field_visibility = self.parse_visibility()?;
            let field_name = self.expect_identifier("field name")?;
            self.expect(&TokenType::Colon)?;
            let field_type = self.parse_type()?;
            
            let field_span = self.previous_span();
            let field = Field {
                name: field_name,
                field_type,
                visibility: field_visibility,
                span: field_span,
            };
            fields.push(field);
            
            if !self.check(&TokenType::RightBrace) {
                self.expect(&TokenType::Comma)?;
            }
        }
        
        self.expect(&TokenType::RightBrace)?;
        
        let end_span = self.previous_span();
        let span = start_span.to(end_span);
        let id = self.node_id_gen.next();
        
        let decl = Declaration::Struct {
            name,
            generic_params,
            fields,
            visibility,
        };
        
        Ok(Node::new(decl, span, id))
    }
    
    /// Parse an enum declaration
    fn parse_enum(&mut self, visibility: Visibility) -> Result<DeclarationNode, ParseError> {
        let start_span = self.current_token.span;
        self.expect(&TokenType::Enum)?;
        
        let name = self.expect_identifier("enum name")?;
        let generic_params = self.parse_generic_params()?;
        
        self.expect(&TokenType::LeftBrace)?;
        let mut variants = Vec::new();
        
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            let variant_name = self.expect_identifier("variant name")?;
            let variant_span = self.previous_span();
            
            let fields = if self.check(&TokenType::LeftParen) {
                // Tuple variant
                self.advance()?;
                let mut types = Vec::new();
                
                while !self.check(&TokenType::RightParen) && !self.is_at_end() {
                    types.push(self.parse_type()?);
                    if !self.check(&TokenType::RightParen) {
                        self.expect(&TokenType::Comma)?;
                    }
                }
                
                self.expect(&TokenType::RightParen)?;
                VariantFields::Tuple(types)
            } else if self.check(&TokenType::LeftBrace) {
                // Struct variant
                self.advance()?;
                let mut fields = Vec::new();
                
                while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
                    let field_name = self.expect_identifier("field name")?;
                    self.expect(&TokenType::Colon)?;
                    let field_type = self.parse_type()?;
                    
                    let field = Field {
                        name: field_name,
                        field_type,
                        visibility: Visibility::Private,
                        span: self.previous_span(),
                    };
                    fields.push(field);
                    
                    if !self.check(&TokenType::RightBrace) {
                        self.expect(&TokenType::Comma)?;
                    }
                }
                
                self.expect(&TokenType::RightBrace)?;
                VariantFields::Struct(fields)
            } else {
                VariantFields::Unit
            };
            
            let discriminant = if self.check(&TokenType::Assign) {
                self.advance()?;
                Some(self.parse_expression()?)
            } else {
                None
            };
            
            let variant = EnumVariant {
                name: variant_name,
                fields,
                discriminant,
                span: variant_span,
            };
            variants.push(variant);
            
            if !self.check(&TokenType::RightBrace) {
                self.expect(&TokenType::Comma)?;
            }
        }
        
        self.expect(&TokenType::RightBrace)?;
        
        let end_span = self.previous_span();
        let span = start_span.to(end_span);
        let id = self.node_id_gen.next();
        
        let decl = Declaration::Enum {
            name,
            generic_params,
            variants,
            visibility,
        };
        
        Ok(Node::new(decl, span, id))
    }
    
    /// Parse expressions with operator precedence climbing
    fn parse_expression(&mut self) -> Result<ExprNode, ParseError> {
        self.parse_binary_expression(0)
    }
    
    /// Parse binary expressions using precedence climbing
    fn parse_binary_expression(&mut self, min_precedence: u8) -> Result<ExprNode, ParseError> {
        let mut left = self.parse_unary_expression()?;
        
        while let Some(op) = self.current_binary_operator() {
            let precedence = op.precedence();
            if precedence < min_precedence {
                break;
            }
            
            self.advance()?; // consume operator
            
            let next_min_precedence = if op.is_left_associative() {
                precedence + 1
            } else {
                precedence
            };
            
            let right = self.parse_binary_expression(next_min_precedence)?;
            
            let span = left.span().to(right.span());
            let id = self.node_id_gen.next();
            
            left = Node::new(
                Expr::Binary {
                    left: Box::new(left),
                    operator: op,
                    right: Box::new(right),
                },
                span,
                id,
            );
        }
        
        Ok(left)
    }
    
    /// Parse unary expressions
    fn parse_unary_expression(&mut self) -> Result<ExprNode, ParseError> {
        if let Some(op) = self.current_unary_operator() {
            let start_span = self.current_token.span;
            self.advance()?;
            
            let operand = self.parse_unary_expression()?;
            let span = start_span.to(operand.span());
            let id = self.node_id_gen.next();
            
            Ok(Node::new(
                Expr::Unary {
                    operator: op,
                    operand: Box::new(operand),
                },
                span,
                id,
            ))
        } else {
            self.parse_postfix_expression()
        }
    }
    
    /// Parse postfix expressions (calls, indexing, field access)
    fn parse_postfix_expression(&mut self) -> Result<ExprNode, ParseError> {
        let mut expr = self.parse_primary_expression()?;
        
        loop {
            match &self.current_token.token_type {
                TokenType::LeftParen => {
                    // Function call
                    self.advance()?;
                    let mut args = Vec::new();
                    
                    while !self.check(&TokenType::RightParen) && !self.is_at_end() {
                        args.push(self.parse_expression()?);
                        if !self.check(&TokenType::RightParen) {
                            self.expect(&TokenType::Comma)?;
                        }
                    }
                    
                    let end_span = self.current_token.span;
                    self.expect(&TokenType::RightParen)?;
                    
                    let span = expr.span().to(end_span);
                    let id = self.node_id_gen.next();
                    
                    expr = Node::new(
                        Expr::Call {
                            function: Box::new(expr),
                            args,
                        },
                        span,
                        id,
                    );
                },
                TokenType::LeftBracket => {
                    // Array indexing
                    self.advance()?;
                    let index = self.parse_expression()?;
                    let end_span = self.current_token.span;
                    self.expect(&TokenType::RightBracket)?;
                    
                    let span = expr.span().to(end_span);
                    let id = self.node_id_gen.next();
                    
                    expr = Node::new(
                        Expr::Index {
                            array: Box::new(expr),
                            index: Box::new(index),
                        },
                        span,
                        id,
                    );
                },
                TokenType::Dot => {
                    // Field access or method call
                    self.advance()?;
                    let field_name = self.expect_identifier("field or method name")?;
                    
                    if self.check(&TokenType::LeftParen) {
                        // Method call
                        self.advance()?;
                        let mut args = Vec::new();
                        
                        while !self.check(&TokenType::RightParen) && !self.is_at_end() {
                            args.push(self.parse_expression()?);
                            if !self.check(&TokenType::RightParen) {
                                self.expect(&TokenType::Comma)?;
                            }
                        }
                        
                        let end_span = self.current_token.span;
                        self.expect(&TokenType::RightParen)?;
                        
                        let span = expr.span().to(end_span);
                        let id = self.node_id_gen.next();
                        
                        expr = Node::new(
                            Expr::MethodCall {
                                receiver: Box::new(expr),
                                method: field_name,
                                args,
                            },
                            span,
                            id,
                        );
                    } else {
                        // Field access
                        let span = expr.span().to(self.previous_span());
                        let id = self.node_id_gen.next();
                        
                        expr = Node::new(
                            Expr::FieldAccess {
                                object: Box::new(expr),
                                field: field_name,
                            },
                            span,
                            id,
                        );
                    }
                },
                _ => break,
            }
        }
        
        Ok(expr)
    }
    
    /// Parse primary expressions (literals, identifiers, parenthesized expressions)
    fn parse_primary_expression(&mut self) -> Result<ExprNode, ParseError> {
        let span = self.current_token.span;
        let id = self.node_id_gen.next();
        
        match &self.current_token.token_type {
            // Literals
            TokenType::IntegerLiteral(value) => {
                let value = *value;
                self.advance()?;
                Ok(Node::new(
                    Expr::Literal(Literal::Integer(value)),
                    span,
                    id,
                ))
            },
            TokenType::FloatLiteral(value) => {
                let value = *value;
                self.advance()?;
                Ok(Node::new(
                    Expr::Literal(Literal::Float(value)),
                    span,
                    id,
                ))
            },
            TokenType::StringLiteral(ref value) => {
                let value = value.clone();
                self.advance()?;
                Ok(Node::new(
                    Expr::Literal(Literal::String(value)),
                    span,
                    id,
                ))
            },
            TokenType::CharLiteral(value) => {
                let value = *value;
                self.advance()?;
                Ok(Node::new(
                    Expr::Literal(Literal::Character(value)),
                    span,
                    id,
                ))
            },
            TokenType::True => {
                self.advance()?;
                Ok(Node::new(
                    Expr::Literal(Literal::Boolean(true)),
                    span,
                    id,
                ))
            },
            TokenType::False => {
                self.advance()?;
                Ok(Node::new(
                    Expr::Literal(Literal::Boolean(false)),
                    span,
                    id,
                ))
            },
            TokenType::Null => {
                self.advance()?;
                Ok(Node::new(
                    Expr::Literal(Literal::Null),
                    span,
                    id,
                ))
            },
            
            // Identifier
            TokenType::Identifier(ref name) => {
                let name = name.clone();
                self.advance()?;
                Ok(Node::new(
                    Expr::Identifier(name),
                    span,
                    id,
                ))
            },
            
            // Parenthesized expression
            TokenType::LeftParen => {
                self.advance()?;
                let expr = self.parse_expression()?;
                self.expect(&TokenType::RightParen)?;
                Ok(expr)
            },
            
            // Array literal
            TokenType::LeftBracket => {
                self.advance()?;
                let mut elements = Vec::new();
                
                while !self.check(&TokenType::RightBracket) && !self.is_at_end() {
                    elements.push(self.parse_expression()?);
                    if !self.check(&TokenType::RightBracket) {
                        self.expect(&TokenType::Comma)?;
                    }
                }
                
                let end_span = self.current_token.span;
                self.expect(&TokenType::RightBracket)?;
                
                Ok(Node::new(
                    Expr::Array(elements),
                    span.to(end_span),
                    id,
                ))
            },
            
            // Block expression
            TokenType::LeftBrace => {
                let statements = self.parse_block_statements()?;
                let end_span = self.previous_span();
                
                Ok(Node::new(
                    Expr::Block(statements),
                    span.to(end_span),
                    id,
                ))
            },
            
            // If expression
            TokenType::If => {
                self.advance()?;
                let condition = self.parse_expression()?;
                let then_expr = self.parse_expression()?;
                
                let else_expr = if self.check(&TokenType::Else) {
                    self.advance()?;
                    Some(Box::new(self.parse_expression()?))
                } else {
                    None
                };
                
                let end_span = else_expr.as_ref()
                    .map(|e| e.span())
                    .unwrap_or_else(|| then_expr.span());
                
                Ok(Node::new(
                    Expr::If {
                        condition: Box::new(condition),
                        then_expr: Box::new(then_expr),
                        else_expr,
                    },
                    span.to(end_span),
                    id,
                ))
            },
            
            _ => Err(ParseError::UnexpectedToken {
                found: self.current_token.lexeme.clone(),
                expected: "expression".to_string(),
                span: self.current_token.span,
            }),
        }
    }
    
    // Utility methods for parser state management
    
    fn is_at_end(&self) -> bool {
        matches!(self.current_token.token_type, TokenType::Eof)
    }
    
    fn check(&self, token_type: &TokenType) -> bool {
        std::mem::discriminant(&self.current_token.token_type) == std::mem::discriminant(token_type)
    }
    
    fn advance(&mut self) -> Result<(), ParseError> {
        if !self.is_at_end() {
            self.current_token = self.lexer.next_token()
                .map_err(|e| ParseError::LexError(e))?;
        }
        Ok(())
    }
    
    fn expect(&mut self, expected: &TokenType) -> Result<(), ParseError> {
        if self.check(expected) {
            self.advance()
        } else {
            Err(ParseError::UnexpectedToken {
                found: self.current_token.lexeme.clone(),
                expected: format!("{:?}", expected),
                span: self.current_token.span,
            })
        }
    }
    
    fn expect_identifier(&mut self, context: &str) -> Result<String, ParseError> {
        match &self.current_token.token_type {
            TokenType::Identifier(name) => {
                let name = name.clone();
                self.advance()?;
                Ok(name)
            },
            _ => Err(ParseError::UnexpectedToken {
                found: self.current_token.lexeme.clone(),
                expected: context.to_string(),
                span: self.current_token.span,
            }),
        }
    }
    
    fn previous_span(&self) -> Span {
        // This is a simplified implementation
        // In a real parser, you'd track the previous token's span
        self.current_token.span
    }
    
    fn current_binary_operator(&self) -> Option<BinaryOp> {
        match &self.current_token.token_type {
            TokenType::Plus => Some(BinaryOp::Add),
            TokenType::Minus => Some(BinaryOp::Subtract),
            TokenType::Star => Some(BinaryOp::Multiply),
            TokenType::Slash => Some(BinaryOp::Divide),
            TokenType::Percent => Some(BinaryOp::Modulo),
            TokenType::Power => Some(BinaryOp::Power),
            TokenType::Equal => Some(BinaryOp::Equal),
            TokenType::NotEqual => Some(BinaryOp::NotEqual),
            TokenType::Less => Some(BinaryOp::Less),
            TokenType::Greater => Some(BinaryOp::Greater),
            TokenType::LessEqual => Some(BinaryOp::LessEqual),
            TokenType::GreaterEqual => Some(BinaryOp::GreaterEqual),
            TokenType::And => Some(BinaryOp::And),
            TokenType::OrOr => Some(BinaryOp::Or),
            TokenType::Ampersand => Some(BinaryOp::BitAnd),
            TokenType::Pipe => Some(BinaryOp::BitOr),
            TokenType::Caret => Some(BinaryOp::BitXor),
            TokenType::LeftShift => Some(BinaryOp::LeftShift),
            TokenType::RightShift => Some(BinaryOp::RightShift),
            _ => None,
        }
    }
    
    fn current_unary_operator(&self) -> Option<UnaryOp> {
        match &self.current_token.token_type {
            TokenType::Plus => Some(UnaryOp::Plus),
            TokenType::Minus => Some(UnaryOp::Minus),
            TokenType::Not => Some(UnaryOp::Not),
            TokenType::Tilde => Some(UnaryOp::BitNot),
            _ => None,
        }
    }
    
    // Stub implementations for remaining parsing methods
    // These will be implemented in subsequent iterations
    
    fn parse_visibility(&mut self) -> Result<Visibility, ParseError> {
        if self.check(&TokenType::Pub) {
            self.advance()?;
            Ok(Visibility::Public)
        } else {
            Ok(Visibility::Private)
        }
    }
    
    fn parse_generic_params(&mut self) -> Result<Vec<GenericParam>, ParseError> {
        // TODO: Implement generic parameter parsing
        Ok(Vec::new())
    }
    
    fn parse_parameter_list(&mut self) -> Result<Vec<Parameter>, ParseError> {
        let mut parameters = Vec::new();
        
        while !self.check(&TokenType::RightParen) && !self.is_at_end() {
            let param = self.parse_parameter()?;
            parameters.push(param);
            
            if !self.check(&TokenType::RightParen) {
                self.expect(&TokenType::Comma)?;
            }
        }
        
        Ok(parameters)
    }
    
    fn parse_parameter(&mut self) -> Result<Parameter, ParseError> {
        let pattern = self.parse_pattern()?;
        self.expect(&TokenType::Colon)?;
        let param_type = self.parse_type()?;
        
        let default = if self.check(&TokenType::Assign) {
            self.advance()?;
            Some(self.parse_expression()?)
        } else {
            None
        };
        
        Ok(Parameter {
            pattern,
            param_type,
            default,
            span: self.current_token.span, // Simplified
        })
    }
    
    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        match &self.current_token.token_type {
            TokenType::Identifier(name) => {
                let name = name.clone();
                self.advance()?;
                Ok(Pattern::Identifier(name))
            },
            _ => Err(ParseError::UnexpectedToken {
                found: self.current_token.lexeme.clone(),
                expected: "pattern".to_string(),
                span: self.current_token.span,
            }),
        }
    }
    
    fn parse_type(&mut self) -> Result<TypeNode, ParseError> {
        let span = self.current_token.span;
        let id = self.node_id_gen.next();
        
        let ty = match &self.current_token.token_type {
            // Ownership types
            TokenType::Own => {
                self.advance()?;
                let inner_type = self.parse_type()?;
                Type::Own(Box::new(inner_type))
            },
            TokenType::Ref => {
                self.advance()?;
                let inner_type = self.parse_type()?;
                Type::Ref(Box::new(inner_type))
            },
            TokenType::Mut if self.peek_is(&TokenType::Ref) => {
                self.advance()?; // consume 'mut'
                self.advance()?; // consume 'ref'
                let inner_type = self.parse_type()?;
                Type::MutRef(Box::new(inner_type))
            },
            TokenType::Shared => {
                self.advance()?;
                let inner_type = self.parse_type()?;
                Type::Shared(Box::new(inner_type))
            },
            
            // Primitive types
            TokenType::Identifier(name) => {
                let name = name.clone();
                self.advance()?;
                
                match name.as_str() {
                    "i8" => Type::I8,
                    "i16" => Type::I16,
                    "i32" => Type::I32,
                    "i64" => Type::I64,
                    "i128" => Type::I128,
                    "u8" => Type::U8,
                    "u16" => Type::U16,
                    "u32" => Type::U32,
                    "u64" => Type::U64,
                    "u128" => Type::U128,
                    "f32" => Type::F32,
                    "f64" => Type::F64,
                    "bool" => Type::Bool,
                    "char" => Type::Char,
                    "str" => Type::Str,
                    _ => Type::Named {
                        name,
                        generic_args: Vec::new(), // TODO: Parse generic arguments
                    },
                }
            },
            
            // Array types
            TokenType::LeftBracket => {
                self.advance()?;
                let element_type = self.parse_type()?;
                
                let size = if self.check(&TokenType::Semicolon) {
                    self.advance()?;
                    // TODO: Parse array size expression
                    Some(0)
                } else {
                    None
                };
                
                self.expect(&TokenType::RightBracket)?;
                Type::Array(Box::new(element_type), size)
            },
            
            // Unit type
            TokenType::LeftParen => {
                self.advance()?;
                if self.check(&TokenType::RightParen) {
                    self.advance()?;
                    Type::Unit
                } else {
                    // Tuple type
                    let mut types = Vec::new();
                    types.push(self.parse_type()?);
                    
                    while self.check(&TokenType::Comma) {
                        self.advance()?;
                        if self.check(&TokenType::RightParen) {
                            break;
                        }
                        types.push(self.parse_type()?);
                    }
                    
                    self.expect(&TokenType::RightParen)?;
                    Type::Tuple(types)
                }
            },
            
            _ => return Err(ParseError::UnexpectedToken {
                found: self.current_token.lexeme.clone(),
                expected: "type".to_string(),
                span: self.current_token.span,
            }),
        };
        
        Ok(Node::new(ty, span, id))
    }
    
    fn parse_block(&mut self) -> Result<Block, ParseError> {
        self.expect(&TokenType::LeftBrace)?;
        let statements = self.parse_block_statements()?;
        self.expect(&TokenType::RightBrace)?;
        
        Ok(Block::with_statements(statements))
    }
    
    fn parse_block_statements(&mut self) -> Result<Vec<StmtNode>, ParseError> {
        let mut statements = Vec::new();
        
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
        }
        
        Ok(statements)
    }
    
    fn parse_statement(&mut self) -> Result<StmtNode, ParseError> {
        let span = self.current_token.span;
        let id = self.node_id_gen.next();
        
        let stmt = match &self.current_token.token_type {
            TokenType::Let => {
                self.advance()?;
                let mutable = if self.check(&TokenType::Mut) {
                    self.advance()?;
                    true
                } else {
                    false
                };
                
                let pattern = self.parse_pattern()?;
                
                let type_annotation = if self.check(&TokenType::Colon) {
                    self.advance()?;
                    Some(self.parse_type()?)
                } else {
                    None
                };
                
                let initializer = if self.check(&TokenType::ColonAssign) || self.check(&TokenType::Assign) {
                    self.advance()?;
                    Some(self.parse_expression()?)
                } else {
                    None
                };
                
                self.expect(&TokenType::Semicolon)?;
                
                Statement::Let {
                    pattern,
                    type_annotation,
                    initializer,
                    mutable,
                }
            },
            
            TokenType::Return => {
                self.advance()?;
                let expr = if self.check(&TokenType::Semicolon) {
                    None
                } else {
                    Some(self.parse_expression()?)
                };
                self.expect(&TokenType::Semicolon)?;
                Statement::Return(expr)
            },
            
            _ => {
                // Expression statement
                let expr = self.parse_expression()?;
                self.expect(&TokenType::Semicolon)?;
                Statement::Expression(expr)
            },
        };
        
        Ok(Node::new(stmt, span, id))
    }
    
    fn peek_is(&self, token_type: &TokenType) -> bool {
        // Simplified peek implementation
        // In a real parser, you'd maintain a lookahead buffer
        false
    }
    
    // Stub methods for remaining declaration types
    fn parse_trait(&mut self, _visibility: Visibility) -> Result<DeclarationNode, ParseError> {
        Err(ParseError::UnexpectedToken {
            found: self.current_token.lexeme.clone(),
            expected: "trait implementation".to_string(),
            span: self.current_token.span,
        })
    }
    
    fn parse_impl(&mut self) -> Result<DeclarationNode, ParseError> {
        Err(ParseError::UnexpectedToken {
            found: self.current_token.lexeme.clone(),
            expected: "impl implementation".to_string(),
            span: self.current_token.span,
        })
    }
    
    fn parse_type_alias(&mut self, _visibility: Visibility) -> Result<DeclarationNode, ParseError> {
        Err(ParseError::UnexpectedToken {
            found: self.current_token.lexeme.clone(),
            expected: "type alias implementation".to_string(),
            span: self.current_token.span,
        })
    }
    
    fn parse_const(&mut self, _visibility: Visibility) -> Result<DeclarationNode, ParseError> {
        Err(ParseError::UnexpectedToken {
            found: self.current_token.lexeme.clone(),
            expected: "const implementation".to_string(),
            span: self.current_token.span,
        })
    }
    
    fn parse_static(&mut self, _visibility: Visibility) -> Result<DeclarationNode, ParseError> {
        Err(ParseError::UnexpectedToken {
            found: self.current_token.lexeme.clone(),
            expected: "static implementation".to_string(),
            span: self.current_token.span,
        })
    }
    
    fn parse_module(&mut self, _visibility: Visibility) -> Result<DeclarationNode, ParseError> {
        Err(ParseError::UnexpectedToken {
            found: self.current_token.lexeme.clone(),
            expected: "module implementation".to_string(),
            span: self.current_token.span,
        })
    }
    
    fn parse_use(&mut self, _visibility: Visibility) -> Result<DeclarationNode, ParseError> {
        Err(ParseError::UnexpectedToken {
            found: self.current_token.lexeme.clone(),
            expected: "use implementation".to_string(),
            span: self.current_token.span,
        })
    }
    
    fn parse_extern_block(&mut self) -> Result<DeclarationNode, ParseError> {
        Err(ParseError::UnexpectedToken {
            found: self.current_token.lexeme.clone(),
            expected: "extern block implementation".to_string(),
            span: self.current_token.span,
        })
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
    LexError(LexError),
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
            ParseError::LexError(e) => {
                write!(f, "Lexical error: {}", e)
            },
        }
    }
}

impl std::error::Error for ParseError {}