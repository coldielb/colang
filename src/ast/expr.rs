use crate::ast::{Node, TypeNode};
use crate::lexer::TokenType;
use std::fmt;

/// Expression nodes in the COLANG AST
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Literal values
    Literal(Literal),
    
    /// Variable references
    Identifier(String),
    
    /// Binary operations
    Binary {
        left: Box<ExprNode>,
        operator: BinaryOp,
        right: Box<ExprNode>,
    },
    
    /// Unary operations
    Unary {
        operator: UnaryOp,
        operand: Box<ExprNode>,
    },
    
    /// Function calls
    Call {
        function: Box<ExprNode>,
        args: Vec<ExprNode>,
    },
    
    /// Method calls
    MethodCall {
        receiver: Box<ExprNode>,
        method: String,
        args: Vec<ExprNode>,
    },
    
    /// Field access
    FieldAccess {
        object: Box<ExprNode>,
        field: String,
    },
    
    /// Array indexing
    Index {
        array: Box<ExprNode>,
        index: Box<ExprNode>,
    },
    
    /// Array literals
    Array(Vec<ExprNode>),
    
    /// Tuple literals
    Tuple(Vec<ExprNode>),
    
    /// Struct literals
    Struct {
        name: String,
        fields: Vec<FieldInit>,
    },
    
    /// Conditional expressions
    If {
        condition: Box<ExprNode>,
        then_expr: Box<ExprNode>,
        else_expr: Option<Box<ExprNode>>,
    },
    
    /// Match expressions
    Match {
        expr: Box<ExprNode>,
        arms: Vec<MatchArm>,
    },
    
    /// Block expressions
    Block(Vec<StmtNode>),
    
    /// Assignment expressions
    Assign {
        target: Box<ExprNode>,
        value: Box<ExprNode>,
    },
    
    /// Compound assignment expressions
    CompoundAssign {
        target: Box<ExprNode>,
        operator: BinaryOp,
        value: Box<ExprNode>,
    },
    
    /// Type casting
    Cast {
        expr: Box<ExprNode>,
        target_type: TypeNode,
    },
    
    /// Reference creation
    Reference {
        mutable: bool,
        expr: Box<ExprNode>,
    },
    
    /// Dereference
    Dereference(Box<ExprNode>),
    
    /// Ownership transfer
    Move(Box<ExprNode>),
    
    /// Result propagation (or return)
    Try(Box<ExprNode>),
    
    /// Lambda expressions
    Lambda {
        params: Vec<ExprParameter>,
        return_type: Option<TypeNode>,
        body: Box<ExprNode>,
    },
    
    /// Range expressions
    Range {
        start: Option<Box<ExprNode>>,
        end: Option<Box<ExprNode>>,
        inclusive: bool,
    },
    
    /// Error expression (for error recovery)
    Error,
}

pub type ExprNode = Node<Expr>;

// Forward declaration to avoid circular dependencies
pub type StmtNode = crate::ast::Node<crate::ast::stmt::Statement>;

/// Literal values
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Character(char),
    Boolean(bool),
    Null,
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,
    
    // Comparison
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    
    // Logical
    And,
    Or,
    
    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    LeftShift,
    RightShift,
}

impl BinaryOp {
    /// Get the precedence of this operator (higher number = higher precedence)
    pub fn precedence(self) -> u8 {
        match self {
            BinaryOp::Or => 1,
            BinaryOp::And => 2,
            BinaryOp::Equal | BinaryOp::NotEqual => 3,
            BinaryOp::Less | BinaryOp::Greater | BinaryOp::LessEqual | BinaryOp::GreaterEqual => 4,
            BinaryOp::BitOr => 5,
            BinaryOp::BitXor => 6,
            BinaryOp::BitAnd => 7,
            BinaryOp::LeftShift | BinaryOp::RightShift => 8,
            BinaryOp::Add | BinaryOp::Subtract => 9,
            BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => 10,
            BinaryOp::Power => 11,
        }
    }
    
    /// Check if this operator is left-associative
    pub fn is_left_associative(self) -> bool {
        !matches!(self, BinaryOp::Power)
    }
    
    /// Check if this operator is comparison
    pub fn is_comparison(self) -> bool {
        matches!(self,
            BinaryOp::Equal | BinaryOp::NotEqual |
            BinaryOp::Less | BinaryOp::Greater |
            BinaryOp::LessEqual | BinaryOp::GreaterEqual
        )
    }
    
    /// Check if this operator is arithmetic
    pub fn is_arithmetic(self) -> bool {
        matches!(self,
            BinaryOp::Add | BinaryOp::Subtract |
            BinaryOp::Multiply | BinaryOp::Divide |
            BinaryOp::Modulo | BinaryOp::Power
        )
    }
    
    /// Check if this operator is logical
    pub fn is_logical(self) -> bool {
        matches!(self, BinaryOp::And | BinaryOp::Or)
    }
    
    /// Check if this operator is bitwise
    pub fn is_bitwise(self) -> bool {
        matches!(self,
            BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor |
            BinaryOp::LeftShift | BinaryOp::RightShift
        )
    }
}

impl From<TokenType> for BinaryOp {
    fn from(token: TokenType) -> Self {
        match token {
            TokenType::Plus => BinaryOp::Add,
            TokenType::Minus => BinaryOp::Subtract,
            TokenType::Star => BinaryOp::Multiply,
            TokenType::Slash => BinaryOp::Divide,
            TokenType::Percent => BinaryOp::Modulo,
            TokenType::Power => BinaryOp::Power,
            TokenType::Equal => BinaryOp::Equal,
            TokenType::NotEqual => BinaryOp::NotEqual,
            TokenType::Less => BinaryOp::Less,
            TokenType::Greater => BinaryOp::Greater,
            TokenType::LessEqual => BinaryOp::LessEqual,
            TokenType::GreaterEqual => BinaryOp::GreaterEqual,
            TokenType::And => BinaryOp::And,
            TokenType::OrOr => BinaryOp::Or,
            TokenType::Ampersand => BinaryOp::BitAnd,
            TokenType::Pipe => BinaryOp::BitOr,
            TokenType::Caret => BinaryOp::BitXor,
            TokenType::LeftShift => BinaryOp::LeftShift,
            TokenType::RightShift => BinaryOp::RightShift,
            _ => panic!("Invalid token type for binary operator: {:?}", token),
        }
    }
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
    BitNot,
}

impl From<TokenType> for UnaryOp {
    fn from(token: TokenType) -> Self {
        match token {
            TokenType::Plus => UnaryOp::Plus,
            TokenType::Minus => UnaryOp::Minus,
            TokenType::Not => UnaryOp::Not,
            TokenType::Tilde => UnaryOp::BitNot,
            _ => panic!("Invalid token type for unary operator: {:?}", token),
        }
    }
}

/// Field initialization in struct literals
#[derive(Debug, Clone, PartialEq)]
pub struct FieldInit {
    pub name: String,
    pub value: ExprNode,
}

/// Match expression arms
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<ExprNode>,
    pub body: ExprNode,
}

/// Patterns for match expressions and function parameters
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// Wildcard pattern (_)
    Wildcard,
    
    /// Identifier pattern (binds to variable)
    Identifier(String),
    
    /// Literal pattern
    Literal(Literal),
    
    /// Tuple pattern
    Tuple(Vec<Pattern>),
    
    /// Array pattern
    Array(Vec<Pattern>),
    
    /// Struct pattern
    Struct {
        name: String,
        fields: Vec<FieldPattern>,
        rest: bool, // true if there's a .. pattern
    },
    
    /// Enum pattern
    Enum {
        name: String,
        variant: String,
        fields: Vec<Pattern>,
    },
    
    /// Reference pattern
    Reference {
        mutable: bool,
        pattern: Box<Pattern>,
    },
    
    /// Or pattern (pattern1 | pattern2)
    Or(Vec<Pattern>),
    
    /// Range pattern (start..end or start..=end)
    Range {
        start: Box<Pattern>,
        end: Box<Pattern>,
        inclusive: bool,
    },
}

/// Field pattern in struct patterns
#[derive(Debug, Clone, PartialEq)]
pub struct FieldPattern {
    pub name: String,
    pub pattern: Pattern,
}

/// Function parameter for expressions (like lambda parameters)
#[derive(Debug, Clone, PartialEq)]
pub struct ExprParameter {
    pub pattern: Pattern,
    pub param_type: Option<TypeNode>,
    pub default: Option<ExprNode>,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            BinaryOp::Add => "+",
            BinaryOp::Subtract => "-",
            BinaryOp::Multiply => "*",
            BinaryOp::Divide => "/",
            BinaryOp::Modulo => "%",
            BinaryOp::Power => "**",
            BinaryOp::Equal => "==",
            BinaryOp::NotEqual => "!=",
            BinaryOp::Less => "<",
            BinaryOp::Greater => ">",
            BinaryOp::LessEqual => "<=",
            BinaryOp::GreaterEqual => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
            BinaryOp::BitAnd => "&",
            BinaryOp::BitOr => "|",
            BinaryOp::BitXor => "^",
            BinaryOp::LeftShift => "<<",
            BinaryOp::RightShift => ">>",
        };
        write!(f, "{}", s)
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            UnaryOp::Plus => "+",
            UnaryOp::Minus => "-",
            UnaryOp::Not => "!",
            UnaryOp::BitNot => "~",
        };
        write!(f, "{}", s)
    }
}