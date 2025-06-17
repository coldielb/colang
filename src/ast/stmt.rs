use crate::ast::{Node, TypeNode, ExprNode, expr::Pattern};

/// Statement nodes in the COLANG AST
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    /// Expression statements
    Expression(ExprNode),
    
    /// Variable declarations
    Let {
        pattern: Pattern,
        type_annotation: Option<TypeNode>,
        initializer: Option<ExprNode>,
        mutable: bool,
    },
    
    /// Constant declarations
    Const {
        name: String,
        type_annotation: Option<TypeNode>,
        initializer: ExprNode,
    },
    
    /// Static variable declarations
    Static {
        name: String,
        type_annotation: TypeNode,
        initializer: ExprNode,
        mutable: bool,
    },
    
    /// Assignment statements
    Assign {
        target: ExprNode,
        value: ExprNode,
    },
    
    /// Compound assignment statements
    CompoundAssign {
        target: ExprNode,
        operator: crate::ast::expr::BinaryOp,
        value: ExprNode,
    },
    
    /// If statements
    If {
        condition: ExprNode,
        then_block: Block,
        else_block: Option<Block>,
    },
    
    /// While loops
    While {
        condition: ExprNode,
        body: Block,
    },
    
    /// For loops
    For {
        pattern: Pattern,
        iterator: ExprNode,
        body: Block,
    },
    
    /// Match statements
    Match {
        expr: ExprNode,
        arms: Vec<crate::ast::expr::MatchArm>,
    },
    
    /// Break statement
    Break(Option<ExprNode>),
    
    /// Continue statement
    Continue,
    
    /// Return statement
    Return(Option<ExprNode>),
    
    /// Block statements
    Block(Block),
    
    /// Empty statement
    Empty,
}

pub type StatementNode = Node<Statement>;

/// Block of statements
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<StatementNode>,
    pub expression: Option<ExprNode>, // Optional trailing expression
}

impl Block {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
            expression: None,
        }
    }
    
    pub fn with_statements(statements: Vec<StatementNode>) -> Self {
        Self {
            statements,
            expression: None,
        }
    }
    
    pub fn with_expression(mut self, expression: ExprNode) -> Self {
        self.expression = Some(expression);
        self
    }
    
    pub fn is_empty(&self) -> bool {
        self.statements.is_empty() && self.expression.is_none()
    }
    
    pub fn len(&self) -> usize {
        self.statements.len() + if self.expression.is_some() { 1 } else { 0 }
    }
}

impl Default for Block {
    fn default() -> Self {
        Self::new()
    }
}

impl From<Vec<StatementNode>> for Block {
    fn from(statements: Vec<StatementNode>) -> Self {
        Self::with_statements(statements)
    }
}