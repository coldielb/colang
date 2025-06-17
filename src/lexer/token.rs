use logos::Logos;
use std::fmt;
use crate::util::Span;

/// Token types for COLANG lexical analysis
#[derive(Logos, Debug, Clone, PartialEq)]
pub enum TokenType {
    // Keywords
    #[token("own")]
    Own,
    #[token("ref")]
    Ref,
    #[token("mut")]
    Mut,
    #[token("shared")]
    Shared,
    #[token("or")]
    Or,
    #[token("return")]
    Return,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("fn")]
    Fn,
    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,
    #[token("trait")]
    Trait,
    #[token("impl")]
    Impl,
    #[token("use")]
    Use,
    #[token("pub")]
    Pub,
    #[token("let")]
    Let,
    #[token("const")]
    Const,
    #[token("static")]
    Static,
    #[token("match")]
    Match,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("unsafe")]
    Unsafe,
    #[token("extern")]
    Extern,
    #[token("mod")]
    Mod,
    #[token("type")]
    Type,
    #[token("where")]
    Where,
    #[token("as")]
    As,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("null")]
    Null,
    
    // Operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("**")]
    Power,
    
    // Comparison
    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("<")]
    Less,
    #[token(">")]
    Greater,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,
    
    // Logical
    #[token("&&")]
    And,
    #[token("||")]
    OrOr,
    #[token("!")]
    Not,
    
    // Bitwise
    #[token("&")]
    Ampersand,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,
    #[token("<<")]
    LeftShift,
    #[token(">>")]
    RightShift,
    #[token("~")]
    Tilde,
    
    // Assignment
    #[token("=")]
    Assign,
    #[token("+=")]
    PlusAssign,
    #[token("-=")]
    MinusAssign,
    #[token("*=")]
    StarAssign,
    #[token("/=")]
    SlashAssign,
    #[token("%=")]
    PercentAssign,
    #[token("&=")]
    AndAssign,
    #[token("|=")]
    OrAssign,
    #[token("^=")]
    XorAssign,
    #[token("<<=")]
    LeftShiftAssign,
    #[token(">>=")]
    RightShiftAssign,
    
    // Type inference
    #[token(":=")]
    ColonAssign,
    
    // Delimiters
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    
    // Punctuation
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("..")]
    DotDot,
    #[token("...")]
    DotDotDot,
    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,
    #[token("?")]
    Question,
    #[token("@")]
    At,
    #[token("#")]
    Hash,
    #[token("$")]
    Dollar,
    
    // Literals
    #[regex(r"[0-9]+", |lex| lex.slice().parse::<i64>().ok())]
    IntegerLiteral(i64),
    
    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse::<f64>().ok())]
    FloatLiteral(f64),
    
    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice().to_owned())]
    StringLiteral(String),
    
    #[regex(r"'([^'\\]|\\.)'", |lex| lex.slice().chars().nth(1))]
    CharLiteral(char),
    
    // Identifiers
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_owned())]
    Identifier(String),
    
    // Whitespace and comments (ignored)
    #[regex(r"[ \t\r\n\f]+", logos::skip)]
    #[regex(r"//[^\r\n]*", logos::skip)]
    #[regex(r"/\*([^*]|\*[^/])*\*/", logos::skip)]
    
    // Error handling (default for unrecognized tokens)
    Error,
    
    // End of file
    Eof,
}

/// A token with position information
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
    pub lexeme: String,
}

impl Token {
    pub fn new(token_type: TokenType, span: Span, lexeme: String) -> Self {
        Self {
            token_type,
            span,
            lexeme,
        }
    }
    
    pub fn eof(position: usize) -> Self {
        Self {
            token_type: TokenType::Eof,
            span: Span::new(position, position, 0, 0),
            lexeme: String::new(),
        }
    }
    
    pub fn is_eof(&self) -> bool {
        matches!(self.token_type, TokenType::Eof)
    }
    
    pub fn is_keyword(&self) -> bool {
        matches!(self.token_type, 
            TokenType::Own | TokenType::Ref | TokenType::Mut | TokenType::Shared |
            TokenType::Or | TokenType::Return | TokenType::If | TokenType::Else |
            TokenType::While | TokenType::For | TokenType::In | TokenType::Fn |
            TokenType::Struct | TokenType::Enum | TokenType::Trait | TokenType::Impl |
            TokenType::Use | TokenType::Pub | TokenType::Let | TokenType::Const |
            TokenType::Static | TokenType::Match | TokenType::Break | TokenType::Continue |
            TokenType::Unsafe | TokenType::Extern | TokenType::Mod | TokenType::Type |
            TokenType::Where | TokenType::As | TokenType::True | TokenType::False |
            TokenType::Null
        )
    }
    
    pub fn is_literal(&self) -> bool {
        matches!(self.token_type,
            TokenType::IntegerLiteral(_) | TokenType::FloatLiteral(_) |
            TokenType::StringLiteral(_) | TokenType::CharLiteral(_) |
            TokenType::True | TokenType::False | TokenType::Null
        )
    }
    
    pub fn is_operator(&self) -> bool {
        matches!(self.token_type,
            TokenType::Plus | TokenType::Minus | TokenType::Star | TokenType::Slash |
            TokenType::Percent | TokenType::Power | TokenType::Equal | TokenType::NotEqual |
            TokenType::Less | TokenType::Greater | TokenType::LessEqual | TokenType::GreaterEqual |
            TokenType::And | TokenType::OrOr | TokenType::Not | TokenType::Ampersand |
            TokenType::Pipe | TokenType::Caret | TokenType::LeftShift | TokenType::RightShift |
            TokenType::Tilde
        )
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}({})", self.token_type, self.lexeme)
    }
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::Own => write!(f, "own"),
            TokenType::Ref => write!(f, "ref"),
            TokenType::Mut => write!(f, "mut"),
            TokenType::Shared => write!(f, "shared"),
            TokenType::Or => write!(f, "or"),
            TokenType::Return => write!(f, "return"),
            TokenType::If => write!(f, "if"),
            TokenType::Else => write!(f, "else"),
            TokenType::While => write!(f, "while"),
            TokenType::For => write!(f, "for"),
            TokenType::In => write!(f, "in"),
            TokenType::Fn => write!(f, "fn"),
            TokenType::Struct => write!(f, "struct"),
            TokenType::Enum => write!(f, "enum"),
            TokenType::Trait => write!(f, "trait"),
            TokenType::Impl => write!(f, "impl"),
            TokenType::Use => write!(f, "use"),
            TokenType::Pub => write!(f, "pub"),
            TokenType::Let => write!(f, "let"),
            TokenType::Const => write!(f, "const"),
            TokenType::Static => write!(f, "static"),
            TokenType::Match => write!(f, "match"),
            TokenType::Break => write!(f, "break"),
            TokenType::Continue => write!(f, "continue"),
            TokenType::Unsafe => write!(f, "unsafe"),
            TokenType::Extern => write!(f, "extern"),
            TokenType::Mod => write!(f, "mod"),
            TokenType::Type => write!(f, "type"),
            TokenType::Where => write!(f, "where"),
            TokenType::As => write!(f, "as"),
            TokenType::True => write!(f, "true"),
            TokenType::False => write!(f, "false"),
            TokenType::Null => write!(f, "null"),
            TokenType::Identifier(name) => write!(f, "{}", name),
            TokenType::IntegerLiteral(val) => write!(f, "{}", val),
            TokenType::FloatLiteral(val) => write!(f, "{}", val),
            TokenType::StringLiteral(val) => write!(f, "\"{}\"", val),
            TokenType::CharLiteral(val) => write!(f, "'{}'", val),
            TokenType::Eof => write!(f, "EOF"),
            TokenType::Error => write!(f, "ERROR"),
            _ => write!(f, "{:?}", self),
        }
    }
}