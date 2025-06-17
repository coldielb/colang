use std::fmt;
use thiserror::Error;
use crate::util::Span;

/// Base error type for all compiler errors
#[derive(Error, Debug, Clone)]
pub enum CompilerError {
    #[error("Lexical error: {0}")]
    Lexer(#[from] crate::lexer::LexError),
    
    #[error("Parse error: {0}")]
    Parser(#[from] ParseError),
    
    #[error("Semantic error: {0}")]
    Semantic(#[from] SemanticError),
    
    #[error("Code generation error: {0}")]
    Codegen(#[from] CodegenError),
    
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),
    
    #[error("Internal compiler error: {message}")]
    Internal { message: String },
}

/// Parse errors
#[derive(Error, Debug, Clone)]
pub enum ParseError {
    #[error("Unexpected token '{found}' at {span}, expected {expected}")]
    UnexpectedToken {
        found: String,
        expected: String,
        span: Span,
    },
    
    #[error("Unexpected end of file, expected {expected}")]
    UnexpectedEof { expected: String },
    
    #[error("Invalid syntax at {span}: {message}")]
    InvalidSyntax { message: String, span: Span },
    
    #[error("Missing token '{token}' at {span}")]
    MissingToken { token: String, span: Span },
    
    #[error("Duplicate {item} '{name}' at {span}")]
    Duplicate {
        item: String,
        name: String,
        span: Span,
    },
}

/// Semantic analysis errors
#[derive(Error, Debug, Clone)]
pub enum SemanticError {
    #[error("Undefined variable '{name}' at {span}")]
    UndefinedVariable { name: String, span: Span },
    
    #[error("Undefined function '{name}' at {span}")]
    UndefinedFunction { name: String, span: Span },
    
    #[error("Undefined type '{name}' at {span}")]
    UndefinedType { name: String, span: Span },
    
    #[error("Type mismatch at {span}: expected {expected}, found {found}")]
    TypeMismatch {
        expected: String,
        found: String,
        span: Span,
    },
    
    #[error("Cannot move out of {kind} at {span}")]
    CannotMove { kind: String, span: Span },
    
    #[error("Use of moved value '{name}' at {span}")]
    UseAfterMove { name: String, span: Span },
    
    #[error("Cannot borrow {name} as {borrow_kind} at {span}")]
    BorrowError {
        name: String,
        borrow_kind: String,
        span: Span,
    },
    
    #[error("Mismatched argument count at {span}: expected {expected}, found {found}")]
    ArgumentCountMismatch {
        expected: usize,
        found: usize,
        span: Span,
    },
    
    #[error("Cannot access private item '{name}' at {span}")]
    PrivateAccess { name: String, span: Span },
    
    #[error("Trait '{trait_name}' not implemented for type '{type_name}' at {span}")]
    TraitNotImplemented {
        trait_name: String,
        type_name: String,
        span: Span,
    },
    
    #[error("Cyclic dependency detected involving '{name}' at {span}")]
    CyclicDependency { name: String, span: Span },
    
    #[error("Invalid ownership operation at {span}: {message}")]
    InvalidOwnership { message: String, span: Span },
}

/// Code generation errors
#[derive(Error, Debug, Clone)]
pub enum CodegenError {
    #[error("Backend error: {message}")]
    Backend { message: String },
    
    #[error("Unsupported target: {target}")]
    UnsupportedTarget { target: String },
    
    #[error("Link error: {message}")]
    Link { message: String },
    
    #[error("Optimization error: {message}")]
    Optimization { message: String },
}

/// Diagnostic level for error reporting
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Note,
    Help,
}

/// A diagnostic message with span information
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub level: DiagnosticLevel,
    pub message: String,
    pub span: Option<Span>,
    pub notes: Vec<String>,
    pub help: Option<String>,
}

impl Diagnostic {
    pub fn error(message: String, span: Option<Span>) -> Self {
        Self {
            level: DiagnosticLevel::Error,
            message,
            span,
            notes: Vec::new(),
            help: None,
        }
    }
    
    pub fn warning(message: String, span: Option<Span>) -> Self {
        Self {
            level: DiagnosticLevel::Warning,
            message,
            span,
            notes: Vec::new(),
            help: None,
        }
    }
    
    pub fn note(message: String, span: Option<Span>) -> Self {
        Self {
            level: DiagnosticLevel::Note,
            message,
            span,
            notes: Vec::new(),
            help: None,
        }
    }
    
    pub fn with_note(mut self, note: String) -> Self {
        self.notes.push(note);
        self
    }
    
    pub fn with_help(mut self, help: String) -> Self {
        self.help = Some(help);
        self
    }
}

impl fmt::Display for DiagnosticLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagnosticLevel::Error => write!(f, "error"),
            DiagnosticLevel::Warning => write!(f, "warning"),
            DiagnosticLevel::Note => write!(f, "note"),
            DiagnosticLevel::Help => write!(f, "help"),
        }
    }
}

/// Result type for compiler operations
pub type CompilerResult<T> = Result<T, CompilerError>;

/// Collect multiple diagnostics
#[derive(Debug, Default)]
pub struct DiagnosticCollector {
    pub diagnostics: Vec<Diagnostic>,
}

impl DiagnosticCollector {
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }
    
    pub fn error(&mut self, message: String, span: Option<Span>) {
        self.diagnostics.push(Diagnostic::error(message, span));
    }
    
    pub fn warning(&mut self, message: String, span: Option<Span>) {
        self.diagnostics.push(Diagnostic::warning(message, span));
    }
    
    pub fn note(&mut self, message: String, span: Option<Span>) {
        self.diagnostics.push(Diagnostic::note(message, span));
    }
    
    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| matches!(d.level, DiagnosticLevel::Error))
    }
    
    pub fn error_count(&self) -> usize {
        self.diagnostics.iter().filter(|d| matches!(d.level, DiagnosticLevel::Error)).count()
    }
    
    pub fn warning_count(&self) -> usize {
        self.diagnostics.iter().filter(|d| matches!(d.level, DiagnosticLevel::Warning)).count()
    }
    
    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }
    
    pub fn len(&self) -> usize {
        self.diagnostics.len()
    }
    
    pub fn clear(&mut self) {
        self.diagnostics.clear();
    }
}