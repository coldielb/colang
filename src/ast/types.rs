use crate::ast::{Node, NodeId};
use crate::util::Span;
use std::fmt;

/// Type representations in COLANG
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Primitive types
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    Bool,
    Char,
    Str,
    
    /// Unit type (empty tuple)
    Unit,
    
    /// Never type (for functions that never return)
    Never,
    
    /// Pointer types with ownership semantics
    Own(Box<TypeNode>),
    Ref(Box<TypeNode>),
    MutRef(Box<TypeNode>),
    Shared(Box<TypeNode>),
    
    /// Array types
    Array(Box<TypeNode>, Option<u64>), // [T; N] or [T]
    
    /// Tuple types
    Tuple(Vec<TypeNode>),
    
    /// Function types
    Function {
        params: Vec<TypeNode>,
        return_type: Box<TypeNode>,
    },
    
    /// User-defined types
    Named {
        name: String,
        generic_args: Vec<TypeNode>,
    },
    
    /// Generic type parameters
    Generic(String),
    
    /// Type to be inferred
    Inferred,
    
    /// Error type (for error recovery)
    Error,
}

pub type TypeNode = Node<Type>;

impl Type {
    /// Check if this type is a primitive type
    pub fn is_primitive(&self) -> bool {
        matches!(self,
            Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::I128 |
            Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::U128 |
            Type::F32 | Type::F64 | Type::Bool | Type::Char | Type::Str
        )
    }
    
    /// Check if this type is an integer type
    pub fn is_integer(&self) -> bool {
        matches!(self,
            Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::I128 |
            Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::U128
        )
    }
    
    /// Check if this type is a signed integer type
    pub fn is_signed_integer(&self) -> bool {
        matches!(self, Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::I128)
    }
    
    /// Check if this type is an unsigned integer type
    pub fn is_unsigned_integer(&self) -> bool {
        matches!(self, Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::U128)
    }
    
    /// Check if this type is a floating-point type
    pub fn is_float(&self) -> bool {
        matches!(self, Type::F32 | Type::F64)
    }
    
    /// Check if this type is numeric (integer or float)
    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }
    
    /// Check if this type has ownership semantics
    pub fn has_ownership(&self) -> bool {
        matches!(self, Type::Own(_) | Type::Ref(_) | Type::MutRef(_) | Type::Shared(_))
    }
    
    /// Get the underlying type if this is an ownership type
    pub fn underlying_type(&self) -> Option<&TypeNode> {
        match self {
            Type::Own(t) | Type::Ref(t) | Type::MutRef(t) | Type::Shared(t) => Some(t),
            _ => None,
        }
    }
    
    /// Check if this type can be moved (not a reference)
    pub fn is_movable(&self) -> bool {
        !matches!(self, Type::Ref(_) | Type::MutRef(_))
    }
    
    /// Check if this type is mutable
    pub fn is_mutable(&self) -> bool {
        matches!(self, Type::MutRef(_) | Type::Own(_))
    }
    
    /// Get the size in bytes for primitive types
    pub fn size_in_bytes(&self) -> Option<usize> {
        match self {
            Type::I8 | Type::U8 | Type::Bool => Some(1),
            Type::I16 | Type::U16 => Some(2),
            Type::I32 | Type::U32 | Type::F32 | Type::Char => Some(4),
            Type::I64 | Type::U64 | Type::F64 => Some(8),
            Type::I128 | Type::U128 => Some(16),
            Type::Unit => Some(0),
            _ => None, // Size depends on target architecture or is unknown
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I8 => write!(f, "i8"),
            Type::I16 => write!(f, "i16"),
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::I128 => write!(f, "i128"),
            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            Type::U128 => write!(f, "u128"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
            Type::Str => write!(f, "str"),
            Type::Unit => write!(f, "()"),
            Type::Never => write!(f, "!"),
            Type::Own(t) => write!(f, "own {}", t.inner),
            Type::Ref(t) => write!(f, "ref {}", t.inner),
            Type::MutRef(t) => write!(f, "mut ref {}", t.inner),
            Type::Shared(t) => write!(f, "shared {}", t.inner),
            Type::Array(t, Some(size)) => write!(f, "[{}; {}]", t.inner, size),
            Type::Array(t, None) => write!(f, "[{}]", t.inner),
            Type::Tuple(types) => {
                write!(f, "(")?;
                for (i, t) in types.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", t.inner)?;
                }
                write!(f, ")")
            },
            Type::Function { params, return_type } => {
                write!(f, "fn(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", param.inner)?;
                }
                write!(f, ") -> {}", return_type.inner)
            },
            Type::Named { name, generic_args } => {
                write!(f, "{}", name)?;
                if !generic_args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in generic_args.iter().enumerate() {
                        if i > 0 { write!(f, ", ")?; }
                        write!(f, "{}", arg.inner)?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            },
            Type::Generic(name) => write!(f, "{}", name),
            Type::Inferred => write!(f, "_"),
            Type::Error => write!(f, "<??>"),
        }
    }
}

/// Type constraint for generic parameters
#[derive(Debug, Clone, PartialEq)]
pub enum TypeConstraint {
    /// Must implement a specific trait
    Trait(String),
    
    /// Must be a specific type
    Exact(TypeNode),
    
    /// Must satisfy multiple constraints
    Multiple(Vec<TypeConstraint>),
    
    /// Lifetime constraint
    Lifetime(String),
}

/// Generic parameter definition
#[derive(Debug, Clone, PartialEq)]
pub struct GenericParam {
    pub name: String,
    pub constraints: Vec<TypeConstraint>,
    pub default: Option<TypeNode>,
    pub span: Span,
}

impl GenericParam {
    pub fn new(name: String, span: Span) -> Self {
        Self {
            name,
            constraints: Vec::new(),
            default: None,
            span,
        }
    }
    
    pub fn with_constraint(mut self, constraint: TypeConstraint) -> Self {
        self.constraints.push(constraint);
        self
    }
    
    pub fn with_default(mut self, default: TypeNode) -> Self {
        self.default = Some(default);
        self
    }
}