use crate::ast::{Node, TypeNode, ExprNode, stmt::Block, expr::Pattern, types::GenericParam};
use crate::util::Span;

/// Declaration nodes in the COLANG AST
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    /// Function declarations
    Function {
        name: String,
        generic_params: Vec<GenericParam>,
        parameters: Vec<Parameter>,
        return_type: Option<TypeNode>,
        body: Option<Block>,
        visibility: Visibility,
        is_unsafe: bool,
        is_extern: bool,
        abi: Option<String>,
    },
    
    /// Struct declarations
    Struct {
        name: String,
        generic_params: Vec<GenericParam>,
        fields: Vec<Field>,
        visibility: Visibility,
    },
    
    /// Enum declarations
    Enum {
        name: String,
        generic_params: Vec<GenericParam>,
        variants: Vec<EnumVariant>,
        visibility: Visibility,
    },
    
    /// Trait declarations
    Trait {
        name: String,
        generic_params: Vec<GenericParam>,
        supertraits: Vec<TypeNode>,
        items: Vec<TraitItem>,
        visibility: Visibility,
    },
    
    /// Implementation blocks
    Impl {
        generic_params: Vec<GenericParam>,
        trait_ref: Option<TypeNode>, // None for inherent impl
        self_type: TypeNode,
        items: Vec<ImplItem>,
        where_clause: Option<WhereClause>,
    },
    
    /// Type alias declarations
    TypeAlias {
        name: String,
        generic_params: Vec<GenericParam>,
        type_def: TypeNode,
        visibility: Visibility,
    },
    
    /// Constant declarations
    Const {
        name: String,
        type_annotation: TypeNode,
        value: ExprNode,
        visibility: Visibility,
    },
    
    /// Static variable declarations
    Static {
        name: String,
        type_annotation: TypeNode,
        value: ExprNode,
        mutable: bool,
        visibility: Visibility,
    },
    
    /// Module declarations
    Module {
        name: String,
        items: Vec<Item>,
        visibility: Visibility,
    },
    
    /// Use declarations (imports)
    Use {
        path: UsePath,
        visibility: Visibility,
    },
    
    /// Extern block declarations
    ExternBlock {
        abi: Option<String>,
        items: Vec<ExternItem>,
    },
}

pub type DeclarationNode = Node<Declaration>;

/// Top-level items
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Declaration(DeclarationNode),
    // Can be extended with other item types
}

pub type ItemNode = Node<Item>;

/// Visibility modifiers
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
    Restricted(/* TODO: path restrictions */),
}

impl Default for Visibility {
    fn default() -> Self {
        Visibility::Private
    }
}

/// Function parameter
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub pattern: Pattern,
    pub param_type: TypeNode,
    pub default: Option<ExprNode>,
    pub span: Span,
}

/// Struct field
#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub field_type: TypeNode,
    pub visibility: Visibility,
    pub span: Span,
}

/// Enum variant
#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub fields: VariantFields,
    pub discriminant: Option<ExprNode>,
    pub span: Span,
}

/// Enum variant fields
#[derive(Debug, Clone, PartialEq)]
pub enum VariantFields {
    /// Unit variant (no fields)
    Unit,
    /// Tuple variant
    Tuple(Vec<TypeNode>),
    /// Struct variant
    Struct(Vec<Field>),
}

/// Trait items
#[derive(Debug, Clone, PartialEq)]
pub enum TraitItem {
    /// Associated function
    Function {
        name: String,
        generic_params: Vec<GenericParam>,
        parameters: Vec<Parameter>,
        return_type: Option<TypeNode>,
        default_body: Option<Block>,
    },
    
    /// Associated type
    Type {
        name: String,
        generic_params: Vec<GenericParam>,
        bounds: Vec<TypeNode>,
        default: Option<TypeNode>,
    },
    
    /// Associated constant
    Const {
        name: String,
        const_type: TypeNode,
        default: Option<ExprNode>,
    },
}

/// Implementation items
#[derive(Debug, Clone, PartialEq)]
pub enum ImplItem {
    /// Function implementation
    Function {
        name: String,
        generic_params: Vec<GenericParam>,
        parameters: Vec<Parameter>,
        return_type: Option<TypeNode>,
        body: Block,
        visibility: Visibility,
        is_unsafe: bool,
    },
    
    /// Type implementation
    Type {
        name: String,
        generic_params: Vec<GenericParam>,
        type_def: TypeNode,
        visibility: Visibility,
    },
    
    /// Constant implementation
    Const {
        name: String,
        const_type: TypeNode,
        value: ExprNode,
        visibility: Visibility,
    },
}

/// Where clause for generic constraints
#[derive(Debug, Clone, PartialEq)]
pub struct WhereClause {
    pub predicates: Vec<WherePredicate>,
}

/// Where clause predicates
#[derive(Debug, Clone, PartialEq)]
pub enum WherePredicate {
    /// Type bound (T: Trait)
    TypeBound {
        bounded_type: TypeNode,
        bounds: Vec<TypeNode>,
    },
    
    /// Lifetime bound
    LifetimeBound {
        lifetime: String,
        bounds: Vec<String>,
    },
    
    /// Equality constraint (Associated = Type)
    Equality {
        lhs: TypeNode,
        rhs: TypeNode,
    },
}

/// Use declaration paths
#[derive(Debug, Clone, PartialEq)]
pub enum UsePath {
    /// Simple identifier
    Ident(String),
    
    /// Path with segments
    Path {
        segments: Vec<String>,
        alias: Option<String>,
    },
    
    /// Glob import
    Glob(Vec<String>),
    
    /// Multiple imports
    List {
        prefix: Vec<String>,
        items: Vec<UsePath>,
    },
}

/// External block items
#[derive(Debug, Clone, PartialEq)]
pub enum ExternItem {
    /// External function
    Function {
        name: String,
        parameters: Vec<Parameter>,
        return_type: Option<TypeNode>,
        variadic: bool,
    },
    
    /// External static
    Static {
        name: String,
        static_type: TypeNode,
        mutable: bool,
    },
    
    /// External type
    Type {
        name: String,
    },
}

impl Visibility {
    pub fn is_public(&self) -> bool {
        matches!(self, Visibility::Public)
    }
    
    pub fn is_private(&self) -> bool {
        matches!(self, Visibility::Private)
    }
}

impl VariantFields {
    pub fn is_unit(&self) -> bool {
        matches!(self, VariantFields::Unit)
    }
    
    pub fn is_tuple(&self) -> bool {
        matches!(self, VariantFields::Tuple(_))
    }
    
    pub fn is_struct(&self) -> bool {
        matches!(self, VariantFields::Struct(_))
    }
    
    pub fn field_count(&self) -> usize {
        match self {
            VariantFields::Unit => 0,
            VariantFields::Tuple(fields) => fields.len(),
            VariantFields::Struct(fields) => fields.len(),
        }
    }
}