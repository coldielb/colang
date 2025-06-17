use crate::util::Span;
use std::fmt;

/// Node identifier for referencing AST nodes
pub type NodeId = u32;

/// Base trait for all AST nodes
pub trait AstNode: fmt::Debug + Clone {
    fn span(&self) -> Span;
    fn node_id(&self) -> NodeId;
}

/// Wrapper for AST nodes that adds span and node ID information
#[derive(Debug, Clone, PartialEq)]
pub struct Node<T> {
    pub inner: T,
    pub span: Span,
    pub id: NodeId,
}

impl<T> Node<T> {
    pub fn new(inner: T, span: Span, id: NodeId) -> Self {
        Self { inner, span, id }
    }
    
    pub fn dummy(inner: T) -> Self {
        Self {
            inner,
            span: Span::dummy(),
            id: 0,
        }
    }
    
    pub fn map<U, F>(self, f: F) -> Node<U>
    where
        F: FnOnce(T) -> U,
    {
        Node {
            inner: f(self.inner),
            span: self.span,
            id: self.id,
        }
    }
    
    pub fn as_ref(&self) -> Node<&T> {
        Node {
            inner: &self.inner,
            span: self.span,
            id: self.id,
        }
    }
}

impl<T> AstNode for Node<T>
where
    T: fmt::Debug + Clone,
{
    fn span(&self) -> Span {
        self.span
    }
    
    fn node_id(&self) -> NodeId {
        self.id
    }
}

impl<T> std::ops::Deref for Node<T> {
    type Target = T;
    
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> std::ops::DerefMut for Node<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

/// Node generator for creating unique node IDs
#[derive(Debug, Default)]
pub struct NodeIdGenerator {
    next_id: NodeId,
}

impl NodeIdGenerator {
    pub fn new() -> Self {
        Self { next_id: 1 }
    }
    
    pub fn next(&mut self) -> NodeId {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
    
    pub fn create_node<T>(&mut self, inner: T, span: Span) -> Node<T> {
        Node::new(inner, span, self.next())
    }
}