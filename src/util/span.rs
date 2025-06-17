use std::fmt;

/// Represents a location in source code with line and column information
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: u32,
    pub column: u32,
}

impl Span {
    pub fn new(start: usize, end: usize, line: u32, column: u32) -> Self {
        Self { start, end, line, column }
    }
    
    pub fn dummy() -> Self {
        Self { start: 0, end: 0, line: 0, column: 0 }
    }
    
    pub fn len(&self) -> usize {
        self.end - self.start
    }
    
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }
    
    /// Combine two spans into a span that covers both
    pub fn to(&self, other: Span) -> Span {
        Span::new(
            self.start.min(other.start),
            self.end.max(other.end),
            self.line.min(other.line),
            self.column.min(other.column),
        )
    }
    
    /// Check if this span contains another span
    pub fn contains(&self, other: Span) -> bool {
        self.start <= other.start && other.end <= self.end
    }
    
    /// Check if this span overlaps with another span
    pub fn overlaps(&self, other: Span) -> bool {
        self.start < other.end && other.start < self.end
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl Default for Span {
    fn default() -> Self {
        Self::dummy()
    }
}