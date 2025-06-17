/// Utility functions and shared components for the COLANG compiler
/// 
/// This module provides common functionality used across multiple compiler phases,
/// including error handling, source position tracking, and diagnostic formatting.
/// 
/// Utilities include:
/// - Source position and span tracking
/// - Error reporting and diagnostic formatting
/// - String interning for efficient identifier handling
/// - Common data structures and algorithms
/// - Debugging and profiling helpers

pub mod span;
pub mod error;
pub mod intern;
pub mod diagnostic;
pub mod debug;

pub use span::*;
pub use error::*;
pub use intern::*;
pub use diagnostic::*;
pub use debug::*;