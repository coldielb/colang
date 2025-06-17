/// COLANG Standard Library
/// 
/// This module contains the standard library implementation for COLANG.
/// The standard library is designed to be minimal yet complete, providing
/// essential functionality while maintaining the language's zero-cost philosophy.
/// 
/// Core modules:
/// - Memory management utilities (alloc, smart pointers)
/// - Collections (Vec, HashMap, etc.)
/// - I/O and file system operations
/// - String manipulation and text processing
/// - Concurrency primitives and thread-safe collections
/// - Result and Option types for error handling

pub mod prelude;
pub mod memory;
pub mod collections;
pub mod io;
pub mod string;
pub mod thread;
pub mod result;

pub use prelude::*;