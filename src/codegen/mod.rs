/// Code generation backend for COLANG
/// 
/// This module generates target machine code from the COLANG IR.
/// Initially targeting LLVM IR for maximum portability and optimization,
/// with provisions for additional backends in the future.
/// 
/// Code generation features:
/// - LLVM IR emission with debug information
/// - Optimized ownership transfer (zero-cost where possible)
/// - Native calling convention support for C interoperability
/// - Exception handling for error propagation
/// - Platform-specific optimizations

pub mod llvm;
pub mod target;
pub mod debug;

pub use llvm::*;
pub use target::*;
pub use debug::*;