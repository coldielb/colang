/// Target-specific code generation for COLANG
/// 
/// This module handles binary generation from LLVM IR using LLVM tools.
/// It provides a backend-agnostic interface for generating executables.

use std::path::{Path, PathBuf};
use std::process::Command;
use std::fs;

/// Binary generation errors
#[derive(Debug, Clone)]
pub enum BinaryGenError {
    LlvmToolsNotFound { tool: String },
    CompilationFailed { message: String, stderr: String },
    IoError { message: String },
    UnsupportedTarget { target: String },
}

impl std::fmt::Display for BinaryGenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryGenError::LlvmToolsNotFound { tool } => {
                write!(f, "LLVM tool '{}' not found. Please install LLVM tools.", tool)
            },
            BinaryGenError::CompilationFailed { message, stderr } => {
                write!(f, "Compilation failed: {}\nStderr: {}", message, stderr)
            },
            BinaryGenError::IoError { message } => {
                write!(f, "I/O error: {}", message)
            },
            BinaryGenError::UnsupportedTarget { target } => {
                write!(f, "Unsupported target: {}", target)
            },
        }
    }
}

impl std::error::Error for BinaryGenError {}

/// Binary generator that uses LLVM tools
pub struct BinaryGenerator {
    /// Target triple (e.g., "x86_64-unknown-linux-gnu")
    target_triple: String,
    /// Optimization level (0-3)
    optimization_level: u8,
    /// Whether to include debug information
    debug_info: bool,
}

impl BinaryGenerator {
    /// Create a new binary generator
    pub fn new() -> Self {
        Self {
            target_triple: default_target_triple(),
            optimization_level: 0,
            debug_info: false,
        }
    }
    
    /// Set the target triple
    pub fn with_target(mut self, target: String) -> Self {
        self.target_triple = target;
        self
    }
    
    /// Set optimization level
    pub fn with_optimization(mut self, level: u8) -> Self {
        self.optimization_level = level;
        self
    }
    
    /// Enable debug information
    pub fn with_debug_info(mut self, debug: bool) -> Self {
        self.debug_info = debug;
        self
    }
    
    /// Generate binary from LLVM IR
    pub fn generate_binary(
        &self,
        llvm_ir: &str,
        output_path: &Path,
    ) -> Result<(), BinaryGenError> {
        // Write LLVM IR to temporary file
        let temp_dir = std::env::temp_dir();
        let temp_ll = temp_dir.join(format!("colang_{}.ll", std::process::id()));
        let temp_o = temp_dir.join(format!("colang_{}.o", std::process::id()));
        
        fs::write(&temp_ll, llvm_ir)
            .map_err(|e| BinaryGenError::IoError {
                message: format!("Failed to write temporary LLVM IR file: {}", e),
            })?;
        
        // Find llc executable (try homebrew LLVM@16 first, then system PATH)
        let llc_path = Self::find_llc_executable()?;
        
        // Compile LLVM IR to object file using llc
        let llc_result = Command::new(&llc_path)
            .arg("-filetype=obj")
            .arg(format!("-O{}", self.optimization_level))
            .arg("--relocation-model=pic")
            .arg(&temp_ll)
            .arg("-o")
            .arg(&temp_o)
            .output();
        
        let llc_output = llc_result.map_err(|_| BinaryGenError::LlvmToolsNotFound {
            tool: "llc".to_string(),
        })?;
        
        if !llc_output.status.success() {
            let stderr = String::from_utf8_lossy(&llc_output.stderr);
            return Err(BinaryGenError::CompilationFailed {
                message: "LLC compilation failed".to_string(),
                stderr: stderr.to_string(),
            });
        }
        
        // Link object file to executable using clang or gcc
        let linker_result = self.link_executable(&temp_o, output_path);
        
        // Clean up temporary files
        let _ = fs::remove_file(&temp_ll);
        let _ = fs::remove_file(&temp_o);
        
        linker_result
    }
    
    /// Link object file to executable
    fn link_executable(&self, object_path: &Path, output_path: &Path) -> Result<(), BinaryGenError> {
        // Try clang first, then gcc as fallback
        let linkers = ["clang", "gcc"];
        
        for linker in &linkers {
            let mut cmd = Command::new(linker);
            cmd.arg(object_path)
                .arg("-o")
                .arg(output_path);
            
            // Add optimization flags
            if self.optimization_level > 0 {
                cmd.arg(format!("-O{}", self.optimization_level));
            }
            
            // Add debug info if requested
            if self.debug_info {
                cmd.arg("-g");
            }
            
            // Add target triple if not default
            if self.target_triple != default_target_triple() {
                cmd.arg(format!("--target={}", self.target_triple));
            }
            
            match cmd.output() {
                Ok(output) => {
                    if output.status.success() {
                        return Ok(());
                    }
                    
                    let stderr = String::from_utf8_lossy(&output.stderr);
                    return Err(BinaryGenError::CompilationFailed {
                        message: format!("Linking with {} failed", linker),
                        stderr: stderr.to_string(),
                    });
                },
                Err(_) => {
                    // Try next linker
                    continue;
                }
            }
        }
        
        Err(BinaryGenError::LlvmToolsNotFound {
            tool: "clang or gcc".to_string(),
        })
    }
    
    /// Find llc executable in homebrew LLVM@16 or system PATH
    fn find_llc_executable() -> Result<PathBuf, BinaryGenError> {
        // Try homebrew LLVM@16 first (common on macOS)
        let homebrew_paths = [
            "/opt/homebrew/opt/llvm@16/bin/llc",  // Apple Silicon
            "/usr/local/opt/llvm@16/bin/llc",     // Intel Mac
        ];
        
        for path in &homebrew_paths {
            if Path::new(path).exists() {
                return Ok(PathBuf::from(path));
            }
        }
        
        // Check if llc is in system PATH
        if Command::new("llc").arg("--version").output().is_ok() {
            return Ok(PathBuf::from("llc"));
        }
        
        Err(BinaryGenError::LlvmToolsNotFound {
            tool: "llc".to_string(),
        })
    }
    
    /// Check if LLVM tools are available
    pub fn check_tools_available() -> Result<(), BinaryGenError> {
        // Check for llc
        Self::find_llc_executable()?;
        
        // Check for linker (clang or gcc)
        let has_clang = Command::new("clang").arg("--version").output().is_ok();
        let has_gcc = Command::new("gcc").arg("--version").output().is_ok();
        
        if !has_clang && !has_gcc {
            return Err(BinaryGenError::LlvmToolsNotFound {
                tool: "clang or gcc".to_string(),
            });
        }
        
        Ok(())
    }
}

/// Get the default target triple for the current platform
pub fn default_target_triple() -> String {
    #[cfg(all(target_arch = "x86_64", target_os = "linux"))]
    return "x86_64-unknown-linux-gnu".to_string();
    
    #[cfg(all(target_arch = "x86_64", target_os = "macos"))]
    return "x86_64-apple-darwin".to_string();
    
    #[cfg(all(target_arch = "x86_64", target_os = "windows"))]
    return "x86_64-pc-windows-msvc".to_string();
    
    #[cfg(all(target_arch = "aarch64", target_os = "linux"))]
    return "aarch64-unknown-linux-gnu".to_string();
    
    #[cfg(all(target_arch = "aarch64", target_os = "macos"))]
    return "aarch64-apple-darwin".to_string();
    
    // Fallback
    "x86_64-unknown-linux-gnu".to_string()
}

impl Default for BinaryGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience function to generate binary from LLVM IR
pub fn generate_binary_from_llvm(
    llvm_ir: &str,
    output_path: &Path,
    target: Option<&str>,
    optimization: u8,
    debug: bool,
) -> Result<(), BinaryGenError> {
    let mut generator = BinaryGenerator::new()
        .with_optimization(optimization)
        .with_debug_info(debug);
    
    if let Some(target_triple) = target {
        generator = generator.with_target(target_triple.to_string());
    }
    
    generator.generate_binary(llvm_ir, output_path)
}
