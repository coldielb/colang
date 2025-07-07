use clap::{Args, Parser, Subcommand};
use std::path::PathBuf;
use anyhow::Result;

mod lexer;
mod parser;
mod ast;
mod semantics;
mod ir;
mod codegen;
mod util;

#[path = "std/mod.rs"]
mod colang_std;

#[derive(Parser)]
#[command(
    name = "colang",
    version = "0.1.0",
    about = "The COLANG compiler - A modern systems programming language",
    long_about = "COLANG is a systems programming language designed as a bridge between C/C++ and Rust paradigms, providing explicit ownership semantics with memory safety and zero-cost abstractions."
)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
    
    #[arg(short, long, global = true)]
    verbose: bool,
    
    #[arg(long, global = true)]
    color: Option<String>,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile COLANG source files
    Compile(CompileArgs),
    /// Check COLANG source files for errors without generating code
    Check(CheckArgs),
    /// Run a COLANG program directly
    Run(RunArgs),
    /// Show information about the COLANG installation
    Version,
}

#[derive(Args)]
struct CompileArgs {
    /// Input source file
    input: PathBuf,
    
    /// Output file (defaults to input filename with appropriate extension)
    #[arg(short, long)]
    output: Option<PathBuf>,
    
    /// Optimization level (0-3)
    #[arg(short = 'O', long, default_value = "0")]
    optimization: u8,
    
    /// Enable debug information
    #[arg(short, long)]
    debug: bool,
    
    /// Target triple (e.g., x86_64-unknown-linux-gnu)
    #[arg(long)]
    target: Option<String>,
    
    /// Emit IR instead of machine code
    #[arg(long)]
    emit_ir: bool,
    
    /// Emit LLVM IR
    #[arg(long)]
    emit_llvm: bool,
    
    /// Library search paths
    #[arg(short = 'L', long = "library-path")]
    library_paths: Vec<PathBuf>,
    
    /// Link libraries
    #[arg(short = 'l', long = "link")]
    libraries: Vec<String>,
}

#[derive(Args)]
struct CheckArgs {
    /// Input source file
    input: PathBuf,
    
    /// Check all files in the project
    #[arg(long)]
    all: bool,
}

#[derive(Args)]
struct RunArgs {
    /// Input source file
    input: PathBuf,
    
    /// Arguments to pass to the program
    #[arg(last = true)]
    args: Vec<String>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    
    match cli.command {
        Commands::Compile(args) => compile_file(args, cli.verbose),
        Commands::Check(args) => check_file(args, cli.verbose),
        Commands::Run(args) => run_file(args, cli.verbose),
        Commands::Version => {
            println!("COLANG compiler version {}", env!("CARGO_PKG_VERSION"));
            println!("Built with Rust {}", env!("CARGO_PKG_RUST_VERSION"));
            Ok(())
        }
    }
}

fn compile_file(args: CompileArgs, verbose: bool) -> Result<()> {
    if verbose {
        println!("Compiling: {}", args.input.display());
    }
    
    // Read source file
    let source = std::fs::read_to_string(&args.input)
        .map_err(|e| anyhow::anyhow!("Failed to read file '{}': {}", args.input.display(), e))?;
    
    if verbose {
        println!("Read {} bytes from {}", source.len(), args.input.display());
    }
    
    // 1. Lexical analysis
    if verbose {
        println!("Starting lexical analysis...");
    }
    let mut lexer = lexer::Lexer::new(&source);
    
    // 2. Parsing
    if verbose {
        println!("Starting parsing...");
    }
    let mut parser = parser::Parser::new(lexer)
        .map_err(|e| anyhow::anyhow!("Parser initialization failed: {}", e))?;
    
    let program = parser.parse_program()
        .map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;
    
    if verbose {
        println!("Successfully parsed program with {} items", program.len());
        
        // Pretty print the AST for debugging
        let mut ast_printer = util::debug::AstPrinter::new();
        let ast_output = ast_printer.print_program(&program);
        println!("AST:\n{}", ast_output);
    }
    
    // 3. Semantic analysis
    if verbose {
        println!("Starting semantic analysis...");
    }
    
    let mut type_checker = semantics::TypeChecker::new();
    match type_checker.check_program(&program) {
        Ok(()) => {
            if verbose {
                println!("✓ Semantic analysis passed");
                println!("Symbol table:\n{}", type_checker.symbol_table().debug_print());
            }
        },
        Err(errors) => {
            println!("Semantic errors found:");
            for error in &errors {
                println!("  {}", error);
            }
            return Err(anyhow::anyhow!("Compilation failed due to semantic errors"));
        }
    }
    
    // 4. IR generation
    if verbose {
        println!("Starting IR generation...");
    }
    
    let mut ir_builder = ir::builder::IrBuilder::new();
    let mut ir_module = ir_builder.build_program(&program, type_checker.symbol_table())
        .map_err(|e| anyhow::anyhow!("IR generation failed: {}", e))?;
    
    if verbose {
        println!("✓ IR generation completed");
        println!("Generated {} functions", ir_module.functions.len());
        
        if args.emit_ir {
            println!("COLANG IR:\n{}", ir_module.display());
        }
    }
    
    // 5. LLVM IR generation
    if verbose {
        println!("Starting LLVM IR generation...");
    }
    
    let mut llvm_ir = codegen::llvm::generate_llvm_ir(&ir_module)
        .map_err(|e| anyhow::anyhow!("LLVM IR generation failed: {}", e))?;
    
    if verbose {
        println!("✓ LLVM IR generation completed");
    }
    
    // Emit IR if requested
    if args.emit_ir {
        let ir_output_path = args.output.clone().unwrap_or_else(|| {
            args.input.with_extension("ir")
        });
        std::fs::write(&ir_output_path, ir_module.display())
            .map_err(|e| anyhow::anyhow!("Failed to write IR file '{}': {}", ir_output_path.display(), e))?;
        if verbose {
            println!("IR written to: {}", ir_output_path.display());
        }
    }
    
    // Emit LLVM IR if requested
    if args.emit_llvm {
        let llvm_output_path = args.output.clone().unwrap_or_else(|| {
            args.input.with_extension("ll")
        });
        std::fs::write(&llvm_output_path, &llvm_ir)
            .map_err(|e| anyhow::anyhow!("Failed to write LLVM IR file '{}': {}", llvm_output_path.display(), e))?;
        if verbose {
            println!("LLVM IR written to: {}", llvm_output_path.display());
        }
    }
    
    if verbose && args.emit_llvm {
        println!("LLVM IR:\n{}", llvm_ir);
    }
    
    // 6. Optimization passes
    if args.optimization > 0 {
        if verbose {
            println!("Starting optimization passes (level {})...", args.optimization);
        }
        
        let mut pass_manager = if args.optimization >= 2 {
            ir::optimizer::PassManager::standard_pipeline()
        } else {
            // Level 1: Basic optimizations only
            let mut manager = ir::optimizer::PassManager::new();
            manager.add_pass(ir::optimizer::ConstantFolding::new());
            manager.add_pass(ir::optimizer::DeadCodeElimination::new());
            manager
        };
        
        let optimized = pass_manager.run(&mut ir_module)
            .map_err(|e| anyhow::anyhow!("Optimization failed: {}", e))?;
        
        if verbose {
            if optimized {
                println!("✓ Optimizations applied");
            } else {
                println!("✓ No optimizations needed");
            }
        }
        
        // Regenerate LLVM IR after optimization
        if verbose {
            println!("Regenerating LLVM IR after optimization...");
        }
        
        llvm_ir = codegen::llvm::generate_llvm_ir(&ir_module)
            .map_err(|e| anyhow::anyhow!("LLVM IR generation failed after optimization: {}", e))?;
    }
    
    // 7. Binary generation (if not emitting IR/LLVM)
    if !args.emit_ir && !args.emit_llvm {
        if verbose {
            println!("Starting binary generation...");
        }
        
        let binary_output_path = args.output.clone().unwrap_or_else(|| {
            args.input.with_extension("")
        });
        
        codegen::target::generate_binary_from_llvm(
            &llvm_ir,
            &binary_output_path,
            args.target.as_deref(),
            args.optimization,
            args.debug,
        ).map_err(|e| anyhow::anyhow!("Binary generation failed: {}", e))?;
        
        if verbose {
            println!("✓ Binary generated: {}", binary_output_path.display());
        }
    }
    
    if args.emit_ir || args.emit_llvm {
        println!("✓ Successfully generated requested output");
    } else {
        println!("✓ Successfully compiled COLANG program");
    }
    Ok(())
}

fn check_file(args: CheckArgs, verbose: bool) -> Result<()> {
    if verbose {
        println!("Checking: {}", args.input.display());
    }
    
    // Read source file
    let source = std::fs::read_to_string(&args.input)
        .map_err(|e| anyhow::anyhow!("Failed to read file '{}': {}", args.input.display(), e))?;
    
    // 1. Lexical analysis
    if verbose {
        println!("Starting lexical analysis...");
    }
    let lexer = lexer::Lexer::new(&source);
    
    // 2. Parsing
    if verbose {
        println!("Starting parsing...");
    }
    let mut parser = parser::Parser::new(lexer)
        .map_err(|e| anyhow::anyhow!("Parser initialization failed: {}", e))?;
    
    let program = parser.parse_program()
        .map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;
    
    if verbose {
        println!("Successfully parsed program with {} items", program.len());
    }
    
    // 3. Semantic analysis
    if verbose {
        println!("Starting semantic analysis...");
    }
    
    let mut type_checker = semantics::TypeChecker::new();
    match type_checker.check_program(&program) {
        Ok(()) => {
            if verbose {
                println!("✓ Semantic analysis passed");
            }
        },
        Err(errors) => {
            println!("Semantic errors found:");
            for error in &errors {
                println!("  {}", error);
            }
            return Err(anyhow::anyhow!("Type checking failed"));
        }
    }
    
    println!("✓ COLANG program is semantically valid!");
    Ok(())
}

fn run_file(args: RunArgs, verbose: bool) -> Result<()> {
    if verbose {
        println!("Running: {}", args.input.display());
    }
    
    let executable_path = if args.input.extension().and_then(|s| s.to_str()) == Some("co") {
        // Compile .co file to temporary executable
        if verbose {
            println!("Compiling COLANG source...");
        }
        
        // Read and compile the source file
        let source = std::fs::read_to_string(&args.input)
            .map_err(|e| anyhow::anyhow!("Failed to read file '{}': {}", args.input.display(), e))?;
        
        // Compile through the full pipeline
        let temp_dir = std::env::temp_dir();
        let temp_executable = temp_dir.join(format!("colang_run_{}", std::process::id()));
        
        compile_source_to_binary(&source, &temp_executable, &args.input, verbose)?;
        
        temp_executable
    } else {
        // Assume it's already a binary
        args.input.clone()
    };
    
    // Execute the binary
    if verbose {
        println!("Executing: {}", executable_path.display());
    }
    
    let mut cmd = std::process::Command::new(&executable_path);
    cmd.args(&args.args);
    
    let exit_status = cmd.status()
        .map_err(|e| anyhow::anyhow!("Failed to execute '{}': {}", executable_path.display(), e))?;
    
    // Clean up temporary executable if we created one
    if args.input.extension().and_then(|s| s.to_str()) == Some("co") {
        let _ = std::fs::remove_file(&executable_path);
    }
    
    if !exit_status.success() {
        if let Some(code) = exit_status.code() {
            std::process::exit(code);
        } else {
            std::process::exit(1);
        }
    }
    
    Ok(())
}

/// Helper function to compile source code to binary
fn compile_source_to_binary(source: &str, output_path: &std::path::Path, input_path: &std::path::Path, verbose: bool) -> Result<()> {
    // Lexical analysis
    let lexer = lexer::Lexer::new(source);
    
    // Parsing  
    let mut parser = parser::Parser::new(lexer)
        .map_err(|e| anyhow::anyhow!("Parser initialization failed: {}", e))?;
    
    let program = parser.parse_program()
        .map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;
    
    // Semantic analysis
    let mut type_checker = semantics::TypeChecker::new();
    match type_checker.check_program(&program) {
        Ok(()) => {},
        Err(errors) => {
            println!("Semantic errors found:");
            for error in &errors {
                println!("  {}", error);
            }
            return Err(anyhow::anyhow!("Compilation failed due to semantic errors"));
        }
    }
    
    // IR generation
    let mut ir_builder = ir::builder::IrBuilder::new();
    let mut ir_module = ir_builder.build_program(&program, type_checker.symbol_table())
        .map_err(|e| anyhow::anyhow!("IR generation failed: {}", e))?;
    
    // Basic optimizations
    let mut pass_manager = ir::optimizer::PassManager::new();
    pass_manager.add_pass(ir::optimizer::ConstantFolding::new());
    pass_manager.add_pass(ir::optimizer::DeadCodeElimination::new());
    
    let _ = pass_manager.run(&mut ir_module)
        .map_err(|e| anyhow::anyhow!("Optimization failed: {}", e))?;
    
    // LLVM IR generation
    let llvm_ir = codegen::llvm::generate_llvm_ir(&ir_module)
        .map_err(|e| anyhow::anyhow!("LLVM IR generation failed: {}", e))?;
    
    // Binary generation
    codegen::target::generate_binary_from_llvm(
        &llvm_ir,
        output_path,
        None, // Use default target
        1,    // Basic optimization
        false, // No debug info for temp executable
    ).map_err(|e| anyhow::anyhow!("Binary generation failed: {}", e))?;
    
    Ok(())
}
