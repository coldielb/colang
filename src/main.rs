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
    
    // 3. Semantic analysis (not yet implemented)
    if verbose {
        println!("Semantic analysis not yet implemented");
    }
    
    // 4. IR generation (not yet implemented)
    if verbose {
        println!("IR generation not yet implemented");
    }
    
    // 5. Optimization (not yet implemented)
    if verbose {
        println!("Optimization not yet implemented");
    }
    
    // 6. Code generation (not yet implemented)
    if verbose {
        println!("Code generation not yet implemented");
    }
    
    println!("Successfully parsed COLANG program!");
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
    
    // 3. Semantic analysis (not yet implemented)
    if verbose {
        println!("Semantic analysis not yet implemented");
    }
    
    println!("✓ COLANG program is syntactically valid!");
    Ok(())
}

fn run_file(args: RunArgs, verbose: bool) -> Result<()> {
    if verbose {
        println!("Running: {}", args.input.display());
    }
    
    // TODO: Implement run pipeline (compile + execute)
    // 1. Compile to temporary executable
    // 2. Execute with provided arguments
    
    eprintln!("Run mode not yet implemented");
    std::process::exit(1);
}
