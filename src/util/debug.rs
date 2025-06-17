/// Debug utilities for the COLANG compiler
/// 
/// This module provides debugging and profiling helpers for compiler development,
/// including AST pretty-printing, performance measurement, and internal state inspection.

use std::time::{Duration, Instant};
use std::collections::HashMap;
use crate::ast::*;

/// Performance profiler for compiler phases
#[derive(Debug, Default)]
pub struct Profiler {
    timings: HashMap<String, Duration>,
    start_times: HashMap<String, Instant>,
}

impl Profiler {
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Start timing a phase
    pub fn start(&mut self, phase: &str) {
        self.start_times.insert(phase.to_string(), Instant::now());
    }
    
    /// End timing a phase and record the duration
    pub fn end(&mut self, phase: &str) {
        if let Some(start_time) = self.start_times.remove(phase) {
            let duration = start_time.elapsed();
            *self.timings.entry(phase.to_string()).or_insert(Duration::ZERO) += duration;
        }
    }
    
    /// Get the total time spent in a phase
    pub fn get_time(&self, phase: &str) -> Option<Duration> {
        self.timings.get(phase).copied()
    }
    
    /// Get all recorded timings
    pub fn get_all_timings(&self) -> &HashMap<String, Duration> {
        &self.timings
    }
    
    /// Print a summary of all timings
    pub fn print_summary(&self) {
        println!("Compilation Phase Timings:");
        println!("{:<20} {:>10}", "Phase", "Time (ms)");
        println!("{:-<32}", "");
        
        let mut sorted_timings: Vec<_> = self.timings.iter().collect();
        sorted_timings.sort_by(|a, b| b.1.cmp(a.1));
        
        for (phase, duration) in sorted_timings {
            println!("{:<20} {:>10.2}", phase, duration.as_secs_f64() * 1000.0);
        }
        
        let total: Duration = self.timings.values().sum();
        println!("{:-<32}", "");
        println!("{:<20} {:>10.2}", "Total", total.as_secs_f64() * 1000.0);
    }
    
    /// Clear all timings
    pub fn clear(&mut self) {
        self.timings.clear();
        self.start_times.clear();
    }
}

/// Macro for timing code blocks
#[macro_export]
macro_rules! time_phase {
    ($profiler:expr, $phase:expr, $block:block) => {{
        $profiler.start($phase);
        let result = $block;
        $profiler.end($phase);
        result
    }};
}

/// AST pretty printer for debugging
pub struct AstPrinter {
    indent_level: usize,
    indent_size: usize,
}

impl AstPrinter {
    pub fn new() -> Self {
        Self {
            indent_level: 0,
            indent_size: 2,
        }
    }
    
    pub fn with_indent_size(mut self, size: usize) -> Self {
        self.indent_size = size;
        self
    }
    
    /// Print a complete program
    pub fn print_program(&mut self, program: &Program) -> String {
        let mut output = String::new();
        output.push_str("Program {\n");
        self.indent();
        
        for item in &program.items {
            output.push_str(&self.print_item(item));
            output.push('\n');
        }
        
        self.dedent();
        output.push_str("}\n");
        output
    }
    
    /// Print an item
    pub fn print_item(&mut self, item: &ItemNode) -> String {
        match &item.inner {
            Item::Declaration(decl) => self.print_declaration(decl),
        }
    }
    
    /// Print a declaration
    pub fn print_declaration(&mut self, decl: &DeclarationNode) -> String {
        let mut output = String::new();
        output.push_str(&self.current_indent());
        
        match &decl.inner {
            Declaration::Function { name, parameters, return_type, body, visibility, .. } => {
                output.push_str(&format!("Function {} (", name));
                for (i, param) in parameters.iter().enumerate() {
                    if i > 0 { output.push_str(", "); }
                    output.push_str(&format!("{:?}: {:?}", param.pattern, param.param_type.inner));
                }
                output.push(')');
                
                if let Some(ret_ty) = return_type {
                    output.push_str(&format!(" -> {:?}", ret_ty.inner));
                }
                
                if let Some(body) = body {
                    output.push_str(" {\n");
                    self.indent();
                    output.push_str(&self.print_block(body));
                    self.dedent();
                    output.push_str(&self.current_indent());
                    output.push('}');
                }
            },
            Declaration::Struct { name, fields, .. } => {
                output.push_str(&format!("Struct {} {{\n", name));
                self.indent();
                for field in fields {
                    output.push_str(&self.current_indent());
                    output.push_str(&format!("{}: {:?}\n", field.name, field.field_type.inner));
                }
                self.dedent();
                output.push_str(&self.current_indent());
                output.push('}');
            },
            Declaration::Enum { name, variants, .. } => {
                output.push_str(&format!("Enum {} {{\n", name));
                self.indent();
                for variant in variants {
                    output.push_str(&self.current_indent());
                    output.push_str(&format!("{:?}\n", variant));
                }
                self.dedent();
                output.push_str(&self.current_indent());
                output.push('}');
            },
            _ => {
                output.push_str(&format!("{:?}", decl.inner));
            },
        }
        
        output
    }
    
    /// Print a block
    pub fn print_block(&mut self, block: &Block) -> String {
        let mut output = String::new();
        
        for stmt in &block.statements {
            output.push_str(&self.print_statement(stmt));
            output.push('\n');
        }
        
        if let Some(expr) = &block.expression {
            output.push_str(&self.current_indent());
            output.push_str(&self.print_expression(expr));
            output.push('\n');
        }
        
        output
    }
    
    /// Print a statement
    pub fn print_statement(&mut self, stmt: &StatementNode) -> String {
        let mut output = String::new();
        output.push_str(&self.current_indent());
        
        match &stmt.inner {
            Statement::Expression(expr) => {
                output.push_str(&self.print_expression(expr));
                output.push(';');
            },
            Statement::Let { pattern, type_annotation, initializer, mutable } => {
                output.push_str("let ");
                if *mutable { output.push_str("mut "); }
                output.push_str(&format!("{:?}", pattern));
                
                if let Some(ty) = type_annotation {
                    output.push_str(&format!(": {:?}", ty.inner));
                }
                
                if let Some(init) = initializer {
                    output.push_str(" = ");
                    output.push_str(&self.print_expression(init));
                }
                output.push(';');
            },
            Statement::Return(expr) => {
                output.push_str("return");
                if let Some(e) = expr {
                    output.push(' ');
                    output.push_str(&self.print_expression(e));
                }
                output.push(';');
            },
            _ => {
                output.push_str(&format!("{:?}", stmt.inner));
            },
        }
        
        output
    }
    
    /// Print an expression
    pub fn print_expression(&mut self, expr: &ExprNode) -> String {
        match &expr.inner {
            Expr::Literal(lit) => format!("{:?}", lit),
            Expr::Identifier(name) => name.clone(),
            Expr::Binary { left, operator, right } => {
                format!("({} {} {})",
                    self.print_expression(left),
                    operator,
                    self.print_expression(right)
                )
            },
            Expr::Unary { operator, operand } => {
                format!("({}{})", operator, self.print_expression(operand))
            },
            Expr::Call { function, args } => {
                let mut output = self.print_expression(function);
                output.push('(');
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { output.push_str(", "); }
                    output.push_str(&self.print_expression(arg));
                }
                output.push(')');
                output
            },
            _ => format!("{:?}", expr.inner),
        }
    }
    
    fn indent(&mut self) {
        self.indent_level += 1;
    }
    
    fn dedent(&mut self) {
        if self.indent_level > 0 {
            self.indent_level -= 1;
        }
    }
    
    fn current_indent(&self) -> String {
        " ".repeat(self.indent_level * self.indent_size)
    }
}

impl Default for AstPrinter {
    fn default() -> Self {
        Self::new()
    }
}

/// Memory usage tracker for debugging memory consumption
#[derive(Debug, Default)]
pub struct MemoryTracker {
    peak_usage: usize,
    current_usage: usize,
    allocations: HashMap<String, usize>,
}

impl MemoryTracker {
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Record an allocation
    pub fn allocate(&mut self, category: &str, size: usize) {
        self.current_usage += size;
        self.peak_usage = self.peak_usage.max(self.current_usage);
        *self.allocations.entry(category.to_string()).or_insert(0) += size;
    }
    
    /// Record a deallocation
    pub fn deallocate(&mut self, category: &str, size: usize) {
        self.current_usage = self.current_usage.saturating_sub(size);
        if let Some(cat_usage) = self.allocations.get_mut(category) {
            *cat_usage = cat_usage.saturating_sub(size);
        }
    }
    
    /// Get current memory usage
    pub fn current_usage(&self) -> usize {
        self.current_usage
    }
    
    /// Get peak memory usage
    pub fn peak_usage(&self) -> usize {
        self.peak_usage
    }
    
    /// Get allocations by category
    pub fn allocations(&self) -> &HashMap<String, usize> {
        &self.allocations
    }
    
    /// Print memory usage summary
    pub fn print_summary(&self) {
        println!("Memory Usage Summary:");
        println!("Current: {} bytes", self.current_usage);
        println!("Peak: {} bytes", self.peak_usage);
        println!("\nAllocations by category:");
        
        let mut sorted_allocations: Vec<_> = self.allocations.iter().collect();
        sorted_allocations.sort_by(|a, b| b.1.cmp(a.1));
        
        for (category, size) in sorted_allocations {
            println!("  {}: {} bytes", category, size);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::Span;
    
    #[test]
    fn test_profiler() {
        let mut profiler = Profiler::new();
        
        profiler.start("test_phase");
        std::thread::sleep(std::time::Duration::from_millis(10));
        profiler.end("test_phase");
        
        let timing = profiler.get_time("test_phase").unwrap();
        assert!(timing.as_millis() >= 10);
    }
    
    #[test]
    fn test_memory_tracker() {
        let mut tracker = MemoryTracker::new();
        
        tracker.allocate("test", 100);
        assert_eq!(tracker.current_usage(), 100);
        assert_eq!(tracker.peak_usage(), 100);
        
        tracker.allocate("test", 50);
        assert_eq!(tracker.current_usage(), 150);
        assert_eq!(tracker.peak_usage(), 150);
        
        tracker.deallocate("test", 30);
        assert_eq!(tracker.current_usage(), 120);
        assert_eq!(tracker.peak_usage(), 150);
    }
}