/// COLANG IR Optimization Framework
/// 
/// This module provides optimization passes for COLANG's SSA-form IR.
/// Optimizations are implemented as passes that transform the IR while
/// preserving program semantics.

use std::collections::{HashMap, HashSet};
use crate::ir::instruction::{Instruction, InstructionKind, IrType, ValueId, BlockId, FunctionId};
use crate::ir::basic_block::{ControlFlowGraph, BasicBlock};
use crate::ir::builder::IrModule;

/// Trait for optimization passes
pub trait OptimizationPass {
    /// Get the name of this optimization pass
    fn name(&self) -> &str;
    
    /// Run this optimization pass on a function
    fn run_on_function(&mut self, cfg: &mut ControlFlowGraph) -> bool;
    
    /// Run this optimization pass on a module (default implementation runs on each function)
    fn run_on_module(&mut self, module: &mut IrModule) -> bool {
        let mut changed = false;
        for (_, cfg) in &mut module.functions {
            if self.run_on_function(cfg) {
                changed = true;
            }
        }
        changed
    }
}

/// Pass manager for running optimization passes
pub struct PassManager {
    passes: Vec<Box<dyn OptimizationPass>>,
    max_iterations: usize,
}

/// Dead Code Elimination pass
pub struct DeadCodeElimination {
    /// Set of values that are live (used)
    live_values: HashSet<ValueId>,
    /// Set of blocks that are reachable
    reachable_blocks: HashSet<BlockId>,
}

/// Constant Folding pass
pub struct ConstantFolding {
    /// Map from value ID to constant value
    constants: HashMap<ValueId, ConstantValue>,
}

/// Constant Propagation pass
pub struct ConstantPropagation {
    /// Map from value ID to constant value
    constants: HashMap<ValueId, ConstantValue>,
}

/// Copy Propagation pass
pub struct CopyPropagation {
    /// Map from value ID to the value it copies
    copies: HashMap<ValueId, ValueId>,
}

/// Constant values for optimization
#[derive(Debug, Clone, PartialEq)]
pub enum ConstantValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    Unit,
}

/// Optimization errors
#[derive(Debug, Clone)]
pub enum OptimizationError {
    InvalidTransformation { message: String },
    InternalError { message: String },
}

impl std::fmt::Display for OptimizationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OptimizationError::InvalidTransformation { message } => {
                write!(f, "Invalid transformation: {}", message)
            },
            OptimizationError::InternalError { message } => {
                write!(f, "Internal optimization error: {}", message)
            },
        }
    }
}

impl std::error::Error for OptimizationError {}

impl PassManager {
    /// Create a new pass manager
    pub fn new() -> Self {
        Self {
            passes: Vec::new(),
            max_iterations: 10,
        }
    }
    
    /// Add an optimization pass
    pub fn add_pass<P: OptimizationPass + 'static>(&mut self, pass: P) {
        self.passes.push(Box::new(pass));
    }
    
    /// Set maximum number of iterations for iterative optimization
    pub fn set_max_iterations(&mut self, max_iter: usize) {
        self.max_iterations = max_iter;
    }
    
    /// Run all optimization passes on a module
    pub fn run(&mut self, module: &mut IrModule) -> Result<bool, OptimizationError> {
        let mut changed = false;
        let mut iteration = 0;
        
        // Iteratively run passes until no changes or max iterations reached
        loop {
            let mut iter_changed = false;
            
            for pass in &mut self.passes {
                if pass.run_on_module(module) {
                    iter_changed = true;
                    changed = true;
                }
            }
            
            iteration += 1;
            if !iter_changed || iteration >= self.max_iterations {
                break;
            }
        }
        
        Ok(changed)
    }
    
    /// Create a standard optimization pipeline
    pub fn standard_pipeline() -> Self {
        let mut manager = Self::new();
        manager.add_pass(DeadCodeElimination::new());
        manager.add_pass(ConstantFolding::new());
        manager.add_pass(ConstantPropagation::new());
        manager.add_pass(CopyPropagation::new());
        manager
    }
}

impl DeadCodeElimination {
    pub fn new() -> Self {
        Self {
            live_values: HashSet::new(),
            reachable_blocks: HashSet::new(),
        }
    }
    
    /// Mark a value as live
    fn mark_live(&mut self, value: ValueId) {
        self.live_values.insert(value);
    }
    
    /// Mark a block as reachable
    fn mark_reachable(&mut self, block: BlockId) {
        self.reachable_blocks.insert(block);
    }
    
    /// Perform liveness analysis
    fn analyze_liveness(&mut self, cfg: &ControlFlowGraph) {
        self.live_values.clear();
        self.reachable_blocks.clear();
        
        // Mark reachable blocks starting from entry
        let mut worklist = vec![cfg.entry_block];
        self.mark_reachable(cfg.entry_block);
        
        while let Some(block_id) = worklist.pop() {
            if let Some(block) = cfg.get_block(block_id) {
                for &succ in &block.successors {
                    if !self.reachable_blocks.contains(&succ) {
                        self.mark_reachable(succ);
                        worklist.push(succ);
                    }
                }
            }
        }
        
        // Mark live values in reachable blocks
        let reachable_blocks = self.reachable_blocks.clone();
        for &block_id in &reachable_blocks {
            if let Some(block) = cfg.get_block(block_id) {
                for instruction in &block.instructions {
                    // Values with side effects are always live
                    if instruction.has_side_effects() {
                        self.mark_live(instruction.id);
                        
                        // Mark used values as live
                        for &used_val in &instruction.used_values() {
                            self.mark_live(used_val);
                        }
                    }
                }
            }
        }
        
        // Propagate liveness backwards
        let mut changed = true;
        while changed {
            changed = false;
            
            let reachable_blocks = self.reachable_blocks.clone();
            for &block_id in &reachable_blocks {
                if let Some(block) = cfg.get_block(block_id) {
                    for instruction in &block.instructions {
                        if self.live_values.contains(&instruction.id) {
                            for &used_val in &instruction.used_values() {
                                if !self.live_values.contains(&used_val) {
                                    self.mark_live(used_val);
                                    changed = true;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

impl OptimizationPass for DeadCodeElimination {
    fn name(&self) -> &str {
        "dead-code-elimination"
    }
    
    fn run_on_function(&mut self, cfg: &mut ControlFlowGraph) -> bool {
        self.analyze_liveness(cfg);
        
        let mut changed = false;
        
        // Remove unreachable blocks
        let block_ids: Vec<_> = cfg.blocks().map(|(id, _)| *id).collect();
        for block_id in block_ids {
            if !self.reachable_blocks.contains(&block_id) && block_id != cfg.entry_block {
                cfg.blocks.remove(&block_id);
                changed = true;
            }
        }
        
        // Remove dead instructions
        for (_, block) in &mut cfg.blocks {
            let original_len = block.instructions.len();
            block.instructions.retain(|inst| {
                // Keep instructions that are live or have side effects
                self.live_values.contains(&inst.id) || inst.has_side_effects()
            });
            
            if block.instructions.len() != original_len {
                changed = true;
            }
        }
        
        changed
    }
}

impl ConstantFolding {
    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),
        }
    }
    
    /// Try to fold a binary arithmetic operation
    fn fold_binary_op(op: &InstructionKind, lhs: &ConstantValue, rhs: &ConstantValue) -> Option<ConstantValue> {
        match (op, lhs, rhs) {
            // Integer arithmetic
            (InstructionKind::Add { .. }, ConstantValue::Int(a), ConstantValue::Int(b)) => {
                Some(ConstantValue::Int(a + b))
            },
            (InstructionKind::Sub { .. }, ConstantValue::Int(a), ConstantValue::Int(b)) => {
                Some(ConstantValue::Int(a - b))
            },
            (InstructionKind::Mul { .. }, ConstantValue::Int(a), ConstantValue::Int(b)) => {
                Some(ConstantValue::Int(a * b))
            },
            (InstructionKind::Div { .. }, ConstantValue::Int(a), ConstantValue::Int(b)) if *b != 0 => {
                Some(ConstantValue::Int(a / b))
            },
            (InstructionKind::Mod { .. }, ConstantValue::Int(a), ConstantValue::Int(b)) if *b != 0 => {
                Some(ConstantValue::Int(a % b))
            },
            
            // Float arithmetic
            (InstructionKind::Add { .. }, ConstantValue::Float(a), ConstantValue::Float(b)) => {
                Some(ConstantValue::Float(a + b))
            },
            (InstructionKind::Sub { .. }, ConstantValue::Float(a), ConstantValue::Float(b)) => {
                Some(ConstantValue::Float(a - b))
            },
            (InstructionKind::Mul { .. }, ConstantValue::Float(a), ConstantValue::Float(b)) => {
                Some(ConstantValue::Float(a * b))
            },
            (InstructionKind::Div { .. }, ConstantValue::Float(a), ConstantValue::Float(b)) if *b != 0.0 => {
                Some(ConstantValue::Float(a / b))
            },
            
            // Comparisons
            (InstructionKind::Eq { .. }, ConstantValue::Int(a), ConstantValue::Int(b)) => {
                Some(ConstantValue::Bool(a == b))
            },
            (InstructionKind::Ne { .. }, ConstantValue::Int(a), ConstantValue::Int(b)) => {
                Some(ConstantValue::Bool(a != b))
            },
            (InstructionKind::Lt { .. }, ConstantValue::Int(a), ConstantValue::Int(b)) => {
                Some(ConstantValue::Bool(a < b))
            },
            (InstructionKind::Le { .. }, ConstantValue::Int(a), ConstantValue::Int(b)) => {
                Some(ConstantValue::Bool(a <= b))
            },
            (InstructionKind::Gt { .. }, ConstantValue::Int(a), ConstantValue::Int(b)) => {
                Some(ConstantValue::Bool(a > b))
            },
            (InstructionKind::Ge { .. }, ConstantValue::Int(a), ConstantValue::Int(b)) => {
                Some(ConstantValue::Bool(a >= b))
            },
            
            // Logical operations
            (InstructionKind::And { .. }, ConstantValue::Bool(a), ConstantValue::Bool(b)) => {
                Some(ConstantValue::Bool(*a && *b))
            },
            (InstructionKind::Or { .. }, ConstantValue::Bool(a), ConstantValue::Bool(b)) => {
                Some(ConstantValue::Bool(*a || *b))
            },
            
            _ => None,
        }
    }
    
    /// Try to fold a unary operation
    fn fold_unary_op(op: &InstructionKind, operand: &ConstantValue) -> Option<ConstantValue> {
        match (op, operand) {
            (InstructionKind::Neg { .. }, ConstantValue::Int(a)) => {
                Some(ConstantValue::Int(-a))
            },
            (InstructionKind::Neg { .. }, ConstantValue::Float(a)) => {
                Some(ConstantValue::Float(-a))
            },
            (InstructionKind::Not { .. }, ConstantValue::Bool(a)) => {
                Some(ConstantValue::Bool(!a))
            },
            _ => None,
        }
    }
    
    /// Extract constant value from instruction
    fn extract_constant(inst: &Instruction) -> Option<ConstantValue> {
        match &inst.kind {
            InstructionKind::ConstInt { value, .. } => Some(ConstantValue::Int(*value)),
            InstructionKind::ConstFloat { value, .. } => Some(ConstantValue::Float(*value)),
            InstructionKind::ConstBool { value } => Some(ConstantValue::Bool(*value)),
            InstructionKind::ConstUnit => Some(ConstantValue::Unit),
            _ => None,
        }
    }
    
    /// Create a constant instruction from a constant value
    fn create_constant_instruction(value: ConstantValue, result_type: IrType, value_id: ValueId) -> Instruction {
        let kind = match value {
            ConstantValue::Int(val) => InstructionKind::ConstInt { value: val, ty: result_type.clone() },
            ConstantValue::Float(val) => InstructionKind::ConstFloat { value: val, ty: result_type.clone() },
            ConstantValue::Bool(val) => InstructionKind::ConstBool { value: val },
            ConstantValue::Unit => InstructionKind::ConstUnit,
        };
        
        Instruction::new(value_id, kind, result_type, None)
    }
}

impl OptimizationPass for ConstantFolding {
    fn name(&self) -> &str {
        "constant-folding"
    }
    
    fn run_on_function(&mut self, cfg: &mut ControlFlowGraph) -> bool {
        self.constants.clear();
        let mut changed = false;
        
        // Build constant map
        for (_, block) in &cfg.blocks {
            for instruction in &block.instructions {
                if let Some(constant) = Self::extract_constant(instruction) {
                    self.constants.insert(instruction.id, constant);
                }
            }
        }
        
        // Fold constants
        for (_, block) in &mut cfg.blocks {
            for instruction in &mut block.instructions {
                let folded = match &instruction.kind {
                    // Binary operations
                    InstructionKind::Add { lhs, rhs } |
                    InstructionKind::Sub { lhs, rhs } |
                    InstructionKind::Mul { lhs, rhs } |
                    InstructionKind::Div { lhs, rhs } |
                    InstructionKind::Mod { lhs, rhs } |
                    InstructionKind::Eq { lhs, rhs } |
                    InstructionKind::Ne { lhs, rhs } |
                    InstructionKind::Lt { lhs, rhs } |
                    InstructionKind::Le { lhs, rhs } |
                    InstructionKind::Gt { lhs, rhs } |
                    InstructionKind::Ge { lhs, rhs } |
                    InstructionKind::And { lhs, rhs } |
                    InstructionKind::Or { lhs, rhs } => {
                        if let (Some(lhs_const), Some(rhs_const)) = 
                            (self.constants.get(lhs), self.constants.get(rhs)) {
                            Self::fold_binary_op(&instruction.kind, lhs_const, rhs_const)
                        } else {
                            None
                        }
                    },
                    
                    // Unary operations
                    InstructionKind::Neg { operand } |
                    InstructionKind::Not { operand } => {
                        if let Some(operand_const) = self.constants.get(operand) {
                            Self::fold_unary_op(&instruction.kind, operand_const)
                        } else {
                            None
                        }
                    },
                    
                    _ => None,
                };
                
                if let Some(constant_value) = folded {
                    // Replace instruction with constant
                    *instruction = Self::create_constant_instruction(
                        constant_value.clone(),
                        instruction.ty.clone(),
                        instruction.id,
                    );
                    self.constants.insert(instruction.id, constant_value);
                    changed = true;
                }
            }
        }
        
        changed
    }
}

impl ConstantPropagation {
    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),
        }
    }
}

impl OptimizationPass for ConstantPropagation {
    fn name(&self) -> &str {
        "constant-propagation"
    }
    
    fn run_on_function(&mut self, cfg: &mut ControlFlowGraph) -> bool {
        // TODO: Implement constant propagation analysis
        false
    }
}

impl CopyPropagation {
    pub fn new() -> Self {
        Self {
            copies: HashMap::new(),
        }
    }
}

impl OptimizationPass for CopyPropagation {
    fn name(&self) -> &str {
        "copy-propagation"
    }
    
    fn run_on_function(&mut self, cfg: &mut ControlFlowGraph) -> bool {
        // TODO: Implement copy propagation analysis
        false
    }
}

impl Default for PassManager {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for DeadCodeElimination {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for ConstantFolding {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for ConstantPropagation {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for CopyPropagation {
    fn default() -> Self {
        Self::new()
    }
}
