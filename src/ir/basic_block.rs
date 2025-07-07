/// Basic Block Implementation for COLANG IR
/// 
/// This module implements basic blocks and control flow graphs for the IR.
/// Basic blocks are sequences of instructions that execute sequentially without
/// branches, except for the last instruction which may be a terminator.

use std::collections::{HashMap, HashSet};
use std::fmt;
use crate::ir::instruction::{Instruction, InstructionKind, BlockId, ValueId, FunctionId};

/// A basic block containing a sequence of instructions
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BlockId,
    pub instructions: Vec<Instruction>,
    pub predecessors: HashSet<BlockId>,
    pub successors: HashSet<BlockId>,
    pub name: Option<String>,
}

/// Control Flow Graph containing all basic blocks for a function
#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    pub function_id: FunctionId,
    pub blocks: HashMap<BlockId, BasicBlock>,
    pub entry_block: BlockId,
    pub parameters: Vec<(String, ValueId)>,
    pub name: String,
}

/// Iterator for traversing blocks in various orders
pub struct BlockIterator<'a> {
    blocks: &'a HashMap<BlockId, BasicBlock>,
    order: Vec<BlockId>,
    index: usize,
}

impl BasicBlock {
    /// Create a new basic block
    pub fn new(id: BlockId) -> Self {
        Self {
            id,
            instructions: Vec::new(),
            predecessors: HashSet::new(),
            successors: HashSet::new(),
            name: None,
        }
    }
    
    /// Create a new basic block with a name
    pub fn with_name(id: BlockId, name: String) -> Self {
        Self {
            id,
            instructions: Vec::new(),
            predecessors: HashSet::new(),
            successors: HashSet::new(),
            name: Some(name),
        }
    }
    
    /// Add an instruction to this block
    pub fn add_instruction(&mut self, instruction: Instruction) {
        // Ensure we don't add instructions after a terminator
        if self.has_terminator() {
            panic!("Cannot add instruction after terminator in block {}", self.id);
        }
        self.instructions.push(instruction);
    }
    
    /// Check if this block has a terminator instruction
    pub fn has_terminator(&self) -> bool {
        self.instructions.last().map_or(false, |inst| inst.is_terminator())
    }
    
    /// Get the terminator instruction if it exists
    pub fn terminator(&self) -> Option<&Instruction> {
        self.instructions.last().filter(|inst| inst.is_terminator())
    }
    
    /// Add a predecessor block
    pub fn add_predecessor(&mut self, pred_id: BlockId) {
        self.predecessors.insert(pred_id);
    }
    
    /// Add a successor block
    pub fn add_successor(&mut self, succ_id: BlockId) {
        self.successors.insert(succ_id);
    }
    
    /// Remove a predecessor block
    pub fn remove_predecessor(&mut self, pred_id: BlockId) {
        self.predecessors.remove(&pred_id);
    }
    
    /// Remove a successor block
    pub fn remove_successor(&mut self, succ_id: BlockId) {
        self.successors.remove(&succ_id);
    }
    
    /// Check if this block is empty (no instructions)
    pub fn is_empty(&self) -> bool {
        self.instructions.is_empty()
    }
    
    /// Get all values defined in this block
    pub fn defined_values(&self) -> Vec<ValueId> {
        self.instructions.iter().map(|inst| inst.id).collect()
    }
    
    /// Get all values used in this block
    pub fn used_values(&self) -> Vec<ValueId> {
        let mut used = Vec::new();
        for inst in &self.instructions {
            used.extend(inst.used_values());
        }
        used
    }
    
    /// Check if this block dominates another block
    pub fn dominates(&self, other_id: BlockId, cfg: &ControlFlowGraph) -> bool {
        // Simple dominance check - proper implementation would use dominance tree
        if self.id == other_id {
            return true;
        }
        
        // If this is the entry block, it dominates all others
        if self.id == cfg.entry_block {
            return true;
        }
        
        // For now, just check if all paths to other_id go through self
        // This is a simplified implementation
        cfg.all_paths_through(other_id, self.id)
    }
}

impl ControlFlowGraph {
    /// Create a new control flow graph
    pub fn new(function_id: FunctionId, name: String, entry_block: BlockId) -> Self {
        let mut blocks = HashMap::new();
        blocks.insert(entry_block, BasicBlock::new(entry_block));
        
        Self {
            function_id,
            blocks,
            entry_block,
            parameters: Vec::new(),
            name,
        }
    }
    
    /// Add a new basic block
    pub fn add_block(&mut self, id: BlockId) -> &mut BasicBlock {
        self.blocks.insert(id, BasicBlock::new(id));
        self.blocks.get_mut(&id).unwrap()
    }
    
    /// Add a basic block with a name
    pub fn add_named_block(&mut self, id: BlockId, name: String) -> &mut BasicBlock {
        self.blocks.insert(id, BasicBlock::with_name(id, name));
        self.blocks.get_mut(&id).unwrap()
    }
    
    /// Get a basic block by ID
    pub fn get_block(&self, id: BlockId) -> Option<&BasicBlock> {
        self.blocks.get(&id)
    }
    
    /// Get a mutable basic block by ID
    pub fn get_block_mut(&mut self, id: BlockId) -> Option<&mut BasicBlock> {
        self.blocks.get_mut(&id)
    }
    
    /// Connect two blocks (add edge from source to target)
    pub fn connect_blocks(&mut self, source: BlockId, target: BlockId) {
        if let Some(source_block) = self.blocks.get_mut(&source) {
            source_block.add_successor(target);
        }
        if let Some(target_block) = self.blocks.get_mut(&target) {
            target_block.add_predecessor(source);
        }
    }
    
    /// Disconnect two blocks (remove edge from source to target)
    pub fn disconnect_blocks(&mut self, source: BlockId, target: BlockId) {
        if let Some(source_block) = self.blocks.get_mut(&source) {
            source_block.remove_successor(target);
        }
        if let Some(target_block) = self.blocks.get_mut(&target) {
            target_block.remove_predecessor(source);
        }
    }
    
    /// Update control flow edges based on terminator instructions
    pub fn update_control_flow(&mut self) {
        let mut edges_to_add = Vec::new();
        let mut edges_to_remove = Vec::new();
        
        for (block_id, block) in &self.blocks {
            // Clear existing successors for this block
            for &succ in &block.successors {
                edges_to_remove.push((*block_id, succ));
            }
            
            // Add edges based on terminator
            if let Some(terminator) = block.terminator() {
                match &terminator.kind {
                    InstructionKind::Jump { target } => {
                        edges_to_add.push((*block_id, *target));
                    },
                    InstructionKind::Branch { then_block, else_block, .. } => {
                        edges_to_add.push((*block_id, *then_block));
                        edges_to_add.push((*block_id, *else_block));
                    },
                    InstructionKind::Return { .. } => {
                        // No successors for return
                    },
                    _ => {
                        // Other terminators might have different successor patterns
                    }
                }
            }
        }
        
        // Apply edge changes
        for (source, target) in edges_to_remove {
            self.disconnect_blocks(source, target);
        }
        for (source, target) in edges_to_add {
            self.connect_blocks(source, target);
        }
    }
    
    /// Get blocks in reverse post-order (good for many analyses)
    pub fn reverse_postorder(&self) -> Vec<BlockId> {
        let mut visited = HashSet::new();
        let mut order = Vec::new();
        
        fn dfs_postorder(
            block_id: BlockId,
            cfg: &ControlFlowGraph,
            visited: &mut HashSet<BlockId>,
            order: &mut Vec<BlockId>,
        ) {
            if visited.contains(&block_id) {
                return;
            }
            visited.insert(block_id);
            
            if let Some(block) = cfg.get_block(block_id) {
                for &succ in &block.successors {
                    dfs_postorder(succ, cfg, visited, order);
                }
            }
            
            order.push(block_id);
        }
        
        dfs_postorder(self.entry_block, self, &mut visited, &mut order);
        order.reverse();
        order
    }
    
    /// Get blocks in depth-first order
    pub fn depth_first_order(&self) -> Vec<BlockId> {
        let mut visited = HashSet::new();
        let mut order = Vec::new();
        
        fn dfs(
            block_id: BlockId,
            cfg: &ControlFlowGraph,
            visited: &mut HashSet<BlockId>,
            order: &mut Vec<BlockId>,
        ) {
            if visited.contains(&block_id) {
                return;
            }
            visited.insert(block_id);
            order.push(block_id);
            
            if let Some(block) = cfg.get_block(block_id) {
                for &succ in &block.successors {
                    dfs(succ, cfg, visited, order);
                }
            }
        }
        
        dfs(self.entry_block, self, &mut visited, &mut order);
        order
    }
    
    /// Check if all paths to target_id go through through_id
    pub fn all_paths_through(&self, target_id: BlockId, through_id: BlockId) -> bool {
        if target_id == through_id {
            return true;
        }
        
        let mut visited = HashSet::new();
        
        fn has_path_not_through(
            current: BlockId,
            target: BlockId,
            through: BlockId,
            cfg: &ControlFlowGraph,
            visited: &mut HashSet<BlockId>,
        ) -> bool {
            if current == target {
                return true;
            }
            if current == through {
                return false;
            }
            if visited.contains(&current) {
                return false;
            }
            visited.insert(current);
            
            if let Some(block) = cfg.get_block(current) {
                for &succ in &block.successors {
                    if has_path_not_through(succ, target, through, cfg, visited) {
                        return true;
                    }
                }
            }
            
            false
        }
        
        !has_path_not_through(self.entry_block, target_id, through_id, self, &mut visited)
    }
    
    /// Add a function parameter
    pub fn add_parameter(&mut self, name: String, value_id: ValueId) {
        self.parameters.push((name, value_id));
    }
    
    /// Get function parameters
    pub fn get_parameters(&self) -> &[(String, ValueId)] {
        &self.parameters
    }
    
    /// Validate the CFG for structural correctness
    pub fn validate(&self) -> Result<(), String> {
        // Check that entry block exists
        if !self.blocks.contains_key(&self.entry_block) {
            return Err(format!("Entry block {} does not exist", self.entry_block));
        }
        
        // Check that all blocks are reachable from entry
        let reachable = self.depth_first_order();
        let reachable_set: HashSet<_> = reachable.into_iter().collect();
        
        for &block_id in self.blocks.keys() {
            if !reachable_set.contains(&block_id) {
                return Err(format!("Block {} is unreachable from entry", block_id));
            }
        }
        
        // Check that all blocks have proper terminators (except empty blocks)
        for (block_id, block) in &self.blocks {
            if !block.is_empty() && !block.has_terminator() {
                return Err(format!("Block {} has no terminator", block_id));
            }
        }
        
        // Check that successor/predecessor relationships are consistent
        for (block_id, block) in &self.blocks {
            for &succ_id in &block.successors {
                if let Some(succ_block) = self.blocks.get(&succ_id) {
                    if !succ_block.predecessors.contains(block_id) {
                        return Err(format!(
                            "Block {} lists {} as successor, but {} doesn't list {} as predecessor",
                            block_id, succ_id, succ_id, block_id
                        ));
                    }
                } else {
                    return Err(format!("Block {} has non-existent successor {}", block_id, succ_id));
                }
            }
        }
        
        Ok(())
    }
    
    /// Remove unreachable blocks
    pub fn remove_unreachable_blocks(&mut self) {
        let reachable = self.depth_first_order();
        let reachable_set: HashSet<_> = reachable.into_iter().collect();
        
        let to_remove: Vec<_> = self.blocks.keys()
            .filter(|&&id| !reachable_set.contains(&id))
            .copied()
            .collect();
        
        for block_id in to_remove {
            self.blocks.remove(&block_id);
        }
    }
    
    /// Get iterator over blocks
    pub fn blocks(&self) -> impl Iterator<Item = (&BlockId, &BasicBlock)> {
        self.blocks.iter()
    }
    
    /// Get iterator over blocks in reverse post-order
    pub fn blocks_rpo(&self) -> BlockIterator {
        BlockIterator {
            blocks: &self.blocks,
            order: self.reverse_postorder(),
            index: 0,
        }
    }
}

impl<'a> Iterator for BlockIterator<'a> {
    type Item = (BlockId, &'a BasicBlock);
    
    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.order.len() {
            let block_id = self.order[self.index];
            self.index += 1;
            self.blocks.get(&block_id).map(|block| (block_id, block))
        } else {
            None
        }
    }
}

impl fmt::Display for BasicBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Block header
        if let Some(ref name) = self.name {
            writeln!(f, "bb{} ({}):", self.id, name)?;
        } else {
            writeln!(f, "bb{}:", self.id)?;
        }
        
        // Predecessors info
        if !self.predecessors.is_empty() {
            write!(f, "  ; preds: ")?;
            for (i, &pred) in self.predecessors.iter().enumerate() {
                if i > 0 { write!(f, ", ")?; }
                write!(f, "bb{}", pred)?;
            }
            writeln!(f)?;
        }
        
        // Instructions
        for instruction in &self.instructions {
            writeln!(f, "  {}", instruction)?;
        }
        
        Ok(())
    }
}

impl fmt::Display for ControlFlowGraph {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "function @{} {{", self.name)?;
        
        // Parameters
        if !self.parameters.is_empty() {
            write!(f, "  params: ")?;
            for (i, (name, value_id)) in self.parameters.iter().enumerate() {
                if i > 0 { write!(f, ", ")?; }
                write!(f, "%{} {}", value_id, name)?;
            }
            writeln!(f)?;
        }
        
        writeln!(f, "  entry: bb{}", self.entry_block)?;
        writeln!(f)?;
        
        // Blocks in reverse post-order for readability
        let order = self.reverse_postorder();
        for block_id in order {
            if let Some(block) = self.blocks.get(&block_id) {
                writeln!(f, "{}", block)?;
            }
        }
        
        writeln!(f, "}}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::instruction::{InstructionKind, IrType, IdGenerator};
    
    #[test]
    fn test_basic_block_creation() {
        let block = BasicBlock::new(0);
        assert_eq!(block.id, 0);
        assert!(block.instructions.is_empty());
        assert!(block.predecessors.is_empty());
        assert!(block.successors.is_empty());
        assert!(block.name.is_none());
    }
    
    #[test]
    fn test_cfg_creation() {
        let cfg = ControlFlowGraph::new(0, "test".to_string(), 0);
        assert_eq!(cfg.function_id, 0);
        assert_eq!(cfg.name, "test");
        assert_eq!(cfg.entry_block, 0);
        assert_eq!(cfg.blocks.len(), 1);
        assert!(cfg.blocks.contains_key(&0));
    }
    
    #[test]
    fn test_block_connections() {
        let mut cfg = ControlFlowGraph::new(0, "test".to_string(), 0);
        cfg.add_block(1);
        cfg.connect_blocks(0, 1);
        
        let block0 = cfg.get_block(0).unwrap();
        let block1 = cfg.get_block(1).unwrap();
        
        assert!(block0.successors.contains(&1));
        assert!(block1.predecessors.contains(&0));
    }
    
    #[test]
    fn test_terminator_detection() {
        let mut gen = IdGenerator::new();
        let mut block = BasicBlock::new(0);
        
        // Add non-terminator instruction
        let add_inst = crate::ir::instruction::Instruction::new(
            gen.next_value(),
            InstructionKind::Add { lhs: 0, rhs: 1 },
            IrType::I32,
            None,
        );
        block.add_instruction(add_inst);
        assert!(!block.has_terminator());
        
        // Add terminator instruction
        let ret_inst = crate::ir::instruction::Instruction::new(
            gen.next_value(),
            InstructionKind::Return { value: None },
            IrType::Unit,
            None,
        );
        block.add_instruction(ret_inst);
        assert!(block.has_terminator());
    }
    
    #[test]
    fn test_depth_first_order() {
        let mut cfg = ControlFlowGraph::new(0, "test".to_string(), 0);
        cfg.add_block(1);
        cfg.add_block(2);
        cfg.connect_blocks(0, 1);
        cfg.connect_blocks(0, 2);
        
        let order = cfg.depth_first_order();
        assert_eq!(order[0], 0); // Entry block should be first
        assert!(order.contains(&1));
        assert!(order.contains(&2));
    }
}