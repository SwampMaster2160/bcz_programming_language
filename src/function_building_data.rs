use std::collections::HashMap;

use llvm_nhb::{basic_block::BasicBlock, value::Value};

use crate::built_value::BuiltLValue;

pub struct FunctionBuildData<'a, 'b> {
	pub function: Value<'a, 'a>,
	pub block_stack: &'b mut Vec<BlockLevel<'a>>,
}

pub struct BlockLevel<'a> {
	pub local_variables: HashMap<Box<str>, BuiltLValue<'a>>,
	pub basic_blocks: Vec<BasicBlock<'a, 'a>>,
	//pub allocas: [Option<Value<'a, 'a>>; 5],
}

impl<'a> BlockLevel<'a> {
	pub fn last_block(&self) -> &BasicBlock<'a, 'a> {
		self.basic_blocks.last().unwrap()
	}
}