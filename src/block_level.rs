use std::collections::HashMap;

use llvm_nhb::basic_block::BasicBlock;

use crate::built_value::BuiltLValue;

pub struct BlockLevel<'a> {
	pub local_variables: HashMap<Box<str>, BuiltLValue<'a>>,
	pub basic_blocks: Vec<BasicBlock<'a, 'a>>,
}

impl<'a> BlockLevel<'a> {
	pub fn last_block(&self) -> &BasicBlock<'a, 'a> {
		self.basic_blocks.last().unwrap()
	}
}