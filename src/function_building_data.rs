use std::collections::{HashMap, HashSet};

use llvm_nhb::{basic_block::BasicBlock, builder::Builder, value::Value};

use crate::{built_value::BuiltLValue, MainData};

pub struct FunctionBuildData<'a, 'b> {
	pub function: Value<'a, 'a>,
	pub block_stack: &'b mut Vec<BlockLevel<'a>>,
	pub allocas_not_in_use: &'b mut HashSet<Value<'a, 'a>>,
	pub alloca_block: &'b BasicBlock<'a, 'a>,
}

impl<'a, 'b> FunctionBuildData<'a, 'b> {
	pub fn get_alloca(&mut self, main_data: &MainData<'a>, llvm_builder: &'a Builder<'a, 'a>, name: &str) -> Value<'a, 'a> {
		match self.allocas_not_in_use.iter().next() {
			Some(alloca) => {
				let alloca = alloca.clone();
				self.allocas_not_in_use.remove(&alloca);
				alloca
			}
			None => {
				llvm_builder.position_at_end(self.alloca_block);
				let alloca = main_data.int_type.build_alloca(llvm_builder, name);
				let top_block = self.block_stack.last_mut().unwrap();
				top_block.allocas_in_use.insert(alloca.clone());
				llvm_builder.position_at_end(top_block.last_block());
				alloca
			}
		}
	}

	pub fn surrender_alloca(&mut self, alloca: Value<'a, 'a>) {
		let top_block = self.block_stack.last_mut().unwrap();
		top_block.allocas_in_use.remove(&alloca);
		self.allocas_not_in_use.insert(alloca);
	}
}

pub struct BlockLevel<'a> {
	pub local_variables: HashMap<Box<str>, BuiltLValue<'a>>,
	pub basic_blocks: Vec<BasicBlock<'a, 'a>>,
	pub allocas_in_use: HashSet<Value<'a, 'a>>,
	//pub allocas: [Option<Value<'a, 'a>>; 5],
}

impl<'a> BlockLevel<'a> {
	pub fn last_block(&self) -> &BasicBlock<'a, 'a> {
		self.basic_blocks.last().unwrap()
	}
}