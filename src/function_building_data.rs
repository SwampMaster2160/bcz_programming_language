use std::collections::{HashMap, HashSet};

use llvm_nhb::{basic_block::BasicBlock, builder::Builder, types::Type, value::Value};

use crate::{built_value::BuiltLValue, MainData};

pub struct FunctionBuildData<'a, 'b> {
	pub function: Value<'a, 'a>,
	pub block_stack: &'b mut Vec<BlockLevel<'a>>,
	pub allocas_not_in_use: &'b mut HashSet<Value<'a, 'a>>,
	pub array_allocas_not_in_use: &'b mut HashMap<(Type<'a>, u64), HashSet<Value<'a, 'a>>>,
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
		for block in self.block_stack.iter_mut().rev() {
			if block.allocas_in_use.remove(&alloca) {
				self.allocas_not_in_use.insert(alloca);
				return;
			}
		}
		panic!()
	}

	pub fn get_array_alloca(&mut self, element_type: Type<'a>, element_count: u64, llvm_builder: &'a Builder<'a, 'a>, name: &str) -> Value<'a, 'a> {
		let key = (element_type, element_count);
		match self.array_allocas_not_in_use.get(&key).map(|available| available.iter().next()).flatten() {
			Some(alloca) => {
				let alloca = alloca.clone();
				self.array_allocas_not_in_use.get_mut(&key).unwrap().remove(&alloca);
				alloca
			}
			None => {
				llvm_builder.position_at_end(self.alloca_block);
				let alloca = element_type.array_type(element_count as usize).build_alloca(llvm_builder, name);
				let top_block = self.block_stack.last_mut().unwrap();
				let key = (element_type, element_count);
				let allocas_of_type = match top_block.array_allocas_in_use.get_mut(&key) {
					Some(allocas_of_type) => allocas_of_type,
					None => {
						top_block.array_allocas_in_use.insert(key, HashSet::new());
						top_block.array_allocas_in_use.get_mut(&key).unwrap()
					}
				};
				allocas_of_type.insert(alloca.clone());
				//top_block.array_allocas_in_use..insert(alloca.clone());
				llvm_builder.position_at_end(top_block.last_block());
				alloca
			}
		}
	}

	pub fn surrender_array_alloca(&mut self, alloca: Value<'a, 'a>) {
		for block in self.block_stack.iter_mut().rev() {
			for (array_type, arrays) in block.array_allocas_in_use.iter_mut() {
				if arrays.remove(&alloca) {
					let allocas_of_type = match self.array_allocas_not_in_use.get_mut(&array_type) {
						Some(allocas_of_type) => allocas_of_type,
						None => {
							self.array_allocas_not_in_use.insert(array_type.clone(), HashSet::new());
							self.array_allocas_not_in_use.get_mut(&array_type).unwrap()
						}
					};
					allocas_of_type.insert(alloca.clone());
					return;
				}
			}
		}
		panic!()
	}
}

pub struct BlockLevel<'a> {
	pub local_variables: HashMap<Box<str>, BuiltLValue<'a>>,
	pub basic_blocks: Vec<BasicBlock<'a, 'a>>,
	pub allocas_in_use: HashSet<Value<'a, 'a>>,
	pub array_allocas_in_use: HashMap<(Type<'a>, u64), HashSet<Value<'a, 'a>>>,
	//pub allocas: [Option<Value<'a, 'a>>; 5],
}

impl<'a> BlockLevel<'a> {
	pub fn last_block(&self) -> &BasicBlock<'a, 'a> {
		self.basic_blocks.last().unwrap()
	}
}