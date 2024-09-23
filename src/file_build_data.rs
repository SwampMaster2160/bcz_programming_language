use std::collections::HashMap;

use crate::llvm::{llvm_c::LLVMBuilderRef, module::Module, value::Value};

pub struct FileBuildData<'a> {
	pub llvm_module: &'a Module<'a>,
	pub llvm_builder: LLVMBuilderRef,
	pub built_globals: HashMap<Box<str>, Value<'a>>,
	pub entrypoint: Option<Value<'a>>,
}