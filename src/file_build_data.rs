use std::collections::HashMap;

use crate::{built_value::BuiltRValue, llvm::{llvm_c::LLVMBuilderRef, module::Module}};

pub struct FileBuildData<'a> {
	pub llvm_module: &'a Module<'a>,
	pub llvm_builder: LLVMBuilderRef,
	pub built_globals: HashMap<Box<str>, BuiltRValue>,
	pub entrypoint: Option<BuiltRValue>,
}