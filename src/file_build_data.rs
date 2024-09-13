use std::collections::HashMap;

use crate::{built_value::BuiltRValue, llvm_c::{LLVMBuilderRef, LLVMModuleRef}};

pub struct FileBuildData {
	pub llvm_module: LLVMModuleRef,
	pub llvm_builder: LLVMBuilderRef,
	pub built_globals: HashMap<Box<str>, BuiltRValue>
}