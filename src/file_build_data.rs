use std::collections::HashMap;

use crate::llvm::value::Value;

pub struct FileBuildData<'a, 'b> {
	//pub llvm_module: &'a Module<'a>,
	//pub llvm_builder: Builder<'a>,
	pub built_globals: HashMap<Box<str>, Value<'a, 'b>>,
	pub entrypoint: Option<Value<'a, 'b>>,
}