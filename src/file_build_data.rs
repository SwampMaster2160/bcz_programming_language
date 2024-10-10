use std::collections::HashMap;

use llvm_nhb::value::Value;

pub struct FileBuildData<'a, 'b> {
	pub built_globals: HashMap<Box<str>, Value<'a, 'b>>,
	pub entrypoint: Option<Value<'a, 'b>>,
}