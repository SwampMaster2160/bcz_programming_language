use std::collections::HashMap;

use llvm_nhb::value::int::IntValue;

pub struct FileBuildData<'a, 'b> {
	pub built_globals: HashMap<Box<str>, IntValue<'a, 'b>>,
	pub entrypoint: Option<IntValue<'a, 'b>>,
}