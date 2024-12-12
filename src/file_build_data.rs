use std::{collections::HashMap, path::PathBuf};

use llvm_nhb::value::Value;

use crate::built_value::BuiltRValue;

pub struct FileBuildData<'a, 'b> {
	pub built_globals: HashMap<Box<str>, BuiltRValue<'a>>,
	pub built_global_function_signatures: HashMap<Box<str>, Value<'a, 'b>>,
	pub entrypoint: Option<(Value<'a, 'b>, Box<str>)>,
	pub filepath: &'a PathBuf,
}