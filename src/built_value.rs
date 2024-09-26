use crate::{llvm::{builder::Builder, value::Value}, MainData};

#[derive(Clone, Debug)]
pub enum BuiltLValue<'a> {
	AllocaVariable(Value<'a, 'a>),
}

impl<'a> BuiltLValue<'a> {
	pub fn get_value(&self, main_data: &MainData<'a>, llvm_builder: &Builder<'a, 'a>) -> Value<'a, 'a> {
		match self {
			Self::AllocaVariable(alloca_variable) => alloca_variable.build_load(main_data.int_type, llvm_builder, "alloca_read_temp"),
		}
	}

	pub fn set_value(&self, _main_data: &MainData, llvm_builder: &Builder<'a, 'a>, value: &Value<'a, 'a>) -> Value {
		match self {
			Self::AllocaVariable(alloca_variable) => alloca_variable.build_store(value, llvm_builder),
		}
	}
}