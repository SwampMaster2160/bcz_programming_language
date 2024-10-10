use crate::MainData;
use llvm_nhb::{builder::Builder, value::{int::IntValue, value::Value}};

#[derive(Clone, Debug)]
pub enum BuiltLValue<'a> {
	AllocaVariable(Value<'a, 'a>),
	DereferencedPointer(Value<'a, 'a>),
}

impl<'a> BuiltLValue<'a> {
	pub fn get_value(&self, main_data: &MainData<'a>, llvm_builder: &Builder<'a, 'a>) -> IntValue<'a, 'a> {
		match self {
			Self::AllocaVariable(alloca_variable) =>
				alloca_variable.build_load(main_data.int_type.as_type(), llvm_builder, "alloca_read_temp").try_into().unwrap(),
			Self::DereferencedPointer(pointer) =>
				pointer.build_load(main_data.int_type.as_type(), llvm_builder, "alloca_read_temp").try_into().unwrap(),
		}
	}

	pub fn get_pointer(&self, _main_data: &MainData<'a>, _llvm_builder: &Builder<'a, 'a>) -> Value<'a, 'a> {
		match self {
			Self::AllocaVariable(alloca_variable) => alloca_variable.clone(),
			Self::DereferencedPointer(pointer) => pointer.clone(),
		}
	}

	pub fn set_value(&self, _main_data: &MainData, llvm_builder: &Builder<'a, 'a>, value: &IntValue<'a, 'a>) -> Value {
		match self {
			Self::AllocaVariable(alloca_variable) => alloca_variable.build_store(&(value.as_value().into()), llvm_builder),
			Self::DereferencedPointer(pointer) => pointer.build_store(&(value.as_value().into()), llvm_builder),
		}
	}
}