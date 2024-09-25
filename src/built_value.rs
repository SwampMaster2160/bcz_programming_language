use crate::{llvm::{builder::Builder, llvm_c::{LLVMBuildLoad2, LLVMBuildStore}, traits::WrappedReference, value::Value}, MainData};

#[derive(Clone, Debug)]
pub enum BuiltLValue<'a> {
	AllocaVariable(Value<'a, 'a>),
}

impl<'a> BuiltLValue<'a> {
	pub fn get_value(&self, main_data: &MainData, llvm_builder: &Builder) -> Value<'a, 'a> {
		match self {
			Self::AllocaVariable(alloca_variable) => unsafe { Value::from_ref(LLVMBuildLoad2(llvm_builder.get_ref(), main_data.int_type.get_ref(), alloca_variable.get_ref(), c"alloca_read_temp".as_ptr() as *const u8)) },
		}
	}

	pub fn set_value(&self, _main_data: &MainData, llvm_builder: &Builder, value: Value) -> Value {
		match self {
			Self::AllocaVariable(alloca_variable) => unsafe { Value::from_ref(LLVMBuildStore(llvm_builder.get_ref(), value.get_ref(), alloca_variable.get_ref())) },
		}
	}
}