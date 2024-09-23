use crate::{llvm::{llvm_c::{LLVMBuildLoad2, LLVMBuildStore, LLVMBuilderRef}, traits::WrappedReference, value::Value}, MainData};

#[derive(Clone, Debug)]
pub enum BuiltLValue<'a> {
	AllocaVariable(Value<'a>),
}

impl<'a> BuiltLValue<'a> {
	pub fn get_value(&self, main_data: &MainData, llvm_builder: LLVMBuilderRef) -> Value<'static> {
		match self {
			Self::AllocaVariable(alloca_variable) => unsafe { Value::from_ref(LLVMBuildLoad2(llvm_builder, main_data.int_type.get_ref(), alloca_variable.get_ref(), c"alloca_read_temp".as_ptr() as *const u8)) },
		}
	}

	pub fn set_value(&self, _main_data: &MainData, llvm_builder: LLVMBuilderRef, value: Value) -> Value {
		match self {
			Self::AllocaVariable(alloca_variable) => unsafe { Value::from_ref(LLVMBuildStore(llvm_builder, value.get_ref(), alloca_variable.get_ref())) },
		}
	}
}