use crate::{llvm_c::{LLVMBuildLoad2, LLVMBuildPtrToInt, LLVMBuildStore, LLVMBuilderRef, LLVMGetInitializer, LLVMValueRef}, MainData};

#[derive(Clone, Debug)]
pub enum BuiltRValue {
	NumericalValue(LLVMValueRef),
	GlobalVariable(LLVMValueRef),
	Function(LLVMValueRef),
	AllocaVariable(LLVMValueRef),
}

impl BuiltRValue {
	pub fn get_value(&self, main_data: &MainData, llvm_builder: LLVMBuilderRef) -> LLVMValueRef {
		match self {
			Self::NumericalValue(value) => *value,
			Self::GlobalVariable(global_variable) => unsafe { LLVMGetInitializer(*global_variable) },
			Self::Function(function_pointer) => unsafe { LLVMBuildPtrToInt(llvm_builder, *function_pointer, main_data.int_type, c"fn_ptr_to_int_temp".as_ptr() as *const u8) },
			Self::AllocaVariable(alloca_variable) => unsafe { LLVMBuildLoad2(llvm_builder, main_data.int_type, *alloca_variable, c"alloca_read_temp".as_ptr() as *const u8) },
		}
	}
}


#[derive(Clone, Debug)]
pub enum BuiltLValue {
	AllocaVariable(LLVMValueRef),
}

impl BuiltLValue {
	pub fn get_value(&self, _main_data: &MainData, _llvm_builder: LLVMBuilderRef) -> BuiltRValue {
		match self {
			Self::AllocaVariable(alloca_variable) => BuiltRValue::AllocaVariable(*alloca_variable),
		}
	}

	pub fn set_value(&self, main_data: &MainData, llvm_builder: LLVMBuilderRef, value: BuiltRValue) -> LLVMValueRef {
		match self {
			Self::AllocaVariable(alloca_variable) => unsafe { LLVMBuildStore(llvm_builder, value.get_value(main_data, llvm_builder), *alloca_variable) },
		}
	}
}