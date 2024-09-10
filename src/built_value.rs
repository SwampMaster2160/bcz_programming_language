use crate::{llvm_c::{LLVMBuildLoad2, LLVMBuildPtrToInt, LLVMBuilderRef, LLVMGetInitializer, LLVMValueRef}, MainData};

#[derive(Clone, Debug)]
pub enum BuiltValue {
	NumericalValue(LLVMValueRef),
	GlobalVariable(LLVMValueRef),
	Function(LLVMValueRef),
	AllocaVariable(LLVMValueRef),
}

impl BuiltValue {
	pub fn get_value(&self, main_data: &MainData, llvm_builder: LLVMBuilderRef) -> LLVMValueRef {
		match self {
			Self::NumericalValue(value) => *value,
			Self::GlobalVariable(global_variable) => unsafe { LLVMGetInitializer(*global_variable) },
			Self::Function(function_pointer) => unsafe { LLVMBuildPtrToInt(llvm_builder, *function_pointer, main_data.int_type, c"fn_ptr_to_int_temp".as_ptr() as *const u8) },
			Self::AllocaVariable(alloca_variable) => unsafe { LLVMBuildLoad2(llvm_builder, main_data.int_type, *alloca_variable, c"alloca_read_temp".as_ptr() as *const u8) },
		}
	}
}