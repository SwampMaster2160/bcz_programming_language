use crate::llvm_c::{LLVMGetInitializer, LLVMValueRef};

#[derive(Clone, Debug)]
pub enum BuiltValue {
	NumericalValue(LLVMValueRef),
	GlobalVariable(LLVMValueRef),
	Function(LLVMValueRef),
	AllocaVariable(LLVMValueRef),
}

impl BuiltValue {
	pub fn get_value(&self) -> LLVMValueRef {
		match self {
			Self::NumericalValue(value) => *value,
			Self::GlobalVariable(global_variable) => unsafe { LLVMGetInitializer(*global_variable) },
			_ => todo!(),
		}
	}
}