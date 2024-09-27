use super::{llvm_c::LLVMTargetMachineRef, traits::WrappedReference};

#[repr(transparent)]
pub struct TargetMachine {
	module_ref: LLVMTargetMachineRef
}

unsafe impl WrappedReference for TargetMachine {
	type RefType = LLVMTargetMachineRef;
}