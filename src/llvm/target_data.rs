
use super::{llvm_c::LLVMTargetDataRef, traits::WrappedReference};

#[repr(transparent)]
pub struct TargetData {
	module_ref: LLVMTargetDataRef,
}

unsafe impl WrappedReference for TargetData {
	type RefType = LLVMTargetDataRef;
}