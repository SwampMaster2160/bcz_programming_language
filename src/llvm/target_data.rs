use std::marker::PhantomData;

use super::{llvm_c::LLVMTargetDataRef, target_machine::TargetMachine, traits::WrappedReference};

#[repr(transparent)]
pub struct TargetData<'a> {
	module_ref: LLVMTargetDataRef,
	phantom_data_machine: PhantomData<&'a TargetMachine>,
}

unsafe impl<'a> WrappedReference for TargetData<'a> {
	type RefType = LLVMTargetDataRef;
}