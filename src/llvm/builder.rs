use std::marker::PhantomData;

use super::{llvm_c::{LLVMBuilderRef, LLVMDisposeBuilder}, traits::WrappedReference};

#[repr(transparent)]
pub struct Builder<'a> {
	builder_ref: LLVMBuilderRef,
	phantom_data: PhantomData<&'a ()>
}

unsafe impl<'a> WrappedReference for Builder<'a> {
	type RefType = LLVMBuilderRef;
}

impl<'a> Drop for Builder<'a> {
	fn drop(&mut self) {
		unsafe { LLVMDisposeBuilder(self.builder_ref) };
	}
}