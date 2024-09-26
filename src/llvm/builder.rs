use std::marker::PhantomData;

use super::{context::Context, llvm_c::{LLVMBuilderRef, LLVMDisposeBuilder}, module::Module, traits::WrappedReference};

#[repr(transparent)]
pub struct Builder<'c, 'm> {
	builder_ref: LLVMBuilderRef,
	phantom_data_context: PhantomData<&'c Context>,
	phantom_data_module: PhantomData<&'m Module<'c>>,
}

unsafe impl<'c, 'm> WrappedReference for Builder<'c, 'm> {
	type RefType = LLVMBuilderRef;
}

impl<'c, 'm> Drop for Builder<'c, 'm> {
	fn drop(&mut self) {
		unsafe { LLVMDisposeBuilder(self.builder_ref) };
	}
}