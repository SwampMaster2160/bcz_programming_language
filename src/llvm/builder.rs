use std::marker::PhantomData;

use super::{basic_block::BasicBlock, context::Context, llvm_c::{LLVMBuilderRef, LLVMDisposeBuilder, LLVMPositionBuilderAtEnd}, module::Module, traits::WrappedReference};

#[repr(transparent)]
pub struct Builder<'c, 'm> {
	builder_ref: LLVMBuilderRef,
	phantom_data_context: PhantomData<&'c Context>,
	phantom_data_module: PhantomData<&'m Module<'c>>,
}

unsafe impl<'c, 'm> WrappedReference for Builder<'c, 'm> {
	type RefType = LLVMBuilderRef;
}

impl<'c, 'm> Builder<'c, 'm> {
	pub fn position_at_end(&self, position_at_end_of: &BasicBlock<'c, 'm>) {
		unsafe { LLVMPositionBuilderAtEnd(self.builder_ref, position_at_end_of.get_ref()) };
	}
}

impl<'c, 'm> Drop for Builder<'c, 'm> {
	fn drop(&mut self) {
		unsafe { LLVMDisposeBuilder(self.builder_ref) };
	}
}