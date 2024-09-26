use std::marker::PhantomData;

use super::{context::Context, llvm_c::LLVMBasicBlockRef, module::Module, traits::WrappedReference};

#[derive(Clone)]
#[repr(transparent)]
pub struct BasicBlock<'c, 'm> {
	basic_block_ref: LLVMBasicBlockRef,
	phantom_data_context: PhantomData<&'c Context>,
	phantom_data_module: PhantomData<&'m Module<'c>>,
}

unsafe impl<'c, 'm> WrappedReference for BasicBlock<'c, 'm> {
	type RefType = LLVMBasicBlockRef;
}
