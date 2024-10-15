use std::{ffi::CString, marker::PhantomData};

use crate::llvm_c::LLVMInsertBasicBlockInContext;

use super::{context::Context, llvm_c::LLVMBasicBlockRef, module::Module, traits::WrappedReference};

#[derive(Clone)]
#[repr(transparent)]
pub struct BasicBlock<'c, 'm> {
	basic_block_ref: LLVMBasicBlockRef,
	phantom_data_context: PhantomData<&'c Context>,
	phantom_data_module: PhantomData<&'m Module<'c>>,
}

impl<'c, 'm> BasicBlock<'c, 'm> {
	pub fn insert_basic_block_before(&self, context: &'c Context, name: &str) -> BasicBlock<'c, 'm> {
		let name = CString::new(name).unwrap();
		unsafe { BasicBlock::from_ref(LLVMInsertBasicBlockInContext(context.get_ref(), self.basic_block_ref, name.as_ptr())) }
	}
}

unsafe impl<'c, 'm> WrappedReference for BasicBlock<'c, 'm> {
	type RefType = LLVMBasicBlockRef;
}
