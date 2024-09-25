use std::marker::PhantomData;

use super::{context::Context, llvm_c::{LLVMDisposeModule, LLVMDumpModule, LLVMModuleRef}, traits::WrappedReference};

#[repr(transparent)]
pub struct Module<'a> {
	module_ref: LLVMModuleRef,
	phantom_data: PhantomData<&'a Context>,
}

unsafe impl<'a> WrappedReference for Module<'a> {
	type RefType = LLVMModuleRef;
}

impl<'a> Module<'a> {

	#[inline]
	pub fn dump(&self) {
		unsafe { LLVMDumpModule(self.module_ref) };
	}
}

impl<'a> Drop for Module<'a> {
	#[inline]
	fn drop(&mut self) {
		unsafe {
			LLVMDisposeModule(self.module_ref);
		}
	}
}