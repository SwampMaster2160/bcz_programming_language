use std::{marker::PhantomData, mem::ManuallyDrop};

use super::{llvm_c::{LLVMBuilderRef, LLVMDisposeBuilder}, traits::WrappedReference};

#[repr(transparent)]
pub struct Builder<'a> {
	builder_ref: LLVMBuilderRef,
	phantom_data: PhantomData<&'a ()>
}

impl<'a> WrappedReference<LLVMBuilderRef> for Builder<'a> {
	fn get_ref(&self) -> LLVMBuilderRef {
		self.builder_ref
	}

	unsafe fn from_ref(raw_ref: LLVMBuilderRef) -> Self {
		Self { builder_ref: raw_ref, phantom_data: PhantomData }
	}

	fn take_ref(self) -> LLVMBuilderRef {
		ManuallyDrop::new(self).builder_ref
	}
}

impl<'a> Drop for Builder<'a> {
	fn drop(&mut self) {
		unsafe { LLVMDisposeBuilder(self.builder_ref) };
	}
}