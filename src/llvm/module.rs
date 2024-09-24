use std::{marker::PhantomData, mem::ManuallyDrop};

use super::{llvm_c::{LLVMDisposeModule, LLVMDumpModule, LLVMModuleRef}, traits::WrappedReference};

#[repr(transparent)]
pub struct Module<'a> {
	module_ref: LLVMModuleRef,
	phantom_data: PhantomData<&'a ()>,
}

impl<'a> WrappedReference<LLVMModuleRef> for Module<'a> {
	#[inline]
	fn get_ref(&self) -> LLVMModuleRef {
		self.module_ref
	}

	#[inline]
	unsafe fn from_ref(raw_ref: LLVMModuleRef) -> Self {
		Self { module_ref: raw_ref, phantom_data: PhantomData::default() }
	}

	#[inline]
	fn take_ref(self) -> LLVMModuleRef {
		ManuallyDrop::new(self).module_ref
	}
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