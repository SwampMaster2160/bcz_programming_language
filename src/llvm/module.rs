use std::{marker::PhantomData, mem::ManuallyDrop};

use super::llvm_c::{LLVMDisposeModule, LLVMDumpModule, LLVMModuleRef};

pub struct Module<'a> {
	module_ref: LLVMModuleRef,
	phantom_data: PhantomData<&'a ()>,
}

impl<'a> Module<'a> {
	/// Create a new module from a reference.
	///
	/// # Safety
	///
	/// The module reference must be valid.
	#[inline]
	pub unsafe fn from_ref(module_ref: LLVMModuleRef) -> Self {
		Self { module_ref, phantom_data: PhantomData::default() }
	}

	/// Get the module reference.
	#[inline]
	pub fn get_ref(&self) -> LLVMModuleRef {
		self.module_ref
	}

	/// Take the module reference.
	#[inline]
	pub fn take_ref(self) -> LLVMModuleRef {
		ManuallyDrop::new(self).module_ref
	}

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