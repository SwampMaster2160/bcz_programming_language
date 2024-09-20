use std::{iter::once, mem::ManuallyDrop};

use super::{llvm_c::{LLVMContextCreate, LLVMContextDispose, LLVMContextRef, LLVMInt128TypeInContext, LLVMInt16TypeInContext, LLVMInt1TypeInContext, LLVMInt32TypeInContext, LLVMInt64TypeInContext, LLVMInt8TypeInContext, LLVMModuleCreateWithNameInContext, LLVMVoidTypeInContext}, llvm_type::Type, module::Module};

#[allow(non_upper_case_globals)]
static mut context_exists_in_this_thread: bool = false;

pub struct Context {
	context_ref: LLVMContextRef,
}

impl Context {
	/// Get the raw LLVM context reference used for LLVM-C functions.
	#[inline]
	pub fn get_ref(&self) -> LLVMContextRef {
		self.context_ref
	}

	/// Destroy the `Context` object by taking the raw LLVM context reference used for LLVM-C functions and do not dispose of the raw reference.
	#[inline]
	pub fn take_ref(self) -> LLVMContextRef {
		ManuallyDrop::new(self).context_ref
	}

	/// Construct a `Context` object from a raw LLVM context reference.
	///
	/// # Safety
	///
	/// The raw context reference must be valid.
	#[inline]
	pub unsafe fn from_ref(context_ref: LLVMContextRef) -> Self {
		Self { context_ref }
	}

	/// Create an LLVM context for this thread.
	///
	/// # Safety
	///
	/// There should not more than one LLVM context active per thread at a time.
	#[inline]
	pub unsafe fn new_unchecked() -> Self {
		Self { context_ref: LLVMContextCreate() }
	}

	/// Create an LLVM context for this thread.
	///
	/// # Panics
	///
	/// Panics if there is already an LLVM context active for this thread.
	#[inline]
	pub fn new() -> Self {
		unsafe {
			assert!(!context_exists_in_this_thread, "There should not more than one LLVM context active per thread at a time.");
			context_exists_in_this_thread = true;
			Self::new_unchecked()
		}
	}

	#[inline]
	pub fn new_module<'a>(&'a self, name: &str) -> Module<'a> {
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		unsafe { Module::from_ref(LLVMModuleCreateWithNameInContext(name.as_ptr(), self.context_ref)) }
	}

	#[inline]
	pub fn void_type(&self) -> Type {
		unsafe { Type::from_ref(LLVMVoidTypeInContext(self.context_ref)) }
	}

	#[inline]
	pub fn int_1_type(&self) -> Type {
		unsafe { Type::from_ref(LLVMInt1TypeInContext(self.context_ref)) }
	}

	#[inline]
	pub fn int_8_type(&self) -> Type {
		unsafe { Type::from_ref(LLVMInt8TypeInContext(self.context_ref)) }
	}

	#[inline]
	pub fn int_16_type(&self) -> Type {
		unsafe { Type::from_ref(LLVMInt16TypeInContext(self.context_ref)) }
	}

	#[inline]
	pub fn int_32_type(&self) -> Type {
		unsafe { Type::from_ref(LLVMInt32TypeInContext(self.context_ref)) }
	}

	#[inline]
	pub fn int_64_type(&self) -> Type {
		unsafe { Type::from_ref(LLVMInt64TypeInContext(self.context_ref)) }
	}

	#[inline]
	pub fn int_128_type(&self) -> Type {
		unsafe { Type::from_ref(LLVMInt128TypeInContext(self.context_ref)) }
	}
}

impl Drop for Context {
	#[inline]
	fn drop(&mut self) {
		unsafe {
			LLVMContextDispose(self.context_ref);
			context_exists_in_this_thread = false;
		}
	}
}