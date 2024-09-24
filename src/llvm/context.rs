use std::iter::once;

use super::{builder::Builder, llvm_c::{LLVMContextCreate, LLVMContextDispose, LLVMContextRef, LLVMCreateBuilderInContext, LLVMInt128TypeInContext, LLVMInt16TypeInContext, LLVMInt1TypeInContext, LLVMInt32TypeInContext, LLVMInt64TypeInContext, LLVMInt8TypeInContext, LLVMModuleCreateWithNameInContext, LLVMVoidTypeInContext}, llvm_type::Type, module::Module, traits::WrappedReference};

#[allow(non_upper_case_globals)]
static mut context_exists_in_this_thread: bool = false;

#[repr(transparent)]
pub struct Context {
	context_ref: LLVMContextRef,
}

unsafe impl WrappedReference for Context {
	type RefType = LLVMContextRef;
}

impl Context {
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

	#[inline]
	pub fn new_builder<'a>(&'a self) -> Builder<'a> {
		unsafe { Builder::from_ref(LLVMCreateBuilderInContext(self.context_ref)) }
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