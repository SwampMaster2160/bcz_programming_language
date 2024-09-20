use std::{ffi::c_uint, mem::transmute};

use super::llvm_c::{LLVMBool, LLVMFunctionType, LLVMTypeRef};

#[derive(Clone, Copy, Hash)]
pub struct Type {
	type_ref: LLVMTypeRef,
}

impl Type {
	/// Create a new type from a reference.
	///
	/// # Safety
	///
	/// The type reference must be valid.
	#[inline]
	pub unsafe fn from_ref(type_ref: LLVMTypeRef) -> Self {
		Self { type_ref }
	}

	/// Get the type reference.
	#[inline]
	pub fn get_ref(self) -> LLVMTypeRef {
		self.type_ref
	}

	/// Create a function type with `self` as the return type.
	///
	/// # Panics
	///
	/// Panics if the number of parameters exceeds `c_uint::MAX`.
	#[inline]
	pub fn function_type(self, parameter_types: &[Self], is_variable_argument: bool) -> Self {
		if parameter_types.len() > c_uint::MAX as usize {
			panic!("Too many parameters");
		}
		unsafe {
			Self::from_ref(LLVMFunctionType(self.get_ref(), transmute(parameter_types.as_ptr()), parameter_types.len() as c_uint, is_variable_argument as LLVMBool))
		}
	}
}