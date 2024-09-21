use std::{marker::PhantomData, mem::ManuallyDrop};

use super::llvm_c::LLVMValueRef;

pub struct Value<'a> {
	value_ref: LLVMValueRef,
	phantom_data: PhantomData<&'a ()>,
}

impl<'a> Value<'a> {
	/// Get the raw LLVM reference for the LLVM value used for LLVM-C function.
	#[inline]
	pub fn get_ref(&self) -> LLVMValueRef {
		self.value_ref
	}

	/// Destroy the `Value` object by taking the raw LLVM value reference used for LLVM-C functions and do not dispose of the raw reference.
	#[inline]
	pub fn take_ref(self) -> LLVMValueRef {
		ManuallyDrop::new(self).value_ref
	}

	/// Construct a `Value` object from a raw LLVM value reference.
	///
	/// # Safety
	///
	/// The raw value reference must be valid.
	#[inline]
	pub unsafe fn from_ref(value_ref: LLVMValueRef) -> Self {
		Self { value_ref, phantom_data: PhantomData::default() }
	}
}