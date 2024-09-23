use std::{marker::PhantomData, mem::ManuallyDrop};

use super::{llvm_c::LLVMValueRef, traits::WrappedReference};

#[derive(Clone, Debug)]
pub struct Value<'a> {
	value_ref: LLVMValueRef,
	phantom_data: PhantomData<&'a ()>,
}

impl<'a> WrappedReference<LLVMValueRef> for Value<'a> {
	#[inline]
	fn get_ref(&self) -> LLVMValueRef {
		self.value_ref
	}

	#[inline]
	unsafe fn from_ref(raw_ref: LLVMValueRef) -> Self {
		Self { value_ref: raw_ref, phantom_data: PhantomData::default() }
	}

	#[inline]
	fn take_ref(self) -> LLVMValueRef {
		ManuallyDrop::new(self).value_ref
	}
}

impl<'a> Value<'a> {
	
}