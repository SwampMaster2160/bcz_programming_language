use std::marker::PhantomData;

use crate::types::int::IntType;

use super::{context::Context, llvm_c::{LLVMIntPtrTypeInContext, LLVMTargetDataRef}, traits::WrappedReference};

#[repr(transparent)]
pub struct TargetData<'a> {
	target_data_ref: LLVMTargetDataRef,
	phantom_data: PhantomData<&'a ()>
}

impl<'a> TargetData<'a> {
	#[inline]
	pub fn int_ptr_type(&self, context: &'a Context) -> IntType<'a> {
		unsafe { IntType::from_ref(LLVMIntPtrTypeInContext(context.get_ref(), self.target_data_ref)) }
	}
}

unsafe impl<'a> WrappedReference for TargetData<'a> {
	type RefType = LLVMTargetDataRef;
}