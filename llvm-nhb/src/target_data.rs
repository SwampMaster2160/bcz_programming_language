use std::marker::PhantomData;

use super::{context::Context, llvm_c::{LLVMIntPtrTypeInContext, LLVMTargetDataRef}, traits::WrappedReference, types::Type};

#[repr(transparent)]
pub struct TargetData<'a> {
	target_data_ref: LLVMTargetDataRef,
	phantom_data: PhantomData<&'a ()>
}

impl<'a> TargetData<'a> {
	#[inline]
	pub fn int_ptr_type(&self, context: &'a Context) -> Type<'a> {
		unsafe { Type::from_ref(LLVMIntPtrTypeInContext(context.get_ref(), self.target_data_ref)) }
	}
}

unsafe impl<'a> WrappedReference for TargetData<'a> {
	type RefType = LLVMTargetDataRef;
}