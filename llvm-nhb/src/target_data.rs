
use super::{context::Context, llvm_c::{LLVMIntPtrTypeInContext, LLVMTargetDataRef}, traits::WrappedReference, types::Type};

#[repr(transparent)]
pub struct TargetData {
	target_data_ref: LLVMTargetDataRef,
}

impl TargetData {
	#[inline]
	pub fn int_ptr_type<'a>(&self, context: &'a Context) -> Type<'a> {
		unsafe { Type::from_ref(LLVMIntPtrTypeInContext(context.get_ref(), self.target_data_ref)) }
	}
}

unsafe impl WrappedReference for TargetData {
	type RefType = LLVMTargetDataRef;
}