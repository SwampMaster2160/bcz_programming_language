use super::llvm_c::LLVMTypeRef;

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
}