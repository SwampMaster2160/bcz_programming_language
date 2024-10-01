use std::mem::{transmute_copy, ManuallyDrop};

/// For types that wrap a raw pointer
///
/// # Safety
///
/// Any type this trait is implemented for must contain the type set for `RefType` and be `#[repr(transparent)]`.
pub unsafe trait WrappedReference where Self: Sized {
	type RefType: Sized;

	/// Get the raw LLVM reference that is wrapped by this object.
	#[inline]
	fn get_ref(&self) -> Self::RefType {
		unsafe { transmute_copy(self) }
	}

	/// Create a new object from a raw LLVM reference.
	///
	/// # Safety
	///
	/// The raw reference must be valid.
	#[inline]
	unsafe fn from_ref(raw_ref: Self::RefType) -> Self {
		unsafe { transmute_copy(&raw_ref) }
	}

	/// Destroy the wrapper object by taking the raw LLVM reference and do not call any destructors.
	#[inline]
	fn take_ref(self) -> Self::RefType {
		ManuallyDrop::new(self).get_ref()
	}
}