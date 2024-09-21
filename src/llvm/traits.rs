pub trait WrappedReference<T> {
	/// Get the raw LLVM reference that is wrapped by this object.
	fn get_ref(&self) -> T;
	/// Create a new object from a raw LLVM reference.
	///
	/// # Safety
	///
	/// The raw reference must be valid.
	unsafe fn from_ref(raw_ref: T) -> Self;
	/// Destroy the wrapper object by taking the raw LLVM reference and do not call any destructors.
	fn take_ref(self) -> T;
}