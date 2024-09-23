use std::{ffi::c_uint, mem::{transmute, ManuallyDrop}};

use super::{llvm_c::{LLVMBool, LLVMFunctionType, LLVMGetTypeKind, LLVMGetUndef, LLVMTypeKind, LLVMTypeRef}, traits::WrappedReference, value::Value};

#[derive(Clone, Copy, Hash)]
pub struct Type {
	type_ref: LLVMTypeRef,
}

impl WrappedReference<LLVMTypeRef> for Type {
	#[inline]
	fn get_ref(&self) -> LLVMTypeRef {
		self.type_ref
	}

	#[inline]
	unsafe fn from_ref(raw_ref: LLVMTypeRef) -> Self {
		Self { type_ref: raw_ref }
	}

	#[inline]
	fn take_ref(self) -> LLVMTypeRef {
		ManuallyDrop::new(self).type_ref
	}
}

impl Type {
	/// Create a function type with `self` as the return type.
	///
	/// # Panics
	///
	/// Panics if the number of parameters exceeds `c_uint::MAX`.
	pub fn function_type(self, parameter_types: &[Self], is_variable_argument: bool) -> Self {
		if parameter_types.len() > c_uint::MAX as usize {
			panic!("Too many parameters");
		}
		// TODO: See what types this is valid for
		if !self.is_normal() {
			panic!("Invalid parameter type");
		}
		for parameter_type in parameter_types {
			if !parameter_type.is_normal(){
				panic!("Invalid parameter type");
			}
		}
		unsafe {
			Self::from_ref(LLVMFunctionType(self.get_ref(), transmute(parameter_types.as_ptr()), parameter_types.len() as c_uint, is_variable_argument as LLVMBool))
		}
	}

	/// Create an undefined value of this type.
	#[inline]
	pub fn undefined(self) -> Value<'static> {
		// TODO: See what types this is valid for
		if !self.is_normal() {
			panic!("Cannot create an undefined value of this type");
		}
		unsafe { self.undefined_unchecked() }
	}

	/// Create an undefined value of this type.
	#[inline]
	pub unsafe fn undefined_unchecked(self) -> Value<'static> {
		unsafe { Value::from_ref(LLVMGetUndef(self.get_ref())) }
	}

	#[inline]
	fn is_normal(self) -> bool {
		!matches!(
			self.type_kind(),
			LLVMTypeKind::LLVMFunctionTypeKind | LLVMTypeKind::LLVMLabelTypeKind | LLVMTypeKind::LLVMMetadataTypeKind | LLVMTypeKind::LLVMTargetExtTypeKind | LLVMTypeKind::LLVMTokenTypeKind,
		)
	}

	#[inline]
	pub(crate) fn type_kind(&self) -> LLVMTypeKind {
		unsafe { LLVMGetTypeKind(self.get_ref()) }
	}
}