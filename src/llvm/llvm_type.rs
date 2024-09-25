use std::{ffi::c_uint, fmt::Debug, iter::repeat, marker::PhantomData, mem::{transmute, MaybeUninit}};

use super::{context::Context, llvm_c::{LLVMBool, LLVMCountParamTypes, LLVMFunctionType, LLVMGetParamTypes, LLVMGetReturnType, LLVMGetTypeKind, LLVMGetUndef, LLVMIsFunctionVarArg, LLVMTypeKind, LLVMTypeRef}, traits::WrappedReference, value::Value};

#[derive(Clone, Copy, Hash, PartialEq)]
#[repr(transparent)]
pub struct Type<'a> {
	type_ref: LLVMTypeRef,
	phantom_data: PhantomData<&'a Context>
}

unsafe impl<'a> WrappedReference for Type<'a> {
	type RefType = LLVMTypeRef;
}

impl<'a> Type<'a> {
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
	pub fn undefined(self) -> Value<'a, 'a> {
		// TODO: See what types this is valid for
		if !self.is_normal() {
			panic!("Cannot create an undefined value of this type");
		}
		unsafe { self.undefined_unchecked() }
	}

	/// Create an undefined value of this type.
	#[inline]
	pub unsafe fn undefined_unchecked(self) -> Value<'a, 'a> {
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

	pub fn parameter_count(&self) -> usize {
		match self.type_kind() {
			LLVMTypeKind::LLVMFunctionTypeKind => unsafe { LLVMCountParamTypes(self.get_ref()) as usize },
			other => panic!("Type is not a function type: {:?}", other),
		}
	}

	pub fn get_return_type(&self) -> Self {
		match self.type_kind() {
			LLVMTypeKind::LLVMFunctionTypeKind => unsafe { Type::from_ref(LLVMGetReturnType(self.get_ref())) },
			other => panic!("Type is not a function type: {:?}", other),
		}
	}

	pub fn is_variadic(&self) -> bool {
		match self.type_kind() {
			LLVMTypeKind::LLVMFunctionTypeKind => unsafe { LLVMIsFunctionVarArg(self.get_ref()) != 0 },
			other => panic!("Type is not a function type: {:?}", other),
		}
	}

	pub fn parameter_types(&self) -> Box<[Self]> {
		let parameter_count = self.parameter_count();
		let mut parameters: Box<[MaybeUninit<Type>]> = repeat(MaybeUninit::uninit()).take(parameter_count).collect();
		unsafe { LLVMGetParamTypes(self.type_ref, transmute(parameters.as_mut_ptr())) };
		unsafe { transmute(parameters) }
	}
}

impl<'a> Debug for Type<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.type_kind().fmt(f)
	}
}