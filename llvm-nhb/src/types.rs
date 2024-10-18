use std::{ffi::{c_uint, CString}, fmt::Debug, iter::repeat, marker::PhantomData, mem::{transmute, MaybeUninit}};

use crate::llvm_c::LLVMArrayType2;

use super::{builder::Builder, context::Context, target_data::TargetData, traits::WrappedReference, value::Value};
use super::llvm_c::{LLVMBool, LLVMBuildAlloca, LLVMConstInt, LLVMCountParamTypes, LLVMFunctionType, LLVMGetParamTypes, LLVMGetReturnType};
use super::llvm_c::{LLVMGetTypeKind, LLVMGetUndef, LLVMIsFunctionVarArg, LLVMPointerType, LLVMSizeOfTypeInBits, LLVMTypeKind, LLVMTypeRef};

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
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
		if !self.is_normal() && self.type_kind() != LLVMTypeKind::LLVMVoidTypeKind {
			panic!("Invalid parameter type {:?}", self);
		}
		for parameter_type in parameter_types {
			if !parameter_type.is_normal(){
				panic!("Invalid parameter type");
			}
		}
		unsafe {
			Self::from_ref(LLVMFunctionType(
				self.get_ref(),
				transmute(parameter_types.as_ptr()),
				parameter_types.len() as c_uint,
				is_variable_argument as LLVMBool,
			))
		}
	}

	pub fn array_type(self, count: usize) -> Self {
		if !self.is_normal() {
			panic!("Cannot create an array type of this type: {self:?}");
		}
		unsafe { Self::from_ref(LLVMArrayType2(self.type_ref, count.try_into().unwrap())) }
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
	pub(crate) fn is_normal(self) -> bool {
		!matches!(
			self.type_kind(),
			LLVMTypeKind::LLVMFunctionTypeKind | LLVMTypeKind::LLVMLabelTypeKind | LLVMTypeKind::LLVMMetadataTypeKind |
			LLVMTypeKind::LLVMTargetExtTypeKind | LLVMTypeKind::LLVMTokenTypeKind | LLVMTypeKind::LLVMVoidTypeKind,
		)
	}

	#[inline]
	pub fn is_void(self) -> bool {
		self.type_kind() == LLVMTypeKind::LLVMVoidTypeKind
	}

	#[inline]
	pub(crate) fn type_kind(self) -> LLVMTypeKind {
		unsafe { LLVMGetTypeKind(self.get_ref()) }
	}

	pub fn parameter_count(self) -> usize {
		match self.type_kind() {
			LLVMTypeKind::LLVMFunctionTypeKind => unsafe { LLVMCountParamTypes(self.get_ref()) as usize },
			other => panic!("Type is not a function type: {:?}", other),
		}
	}

	pub fn get_return_type(self) -> Self {
		match self.type_kind() {
			LLVMTypeKind::LLVMFunctionTypeKind => unsafe { Type::from_ref(LLVMGetReturnType(self.get_ref())) },
			other => panic!("Type is not a function type: {:?}", other),
		}
	}

	pub fn is_variadic(self) -> bool {
		match self.type_kind() {
			LLVMTypeKind::LLVMFunctionTypeKind => unsafe { LLVMIsFunctionVarArg(self.get_ref()) != 0 },
			other => panic!("Type is not a function type: {:?}", other),
		}
	}

	pub fn parameter_types(self) -> Box<[Self]> {
		let parameter_count = self.parameter_count();
		let mut parameters: Box<[MaybeUninit<Type>]> = repeat(MaybeUninit::uninit()).take(parameter_count).collect();
		unsafe { LLVMGetParamTypes(self.type_ref, transmute(parameters.as_mut_ptr())) };
		unsafe { transmute(parameters) }
	}

	#[inline]
	pub fn pointer_to(self) -> Self {
		unsafe { Type::from_ref(LLVMPointerType(self.type_ref, 0)) }
	}

	pub fn const_int(self, value: u128, sign_extend: bool) -> Value<'a, 'a> {
		match self.type_kind() {
			LLVMTypeKind::LLVMIntegerTypeKind => {},
			other => panic!("Type is not an integer type: {other:?}"),
		}
		unsafe { Value::from_ref(LLVMConstInt(self.type_ref, value.try_into().unwrap(), sign_extend as LLVMBool)) }
	}

	pub fn build_alloca<'m>(self, builder: &Builder<'a, 'm>, name: &str) -> Value<'a, 'm> {
		if !self.is_normal() {
			panic!("Invalid type");
		}
		let name = CString::new(name).unwrap();
		unsafe { Value::from_ref(LLVMBuildAlloca(builder.get_ref(), self.type_ref, name.as_ptr())) }
	}

	pub fn size_in_bits(&self, target_data: &TargetData) -> u128 {
		if !self.is_normal() {
			panic!("Invalid type");
		}
		unsafe { LLVMSizeOfTypeInBits(target_data.get_ref(), self.type_ref).try_into().unwrap() }
	}
}

impl<'a> Debug for Type<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.type_kind().fmt(f)
	}
}