use std::{fmt::{Debug, Formatter}, marker::PhantomData, mem::transmute};

use crate::{context::Context, llvm_c::{LLVMBool, LLVMConstInt, LLVMGetUndef, LLVMSizeOfTypeInBits, LLVMTypeKind, LLVMTypeRef}, target_data::TargetData, traits::WrappedReference, value::int::IntValue};

use super::types::Type;

#[derive(Clone, Copy, Hash, PartialEq)]
#[repr(transparent)]
pub struct IntType<'a> {
	type_ref: LLVMTypeRef,
	phantom_data: PhantomData<&'a Context>
}

unsafe impl<'a> WrappedReference for IntType<'a> {
	type RefType = LLVMTypeRef;
}

impl<'a> IntType<'a> {
	#[inline]
	pub(crate) const fn type_kind(self) -> LLVMTypeKind {
		LLVMTypeKind::LLVMIntegerTypeKind
	}

	#[inline]
	pub fn size_in_bits(&self, target_data: &TargetData) -> u128 {
		unsafe { LLVMSizeOfTypeInBits(target_data.get_ref(), self.type_ref).try_into().unwrap() }
	}

	#[inline]
	pub fn as_type(&self) -> Type<'a> {
		unsafe { transmute(self) }
	}

	pub fn const_int(self, value: u128, sign_extend: bool) -> IntValue<'a, 'a> {
		unsafe { IntValue::from_ref(LLVMConstInt(self.type_ref, value.try_into().unwrap(), sign_extend as LLVMBool)) }
	}

	/// Create an undefined value of this type.
	#[inline]
	pub fn undefined(self) -> IntValue<'a, 'a> {
		unsafe { IntValue::from_ref(LLVMGetUndef(self.get_ref())) }
	}
}

impl<'a> Into<Type<'a>> for IntType<'a> {
	#[inline]
	fn into(self) -> Type<'a> {
		unsafe { transmute(self) }
	}
}

impl<'a> Debug for IntType<'a> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		self.type_kind().fmt(f)
	}
}