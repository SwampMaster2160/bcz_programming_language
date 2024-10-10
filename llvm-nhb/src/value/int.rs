use std::cmp::Ordering;
use std::ffi::{c_int, CString};
use std::fmt::{Debug, Formatter, Write};
use std::marker::PhantomData;
use std::mem::transmute;

use crate::builder::Builder;
use crate::enums::Comparison;
use crate::llvm_c::{LLVMBuildAdd, LLVMBuildAnd, LLVMBuildICmp, LLVMBuildIntToPtr, LLVMBuildMul, LLVMBuildNeg, LLVMBuildNot, LLVMBuildOr, LLVMBuildSDiv, LLVMBuildSExt, LLVMBuildSRem, LLVMBuildSub, LLVMBuildTrunc, LLVMBuildUDiv, LLVMBuildURem, LLVMBuildXor, LLVMBuildZExt, LLVMGetValueKind, LLVMTypeKind, LLVMTypeOf, LLVMValueKind};
use crate::target_data::TargetData;
use crate::types::int::IntType;
use crate::types::types::Type;
use crate::{context::Context, llvm_c::LLVMValueRef, module::Module, traits::WrappedReference};
use crate::value::value::Value;

#[derive(Clone)]
#[repr(transparent)]
pub struct IntValue<'c, 'm> {
	value_ref: LLVMValueRef,
	phantom_data_context: PhantomData<&'c Context>,
	phantom_data_module: PhantomData<&'m Module<'c>>,
}

unsafe impl<'c, 'm> WrappedReference for IntValue<'c, 'm> {
	type RefType = LLVMValueRef;
}

impl<'c, 'm> IntValue<'c, 'm> where IntValue<'c, 'm>: Sized {
	#[inline]
	fn value_kind(&self) -> LLVMValueKind {
		unsafe { LLVMGetValueKind(self.value_ref) }
	}

	#[inline]
	pub fn get_type(&self) -> IntType<'c> {
		unsafe { IntType::from_ref(LLVMTypeOf(self.value_ref)) }
	}

	pub fn build_add(&self, rhs: &Self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		//
		let input_type = self.get_type();
		let input_type_kind = input_type.type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type = rhs.get_type();
		let rhs_type_kind = rhs_type.type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		if input_type != rhs_type {
			panic!("Type mismatch");
		}
		//
		if self.get_type() != rhs.get_type() {
			panic!("Type mismatch");
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildAdd(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_sub(&self, rhs: &Self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		//
		let input_type = self.get_type();
		let input_type_kind = input_type.type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type = rhs.get_type();
		let rhs_type_kind = rhs_type.type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		if input_type != rhs_type {
			panic!("Type mismatch");
		}
		//
		if self.get_type() != rhs.get_type() {
			panic!("Type mismatch");
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildSub(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_mult(&self, rhs: &Self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		//
		let input_type = self.get_type();
		let input_type_kind = input_type.type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type = rhs.get_type();
		let rhs_type_kind = rhs_type.type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		if input_type != rhs_type {
			panic!("Type mismatch");
		}
		//
		if self.get_type() != rhs.get_type() {
			panic!("Type mismatch");
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildMul(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_unsigned_div(&self, rhs: &Self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		//
		let input_type = self.get_type();
		let input_type_kind = input_type.type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type = rhs.get_type();
		let rhs_type_kind = rhs_type.type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		if input_type != rhs_type {
			panic!("Type mismatch");
		}
		//
		if self.get_type() != rhs.get_type() {
			panic!("Type mismatch");
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildUDiv(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_signed_div(&self, rhs: &Self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		//
		let input_type = self.get_type();
		let input_type_kind = input_type.type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type = rhs.get_type();
		let rhs_type_kind = rhs_type.type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		if input_type != rhs_type {
			panic!("Type mismatch");
		}
		//
		if self.get_type() != rhs.get_type() {
			panic!("Type mismatch");
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildSDiv(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_unsigned_modulo(&self, rhs: &Self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		//
		let input_type = self.get_type();
		let input_type_kind = input_type.type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type = rhs.get_type();
		let rhs_type_kind = rhs_type.type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		if input_type != rhs_type {
			panic!("Type mismatch");
		}
		//
		if self.get_type() != rhs.get_type() {
			panic!("Type mismatch");
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildURem(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_signed_truncated_modulo(&self, rhs: &Self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		//
		let input_type = self.get_type();
		let input_type_kind = input_type.type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type = rhs.get_type();
		let rhs_type_kind = rhs_type.type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		if input_type != rhs_type {
			panic!("Type mismatch");
		}
		//
		if self.get_type() != rhs.get_type() {
			panic!("Type mismatch");
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildSRem(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_bitwise_and(&self, rhs: &Self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		//
		let input_type = self.get_type();
		let input_type_kind = input_type.type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type = rhs.get_type();
		let rhs_type_kind = rhs_type.type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		if input_type != rhs_type {
			panic!("Type mismatch");
		}
		//
		if self.get_type() != rhs.get_type() {
			panic!("Type mismatch");
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildAnd(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_bitwise_or(&self, rhs: &Self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		//
		let input_type = self.get_type();
		let input_type_kind = input_type.type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type = rhs.get_type();
		let rhs_type_kind = rhs_type.type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		if input_type != rhs_type {
			panic!("Type mismatch");
		}
		//
		if self.get_type() != rhs.get_type() {
			panic!("Type mismatch");
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildOr(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_bitwise_xor(&self, rhs: &Self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		//
		let input_type = self.get_type();
		let input_type_kind = input_type.type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type = rhs.get_type();
		let rhs_type_kind = rhs_type.type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		if input_type != rhs_type {
			panic!("Type mismatch");
		}
		//
		if self.get_type() != rhs.get_type() {
			panic!("Type mismatch");
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildXor(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_compare(&self, rhs: &Self, builder: &Builder<'c, 'm>, comparison: Comparison, name: &str) -> Self {
		//
		let input_type = self.get_type();
		let input_type_kind = input_type.type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type = rhs.get_type();
		let rhs_type_kind = rhs_type.type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		if input_type != rhs_type {
			panic!("Type mismatch");
		}
		//
		if self.get_type() != rhs.get_type() {
			panic!("Type mismatch");
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildICmp(builder.get_ref(), comparison as c_int, self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_negate(&self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		//
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		//
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildNeg(builder.get_ref(), self.value_ref, name.as_ptr())) }
	}

	pub fn build_bitwise_not(&self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		//
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		//
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildNot(builder.get_ref(), self.value_ref, name.as_ptr())) }
	}

	pub fn build_zero_extend(&self, builder: &Builder<'c, 'm>, target_data: &TargetData, dest_type: IntType<'c>, name: &str) -> Self {
		//
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		let dest_type_kind = dest_type.type_kind();
		if !matches!(dest_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid dest type kind {:?}", dest_type_kind);
		}
		//
		let input_width = self.get_type().size_in_bits(target_data);
		let dest_width = dest_type.size_in_bits(target_data);
		if self.get_type().size_in_bits(target_data) >= dest_type.size_in_bits(target_data) {
			panic!("Cannot zero extend from {input_width} bit to {dest_width} bit");
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildZExt(builder.get_ref(), self.value_ref, dest_type.get_ref(), name.as_ptr())) }
	}

	pub fn build_sign_extend(&self, builder: &Builder<'c, 'm>, target_data: &TargetData, dest_type: IntType<'c>, name: &str) -> Self {
		//
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		let dest_type_kind = dest_type.type_kind();
		if !matches!(dest_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid dest type kind {:?}", dest_type_kind);
		}
		//
		let input_width = self.get_type().size_in_bits(target_data);
		let dest_width = dest_type.size_in_bits(target_data);
		if self.get_type().size_in_bits(target_data) >= dest_type.size_in_bits(target_data) {
			panic!("Cannot sign extend from {input_width} bit to {dest_width} bit");
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildSExt(builder.get_ref(), self.value_ref, dest_type.get_ref(), name.as_ptr())) }
	}

	pub fn build_truncate(&self, builder: &Builder<'c, 'm>, target_data: &TargetData, dest_type: IntType<'c>, name: &str) -> Self {
		//
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		let dest_type_kind = dest_type.type_kind();
		if !matches!(dest_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid dest type kind {:?}", dest_type_kind);
		}
		//
		let input_width = self.get_type().size_in_bits(target_data);
		let dest_width = dest_type.size_in_bits(target_data);
		if self.get_type().size_in_bits(target_data) <= dest_type.size_in_bits(target_data) {
			panic!("Cannot truncate from {input_width} bit to {dest_width} bit");
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildTrunc(builder.get_ref(), self.value_ref, dest_type.get_ref(), name.as_ptr())) }
	}

	pub fn build_unsigned_cast(&self, builder: &Builder<'c, 'm>, target_data: &TargetData<'c>, dest_type: IntType<'c>, name: &str) -> Self {
		//
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		let dest_type_kind = dest_type.type_kind();
		if !matches!(dest_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid dest type kind {:?}", dest_type_kind);
		}
		//
		let input_width = self.get_type().size_in_bits(target_data);
		let dest_width = dest_type.size_in_bits(target_data);
		let name = CString::new(name).unwrap();
		match dest_width.cmp(&input_width) {
			Ordering::Greater
				=> unsafe { Self::from_ref(LLVMBuildZExt(builder.get_ref(), self.value_ref, dest_type.get_ref(), name.as_ptr())) }
			Ordering::Equal => self.clone(),
			Ordering::Less
				=> unsafe { Self::from_ref(LLVMBuildTrunc(builder.get_ref(), self.value_ref, dest_type.get_ref(), name.as_ptr())) }
		}
	}

	pub fn build_signed_cast(&self, builder: &Builder<'c, 'm>, target_data: &TargetData<'c>, dest_type: IntType<'c>, name: &str) -> Self {
		//
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		let dest_type_kind = dest_type.type_kind();
		if !matches!(dest_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid dest type kind {:?}", dest_type_kind);
		}
		//
		let input_width = self.get_type().size_in_bits(target_data);
		let dest_width = dest_type.size_in_bits(target_data);
		let name = CString::new(name).unwrap();
		match dest_width.cmp(&input_width) {
			Ordering::Greater
				=> unsafe { Self::from_ref(LLVMBuildSExt(builder.get_ref(), self.value_ref, dest_type.get_ref(), name.as_ptr())) }
			Ordering::Equal => self.clone(),
			Ordering::Less
				=> unsafe { Self::from_ref(LLVMBuildTrunc(builder.get_ref(), self.value_ref, dest_type.get_ref(), name.as_ptr())) }
		}
	}

	pub fn as_value(&self) -> Value<'c, 'm> {
		unsafe { transmute(self) }
	}

	pub fn build_int_to_ptr(&self, builder: &Builder<'c, 'm>, dest_type: Type<'c>, name: &str) -> Value<'c, 'm> {
		let input_type = self.get_type();
		let dest_type_kind = dest_type.type_kind();
		if !matches!(dest_type_kind, LLVMTypeKind::LLVMFunctionTypeKind | LLVMTypeKind::LLVMPointerTypeKind) {
			panic!("Invalid dest type kind {:?}", dest_type_kind);
		}
		if dest_type != input_type.as_type().pointer_to() {
			panic!("Type mismatch");
		}
		let name = CString::new(name).unwrap();
		unsafe { Value::from_ref(LLVMBuildIntToPtr(builder.get_ref(), self.value_ref, dest_type.get_ref(), name.as_ptr())) }
	}
}

impl<'c, 'm> Debug for IntValue<'c, 'm> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		self.get_type().fmt(f)?;
		f.write_char('/')?;
		self.value_kind().fmt(f)
	}
}

impl<'c, 'm> Into<Value<'c, 'm>> for IntValue<'c, 'm> {
	#[inline]
	fn into(self) -> Value<'c, 'm> {
		unsafe { transmute(self) }
	}
}