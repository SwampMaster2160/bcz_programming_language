use std::{iter::once, marker::PhantomData, mem::ManuallyDrop};

use super::{builder::Builder, llvm_c::{LLVMBuildIntToPtr, LLVMBuildPtrToInt, LLVMTypeKind, LLVMTypeOf, LLVMValueRef}, llvm_type::Type, traits::WrappedReference};

#[derive(Clone, Debug)]
pub struct Value<'a> {
	value_ref: LLVMValueRef,
	phantom_data: PhantomData<&'a ()>,
}

impl<'a> WrappedReference<LLVMValueRef> for Value<'a> {
	#[inline]
	fn get_ref(&self) -> LLVMValueRef {
		self.value_ref
	}

	#[inline]
	unsafe fn from_ref(raw_ref: LLVMValueRef) -> Self {
		Self { value_ref: raw_ref, phantom_data: PhantomData::default() }
	}

	#[inline]
	fn take_ref(self) -> LLVMValueRef {
		ManuallyDrop::new(self).value_ref
	}
}

impl<'a> Value<'a> {
	//#[inline]
	//fn value_kind(&self) -> LLVMValueKind {
	//	unsafe { LLVMGetValueKind(self.value_ref) }
	//}

	#[inline]
	pub fn get_type(&self) -> Type {
		unsafe { Type::from_ref(LLVMTypeOf(self.value_ref)) }
	}

	pub fn build_ptr_to_int(&self, builder: &Builder, dest_type: Type, name: &str) -> Value<'static> {
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMFunctionTypeKind | LLVMTypeKind::LLVMPointerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		let dest_type_kind = dest_type.type_kind();
		if !matches!(dest_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid dest type kind {:?}", dest_type_kind);
		}
		unsafe { Value { value_ref: LLVMBuildPtrToInt(builder.get_ref(), self.value_ref, dest_type.get_ref(), name.as_ptr()), phantom_data: PhantomData::default() } }
	}

	pub fn build_int_to_ptr(&self, builder: &Builder, dest_type: Type, name: &str) -> Value<'static> {
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		let dest_type_kind = dest_type.type_kind();
		if !matches!(dest_type_kind, LLVMTypeKind::LLVMFunctionTypeKind | LLVMTypeKind::LLVMPointerTypeKind) {
			panic!("Invalid dest type kind {:?}", dest_type_kind);
		}
		unsafe { Value { value_ref: LLVMBuildIntToPtr(builder.get_ref(), self.value_ref, dest_type.get_ref(), name.as_ptr()), phantom_data: PhantomData::default() } }
	}
}