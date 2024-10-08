use std::{ffi::{c_uint, CString}, fmt::{Debug, Formatter, Write}, marker::PhantomData, mem::transmute};

use crate::llvm_c::{LLVMBuildAnd, LLVMBuildNot, LLVMBuildOr, LLVMBuildXor};

use super::{basic_block::BasicBlock, builder::Builder, context::Context, enums::{CallingConvention, Linkage}, module::Module, traits::WrappedReference, types::Type};
use super::llvm_c::{LLVMAppendBasicBlockInContext, LLVMBuildAdd, LLVMBuildCall2, LLVMBuildIntToPtr, LLVMBuildLoad2, LLVMBuildMul, LLVMBuildNeg, LLVMSetLinkage};
use super::llvm_c::{LLVMBuildPtrToInt, LLVMBuildRet, LLVMBuildSDiv, LLVMBuildSExt, LLVMBuildSRem, LLVMBuildStore, LLVMBuildSub, LLVMBuildTrunc, LLVMSetInitializer};
use super::llvm_c::{LLVMBuildUDiv, LLVMBuildURem, LLVMBuildZExt, LLVMCountParams, LLVMGetParam, LLVMGetValueKind, LLVMTypeOf, LLVMSetFunctionCallConv};
use super::llvm_c::{LLVMTypeKind, LLVMLinkage, LLVMValueKind, LLVMValueRef};

#[derive(Clone)]
#[repr(transparent)]
pub struct Value<'c, 'm> {
	value_ref: LLVMValueRef,
	phantom_data_context: PhantomData<&'c Context>,
	phantom_data_module: PhantomData<&'m Module<'c>>,
}

unsafe impl<'c, 'm> WrappedReference for Value<'c, 'm> {
	type RefType = LLVMValueRef;
}

impl<'c, 'm> Value<'c, 'm> where Value<'c, 'm>: Sized {
	#[inline]
	fn value_kind(&self) -> LLVMValueKind {
		unsafe { LLVMGetValueKind(self.value_ref) }
	}

	#[inline]
	pub fn get_type(&self) -> Type<'c> {
		unsafe { Type::from_ref(LLVMTypeOf(self.value_ref)) }
	}

	pub fn build_ptr_to_int(&self, builder: &Builder<'c, 'm>, dest_type: Type<'c>, name: &str) -> Self {
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMFunctionTypeKind | LLVMTypeKind::LLVMPointerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		let dest_type_kind = dest_type.type_kind();
		if !matches!(dest_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid dest type kind {:?}", dest_type_kind);
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildPtrToInt(builder.get_ref(), self.value_ref, dest_type.get_ref(), name.as_ptr())) }
	}

	pub fn build_int_to_ptr(&self, builder: &Builder<'c, 'm>, dest_type: Type<'c>, name: &str) -> Self {
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		let dest_type_kind = dest_type.type_kind();
		if !matches!(dest_type_kind, LLVMTypeKind::LLVMFunctionTypeKind | LLVMTypeKind::LLVMPointerTypeKind) {
			panic!("Invalid dest type kind {:?}", dest_type_kind);
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildIntToPtr(builder.get_ref(), self.value_ref, dest_type.get_ref(), name.as_ptr())) }
	}

	pub fn build_zero_extend(&self, builder: &Builder<'c, 'm>, dest_type: Type<'c>, name: &str) -> Self {
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		let dest_type_kind = dest_type.type_kind();
		if !matches!(dest_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid dest type kind {:?}", dest_type_kind);
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildZExt(builder.get_ref(), self.value_ref, dest_type.get_ref(), name.as_ptr())) }
	}

	pub fn build_sign_extend(&self, builder: &Builder<'c, 'm>, dest_type: Type<'c>, name: &str) -> Self {
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		let dest_type_kind = dest_type.type_kind();
		if !matches!(dest_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid dest type kind {:?}", dest_type_kind);
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildSExt(builder.get_ref(), self.value_ref, dest_type.get_ref(), name.as_ptr())) }
	}

	pub fn build_truncate(&self, builder: &Builder<'c, 'm>, dest_type: Type<'c>, name: &str) -> Self {
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		let dest_type_kind = dest_type.type_kind();
		if !matches!(dest_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid dest type kind {:?}", dest_type_kind);
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildTrunc(builder.get_ref(), self.value_ref, dest_type.get_ref(), name.as_ptr())) }
	}

	pub fn build_add(&self, rhs: &Self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type_kind = self.get_type().type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildAdd(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_sub(&self, rhs: &Self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type_kind = self.get_type().type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildSub(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_mult(&self, rhs: &Self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type_kind = self.get_type().type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildMul(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_unsigned_div(&self, rhs: &Self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type_kind = self.get_type().type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildUDiv(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_signed_div(&self, rhs: &Self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type_kind = self.get_type().type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildSDiv(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_unsigned_modulo(&self, rhs: &Self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type_kind = self.get_type().type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildURem(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_signed_truncated_modulo(&self, rhs: &Self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type_kind = self.get_type().type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildSRem(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_negate(&self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildNeg(builder.get_ref(), self.value_ref, name.as_ptr())) }
	}

	pub fn build_bitwise_not(&self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildNot(builder.get_ref(), self.value_ref, name.as_ptr())) }
	}

	pub fn build_bitwise_and(&self, rhs: &Self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type_kind = self.get_type().type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildAnd(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_bitwise_or(&self, rhs: &Self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type_kind = self.get_type().type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildOr(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_bitwise_xor(&self, rhs: &Self, builder: &Builder<'c, 'm>, name: &str) -> Self {
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type_kind = self.get_type().type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildXor(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	/// Call a function with `self` as the function value/pointer.
	///
	/// # Safety
	///
	/// The `self` must represent a function pointer to a function with type `function_type`, this is not checked.
	pub fn build_call(&self, arguments: &[Self], function_type: Type<'c>, builder: &Builder<'c, 'm>, name: &str) -> Self {
		let argument_count_c: c_uint = match arguments.len().try_into() {
			Ok(count) => count,
			Err(_) => panic!("Too many arguments"),
		};
		if function_type.parameter_count() != arguments.len() {
			panic!("Type mismatch");
		}
		for (index, parameter) in function_type.parameter_types().iter().enumerate() {
			if *parameter != arguments[index].get_type() {
				panic!("Type mismatch");
			}
		}
		if self.get_type() != function_type.pointer_to() {
			panic!("Function call for value of type {:?} but call type of {:?}", self.get_type(), function_type);
		}
		let name = CString::new(name).unwrap();
		unsafe {
			Value::from_ref(LLVMBuildCall2(
				builder.get_ref(),
				function_type.get_ref(),
				self.value_ref, transmute(arguments.as_ptr()),
				argument_count_c,
				name.as_ptr(),
			))
		}
	}
	
	pub fn set_initializer(&self, set_to: &Self) {
		let self_type = self.get_type();
		let set_to_type = set_to.get_type();
		match (self.value_kind(), self_type.type_kind()) {
			(LLVMValueKind::LLVMGlobalVariableValueKind, LLVMTypeKind::LLVMPointerTypeKind) => {}
			_ => panic!("Type mismatch")
		}
		if self_type != set_to_type.pointer_to() {
			panic!("Initilize of type {set_to_type:?} to location of type {self_type:?}");
		}
		unsafe { LLVMSetInitializer(self.value_ref, set_to.value_ref) };
	}

	pub fn get_parameter(&self, index: usize) -> Self {
		if index >= self.count_parameters() {
			panic!("Index out of bounds");
		}
		unsafe { Self::from_ref(LLVMGetParam(self.value_ref, index as c_uint)) }
	}

	pub fn count_parameters(&self) -> usize {
		match (self.value_kind(), self.get_type().type_kind()) {
			(LLVMValueKind::LLVMFunctionValueKind, LLVMTypeKind::LLVMPointerTypeKind) => {}
			_ => panic!("Invalid input value {self:?}, should be function")
		}
		unsafe { LLVMCountParams(self.value_ref) as usize }
	}

	pub fn build_load(&self, load_type: Type<'c>, builder: &Builder<'c, 'm>, name: &str) -> Self {
		if !load_type.is_normal() {
			panic!("Invalid load type {load_type:?}");
		}
		let self_type = self.get_type();
		if load_type.pointer_to() != self_type {
			panic!("Load of type {load_type:?} from location of type {self_type:?}");
		}
		let name = CString::new(name).unwrap();
		unsafe { Self::from_ref(LLVMBuildLoad2(builder.get_ref(), load_type.get_ref(), self.value_ref, name.as_ptr())) }
	}

	pub fn build_store(&self, value_to_store: &Self, builder: &Builder<'c, 'm>) -> Self {
		let value_to_store_type = value_to_store.get_type();
		let self_type = self.get_type();
		if self_type != value_to_store_type.pointer_to() {
			panic!("Store of type {value_to_store_type:?} to location of type {self_type:?}");
		}
		unsafe { Self::from_ref(LLVMBuildStore(builder.get_ref(), value_to_store.value_ref, self.value_ref)) }
	}

	pub fn build_return(&self, builder: &Builder) ->Self {
		let self_type = self.get_type();
		if !self_type.is_normal() {
			panic!("Invalid type of return value: {self_type:?}");
		}
		unsafe { Self::from_ref(LLVMBuildRet(builder.get_ref(), self.value_ref)) }
	}

	/// `self` is the function to append the basic block to.
	pub fn append_basic_block(&self, context: &'c Context, name: &str) -> BasicBlock<'c, 'm> {
		match (self.value_kind(), self.get_type().type_kind()) {
			(LLVMValueKind::LLVMFunctionValueKind, LLVMTypeKind::LLVMPointerTypeKind) => {}
			_ => panic!("Invalid input value {self:?}, should be function")
		}
		let name = CString::new(name).unwrap();
		unsafe { BasicBlock::from_ref(LLVMAppendBasicBlockInContext(context.get_ref(), self.value_ref, name.as_ptr())) }
	}

	pub fn set_linkage(&self, linkage: Linkage) {
		match (self.value_kind(), self.get_type().type_kind()) {
			(LLVMValueKind::LLVMGlobalVariableValueKind | LLVMValueKind::LLVMFunctionValueKind, LLVMTypeKind::LLVMPointerTypeKind) => {}
			_ => panic!("Invalid input value {self:?}, should be global variable/function")
		}
		unsafe { LLVMSetLinkage(self.value_ref, linkage as LLVMLinkage) };
	}

	pub fn set_calling_convention(&self, calling_convention: CallingConvention) {
		match (self.value_kind(), self.get_type().type_kind()) {
			(LLVMValueKind::LLVMFunctionValueKind, LLVMTypeKind::LLVMPointerTypeKind) => {}
			_ => panic!("Invalid input value {self:?}, should be function")
		}
		unsafe { LLVMSetFunctionCallConv(self.value_ref, calling_convention as c_uint) };
	}
}

impl<'c, 'm> Debug for Value<'c, 'm> {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		self.get_type().fmt(f)?;
		f.write_char('/')?;
		self.value_kind().fmt(f)
	}
}