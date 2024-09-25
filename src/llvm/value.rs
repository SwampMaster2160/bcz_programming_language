use std::{ffi::c_uint, iter::once, marker::PhantomData, mem::transmute};

use super::{builder::Builder, llvm_c::{LLVMBuildAdd, LLVMBuildCall2, LLVMBuildIntToPtr, LLVMBuildMul, LLVMBuildNeg, LLVMBuildPtrToInt, LLVMBuildSDiv, LLVMBuildSExt, LLVMBuildSRem, LLVMBuildSub, LLVMBuildTrunc, LLVMBuildUDiv, LLVMBuildURem, LLVMBuildZExt, LLVMTypeKind, LLVMTypeOf, LLVMValueRef}, llvm_type::Type, traits::WrappedReference};

#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct Value<'a> {
	value_ref: LLVMValueRef,
	phantom_data: PhantomData<&'a ()>,
}

unsafe impl<'a> WrappedReference for Value<'a> {
	type RefType = LLVMValueRef;
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

	pub fn build_ptr_to_int(&self, builder: &Builder, dest_type: Type, name: &str) -> Value<'a> {
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMFunctionTypeKind | LLVMTypeKind::LLVMPointerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		let dest_type_kind = dest_type.type_kind();
		if !matches!(dest_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid dest type kind {:?}", dest_type_kind);
		}
		unsafe { Self::from_ref(LLVMBuildPtrToInt(builder.get_ref(), self.value_ref, dest_type.get_ref(), name.as_ptr())) }
	}

	pub fn build_int_to_ptr(&self, builder: &Builder, dest_type: Type, name: &str) -> Value<'a> {
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		let dest_type_kind = dest_type.type_kind();
		if !matches!(dest_type_kind, LLVMTypeKind::LLVMFunctionTypeKind | LLVMTypeKind::LLVMPointerTypeKind) {
			panic!("Invalid dest type kind {:?}", dest_type_kind);
		}
		unsafe { Self::from_ref(LLVMBuildIntToPtr(builder.get_ref(), self.value_ref, dest_type.get_ref(), name.as_ptr())) }
	}

	pub fn build_zero_extend(&self, builder: &Builder, dest_type: Type, name: &str) -> Value<'a> {
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		let dest_type_kind = dest_type.type_kind();
		if !matches!(dest_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid dest type kind {:?}", dest_type_kind);
		}
		unsafe { Self::from_ref(LLVMBuildZExt(builder.get_ref(), self.value_ref, dest_type.get_ref(), name.as_ptr())) }
	}

	pub fn build_sign_extend(&self, builder: &Builder, dest_type: Type, name: &str) -> Value<'a> {
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		let dest_type_kind = dest_type.type_kind();
		if !matches!(dest_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid dest type kind {:?}", dest_type_kind);
		}
		unsafe { Self::from_ref(LLVMBuildSExt(builder.get_ref(), self.value_ref, dest_type.get_ref(), name.as_ptr())) }
	}

	pub fn build_truncate(&self, builder: &Builder, dest_type: Type, name: &str) -> Value<'a> {
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		let dest_type_kind = dest_type.type_kind();
		if !matches!(dest_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid dest type kind {:?}", dest_type_kind);
		}
		unsafe { Self::from_ref(LLVMBuildTrunc(builder.get_ref(), self.value_ref, dest_type.get_ref(), name.as_ptr())) }
	}

	pub fn build_add(&self, rhs: &Value<'a>, builder: &Builder, name: &str) -> Value<'a> {
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type_kind = self.get_type().type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		unsafe { Self::from_ref(LLVMBuildAdd(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_sub(&self, rhs: &Value<'a>, builder: &Builder, name: &str) -> Value<'a> {
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type_kind = self.get_type().type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		unsafe { Self::from_ref(LLVMBuildSub(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_mult(&self, rhs: &Value<'a>, builder: &Builder, name: &str) -> Value<'a> {
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type_kind = self.get_type().type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		unsafe { Self::from_ref(LLVMBuildMul(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_unsigned_div(&self, rhs: &Value<'a>, builder: &Builder, name: &str) -> Value<'a> {
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type_kind = self.get_type().type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		unsafe { Self::from_ref(LLVMBuildUDiv(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_signed_div(&self, rhs: &Value<'a>, builder: &Builder, name: &str) -> Value<'a> {
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type_kind = self.get_type().type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		unsafe { Self::from_ref(LLVMBuildSDiv(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_unsigned_modulo(&self, rhs: &Value<'a>, builder: &Builder, name: &str) -> Value<'a> {
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type_kind = self.get_type().type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		unsafe { Self::from_ref(LLVMBuildURem(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_signed_truncated_modulo(&self, rhs: &Value<'a>, builder: &Builder, name: &str) -> Value<'a> {
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid LHS input type kind {:?}", input_type_kind);
		}
		let rhs_type_kind = self.get_type().type_kind();
		if !matches!(rhs_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid RHS input type kind {:?}", rhs_type_kind);
		}
		unsafe { Self::from_ref(LLVMBuildSRem(builder.get_ref(), self.value_ref, rhs.value_ref, name.as_ptr())) }
	}

	pub fn build_negate(&self, builder: &Builder, name: &str) -> Value<'a> {
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		let input_type_kind = self.get_type().type_kind();
		if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
			panic!("Invalid input type kind {:?}", input_type_kind);
		}
		unsafe { Self::from_ref(LLVMBuildNeg(builder.get_ref(), self.value_ref, name.as_ptr())) }
	}

	/// Call a function with `self` as the function value/pointer.
	///
	/// # Safety
	///
	/// The `self` must represent a function pointer to a function with type `function_type`, this is not checked.
	pub unsafe fn build_call<'b>(&'a self, arguments: &[Value<'b>], function_type: Type, builder: &Builder, name: &str) -> Value<'b> {
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
		//if self.get_type() != function_type {
		//	panic!("Function call for value of type {:?} but call type of {:?}", self.get_type(), function_type);
		//}
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		unsafe {
			Value::from_ref(LLVMBuildCall2(builder.get_ref(), function_type.get_ref(), self.value_ref, transmute(arguments.as_ptr()), argument_count_c, name.as_ptr()))
		}
	}

	//pub fn build_return(&self, builder: &Builder) -> Value<'a> {
	//	let input_type_kind = self.get_type().type_kind();
	//	if !matches!(input_type_kind, LLVMTypeKind::LLVMIntegerTypeKind) {
	//		panic!("Invalid input type kind {:?}", input_type_kind);
	//	}
	//	unsafe { Self::from_ref(LLVMBuildRet(builder.get_ref(), self.value_ref)) }
	//}
}