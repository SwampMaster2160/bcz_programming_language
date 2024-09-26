use std::{iter::once, marker::PhantomData};

use super::{context::Context, llvm_c::{LLVMAddFunction, LLVMAddGlobal, LLVMDisposeModule, LLVMDumpModule, LLVMModuleRef, LLVMTypeKind}, types::Type, traits::WrappedReference, value::Value};

#[repr(transparent)]
pub struct Module<'c> {
	module_ref: LLVMModuleRef,
	phantom_data: PhantomData<&'c Context>,
}

unsafe impl<'c> WrappedReference for Module<'c> {
	type RefType = LLVMModuleRef;
}

impl<'c> Module<'c> {
	#[inline]
	pub fn dump(&self) {
		unsafe { LLVMDumpModule(self.module_ref) };
	}

	pub fn add_global<'m>(&'m self, global_type: Type<'c>, name: &str) -> Value<'c, 'm> {
		match global_type {
			invalid if !invalid.is_normal() => panic!("Invalid global type {invalid:?}"),
			_ => {}
		}
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		unsafe { Value::from_ref(LLVMAddGlobal(self.module_ref, global_type.get_ref(), name.as_ptr())) }
	}

	pub fn add_function<'m>(&'m self, function_type: Type<'c>, name: &str) -> Value<'c, 'm> {
		match function_type.type_kind() {
			LLVMTypeKind::LLVMFunctionTypeKind => {}
			invalid => panic!("Invalid global type {invalid:?}")
		}
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		unsafe { Value::from_ref(LLVMAddFunction(self.module_ref, name.as_ptr(), function_type.get_ref())) }
	}
}

impl<'c> Drop for Module<'c> {
	#[inline]
	fn drop(&mut self) {
		unsafe {
			LLVMDisposeModule(self.module_ref);
		}
	}
}