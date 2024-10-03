use std::{ffi::{c_int, CStr}, iter::once, marker::PhantomData, ptr::null_mut};

use super::{context::Context, enums::CodegenFileType, target_data::TargetData, target_machine::TargetMachine, traits::WrappedReference, types::Type, value::Value};
use super::llvm_c::{LLVMAddFunction, LLVMAddGlobal, LLVMDisposeMessage, LLVMDisposeModule, LLVMDumpModule};
use super::llvm_c::{LLVMModuleRef, LLVMSetModuleDataLayout, LLVMSetTarget, LLVMTargetMachineEmitToFile, LLVMTypeKind};

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

	pub fn set_data_layout(&self, data_layout: &TargetData) {
		unsafe { LLVMSetModuleDataLayout(self.module_ref, data_layout.get_ref()) };
	}

	pub fn set_target_triple(&self, target_triple: &str) {
		let target_triple: Box<[u8]> = target_triple.bytes().chain(once(0)).collect();
		unsafe { LLVMSetTarget(self.module_ref, target_triple.as_ptr()) };
	}

	pub fn emit_to_file(&self, target_machine: &TargetMachine, filepath: &str, codegen_type: CodegenFileType) -> Result<(), String> {
		let filepath: Box<[u8]> = filepath.bytes().chain(once(0)).collect();
		let mut error: *mut u8 = null_mut();
		let result = unsafe { LLVMTargetMachineEmitToFile(
			target_machine.get_ref(), self.module_ref, filepath.as_ptr(), codegen_type as c_int, &mut error
		) } != 0;
		let out = match result {
			false => Ok(()),
			true => Err({
				let error = unsafe { CStr::from_ptr(error as *const i8) };
				error.to_str().unwrap().to_string()
			})
		};
		unsafe { LLVMDisposeMessage(error) };
		out
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