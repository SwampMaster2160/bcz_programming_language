use std::{ffi::{c_char, CStr, CString}, ptr::null_mut};

use super::{enums::{CodeModel, CodegenOptLevel, RealocMode}, target_machine::TargetMachine, traits::WrappedReference};
use super::llvm_c::{LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetMachine, LLVMDisposeMessage, LLVMGetTargetFromTriple, LLVMRelocMode, LLVMTargetRef};

#[repr(transparent)]
pub struct Target {
	machine_ref: LLVMTargetRef,
}

impl Target {
	pub fn from_triple(triple: &str) -> Result<Self, String> {
		let mut target = null_mut();
		let mut error = null_mut();
		let triple = CString::new(triple).unwrap();
		let result = unsafe { LLVMGetTargetFromTriple(triple.as_ptr(), &mut target, &mut error) } != 0;
		let out = match result {
			true => Err(unsafe { CStr::from_ptr(error as *const c_char).to_str().unwrap().to_string() }),
			false => Ok(Self { machine_ref: target }),
		};
		unsafe { LLVMDisposeMessage(error) };
		out
	}

	pub fn create_target_machine(&self, triple: &str, cpu: &str, features: &str, opt_level: CodegenOptLevel, realoc_mode: RealocMode, code_model: CodeModel)
		-> TargetMachine {
		let triple = CString::new(triple).unwrap();
		let features = CString::new(features).unwrap();
		let cpu = CString::new(cpu).unwrap();
		unsafe { TargetMachine::from_ref(
			LLVMCreateTargetMachine(
				self.machine_ref,
				triple.as_ptr(),
				cpu.as_ptr(),
				features.as_ptr(),
				opt_level as LLVMCodeGenOptLevel,
				realoc_mode as LLVMRelocMode,
				code_model as LLVMCodeModel,
			)
		) }
	}
}

unsafe impl WrappedReference for Target {
	type RefType = LLVMTargetRef;
}