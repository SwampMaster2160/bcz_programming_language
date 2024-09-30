use std::{ffi::CStr, iter::once, ptr::null_mut};

use super::{enums::{CodeModel, CodegenOptLevel, RealocMode}, llvm_c::{LLVMCodeGenOptLevel, LLVMCodeModel, LLVMCreateTargetMachine, LLVMDisposeMessage, LLVMGetTargetFromTriple, LLVMRelocMode, LLVMTargetRef}, target_machine::TargetMachine, traits::WrappedReference};

#[repr(transparent)]
pub struct Target {
	machine_ref: LLVMTargetRef,
}

impl Target {
	pub fn from_triple(triple: &str) -> Result<Self, String> {
		let triple_c: Box<[u8]> = triple.bytes().chain(once(0)).collect();
		let mut target = null_mut();
		let mut error = null_mut();
		let result = unsafe { LLVMGetTargetFromTriple(triple_c.as_ptr(), &mut target, &mut error) } != 0;
		let out = match result {
			true => Err(unsafe { CStr::from_ptr(error as *const i8).to_str().unwrap().to_string() }),
			false => Ok(Self { machine_ref: target }),
		};
		unsafe { LLVMDisposeMessage(error) };
		out
	}

	pub fn create_target_machine(&self, triple: &str, cpu: &str, features: &str, opt_level: CodegenOptLevel, realoc_mode: RealocMode, code_model: CodeModel)
		-> TargetMachine {
		let triple_c: Box<[u8]> = triple.bytes().chain(once(0)).collect();
		let cpu_c: Box<[u8]> = cpu.bytes().chain(once(0)).collect();
		let features_c: Box<[u8]> = features.bytes().chain(once(0)).collect();
		unsafe { TargetMachine::from_ref(
			LLVMCreateTargetMachine(
				self.machine_ref,
				triple_c.as_ptr(),
				cpu_c.as_ptr(),
				features_c.as_ptr(),
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