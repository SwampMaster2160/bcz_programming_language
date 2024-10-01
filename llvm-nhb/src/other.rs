use super::llvm_c::{LLVMInitializeX86AsmParser, LLVMInitializeX86AsmPrinter, LLVMInitializeX86Target, LLVMInitializeX86TargetInfo, LLVMInitializeX86TargetMC};

pub fn initialize_x86() {
	unsafe { LLVMInitializeX86TargetInfo() };
	unsafe { LLVMInitializeX86Target() };
	unsafe { LLVMInitializeX86TargetMC() };
	unsafe { LLVMInitializeX86AsmParser() };
	unsafe { LLVMInitializeX86AsmPrinter() };
}