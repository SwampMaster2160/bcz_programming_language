use std::ffi::{c_int, c_uint, c_ulonglong, c_void};

pub type LLVMContextRef = *mut c_void;
pub type LLVMModuleRef = *mut c_void;
pub type LLVMBuilderRef = *mut c_void;
pub type LLVMTargetRef = *mut c_void;
pub type LLVMTargetMachineRef = *mut c_void;
pub type LLVMTargetDataRef = *mut c_void;
pub type LLVMTypeRef = *mut c_void;
pub type LLVMValueRef = *mut c_void;
pub type LLVMBasicBlockRef = *mut c_void;

pub type LLVMBool = c_int;
pub type LLVMCodeGenOptLevel = c_int;
pub type LLVMRelocMode = c_int;
pub type LLVMCodeModel = c_int;
pub type LLVMLinkage = c_int;
pub type LLVMCodeGenFileType = c_int;

#[link(name = "C:/Program Files/LLVM/lib/LLVM-C")]
extern "C" {
	// Core
	pub fn LLVMGetVersion(major: *mut c_uint, minor: *mut c_uint, patch: *mut c_uint) -> c_void;
	pub fn LLVMDisposeMessage(Message: *mut u8) -> c_void;
	// Core/Contexts
	pub fn LLVMContextCreate() -> LLVMContextRef;
	pub fn LLVMContextDispose(C: LLVMContextRef) -> c_void;
	// Core/Modules
	pub fn LLVMModuleCreateWithNameInContext(ModuleID: *const u8, C: LLVMContextRef) -> LLVMModuleRef;
	pub fn LLVMDisposeModule(M: LLVMModuleRef) -> c_void;
	pub fn LLVMDumpModule(M: LLVMModuleRef) -> c_void;
	pub fn LLVMSetTarget(M: LLVMModuleRef, Triple: *const u8) -> c_void;
	pub fn LLVMAddFunction(M: LLVMModuleRef, Name: *const u8, FunctionTy: LLVMTypeRef) -> LLVMValueRef;
	// Core/Types/Integer Types
	pub fn LLVMInt1TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
	pub fn LLVMInt8TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
	pub fn LLVMInt16TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
	pub fn LLVMInt32TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
	pub fn LLVMInt64TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
	pub fn LLVMInt128TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
	// Core/Types/Other Types
	pub fn LLVMVoidTypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
	// Core/Types/Function Types
	pub fn LLVMFunctionType(ReturnType: LLVMTypeRef, ParamTypes: *const LLVMTypeRef, ParamCount: c_uint, IsVarArg: LLVMBool) -> LLVMTypeRef;
	// Core/Types/Sequential Types
	pub fn LLVMArrayType2(ElementType: LLVMTypeRef, ElementCount: u64) -> LLVMTypeRef;
	// Core/Values/Constants
	pub fn LLVMGetUndef(Ty: LLVMTypeRef) -> LLVMValueRef;
	// Core/Values/Constants/Global Values
	pub fn LLVMSetLinkage(Global: LLVMValueRef, Linkage: LLVMLinkage) -> c_void;
	// Core/Values/Constants/Global Variables
	pub fn LLVMAddGlobal(M: LLVMModuleRef, Ty: LLVMTypeRef, Name: *const u8) -> LLVMValueRef;
	pub fn LLVMSetInitializer(GlobalVar: LLVMValueRef, ConstantVal: LLVMValueRef) -> c_void;
	pub fn LLVMGetInitializer(GlobalVar: LLVMValueRef) -> LLVMValueRef;
	// Core/Values/Constants/Composite Constants
	pub fn LLVMConstStringInContext(C: LLVMContextRef, Str: *const u8, Length: c_uint, DontNullTerminate: LLVMBool) -> LLVMValueRef;
	// Core/Values/Constants/Function values
	pub fn LLVMSetFunctionCallConv(Fn: LLVMValueRef, CC: c_uint) -> c_void;
	// Core/Values/Constants/Function values/Function Parameters
	pub fn LLVMGetParam(Fn: LLVMValueRef, Index: c_uint) -> LLVMValueRef;
	// Core/Values/Constants/Scalar constants
	pub fn LLVMConstInt(IntTy: LLVMTypeRef, N: c_ulonglong, SignExtend: LLVMBool) -> LLVMValueRef;
	// Core/Basic Block
	pub fn LLVMAppendBasicBlockInContext(C: LLVMContextRef, Fn: LLVMValueRef, Name: *const u8) -> LLVMBasicBlockRef;
	// Instruction Builders
	pub fn LLVMCreateBuilderInContext(C: LLVMContextRef) -> LLVMBuilderRef;
	pub fn LLVMDisposeBuilder(Builder: LLVMBuilderRef) -> c_void;
	pub fn LLVMPositionBuilderAtEnd(Builder: LLVMBuilderRef, Block: LLVMBasicBlockRef) -> c_void;
	pub fn LLVMBuildPtrToInt(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: *const u8) -> LLVMValueRef;
	pub fn LLVMBuildCall2(B: LLVMBuilderRef, Ty: LLVMTypeRef, Fn: LLVMValueRef, Args: *const LLVMValueRef, NumArgs: c_uint, Name: *const u8) -> LLVMValueRef;
	pub fn LLVMBuildAdd(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const u8) -> LLVMValueRef;
	pub fn LLVMBuildSub(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const u8) -> LLVMValueRef;
	pub fn LLVMBuildMul(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const u8) -> LLVMValueRef;
	pub fn LLVMBuildUDiv(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const u8) -> LLVMValueRef;
	pub fn LLVMBuildURem(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const u8) -> LLVMValueRef;
	pub fn LLVMBuildSDiv(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const u8) -> LLVMValueRef;
	pub fn LLVMBuildSRem(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const u8) -> LLVMValueRef;
	pub fn LLVMBuildRet(B: LLVMBuilderRef, V: LLVMValueRef) -> LLVMValueRef;
	pub fn LLVMBuildAlloca(B: LLVMBuilderRef, Ty: LLVMTypeRef, Name: *const u8) -> LLVMValueRef;
	pub fn LLVMBuildStore(B: LLVMBuilderRef, Val: LLVMValueRef, Ptr: LLVMValueRef) -> LLVMValueRef;
	pub fn LLVMBuildLoad2(B: LLVMBuilderRef, Ty: LLVMTypeRef, PointerVal: LLVMValueRef, Name: *const u8) -> LLVMValueRef;
	pub fn LLVMBuildNeg(B: LLVMBuilderRef, V: LLVMValueRef, Name: *const u8) -> LLVMValueRef;
	// Target information
	pub fn LLVMInitializeX86TargetInfo() -> c_void;
	pub fn LLVMInitializeX86Target() -> c_void;
	pub fn LLVMInitializeX86TargetMC() -> c_void;
	pub fn LLVMInitializeX86AsmParser() -> c_void;
	pub fn LLVMInitializeX86AsmPrinter() -> c_void;
	pub fn LLVMGetDefaultTargetTriple() -> *mut u8;
	pub fn LLVMGetTargetFromTriple(Triple: *const u8, T: *mut LLVMTargetRef, ErrorMessage: *mut *mut u8) -> LLVMBool;
	pub fn LLVMCreateTargetMachine(
		T: LLVMTargetRef, Triple: *const u8, CPU: *const u8, Features: *const u8, Level: LLVMCodeGenOptLevel, Reloc: LLVMRelocMode, CodeModel: LLVMCodeModel
	) -> LLVMTargetMachineRef;
	pub fn LLVMCreateTargetDataLayout(T: LLVMTargetMachineRef) -> LLVMTargetDataRef;
	pub fn LLVMIntPtrTypeInContext(C: LLVMContextRef, TD: LLVMTargetDataRef) -> LLVMTypeRef;
	pub fn LLVMSetModuleDataLayout(M: LLVMModuleRef, DL: LLVMTargetDataRef) -> c_void;
	pub fn LLVMTargetMachineEmitToFile(
		T: LLVMTargetMachineRef, M: LLVMModuleRef, Filename: *const u8, codegen: LLVMCodeGenFileType, ErrorMessage: *mut *mut u8
	) -> LLVMBool;
	pub fn LLVMSizeOfTypeInBits(TD: LLVMTargetDataRef, Ty: LLVMTypeRef) -> c_ulonglong;
}
#[allow(non_upper_case_globals)]
pub const LLVMCodeGenLevelDefault: LLVMCodeGenOptLevel = 2;
#[allow(non_upper_case_globals)]
pub const LLVMRelocDefault: LLVMRelocMode = 0;
#[allow(non_upper_case_globals)]
pub const LLVMCodeModelDefault: LLVMCodeGenOptLevel = 0;
#[allow(non_upper_case_globals)]
pub const LLVMDLLImportLinkage: LLVMLinkage = 10;
#[allow(non_upper_case_globals)]
pub const LLVMExternalLinkage: LLVMLinkage = 0;
#[allow(non_upper_case_globals)]
pub const LLVMWin64CallConv: c_uint = 79;
#[allow(non_upper_case_globals)]
pub const LLVMObjectFile: LLVMCodeGenFileType = 1; 