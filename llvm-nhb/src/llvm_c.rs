use std::ffi::{c_char, c_int, c_uint, c_ulonglong, c_void};

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
pub type LLVMIntPredicate = c_int;

#[link(name = "C:/Program Files/LLVM/lib/LLVM-C")]
extern "C" {
	// Core
	pub fn LLVMGetVersion(major: *mut c_uint, minor: *mut c_uint, patch: *mut c_uint) -> c_void;
	pub fn LLVMDisposeMessage(Message: *mut c_char) -> c_void;
	// Core/Contexts
	pub fn LLVMContextCreate() -> LLVMContextRef;
	pub fn LLVMContextDispose(C: LLVMContextRef) -> c_void;
	// Core/Modules
	pub fn LLVMModuleCreateWithNameInContext(ModuleID: *const c_char, C: LLVMContextRef) -> LLVMModuleRef;
	pub fn LLVMDisposeModule(M: LLVMModuleRef) -> c_void;
	pub fn LLVMDumpModule(M: LLVMModuleRef) -> c_void;
	pub fn LLVMSetTarget(M: LLVMModuleRef, Triple: *const c_char) -> c_void;
	pub fn LLVMAddFunction(M: LLVMModuleRef, Name: *const c_char, FunctionTy: LLVMTypeRef) -> LLVMValueRef;
	// Core/Types
	pub fn LLVMGetTypeKind(Ty: LLVMTypeRef) -> LLVMTypeKind;
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
	pub fn LLVMCountParamTypes(FunctionTy: LLVMTypeRef) -> c_uint;
	pub fn LLVMGetReturnType(FunctionTy: LLVMTypeRef) -> LLVMTypeRef;
	pub fn LLVMIsFunctionVarArg(FunctionTy: LLVMTypeRef) -> LLVMBool;
	pub fn LLVMGetParamTypes(FunctionTy: LLVMTypeRef, Dest: *mut LLVMTypeRef) -> c_void;
	// Core/Types/Sequential Types
	pub fn LLVMArrayType2(ElementType: LLVMTypeRef, ElementCount: u64) -> LLVMTypeRef;
	pub fn LLVMPointerType(ElementType: LLVMTypeRef, AddressSpace: c_uint) -> LLVMTypeRef;
	pub fn LLVMGetElementType(Ty: LLVMTypeRef) -> LLVMTypeRef;
	// Core/Values/Constants
	pub fn LLVMGetUndef(Ty: LLVMTypeRef) -> LLVMValueRef;
	// Core/Values/Constants/Global Values
	pub fn LLVMSetLinkage(Global: LLVMValueRef, Linkage: LLVMLinkage) -> c_void;
	// Core/Values/Constants/Global Variables
	pub fn LLVMAddGlobal(M: LLVMModuleRef, Ty: LLVMTypeRef, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMSetInitializer(GlobalVar: LLVMValueRef, ConstantVal: LLVMValueRef) -> c_void;
	pub fn LLVMGetInitializer(GlobalVar: LLVMValueRef) -> LLVMValueRef;
	// Core/Values/Constants/Composite Constants
	pub fn LLVMConstStringInContext(C: LLVMContextRef, Str: *const c_char, Length: c_uint, DontNullTerminate: LLVMBool) -> LLVMValueRef;
	// Core/Values/Constants/Function values
	pub fn LLVMSetFunctionCallConv(Fn: LLVMValueRef, CC: c_uint) -> c_void;
	// Core/Values/Constants/Function values/Function Parameters
	pub fn LLVMGetParam(Fn: LLVMValueRef, Index: c_uint) -> LLVMValueRef;
	pub fn LLVMCountParams(Fn: LLVMValueRef) -> c_uint;
	// Core/Values/Constants/Scalar constants
	pub fn LLVMConstInt(IntTy: LLVMTypeRef, N: c_ulonglong, SignExtend: LLVMBool) -> LLVMValueRef;
	// Core/Values/General APIs
	pub fn LLVMGetValueKind(Val: LLVMValueRef) -> LLVMValueKind;
	pub fn LLVMTypeOf(Val: LLVMValueRef) -> LLVMTypeRef;
	// Core/Basic Block
	pub fn LLVMAppendBasicBlockInContext(C: LLVMContextRef, Fn: LLVMValueRef, Name: *const c_char) -> LLVMBasicBlockRef;
	// Instruction Builders
	pub fn LLVMCreateBuilderInContext(C: LLVMContextRef) -> LLVMBuilderRef;
	pub fn LLVMDisposeBuilder(Builder: LLVMBuilderRef) -> c_void;
	pub fn LLVMPositionBuilderAtEnd(Builder: LLVMBuilderRef, Block: LLVMBasicBlockRef) -> c_void;
	pub fn LLVMBuildPtrToInt(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMBuildIntToPtr(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMBuildZExt(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMBuildSExt(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMBuildTrunc(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMBuildCall2(B: LLVMBuilderRef, Ty: LLVMTypeRef, Fn: LLVMValueRef, Args: *const LLVMValueRef, NumArgs: c_uint, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMBuildAdd(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMBuildSub(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMBuildMul(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMBuildUDiv(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMBuildURem(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMBuildSDiv(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMBuildSRem(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMBuildAnd(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMBuildOr(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMBuildXor(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMBuildRet(B: LLVMBuilderRef, V: LLVMValueRef) -> LLVMValueRef;
	pub fn LLVMBuildRetVoid(B: LLVMBuilderRef) -> LLVMValueRef;
	pub fn LLVMBuildAlloca(B: LLVMBuilderRef, Ty: LLVMTypeRef, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMBuildStore(B: LLVMBuilderRef, Val: LLVMValueRef, Ptr: LLVMValueRef) -> LLVMValueRef;
	pub fn LLVMBuildLoad2(B: LLVMBuilderRef, Ty: LLVMTypeRef, PointerVal: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMBuildNeg(B: LLVMBuilderRef, V: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMBuildNot(B: LLVMBuilderRef, V: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub fn LLVMBuildICmp(B: LLVMBuilderRef, Op: LLVMIntPredicate, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	// Target information
	pub fn LLVMInitializeX86TargetInfo() -> c_void;
	pub fn LLVMInitializeX86Target() -> c_void;
	pub fn LLVMInitializeX86TargetMC() -> c_void;
	pub fn LLVMInitializeX86AsmParser() -> c_void;
	pub fn LLVMInitializeX86AsmPrinter() -> c_void;
	pub fn LLVMGetDefaultTargetTriple() -> *mut c_char;
	pub fn LLVMGetTargetFromTriple(Triple: *const c_char, T: *mut LLVMTargetRef, ErrorMessage: *mut *mut c_char) -> LLVMBool;
	pub fn LLVMCreateTargetMachine(
		T: LLVMTargetRef, Triple: *const c_char,
		CPU: *const c_char, Features: *const c_char, Level: LLVMCodeGenOptLevel, Reloc: LLVMRelocMode, CodeModel: LLVMCodeModel
	) -> LLVMTargetMachineRef;
	pub fn LLVMCreateTargetDataLayout(T: LLVMTargetMachineRef) -> LLVMTargetDataRef;
	pub fn LLVMIntPtrTypeInContext(C: LLVMContextRef, TD: LLVMTargetDataRef) -> LLVMTypeRef;
	pub fn LLVMSetModuleDataLayout(M: LLVMModuleRef, DL: LLVMTargetDataRef) -> c_void;
	pub fn LLVMTargetMachineEmitToFile(
		T: LLVMTargetMachineRef, M: LLVMModuleRef, Filename: *const c_char, codegen: LLVMCodeGenFileType, ErrorMessage: *mut *mut c_char
	) -> LLVMBool;
	pub fn LLVMSizeOfTypeInBits(TD: LLVMTargetDataRef, Ty: LLVMTypeRef) -> c_ulonglong;
}

#[allow(non_upper_case_globals)]
pub const LLVMCodeGenLevelDefault: LLVMCodeGenOptLevel = 2;
#[allow(non_upper_case_globals)]
pub const LLVMRelocDefault: LLVMRelocMode = 0;
#[allow(non_upper_case_globals)]
pub const LLVMCodeModelDefault: LLVMCodeModel = 0;
#[allow(non_upper_case_globals)]
pub const LLVMDLLImportLinkage: LLVMLinkage = 10;
#[allow(non_upper_case_globals)]
pub const LLVMExternalLinkage: LLVMLinkage = 0;
#[allow(non_upper_case_globals)]
pub const LLVMWin64CallConv: c_uint = 79;
#[allow(non_upper_case_globals)]
pub const LLVMObjectFile: LLVMCodeGenFileType = 1;

#[repr(C)]
#[allow(non_camel_case_types)]
#[derive(PartialEq, Eq, Debug)]
pub enum LLVMTypeKind {
	LLVMVoidTypeKind = 0,
	LLVMHalfTypeKind = 1,
	LLVMFloatTypeKind = 2,
	LLVMDoubleTypeKind = 3,
	LLVMX86_FP80TypeKind = 4,
	LLVMFP128TypeKind = 5,
	LLVMPPC_FP128TypeKind = 6,
	LLVMLabelTypeKind = 7,
	LLVMIntegerTypeKind = 8,
	LLVMFunctionTypeKind = 9,
	LLVMStructTypeKind = 10,
	LLVMArrayTypeKind = 11,
	LLVMPointerTypeKind = 12,
	LLVMVectorTypeKind = 13,
	LLVMMetadataTypeKind = 14,
	LLVMTokenTypeKind = 16,
	LLVMScalableVectorTypeKind = 17,
	LLVMBFloatTypeKind = 18,
	LLVMX86_AMXTypeKind = 19,
	LLVMTargetExtTypeKind = 20,
}

#[repr(C)]
#[allow(non_camel_case_types)]
#[derive(PartialEq, Eq, Debug)]
pub enum LLVMValueKind {
	LLVMArgumentValueKind,
	LLVMBasicBlockValueKind,
	LLVMMemoryUseValueKind,
	LLVMMemoryDefValueKind,
	LLVMMemoryPhiValueKind,

	LLVMFunctionValueKind,
	LLVMGlobalAliasValueKind,
	LLVMGlobalIFuncValueKind,
	LLVMGlobalVariableValueKind,
	LLVMBlockAddressValueKind,
	LLVMConstantExprValueKind,
	LLVMConstantArrayValueKind,
	LLVMConstantStructValueKind,
	LLVMConstantVectorValueKind,

	LLVMUndefValueValueKind,
	LLVMConstantAggregateZeroValueKind,
	LLVMConstantDataArrayValueKind,
	LLVMConstantDataVectorValueKind,
	LLVMConstantIntValueKind,
	LLVMConstantFPValueKind,
	LLVMConstantPointerNullValueKind,
	LLVMConstantTokenNoneValueKind,

	LLVMMetadataAsValueValueKind,
	LLVMInlineAsmValueKind,

	LLVMInstructionValueKind,
	LLVMPoisonValueValueKind,
	LLVMConstantTargetNoneValueKind,
	LLVMConstantPtrAuthValueKind,
}