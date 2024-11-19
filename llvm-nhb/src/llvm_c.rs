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
unsafe extern "C" {
	// Core
	pub unsafe fn LLVMGetVersion(major: *mut c_uint, minor: *mut c_uint, patch: *mut c_uint) -> c_void;
	pub unsafe fn LLVMDisposeMessage(Message: *mut c_char) -> c_void;
	// Core/Contexts
	pub unsafe fn LLVMContextCreate() -> LLVMContextRef;
	pub unsafe fn LLVMContextDispose(C: LLVMContextRef) -> c_void;
	// Core/Modules
	pub unsafe fn LLVMModuleCreateWithNameInContext(ModuleID: *const c_char, C: LLVMContextRef) -> LLVMModuleRef;
	pub unsafe fn LLVMDisposeModule(M: LLVMModuleRef) -> c_void;
	pub unsafe fn LLVMDumpModule(M: LLVMModuleRef) -> c_void;
	pub unsafe fn LLVMSetTarget(M: LLVMModuleRef, Triple: *const c_char) -> c_void;
	pub unsafe fn LLVMAddFunction(M: LLVMModuleRef, Name: *const c_char, FunctionTy: LLVMTypeRef) -> LLVMValueRef;
	// Core/Types
	pub unsafe fn LLVMGetTypeKind(Ty: LLVMTypeRef) -> LLVMTypeKind;
	// Core/Types/Integer Types
	pub unsafe fn LLVMInt1TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
	pub unsafe fn LLVMInt8TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
	pub unsafe fn LLVMInt16TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
	pub unsafe fn LLVMInt32TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
	pub unsafe fn LLVMInt64TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
	pub unsafe fn LLVMInt128TypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
	// Core/Types/Other Types
	pub unsafe fn LLVMVoidTypeInContext(C: LLVMContextRef) -> LLVMTypeRef;
	// Core/Types/Function Types
	pub unsafe fn LLVMFunctionType(ReturnType: LLVMTypeRef, ParamTypes: *const LLVMTypeRef, ParamCount: c_uint, IsVarArg: LLVMBool) -> LLVMTypeRef;
	pub unsafe fn LLVMCountParamTypes(FunctionTy: LLVMTypeRef) -> c_uint;
	pub unsafe fn LLVMGetReturnType(FunctionTy: LLVMTypeRef) -> LLVMTypeRef;
	pub unsafe fn LLVMIsFunctionVarArg(FunctionTy: LLVMTypeRef) -> LLVMBool;
	pub unsafe fn LLVMGetParamTypes(FunctionTy: LLVMTypeRef, Dest: *mut LLVMTypeRef) -> c_void;
	// Core/Types/Sequential Types
	pub unsafe fn LLVMArrayType2(ElementType: LLVMTypeRef, ElementCount: u64) -> LLVMTypeRef;
	pub unsafe fn LLVMPointerType(ElementType: LLVMTypeRef, AddressSpace: c_uint) -> LLVMTypeRef;
	pub unsafe fn LLVMGetElementType(Ty: LLVMTypeRef) -> LLVMTypeRef;
	// Core/Values/Constants
	pub unsafe fn LLVMGetUndef(Ty: LLVMTypeRef) -> LLVMValueRef;
	// Core/Values/Constants/Global Values
	pub unsafe fn LLVMSetLinkage(Global: LLVMValueRef, Linkage: LLVMLinkage) -> c_void;
	// Core/Values/Constants/Global Variables
	pub unsafe fn LLVMAddGlobal(M: LLVMModuleRef, Ty: LLVMTypeRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMSetInitializer(GlobalVar: LLVMValueRef, ConstantVal: LLVMValueRef) -> c_void;
	pub unsafe fn LLVMGetInitializer(GlobalVar: LLVMValueRef) -> LLVMValueRef;
	pub unsafe fn LLVMSetGlobalConstant(GlobalVar: LLVMValueRef, IsConstant: LLVMBool) -> c_void;
	// Core/Values/Constants/Composite Constants
	pub unsafe fn LLVMConstStringInContext(C: LLVMContextRef, Str: *const c_char, Length: c_uint, DontNullTerminate: LLVMBool) -> LLVMValueRef;
	// Core/Values/Constants/Function values
	pub unsafe fn LLVMSetFunctionCallConv(Fn: LLVMValueRef, CC: c_uint) -> c_void;
	// Core/Values/Constants/Function values/Function Parameters
	pub unsafe fn LLVMGetParam(Fn: LLVMValueRef, Index: c_uint) -> LLVMValueRef;
	pub unsafe fn LLVMCountParams(Fn: LLVMValueRef) -> c_uint;
	// Core/Values/Constants/Scalar constants
	pub unsafe fn LLVMConstInt(IntTy: LLVMTypeRef, N: c_ulonglong, SignExtend: LLVMBool) -> LLVMValueRef;
	// Core/Values/General APIs
	pub unsafe fn LLVMGetValueKind(Val: LLVMValueRef) -> LLVMValueKind;
	pub unsafe fn LLVMTypeOf(Val: LLVMValueRef) -> LLVMTypeRef;
	// Core/Basic Block
	pub unsafe fn LLVMAppendBasicBlockInContext(C: LLVMContextRef, Fn: LLVMValueRef, Name: *const c_char) -> LLVMBasicBlockRef;
	pub unsafe fn LLVMInsertBasicBlockInContext(C: LLVMContextRef, BB: LLVMBasicBlockRef, Name: *const c_char) -> LLVMBasicBlockRef;
	// Instruction Builders
	pub unsafe fn LLVMCreateBuilderInContext(C: LLVMContextRef) -> LLVMBuilderRef;
	pub unsafe fn LLVMDisposeBuilder(Builder: LLVMBuilderRef) -> c_void;
	pub unsafe fn LLVMPositionBuilderAtEnd(Builder: LLVMBuilderRef, Block: LLVMBasicBlockRef) -> c_void;
	pub unsafe fn LLVMBuildPtrToInt(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildIntToPtr(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildZExt(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildSExt(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildTrunc(B: LLVMBuilderRef, Val: LLVMValueRef, DestTy: LLVMTypeRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildCall2(B: LLVMBuilderRef, Ty: LLVMTypeRef, Fn: LLVMValueRef, Args: *const LLVMValueRef, NumArgs: c_uint, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildAdd(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildSub(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildMul(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildUDiv(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildURem(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildSDiv(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildSRem(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildAnd(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildOr(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildXor(B: LLVMBuilderRef, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildRet(B: LLVMBuilderRef, V: LLVMValueRef) -> LLVMValueRef;
	pub unsafe fn LLVMBuildRetVoid(B: LLVMBuilderRef) -> LLVMValueRef;
	pub unsafe fn LLVMBuildAlloca(B: LLVMBuilderRef, Ty: LLVMTypeRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildStore(B: LLVMBuilderRef, Val: LLVMValueRef, Ptr: LLVMValueRef) -> LLVMValueRef;
	pub unsafe fn LLVMBuildLoad2(B: LLVMBuilderRef, Ty: LLVMTypeRef, PointerVal: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildNeg(B: LLVMBuilderRef, V: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildNot(B: LLVMBuilderRef, V: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildICmp(B: LLVMBuilderRef, Op: LLVMIntPredicate, LHS: LLVMValueRef, RHS: LLVMValueRef, Name: *const c_char) -> LLVMValueRef;
	pub unsafe fn LLVMBuildBr(B: LLVMBuilderRef, Dest: LLVMBasicBlockRef) -> LLVMValueRef;
	pub unsafe fn LLVMBuildCondBr(B: LLVMBuilderRef, If: LLVMValueRef, Then: LLVMBasicBlockRef, Else: LLVMBasicBlockRef) -> LLVMValueRef;
	pub unsafe fn LLVMBuildGEP2(B: LLVMBuilderRef, Ty: LLVMTypeRef, Pointer: LLVMValueRef, Indices: *mut LLVMValueRef, NumIndices: c_uint, Name: *const c_char) -> LLVMValueRef;
	// Target information
	pub unsafe fn LLVMInitializeX86TargetInfo() -> c_void;
	pub unsafe fn LLVMInitializeX86Target() -> c_void;
	pub unsafe fn LLVMInitializeX86TargetMC() -> c_void;
	pub unsafe fn LLVMInitializeX86AsmParser() -> c_void;
	pub unsafe fn LLVMInitializeX86AsmPrinter() -> c_void;
	pub unsafe fn LLVMGetDefaultTargetTriple() -> *mut c_char;
	pub unsafe fn LLVMGetTargetFromTriple(Triple: *const c_char, T: *mut LLVMTargetRef, ErrorMessage: *mut *mut c_char) -> LLVMBool;
	pub unsafe fn LLVMCreateTargetMachine(
		T: LLVMTargetRef, Triple: *const c_char,
		CPU: *const c_char, Features: *const c_char, Level: LLVMCodeGenOptLevel, Reloc: LLVMRelocMode, CodeModel: LLVMCodeModel
	) -> LLVMTargetMachineRef;
	pub unsafe fn LLVMCreateTargetDataLayout(T: LLVMTargetMachineRef) -> LLVMTargetDataRef;
	pub unsafe fn LLVMIntPtrTypeInContext(C: LLVMContextRef, TD: LLVMTargetDataRef) -> LLVMTypeRef;
	pub unsafe fn LLVMSetModuleDataLayout(M: LLVMModuleRef, DL: LLVMTargetDataRef) -> c_void;
	pub unsafe fn LLVMTargetMachineEmitToFile(
		T: LLVMTargetMachineRef, M: LLVMModuleRef, Filename: *const c_char, codegen: LLVMCodeGenFileType, ErrorMessage: *mut *mut c_char
	) -> LLVMBool;
	pub unsafe fn LLVMSizeOfTypeInBits(TD: LLVMTargetDataRef, Ty: LLVMTypeRef) -> c_ulonglong;
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