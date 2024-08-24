use std::env::args;

use compiler_arguments::process_arguments;

pub mod llvm_c;
mod compiler_arguments;
pub mod error;

fn main() {
	let arguments: Box<[String]> = args().skip(1).collect();
	let arguments: Box<[&str]> = arguments.iter().map(|argument| argument.as_str()).collect();
	let result = process_arguments(&arguments);
	if let Err(error) = result {
		println!("Error while processing compiler arguments: {error}.");
		return;
	}
}

//fn main() {
//	// Print debug info
//	let args: Vec<String> = args().collect();
//	dbg!(args);
//	let mut version_major: c_uint = 0;
//	let mut version_minor: c_uint = 0;
//	let mut version_patch: c_uint = 0;
//	unsafe { LLVMGetVersion(&mut version_major, &mut version_minor, &mut version_patch) };
//	println!("{version_major}.{version_minor}.{version_patch}");
//	// Setup
//	let context = unsafe { LLVMContextCreate() };
//	let module = unsafe { LLVMModuleCreateWithNameInContext("my_module\0".as_ptr(), context) };
//	let builder = unsafe { LLVMCreateBuilderInContext(context) };
//	// Target machine
//	unsafe { LLVMInitializeX86TargetInfo() };
//	unsafe { LLVMInitializeX86Target() };
//	unsafe { LLVMInitializeX86TargetMC() };
//	unsafe { LLVMInitializeX86AsmParser() };
//	unsafe { LLVMInitializeX86AsmPrinter() };
//	let target_triple = unsafe { LLVMGetDefaultTargetTriple() };
//	unsafe { LLVMSetTarget(module, target_triple) };
//	let mut target: LLVMTargetRef = null_mut();
//	let result = unsafe { LLVMGetTargetFromTriple(target_triple, &mut target, null_mut()) };
//	if result != 0 {
//		panic!("Failed to get target");
//	}
//	let target_machine = unsafe { LLVMCreateTargetMachine(
//		target, target_triple, "generic\0".as_ptr(), "\0".as_ptr(),
//		LLVMCodeGenLevelDefault, LLVMRelocDefault, LLVMCodeModelDefault,
//	)};
//	let data_layout = unsafe { LLVMCreateTargetDataLayout(target_machine) };
//	// Types
//	let int_type = unsafe { LLVMIntPtrTypeInContext(context, data_layout) };
//	let int_32_type = unsafe { LLVMInt32TypeInContext(context) };
//	let int_8_type = unsafe { LLVMInt8TypeInContext(context) };
//	//let void_type = unsafe { LLVMVoidTypeInContext(context) };
//	// Link to beep
//	let beep_function_type = unsafe {
//		LLVMFunctionType(int_32_type, [int_32_type, int_32_type].as_ptr(), 2, false as LLVMBool)
//	};
//	let beep_function = unsafe { LLVMAddFunction(module, "Beep\0".as_ptr(), beep_function_type) };
//	unsafe { LLVMSetLinkage(beep_function, LLVMDLLImportLinkage) };
//	unsafe { LLVMSetFunctionCallConv(beep_function, LLVMWin64CallConv) };
//	// Link to printf
//	let printf_function_type = unsafe {
//		LLVMFunctionType(int_32_type, [int_type].as_ptr(), 1, false as LLVMBool)
//	};
//	let printf_function = unsafe { LLVMAddFunction(module, "printf\0".as_ptr(), printf_function_type) };
//	unsafe { LLVMSetLinkage(printf_function, LLVMDLLImportLinkage) };
//	unsafe { LLVMSetFunctionCallConv(printf_function, LLVMWin64CallConv) };
//	// Create string
//	let string = unsafe { LLVMAddGlobal(module, LLVMArrayType2(int_8_type, 13), "myString".as_ptr()) };
//	unsafe { LLVMSetInitializer(
//		string, LLVMConstStringInContext(context, "Hello World\n\0".as_ptr(), 12, false as LLVMBool))
//	};
//	// Create main function
//	let main_function_type = unsafe {
//		LLVMFunctionType(int_32_type, [int_type, int_type, int_type, int_32_type].as_ptr(), 4, false as LLVMBool)
//	};
//	let main_function = unsafe { LLVMAddFunction(module, "WinMain\0".as_ptr(), main_function_type) };
//	unsafe { LLVMSetLinkage(main_function, LLVMExternalLinkage) };
//	unsafe { LLVMSetFunctionCallConv(main_function, LLVMWin64CallConv) };
//	let main_function_basic_block = unsafe { LLVMAppendBasicBlockInContext(context, main_function, "entry\0".as_ptr()) };
//	unsafe { LLVMPositionBuilderAtEnd(builder, main_function_basic_block) };
//	/*let beep_result = */unsafe {
//		LLVMBuildCall2(
//			builder, beep_function_type, beep_function,
//			[
//				LLVMConstInt(int_32_type, 400, false as LLVMBool),
//				LLVMConstInt(int_32_type, 1000, false as LLVMBool),
//			].as_ptr(),
//			2, "\0".as_ptr()
//		)
//	};
//	let int_string_pointer = unsafe { LLVMBuildPtrToInt(builder, string, int_type, "ptrtoint\0".as_ptr()) };
//	unsafe {
//		LLVMBuildCall2(
//			builder, printf_function_type, printf_function,
//			[int_string_pointer].as_ptr(),
//			1, "\0".as_ptr()
//		)
//	};
//	unsafe { LLVMBuildRet(builder, LLVMConstInt(int_32_type, 69, false as LLVMBool)) };
//
//	unsafe { LLVMDisposeBuilder(builder) };
//
//	// Dump
//	unsafe { LLVMDumpModule(module) };
//	// Compile to .o file
//	unsafe { LLVMSetModuleDataLayout(module, data_layout) };
//	unsafe { LLVMTargetMachineEmitToFile(target_machine, module, "test.o\0".as_ptr(), LLVMObjectFile, null_mut()) };
//	// Clean up
//	unsafe { LLVMDisposeModule(module) };
//	unsafe { LLVMContextDispose(context) };
//	// Link .o to create .exe
//	//system("gcc test.o -o test.exe");
//	Command::new("gcc")
//		.arg("test.o")
//		.arg("-o")
//		.arg("test.exe")
//		.output()
//		.ok();
//}
