use std::{collections::{HashMap, HashSet}, env::{args, current_dir}, ffi::CString, mem::take, path::PathBuf, ptr::null_mut};

use compile::compile_file;
use compiler_arguments::process_arguments;
use llvm_c::{
	LLVMCodeGenLevelDefault, LLVMCodeModelDefault, LLVMContextCreate, LLVMContextDispose, LLVMContextRef, LLVMCreateTargetDataLayout, LLVMCreateTargetMachine, LLVMGetTargetFromTriple, LLVMInitializeX86AsmParser, LLVMInitializeX86AsmPrinter, LLVMInitializeX86Target, LLVMInitializeX86TargetInfo, LLVMInitializeX86TargetMC, LLVMIntPtrTypeInContext, LLVMRelocDefault, LLVMSizeOfTypeInBits, LLVMTargetDataRef, LLVMTargetMachineRef, LLVMTargetRef, LLVMTypeRef
};
use token::{Keyword, OperatorSymbol, OperatorType, Separator};

mod llvm_c;
mod compiler_arguments;
mod error;
mod compile;
mod token;
mod ast_node;
mod parse;

pub struct MainData<'a> {
	do_link: bool,
	primary_output_file: Option<&'a str>,
	filepaths_to_compile: Vec<&'a str>,
	compiler_working_directory: PathBuf,
	source_path: PathBuf,
	binary_path: PathBuf,
	print_tokens: bool,
	print_ast_nodes: bool,
	print_after_analyzer: bool,
	dump_llvm_module: bool,
	llvm_context: LLVMContextRef,
	llvm_data_layout: LLVMTargetDataRef,
	int_type: LLVMTypeRef,
	llvm_target_triple: CString,
	int_bit_width: u8,
	int_max_value: u64,
	char_to_separator_mapping: HashMap<char, Separator>,
	str_to_operator_mapping: HashMap<&'static str, OperatorSymbol>,
	operator_character_set: HashSet<char>,
	char_to_operator_type_mapping: HashMap<char, OperatorType>,
	str_to_keyword_mapping: HashMap<&'static str, Keyword>,
	//llvm_target: LLVMTargetRef,
	llvm_target_machine: LLVMTargetMachineRef,
}

impl<'a> MainData<'a> {
	pub fn new() -> Self {
		Self {
			do_link: true,
			primary_output_file: None,
			filepaths_to_compile: Vec::new(),
			compiler_working_directory: current_dir().unwrap(),
			source_path: PathBuf::new(),
			binary_path: PathBuf::new(),
			print_tokens: false,
			print_ast_nodes: false,
			llvm_context: unsafe { LLVMContextCreate() },
			llvm_data_layout: null_mut(),
			int_type: null_mut(),
			int_bit_width: 0,
			int_max_value: 0,
			char_to_separator_mapping: Separator::get_symbols_map(),
			str_to_operator_mapping: OperatorSymbol::get_symbols_map(),
			operator_character_set: OperatorSymbol::get_character_set(),
			char_to_operator_type_mapping: OperatorType::get_symbols_map(),
			str_to_keyword_mapping: Keyword::get_symbols_map(),
			print_after_analyzer: false,
			dump_llvm_module: false,
			llvm_target_triple: CString::default(),
			//llvm_target: null_mut(),
			llvm_target_machine: null_mut(),
		}
	}
}

fn main() {
	let mut main_data = MainData::new();
	// Get and process arguments
	let arguments: Box<[Box<str>]> = args().skip(1).map(|string| string.into_boxed_str()).collect();
	let arguments: Box<[&str]> = arguments.iter().map(|argument| &**argument).collect();
	let result = process_arguments(&mut main_data, &arguments);
	if let Err(error) = result {
		println!("Error while processing compiler arguments: {error}.");
		return;
	}
	// Setup LLVM
	// TODO: Non-X86
	unsafe { LLVMInitializeX86TargetInfo() };
	unsafe { LLVMInitializeX86Target() };
	unsafe { LLVMInitializeX86TargetMC() };
	unsafe { LLVMInitializeX86AsmParser() };
	unsafe { LLVMInitializeX86AsmPrinter() };
	main_data.llvm_target_triple = c"x86_64-pc-windows-msvc".into();
	let mut llvm_target: LLVMTargetRef = null_mut();
	let result = unsafe { LLVMGetTargetFromTriple(main_data.llvm_target_triple.as_ptr() as *const u8, &mut llvm_target, null_mut()) };
	if result != 0 {
		println!("Error: failed to get target.");
		return;
	}
	main_data.llvm_target_machine = unsafe {
		LLVMCreateTargetMachine(
			llvm_target,
			main_data.llvm_target_triple.as_ptr() as *const u8,
			"generic\0".as_ptr(),
			"\0".as_ptr(),
			LLVMCodeGenLevelDefault,
			LLVMRelocDefault,
			LLVMCodeModelDefault,
		)
	};
	main_data.llvm_data_layout = unsafe { LLVMCreateTargetDataLayout(main_data.llvm_target_machine) };
	// Get info about machine being compiled for
	main_data.int_type = unsafe { LLVMIntPtrTypeInContext(main_data.llvm_context, main_data.llvm_data_layout) };
	let int_type_width = unsafe { LLVMSizeOfTypeInBits(main_data.llvm_data_layout, main_data.int_type) };
	if int_type_width > 64 {
		println!("Error: Unsupported architecture, bit width of {int_type_width}, greater than 64.");
		return;
	}
	main_data.int_bit_width = int_type_width as u8;
	main_data.int_max_value = ((1u128 << main_data.int_bit_width) - 1) as u64;
	// Compile
	for filepath in take(&mut main_data.filepaths_to_compile).iter() {
		let absolute_filepath = main_data.source_path.join(filepath);
		let result = compile_file(&mut main_data, &absolute_filepath);
		if let Err((error, error_file, error_line, error_column)) = result {
			print!("Error while compiling {}:{error_line}:{error_column}: {error}.", error_file.display());
			return;
		}
	}
	// Clean up
	unsafe { LLVMContextDispose(main_data.llvm_context) };
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
