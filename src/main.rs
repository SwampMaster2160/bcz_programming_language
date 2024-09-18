use std::{collections::{HashMap, HashSet}, env::{args, current_dir}, ffi::CString, mem::take, path::PathBuf, process::Command, ptr::null_mut};

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
mod built_value;
mod file_build_data;

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
	print_after_const_evaluate: bool,
	dump_llvm_module: bool,
	llvm_context: LLVMContextRef,
	llvm_data_layout: LLVMTargetDataRef,
	int_type: LLVMTypeRef,
	llvm_target_triple: CString,
	int_bit_width: u8,
	int_max_value: u64,
	sign_bit_mask: u64,
	char_to_separator_mapping: HashMap<char, Separator>,
	str_to_operator_mapping: HashMap<&'static str, OperatorSymbol>,
	operator_character_set: HashSet<char>,
	char_to_operator_type_mapping: HashMap<char, OperatorType>,
	str_to_keyword_mapping: HashMap<&'static str, Keyword>,
	llvm_target_machine: LLVMTargetMachineRef,
	object_files_to_link: Vec<PathBuf>,
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
			print_after_const_evaluate: false,
			llvm_context: unsafe { LLVMContextCreate() },
			llvm_data_layout: null_mut(),
			int_type: null_mut(),
			int_bit_width: 0,
			int_max_value: 0,
			sign_bit_mask: 0,
			char_to_separator_mapping: Separator::get_symbols_map(),
			str_to_operator_mapping: OperatorSymbol::get_symbols_map(),
			operator_character_set: OperatorSymbol::get_character_set(),
			char_to_operator_type_mapping: OperatorType::get_symbols_map(),
			str_to_keyword_mapping: Keyword::get_symbols_map(),
			print_after_analyzer: false,
			dump_llvm_module: false,
			llvm_target_triple: CString::default(),
			llvm_target_machine: null_mut(),
			object_files_to_link: Vec::new(),
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
			c"generic".as_ptr() as *const u8,
			c"".as_ptr() as *const u8,
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
	main_data.sign_bit_mask = main_data.int_max_value & !(main_data.int_max_value >> 1);
	// Compile
	for filepath in take(&mut main_data.filepaths_to_compile).iter() {
		let absolute_filepath = main_data.source_path.join(filepath);
		let result = compile_file(&mut main_data, &absolute_filepath);
		if let Err((error, error_file, error_line, error_column)) = result {
			print!("Error while compiling {}:{error_line}:{error_column}: {error}.", error_file.display());
			return;
		}
	}
	// Link
	if let Some(primary_output_file) = main_data.primary_output_file {
		let primary_output_file_path = main_data.binary_path.join(primary_output_file);
		let mut command = Command::new("gcc");
		for object_file in main_data.object_files_to_link.iter() {
			command.arg(object_file);
		}
		command.arg("-o");
		command.arg(primary_output_file_path);
		command.output().ok();
	}
	// Clean up
	unsafe { LLVMContextDispose(main_data.llvm_context) };
}