use std::{collections::{HashMap, HashSet}, env::{args, current_dir}, ffi::CString, mem::take, path::PathBuf, process::Command, ptr::null_mut};

use compile::compile_file;
use compiler_arguments::process_arguments;
use llvm::{context::Context, llvm_c::{
	LLVMCodeGenLevelDefault, LLVMCodeModelDefault, LLVMCreateTargetDataLayout, LLVMCreateTargetMachine, LLVMGetTargetFromTriple, LLVMInitializeX86AsmParser, LLVMInitializeX86AsmPrinter, LLVMInitializeX86Target, LLVMInitializeX86TargetInfo, LLVMInitializeX86TargetMC, LLVMIntPtrTypeInContext, LLVMRelocDefault, LLVMSizeOfTypeInBits, LLVMTargetDataRef, LLVMTargetMachineRef, LLVMTargetRef
}, llvm_type::Type};
use token::{Keyword, OperatorSymbol, OperatorType, Separator};

mod compiler_arguments;
mod error;
mod compile;
mod token;
mod ast_node;
mod parse;
mod built_value;
mod file_build_data;
pub mod llvm;

/// Info that applies while compiling all files.
pub struct MainData<'a> {
	/// Should the compiled .o files be linked to create a primary output file?
	do_link: bool,
	/// The path of the primary output file realitive to `binary_path`.
	primary_output_file: Option<&'a str>,
	/// A list of paths to source files to compile, paths are realitive to `source_path`.
	filepaths_to_compile: Vec<&'a str>,
	/// The working directory of the compiler.
	compiler_working_directory: PathBuf,
	/// The path of all source files to be compiled are realitive to this path.
	source_path: PathBuf,
	/// The path of all compiled output files are realitive to this path.
	binary_path: PathBuf,
	/// Should the tokens from each file be printed after tokenization of the file.
	print_tokens: bool,
	/// Should the AST nodes from each file be printed after parsing of the files tokens.
	print_ast_nodes: bool,
	/// Should the AST nodes from each global variable be printed after global variables have been separated out and their dependencies have been analyzed.
	print_after_analyzer: bool,
	/// Should the AST nodes from each global variable be printed after constant evaluation.
	print_after_const_evaluate: bool,
	/// Should the built LLVM module be printed for each file after being built.
	dump_llvm_module: bool,
	/// The context for LLVM functions.
	llvm_context: Context,
	/// The data layout fo the target machine.
	llvm_data_layout: LLVMTargetDataRef,
	/// The integer type for the target machine, should be big enough to hold a pointer.
	int_type: Type,
	/// A C string that contains info about the target machine.
	llvm_target_triple: CString,
	/// How many bits width the target machine integer is.
	int_bit_width: u8,
	/// The max value of the target machine's integer.
	int_max_value: u64,
	/// This value has the bit set that is the sign bit on the target machine's integer type.
	sign_bit_mask: u64,
	/// Maps chars to separators.
	char_to_separator_mapping: HashMap<char, Separator>,
	/// Maps strings to operator bases.
	str_to_operator_mapping: HashMap<&'static str, OperatorSymbol>,
	/// The set of characters that are found in operators.
	operator_character_set: HashSet<char>,
	/// Maps chars to operator type modifiers.
	char_to_operator_type_mapping: HashMap<char, OperatorType>,
	/// Maps strings (whithout the '@' prefix) to keywords.
	str_to_keyword_mapping: HashMap<&'static str, Keyword>,
	/// The target machine for LLVM.
	llvm_target_machine: LLVMTargetMachineRef,
	/// A list of object files that have been outputted as a result of compiling that should be linked to create a primary output file.
	object_files_to_link: Vec<PathBuf>,
}

impl<'a> MainData<'a> {
	pub fn new() -> Self {
		let context = Context::new();
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
			int_type: context.void_type(),
			llvm_context: context,
			llvm_data_layout: null_mut(),
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
	main_data.int_type = unsafe { Type::from_ref(LLVMIntPtrTypeInContext(main_data.llvm_context.get_ref(), main_data.llvm_data_layout)) };
	let int_type_width = unsafe { LLVMSizeOfTypeInBits(main_data.llvm_data_layout, main_data.int_type.get_ref()) };
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
	let primary_output_file = match (main_data.primary_output_file, main_data.do_link) {
		(Some(primary_output_file), true) => Some(primary_output_file),
		(None, true) => Some("out.exe"),
		(_, false) => None,
	};
	if let Some(primary_output_file) = primary_output_file {
		let primary_output_file_path = main_data.binary_path.join(primary_output_file);
		let mut command = Command::new("gcc");
		for object_file in main_data.object_files_to_link.iter() {
			command.arg(object_file);
		}
		command.arg("-o");
		command.arg(primary_output_file_path);
		command.output().ok();
	}
}