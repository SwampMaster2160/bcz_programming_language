use std::{collections::{HashMap, HashSet}, env::args, i64, mem::take, num::NonZeroUsize, path::PathBuf, process::Command};

use compile::compile_file;
use compiler_arguments::{process_arguments, CompilerArgumentsData};
use error::Error;
use llvm_nhb::{context::Context, other::initialize_x86, target::Target, target_data::TargetData, target_machine::TargetMachine, types::Type};
use llvm_nhb::enums::{CodeModel, CodegenOptLevel, RealocMode};
use token::{Keyword, OperatorSymbol, OperatorType, Separator};

mod compiler_arguments;
mod error;
mod compile;
mod token;
mod ast_node;
mod parse;
mod built_value;
mod file_build_data;
mod function_building_data;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum OperatingSystem {
	Windows = 0,
	Linux = 1,
}

/// Info that applies while compiling all files.
pub struct MainData<'a> {
	/// Should the compiled .o files be linked to create a primary output file?
	do_link: bool,
	/// The path of the primary output file realitive to `binary_path`.
	primary_output_file: Option<&'a str>,
	/// A list of paths to source files to compile, paths are realitive to `source_path`.
	filepaths_to_compile: Vec<&'a str>,
	/// The working directory of the compiler.
	//compiler_working_directory: PathBuf,
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
	/// Should the built LLVM module be printed for each file after function signatures have been build.
	dump_llvm_module_after_function_signatures_build: bool,
	/// Should the built LLVM module be printed for each file after being built.
	dump_llvm_module: bool,
	/// The context for LLVM functions.
	llvm_context: &'a Context,
	/// The data layout fo the target machine.
	llvm_data_layout: &'a TargetData<'a>,
	/// The integer type for the target machine, should be big enough to hold a pointer.
	int_type: Type<'a>,
	/// The 8-bit integer type for the target machine.
	int_8_type: Type<'a>,
	/// A C string that contains info about the target machine.
	llvm_target_triple: Box<str>,
	//llvm_target_triple: String,
	/// How many bits width the target machine integer is.
	int_bit_width: u8,
	/// How many bytes the target machine integer is wide log 2.
	int_power_width: u8,
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
	llvm_target_machine: &'a TargetMachine,
	/// A list of object files that have been outputted as a result of compiling that should be linked to create a primary output file.
	object_files_to_link: Vec<PathBuf>,
	/// The path to the BCZ standard library.
	standard_library_path: PathBuf,

	operating_system: OperatingSystem,

	link_command: Box<str>,

	libraries_to_link_to: HashSet<Box<str>>,
}

impl<'a> MainData<'a> {
	pub fn new(
		compiler_arguments_data: CompilerArgumentsData<'a>, context: &'a Context, target_machine: &'a TargetMachine, target_data: &'a TargetData<'a>,
		int_type: Type<'a>, int_8_type: Type<'a>,
	) -> Result<Self, Error> {
		// Get standard library path
		let standard_library_path = compiler_arguments_data.compiler_working_directory.join("std").canonicalize().unwrap();
		// Parse target triplet
		//println!("{}", compiler_arguments_data.target_triplet);
		let mut target_triplet_parts = compiler_arguments_data.target_triplet.split('-');
		match target_triplet_parts.next() {
			Some("x86_64") => {}
			Some(other) => return Err(Error::UnsupportedCPU(other.into())),
			None => return Err(Error::InvalidTargetTriplet(compiler_arguments_data.target_triplet.into_string())),
		}
		target_triplet_parts.next();
		let operating_system = match target_triplet_parts.next() {
			Some("windows") => OperatingSystem::Windows,
			Some("linux") => OperatingSystem::Linux,
			Some(other) => return Err(Error::UnsupportedOS(other.into())),
			None => return Err(Error::InvalidTargetTriplet(compiler_arguments_data.target_triplet.into_string())),
		};
		// Pack into struct
		Ok(Self {
			llvm_context: context,
			do_link: compiler_arguments_data.do_link,
			primary_output_file: compiler_arguments_data.primary_output_file,
			filepaths_to_compile: compiler_arguments_data.filepaths_to_compile,
			//compiler_working_directory: compiler_arguments_data.compiler_working_directory,
			source_path: compiler_arguments_data.source_path,
			binary_path: compiler_arguments_data.binary_path,
			print_tokens: compiler_arguments_data.print_tokens,
			print_ast_nodes: compiler_arguments_data.print_ast_nodes,
			print_after_const_evaluate: compiler_arguments_data.print_after_const_evaluate,
			dump_llvm_module_after_function_signatures_build: compiler_arguments_data.dump_llvm_module_after_function_signatures_build,
			int_type,
			llvm_data_layout: target_data,
			int_bit_width: 0,
			int_max_value: 0,
			sign_bit_mask: 0,
			int_power_width: 0,
			char_to_separator_mapping: Separator::get_symbols_map(),
			str_to_operator_mapping: OperatorSymbol::get_symbols_map(),
			operator_character_set: OperatorSymbol::get_character_set(),
			char_to_operator_type_mapping: OperatorType::get_symbols_map(),
			str_to_keyword_mapping: Keyword::get_symbols_map(),
			print_after_analyzer: compiler_arguments_data.print_after_analyzer,
			dump_llvm_module: compiler_arguments_data.dump_llvm_module,
			llvm_target_triple: compiler_arguments_data.target_triplet,
			llvm_target_machine: target_machine,
			object_files_to_link: Vec::new(),
			int_8_type,
			standard_library_path,
			operating_system,
			link_command: compiler_arguments_data.link_command,
			libraries_to_link_to: HashSet::new(),
		})
	}

	pub fn value_to_signed(&self, value: u64) -> i64 {
		let sign_bit = (value & self.sign_bit_mask) != 0;
		(value & (self.int_max_value >> 1)) as i64 | match sign_bit {
			true => i64::MIN,
			false => 0,
		}
	}

	pub fn signed_to_value(&self, signed: i64) -> u64 {
		let sign_bit = (signed & i64::MIN) != 0;
		(signed as u64 & (self.int_max_value >> 1)) | match sign_bit {
			true => self.sign_bit_mask,
			false => 0,
		}
	}
}

fn main() {
	match main_error_handled() {
		Ok(..) => {}
		Err((error, error_location)) => {
			print!("Error");
			if let Some((error_file, error_row_column)) = error_location {
				print!(" in file {}", error_file.display());
				if let Some((error_row, error_column)) = error_row_column {
					print!(":{error_row}");
					if let Some(error_column) = error_column {
						print!(":{error_column}");
					}
				}
			}
			println!(": {error}.");
		}
	}
}

fn main_error_handled() -> Result<(), (Error, Option<(PathBuf, Option<(NonZeroUsize, Option<NonZeroUsize>)>)>)> {
	// Get and process arguments
	let arguments: Box<[Box<str>]> = args().skip(1).map(|string| string.into_boxed_str()).collect();
	let arguments: Box<[&str]> = arguments.iter().map(|argument| &**argument).collect();
	let mut compiler_arguments_data = CompilerArgumentsData::new();
	process_arguments(&arguments, &mut compiler_arguments_data).map_err(|error| (error, None))?;
	// Setup LLVM
	initialize_x86();
	let llvm_target = Target::from_triple(&compiler_arguments_data.target_triplet).map_err(|llvm_error| (Error::CouldNotGetTarget(llvm_error), None))?;
	let llvm_target_machine = llvm_target.create_target_machine(
		&compiler_arguments_data.target_triplet, "generic", "", CodegenOptLevel::Default, RealocMode::Default, CodeModel::Default
	);
	let llvm_data_layout = llvm_target_machine.get_target_data();
	let context = Context::new();
	let int_type = llvm_data_layout.int_ptr_type(&context);
	let int_8_type = context.int_8_type();
	let mut main_data = MainData::new(compiler_arguments_data, &context, &llvm_target_machine, &llvm_data_layout, int_type, int_8_type)
		.map_err(|error| (error, None))?;
	// Get info about machine being compiled for
	let int_type_width = main_data.int_type.size_in_bits(&main_data.llvm_data_layout);
	if int_type_width > 64 {
		return Err((Error::InvalidArchitectureBitWidth(int_type_width), None));
	}
	main_data.int_bit_width = int_type_width as u8;
	main_data.int_max_value = ((1u128 << main_data.int_bit_width) - 1) as u64;
	main_data.sign_bit_mask = main_data.int_max_value & !(main_data.int_max_value >> 1);
	main_data.int_power_width = (main_data.int_bit_width / 8).ilog2() as u8;
	// Compile
	for filepath in take(&mut main_data.filepaths_to_compile).iter() {
		let absolute_filepath = main_data.source_path.join(filepath).canonicalize().unwrap();
		compile_file(&mut main_data, &absolute_filepath)?;
	}
	// Link
	let primary_output_file = match (main_data.primary_output_file, main_data.do_link) {
		(Some(primary_output_file), true) => Some(primary_output_file),
		(None, true) => Some(match main_data.operating_system {
			OperatingSystem::Windows => "out.exe",
			OperatingSystem::Linux => "out",
		}),
		(_, false) => None,
	};
	if let Some(primary_output_file) = primary_output_file {
		let primary_output_file_path = main_data.binary_path.join(primary_output_file);
		let mut command = Command::new(&*main_data.link_command);
		for object_file in main_data.object_files_to_link.iter() {
			command.arg(object_file);
		}
		for library_to_link_to in main_data.libraries_to_link_to.iter() {
			command.arg(&**library_to_link_to);
		}
		if main_data.operating_system == OperatingSystem::Linux {
			command.arg("-nostdlib");
			command.arg("-static");
			command.arg("-no-pie");
		}
		command.arg("-o");
		command.arg(primary_output_file_path);
		let result = command.output().map_err(|_| (Error::ErrorWhileLinking(None), None))?;
		if !result.status.success() {
			return Err((Error::ErrorWhileLinking(result.status.code()), None));
		}
	}
	Ok(())
}