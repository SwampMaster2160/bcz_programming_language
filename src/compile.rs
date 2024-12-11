use std::{collections::{HashMap, HashSet}, fs::{create_dir_all, File}, hash::{DefaultHasher, Hash, Hasher}, io::{BufRead, BufReader}, num::NonZeroUsize, path::{Path, PathBuf}};

use crate::{ast_node::AstNode, error::Error, file_build_data::FileBuildData, parse::parse_tokens, token::Token, MainData, OperatingSystem};
use llvm_nhb::{enums::{CallingConvention, CodegenFileType, Linkage}, module::Module, types::Type};

/// Compiles the file at `filepath`.
pub fn compile_file(main_data: &mut MainData, filepath: &PathBuf) -> Result<(), (Error, Option<(PathBuf, Option<(NonZeroUsize, Option<NonZeroUsize>)>)>)> {
	// Get output path
	let filepath_stem: PathBuf = filepath.file_stem().ok_or_else(|| (Error::UnableToWriteObject, Some((filepath.clone(), None))))?.into();
	let mut output_filepath = main_data.binary_path.clone();
	let mut hasher = DefaultHasher::new();
	filepath.parent().unwrap().hash(&mut hasher);
	output_filepath.push(&format!("{}", hasher.finish()));
	output_filepath.push(match filepath_stem.strip_prefix(&main_data.source_path) {
		Ok(relative) => relative,
		Err(_) => &filepath_stem,
	});
	output_filepath.set_extension("o");
	// Get if we are in the standard library
	let is_in_standard_library = filepath.starts_with(&main_data.standard_library_path);
	// Skip if this file is already compiled
	if main_data.object_files_to_link.contains(&output_filepath) {
		return Ok(());
	}
	// Open file
	println!("{}", filepath.to_str().unwrap());
	let file = File::open(filepath)
		.map_err(|error| (Error::CouldNotOpenFile(error), Some((filepath.clone(), None))))?;
	let mut file_reader = BufReader::new(file);
	// Go over each line
	let mut tokens = Vec::new();
	let mut in_a_block_comment = false;
	for line_number in 1.. {
		let line_number = line_number.try_into().unwrap();
		let mut line_content = String::new();
		// Read the line
		match file_reader.read_line(&mut line_content) {
			// End of file encountered
			Ok(0) => break,
			// Normal
			Ok(_) => {},
			// Error
			Err(_) => return Err((Error::CouldNotReadLine, Some((filepath.clone(), Some((line_number, None)))))),
		}
		// Read tokens from line
		let line_content = line_content.as_str();
		in_a_block_comment = tokenize_line(main_data, line_content, line_number, &mut tokens, in_a_block_comment)
			.map_err(|(error, column)| (error, Some((filepath.clone(), Some((line_number, Some(column)))))))?;
	}
	if in_a_block_comment {
		return Err((Error::UnterminatedBlockComment, Some((filepath.clone(), None))));
	}
	// Print tokens if commanded to do so
	if main_data.print_tokens {
		println!("Tokens from tokenizing file {}:", filepath.display());
		for token in tokens.iter() {
			println!("{:?}", token);
		}
	}
	// Parse
	let mut ast_nodes = parse_tokens(tokens)
		.map_err(|(error, (line, column))| (error, Some((filepath.clone(), Some((line, Some(column)))))))?;
	// Print parsed AST nodes if commanded to do so
	if main_data.print_ast_nodes {
		println!("Tokens from parsing file {}:", filepath.display());
		for ast_node in ast_nodes.iter() {
			ast_node.print_tree(0);
		}
	}
	// Separate global variables out
	let mut globals = HashMap::new();
	for ast_node in ast_nodes.iter_mut() {
		ast_node.separate_globals(&mut globals, true, false)
			.map_err(|(error, (line, column))| (error, Some((filepath.clone(), Some((line, Some(column)))))))?;
	}
	// Get dependencies for each global variable
	let mut import_dependencies = HashSet::new();
	let mut globals_and_dependencies: HashMap<Box<str>, (AstNode, bool, HashSet<Box<str>>)> = HashMap::new();
	for (name, (expression, is_exported)) in globals.into_iter() {
		let mut variable_dependencies = HashSet::new();
		expression.get_variable_dependencies(
			main_data, filepath, &mut variable_dependencies, &mut import_dependencies, &mut Vec::new(), false
		).map_err(|(error, (line, column))| (error, Some((filepath.clone(), Some((line, Some(column)))))))?;
		globals_and_dependencies.insert(name, (expression, is_exported, variable_dependencies));
	}
	// Print global variables if commanded to do so
	if main_data.print_after_analyzer {
		println!("Globals of {}:", filepath.display());
		for (name, (global, is_exported, variable_dependencies)) in globals_and_dependencies.iter() {
			if *is_exported {
				print!("export ");
			}
			print!("{name} -> {:?} = ", variable_dependencies);
			global.print_tree(0);
		}
		println!("Import dependencies of {}:", filepath.display());
		for import_dependency in import_dependencies.iter() {
			println!("{}", import_dependency.display());
		}
	}
	// Compile imports
	for import_dependency_filepath in import_dependencies.iter() {
		compile_file(main_data, import_dependency_filepath)?;
	}
	// Const evaluate globals
	let mut global_function_list = HashSet::new();
	for (name, (global, _is_exported, _)) in globals_and_dependencies.iter_mut() {
		if !global.is_function() {
			continue;
		}
		global_function_list.insert(name.clone());
	}
	let mut globals_and_dependencies_after_const_evaluate: HashMap<Box<str>, (AstNode, bool, HashSet<Box<str>>)> = HashMap::new();
	while globals_and_dependencies.len() > globals_and_dependencies_after_const_evaluate.len() {
		let mut globals_have_been_const_evaluated_this_round = false;
		'a: for (name, (global, is_exported, variable_dependencies)) in globals_and_dependencies.iter_mut() {
			// Make sure that the dependencies are const evaluated
			if globals_and_dependencies_after_const_evaluate.contains_key(name) {
				continue 'a;
			}
			for variable_dependency in variable_dependencies.iter() {
				if !globals_and_dependencies_after_const_evaluate.contains_key(variable_dependency) && !global_function_list.contains(variable_dependency) {
					continue 'a;
				}
			}
			// Const evaluate
			let mut new_global = global.clone();
			let mut new_variable_dependencies = variable_dependencies.clone();
			new_global.const_evaluate(
				main_data, &globals_and_dependencies_after_const_evaluate,
				&mut new_variable_dependencies, &mut Vec::new(), false, false,
				is_in_standard_library
			).map_err(|(error, (line, column))| (error, Some((filepath.clone(), Some((line, Some(column)))))))?;
			// Add to list
			globals_and_dependencies_after_const_evaluate.insert(name.clone(), (new_global, *is_exported, new_variable_dependencies));
			globals_have_been_const_evaluated_this_round = true;
		}
		// If we did not const evaluate anything this round, there is a cyclic dependency
		if !globals_have_been_const_evaluated_this_round {
			let error_pos = globals_and_dependencies.iter().next().unwrap().1.0.start;
			return Err((Error::InvalidDependency, Some((filepath.clone(), Some((error_pos.0, Some(error_pos.1)))))));
		}
	}
	drop(globals_and_dependencies);
	// Print const evaluated globals if commanded to do so
	if main_data.print_after_const_evaluate {
		println!("Const evaluated globals of {}:", filepath.display());
		for (name, (global, is_exported, variable_dependencies)) in globals_and_dependencies_after_const_evaluate.iter() {
			if *is_exported {
				print!("export ");
			}
			print!("{name} -> {:?} = ", variable_dependencies);
			global.print_tree(0);
		}
	}
	// Build LLVM module
	let module_name = match filepath.file_stem() {
		None => "invalid_name",
		Some(stem) => match stem.to_str() {
			None => "invalid_name",
			Some(stem) => stem,
		}
	};
	let llvm_module = main_data.llvm_context.new_module(module_name);
	build_llvm_module(main_data, &llvm_module, globals_and_dependencies_after_const_evaluate, filepath)
		.map_err(|(error, (line, column))| (error, Some((filepath.clone(), Some((line, Some(column)))))))?;
	// Write .o file
	let directory: PathBuf = output_filepath.parent().ok_or_else(|| (Error::UnableToWriteObject, Some((filepath.clone(), None))))?.into();
	if !directory.exists() {
		create_dir_all(directory).map_err(|_| (Error::UnableToWriteObject, Some((filepath.clone(), None))))?;
	}
	let filepath = output_filepath.to_str().ok_or_else(|| (Error::UnableToWriteObject, Some((filepath.clone(), None))))?;
	llvm_module.emit_to_file(&main_data.llvm_target_machine, filepath, CodegenFileType::Object)
		.map_err(|error| (Error::UnableToEmitObjectFile(error), Some((output_filepath.clone(), None))))?;
	main_data.object_files_to_link.push(output_filepath);
	// Return
	Ok(())
}

/// Takes in a line of source code and tokenizes it to `Token`s that are appended to `push_to`.
fn tokenize_line(main_data: &mut MainData, mut line_string: &str, line_number: NonZeroUsize, push_to: &mut Vec<Token>, mut starts_with_block_comment: bool) -> Result<bool, (Error, NonZeroUsize)> {
	let mut column_number = NonZeroUsize::MIN;
	loop {
		// Get how many whitespace chars there are untill the next non-whitespace,
		// chars and bytes are the same size sice since we are only looking for ASCII whitespace chars
		let start_whitespace_length = match line_string.find(|chr: char| !chr.is_ascii_whitespace()) {
			Some(start_whitespace_length) => start_whitespace_length,
			None => break,
		};
		// Skip said amount of chars
		column_number = column_number.saturating_add(start_whitespace_length);
		line_string = &line_string[start_whitespace_length..];
		// Tokenize a token from the string and push to list of read tokens
		let (token, new_line_string, starts_block_comment) = Token::tokenize_from_line(main_data, line_string, line_number, column_number, starts_with_block_comment)
			.map_err(|error| (error, column_number))?;
		match token {
			Some(token) => push_to.push(token),
			None => {},
		}
		starts_with_block_comment = starts_block_comment;
		// Skip over the chars that where consumed by the tokenization
		let bytes_consumed_by_parse = line_string.len() - new_line_string.len();
		let chars_consumed_by_parse = line_string[..bytes_consumed_by_parse].chars().count();
		column_number = column_number.saturating_add(chars_consumed_by_parse);
		line_string = new_line_string;
	}
	Ok(starts_with_block_comment)
}

/// Take in a list of global variables and build them into a LLVM module.
fn build_llvm_module(main_data: &MainData, llvm_module: &Module, globals_and_dependencies: HashMap<Box<str>, (AstNode, bool, HashSet<Box<str>>)>, filepath: &PathBuf)
	-> Result<(), (Error, (NonZeroUsize, NonZeroUsize))> {
	// Set up module
	llvm_module.set_target_triple(&*main_data.llvm_target_triple);
	llvm_module.set_data_layout(&main_data.llvm_data_layout);
	// Create data struct for builder
	let llvm_builder = main_data.llvm_context.new_builder();
	let mut file_build_data = FileBuildData {
		built_globals: HashMap::new(),
		built_global_function_signatures: HashMap::new(),
		entrypoint: None,
		filepath,
	};
	// Build function signatures
	for (name, (global, _is_exported, _)) in globals_and_dependencies.iter() {
		if !global.is_function() {
			continue;
		}
		let function_signature = global.build_function_signature(main_data, &mut file_build_data, llvm_module, &llvm_builder, name, false)?;
		file_build_data.built_global_function_signatures.insert(name.clone(), function_signature);
	}
	// Dump module if commanded to do so after building function signatures
	if main_data.dump_llvm_module_after_function_signatures_build {
		println!("LLVM IR after building function signatures of {}:", filepath.display());
		llvm_module.dump();
	}
	// Build each global in rounds
	let mut globals_built = HashSet::new();
	while globals_and_dependencies.len() > globals_built.len() {
		// Build all globals this round if their dependencies are built
		let mut globals_built_this_round = HashSet::new();
		'a: for (name, (global, is_exported, variable_dependencies)) in globals_and_dependencies.iter() {
			if globals_built.contains(name) {
				continue 'a;
			}
			// Make sure that the dependencies are built
			for variable_dependency in variable_dependencies.iter() {
				if !file_build_data.built_globals.contains_key(variable_dependency) && !file_build_data.built_global_function_signatures.contains_key(variable_dependency) {
					continue 'a;
				}
			}
			// Build
			let built_result = global.build_global_assignment(main_data, llvm_module, &llvm_builder, &mut file_build_data, name, *is_exported)?;
			// Add to list
			file_build_data.built_globals.insert(name.clone(), built_result);
			globals_built_this_round.insert(name.clone());
		}
		// If we did not compile anything this round, there is a cyclic dependency
		if globals_built_this_round.is_empty() {
			return Err((Error::InvalidDependency, globals_and_dependencies.iter().next().unwrap().1.0.start));
		}
		// Remove built globals from the to build list
		for name in globals_built_this_round.iter() {
			globals_built.insert(name.clone());
		}
	}
	// Build entry point
	if let Some(wrapped_entry_point) = file_build_data.entrypoint {
		match main_data.operating_system {
			OperatingSystem::Windows => {
				// Get types of wrapper function
				let int_32_type = main_data.llvm_context.int_32_type();
				let entry_point_function_parameters = [main_data.int_type, main_data.int_type, main_data.int_type, int_32_type];
				let entry_point_function_type = int_32_type.function_type(&entry_point_function_parameters, false);
				// Get wrapped function
				let wrapped_entry_point_function_type = main_data.int_type.function_type(&[], false);
				let wrapped_entry_point_function_pointer_type = wrapped_entry_point_function_type.pointer_to();
				let wrapped_entry_point_function_pointer = wrapped_entry_point
					.build_int_to_ptr(&llvm_builder, wrapped_entry_point_function_pointer_type, "int_to_fn_ptr_temp");
				// Build wrapper function
				let entry_point_function = llvm_module.add_function(entry_point_function_type, "WinMain");
				entry_point_function.set_linkage(Linkage::External);
				entry_point_function.set_calling_convention(CallingConvention::Win64);
				let entry_point_function_basic_block = entry_point_function.append_basic_block(&main_data.llvm_context, "entry");
				llvm_builder.position_at_end(&entry_point_function_basic_block);
				let built_function_call = wrapped_entry_point_function_pointer
					.build_call(&[], wrapped_entry_point_function_type, &llvm_builder, "function_call_temp");
				let truncated_result = built_function_call.build_truncate(&llvm_builder, int_32_type, "trunc_cast_temp");
				truncated_result.build_return(&llvm_builder);
			}
			OperatingSystem::Linux => {

			}
		}
	}
	// Dump module if commanded to do so
	if main_data.dump_llvm_module {
		println!("LLVM IR of {}:", filepath.display());
		llvm_module.dump();
	}
	Ok(())
}

pub fn relative_filepath_to_absolute(main_data: &MainData, current_filepath: &PathBuf, relative_filepath: &str) -> Result<PathBuf, Error> {
	let relative_filepath_path = Path::new(relative_filepath);
	if relative_filepath_path.starts_with("std") {
		let mut result = main_data.standard_library_path.clone();
		for item in relative_filepath_path.iter() {
			if item == "std" {
				continue;
			}
			result = result.join(item);
		}
		return Ok(result.canonicalize().map_err(|_| Error::InvalidFilepath)?)
	}
	let result = current_filepath.parent().ok_or(Error::InvalidFilepath)?
		.join(relative_filepath).canonicalize().map_err(|_| Error::InvalidFilepath)?;
	Ok(result)
}