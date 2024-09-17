use std::{collections::{HashMap, HashSet}, ffi::c_uint, fs::File, io::{BufRead, BufReader}, path::PathBuf, ptr::null};

use crate::{ast_node::AstNode, error::Error, file_build_data::FileBuildData, llvm_c::{LLVMAddFunction, LLVMAppendBasicBlockInContext, LLVMBool, LLVMBuildCall2, LLVMBuildIntToPtr, LLVMBuildRet, LLVMBuildTrunc, LLVMCreateBuilderInContext, LLVMDisposeBuilder, LLVMDisposeModule, LLVMDumpModule, LLVMExternalLinkage, LLVMFunctionType, LLVMInt32TypeInContext, LLVMModuleCreateWithNameInContext, LLVMModuleRef, LLVMPointerType, LLVMPositionBuilderAtEnd, LLVMSetFunctionCallConv, LLVMSetLinkage, LLVMSetModuleDataLayout, LLVMSetTarget, LLVMWin64CallConv}, parse::parse_tokens, token::Token, MainData};

/// Compiles the file at `filepath`.
pub fn compile_file(main_data: &mut MainData, filepath: &PathBuf) -> Result<(), (Error, PathBuf, usize, usize)> {
	// Open file
	let file = File::open(filepath)
		.map_err(|_| (Error::CouldNotOpenFile, filepath.clone(), 1, 1))?;
	let mut file_reader = BufReader::new(file);
	// Go over each line
	let mut tokens = Vec::new();
	for line_number in 1.. {
		let mut line_content = String::new();
		// Read the line
		match file_reader.read_line(&mut line_content) {
			// End of file encountered
			Ok(0) => break,
			// Normal
			Ok(_) => {},
			// Error
			Err(_) => return Err((Error::CouldNotReadLine, filepath.clone(), line_number, 1)),
		}
		// Read tokens from line
		let line_content = line_content.as_str();
		tokenize_line(main_data, line_content, line_number, &mut tokens).map_err(|(error, column)| (error, filepath.clone(), line_number, column))?;
	}
	// Print tokens if commanded to do so
	if main_data.print_tokens {
		println!("Tokens from tokenizing file {}:", filepath.display());
		for token in tokens.iter() {
			println!("{:?}", token);
		}
	}
	// Parse
	let mut ast_nodes = parse_tokens(tokens).map_err(|(error, (line, column))| (error, filepath.clone(), line, column))?;
	// Print parsed AST nodes if commanded to do so
	if main_data.print_tokens {
		println!("Tokens from parsing file {}:", filepath.display());
		for ast_node in ast_nodes.iter() {
			ast_node.print_tree(0);
		}
	}
	// Separate global variables out
	let mut globals = HashMap::new();
	for ast_node in ast_nodes.iter_mut() {
		ast_node.separate_globals(&mut globals, true)
			.map_err(|(error, (line, column))| (error, filepath.clone(), line, column))?;
	}
	// Get dependencies for each global variable
	let mut import_dependencies = HashSet::new();
	let mut globals_and_dependencies: HashMap<Box<str>, (AstNode, HashSet<Box<str>>)> = HashMap::new();
	for (name, expression) in globals.into_iter() {
		let mut variable_dependencies = HashSet::new();
		expression.get_variable_dependencies(&mut variable_dependencies, &mut import_dependencies, &mut HashSet::new(), false, false)
			.map_err(|(error, (line, column))| (error, filepath.clone(), line, column))?;
		globals_and_dependencies.insert(name, (expression, variable_dependencies));
	}
	// Print global variables if commanded to do so
	if main_data.print_after_analyzer {
		println!("Globals of {}:", filepath.display());
		for (name, (global, variable_dependencies)) in globals_and_dependencies.iter() {
			print!("{name} -> {:?} = ", variable_dependencies);
			global.print_tree(0);
		}
		println!("Import dependencies of {}:", filepath.display());
		for import_dependency in import_dependencies {
			println!("{import_dependency}");
		}
	}
	// Const evaluate globals
	let mut globals_and_dependencies_after_const_evaluate: HashMap<Box<str>, (AstNode, HashSet<Box<str>>)> = HashMap::new();
	while globals_and_dependencies.len() > globals_and_dependencies_after_const_evaluate.len() {
		let mut globals_have_been_const_evaluated_this_round = false;
		'a: for (name, (global, variable_dependencies)) in globals_and_dependencies.iter_mut() {
			// Make sure that the dependencies are const evaluated
			if globals_and_dependencies_after_const_evaluate.contains_key(name) {
				continue 'a;
			}
			for variable_dependency in variable_dependencies.iter() {
				if !globals_and_dependencies_after_const_evaluate.contains_key(variable_dependency) {
					continue 'a;
				}
			}
			// Const evaluate
			let mut new_global = global.clone();
			let mut new_variable_dependencies = variable_dependencies.clone();
			new_global.const_evaluate(main_data, &globals_and_dependencies_after_const_evaluate, &mut new_variable_dependencies, false)
				.map_err(|(error, (line, column))| (error, filepath.clone(), line, column))?;
			// Add to list
			globals_and_dependencies_after_const_evaluate.insert(name.clone(), (new_global, new_variable_dependencies));
			globals_have_been_const_evaluated_this_round = true;
		}
		// If we did not const evaluate anything this round, there is a cyclic dependency
		if !globals_have_been_const_evaluated_this_round {
			let error_pos = globals_and_dependencies.iter().next().unwrap().1.0.start;
			return Err((Error::InvalidDependency, filepath.clone(), error_pos.0, error_pos.1));
		}
	}
	drop(globals_and_dependencies);
	// Print const evaluated globals if commanded to do so
	if main_data.print_after_analyzer {
		println!("Const evaluated globals of {}:", filepath.display());
		for (name, (global, variable_dependencies)) in globals_and_dependencies_after_const_evaluate.iter() {
			print!("{name} -> {:?} = ", variable_dependencies);
			global.print_tree(0);
		}
	}
	// TODO: compile import dependencies
	// Build LLVM module
	let mut module_name: Vec<u8> = match filepath.file_stem() {
		None => "invalid_name",
		Some(stem) => match stem.to_str() {
			None => "invalid_name",
			Some(stem) => stem,
		}
	}.bytes().collect();
	module_name.push(0);
	let llvm_module = unsafe { LLVMModuleCreateWithNameInContext(module_name.as_ptr(), main_data.llvm_context) };
	build_llvm_module(main_data, llvm_module, globals_and_dependencies_after_const_evaluate)
		.map_err(|(error, (line, column))| (error, filepath.clone(), line, column))?;
	// Dump module if commanded to do so
	if main_data.dump_llvm_module {
		println!("LLVM IR of {}:", filepath.display());
		unsafe { LLVMDumpModule(llvm_module) };
	}
	// Clean up
	unsafe { LLVMDisposeModule(llvm_module) };
	// Return
	Ok(())
}

/// Takes in a line of source code and tokenizes it to `Token`s that are appended to `push_to`.
fn tokenize_line(main_data: &mut MainData, mut line_string: &str, line_number: usize, push_to: &mut Vec<Token>) -> Result<(), (Error, usize)> {
	let mut column_number = 1;
	loop {
		// Get how many whitespace chars there are untill the next non-whitespace, chars and bytes are the same size sice since we are only looking for ASCII whitespace chars
		let start_whitespace_length = match line_string.find(|chr: char| !chr.is_ascii_whitespace()) {
			Some(start_whitespace_length) => start_whitespace_length,
			None => break,
		};
		// Skip said amount of chars
		column_number += start_whitespace_length;
		line_string = &line_string[start_whitespace_length..];
		// Tokenize a token from the string and push to list of read tokens
		let (token, new_line_string) = Token::tokenize_from_line(main_data, line_string, line_number, column_number)
			.map_err(|error| (error, column_number))?;
		match token {
			Some(token) => push_to.push(token),
			None => {},
		}
		// Skip over the chars that where consumed by the tokenization
		let bytes_consumed_by_parse = line_string.len() - new_line_string.len();
		let chars_consumed_by_parse = &line_string[..bytes_consumed_by_parse].chars().count();
		column_number += chars_consumed_by_parse;
		line_string = new_line_string;
	}
	Ok(())
}

fn build_llvm_module(main_data: &mut MainData, llvm_module: LLVMModuleRef, mut globals_and_dependencies: HashMap<Box<str>, (AstNode, HashSet<Box<str>>)>) -> Result<(), (Error, (usize, usize))> {
	// Set up module
	unsafe { LLVMSetTarget(llvm_module, main_data.llvm_target_triple.as_ptr() as *const u8) };
	unsafe { LLVMSetModuleDataLayout(llvm_module, main_data.llvm_data_layout) };
	// Create data struct for builder
	let llvm_builder = unsafe { LLVMCreateBuilderInContext(main_data.llvm_context) };
	//let mut built_globals: HashMap<Box<str>, BuiltRValue> = HashMap::new();
	let mut file_build_data = FileBuildData {
		llvm_module,
		llvm_builder,
		built_globals: HashMap::new(),
		entrypoint: None,
	};
	// Build each global in rounds
	while !globals_and_dependencies.is_empty() {
		// Build all globals this round in their dependencies are built
		let mut globals_built_this_round = HashSet::new();
		'a: for (name, (global, variable_dependencies)) in globals_and_dependencies.iter() {
			// Make sure that the dependencies are built
			for variable_dependency in variable_dependencies.iter() {
				if !file_build_data.built_globals.contains_key(variable_dependency) {
					continue 'a;
				}
			}
			// Build
			let built_result = global.build_global_assignment(main_data, &mut file_build_data, name)?;
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
			globals_and_dependencies.remove(name);
		}
	}
	// Build entry point
	if let Some(wrapped_entry_point) = file_build_data.entrypoint {
		// Get types of wrapper function
		let int_32_type = unsafe { LLVMInt32TypeInContext(main_data.llvm_context) };
		let entry_point_function_parameters = [main_data.int_type, main_data.int_type, main_data.int_type, int_32_type];
		let entry_point_function_type = unsafe {
			LLVMFunctionType(int_32_type, entry_point_function_parameters.as_ptr(), entry_point_function_parameters.len() as c_uint, false as LLVMBool)
		};
		// Get wrapped function
		let wrapped_entry_point_function_pointer = wrapped_entry_point.get_value(main_data, llvm_builder);
		let wrapped_entry_point_function_parameter_types = [main_data.int_type, main_data.int_type, main_data.int_type, main_data.int_type];
		let wrapped_entry_point_function_type = unsafe {
			LLVMFunctionType(
				main_data.int_type,
				wrapped_entry_point_function_parameter_types.as_ptr(),
				wrapped_entry_point_function_parameter_types.len() as c_uint,
				false as LLVMBool
			)
		};
		let wrapped_entry_point_function_pointer_type = unsafe { LLVMPointerType(wrapped_entry_point_function_type, 0) };
		let wrapped_entry_point_function_pointer = unsafe {
			LLVMBuildIntToPtr(llvm_builder, wrapped_entry_point_function_pointer, wrapped_entry_point_function_pointer_type, c"int_to_fn_ptr_temp".as_ptr() as *const u8)
		};
		// Build wrapper function
		// TODO: Non-Windows
		let entry_point_function = unsafe { LLVMAddFunction(llvm_module, c"WinMain".as_ptr() as *const u8, entry_point_function_type) };
		unsafe { LLVMSetLinkage(entry_point_function, LLVMExternalLinkage) };
		unsafe { LLVMSetFunctionCallConv(entry_point_function, LLVMWin64CallConv) };
		let entry_point_function_basic_block = unsafe { LLVMAppendBasicBlockInContext(main_data.llvm_context, entry_point_function, c"entry".as_ptr() as *const u8) };
		unsafe { LLVMPositionBuilderAtEnd(llvm_builder, entry_point_function_basic_block) };
		let built_function_call = unsafe {
			LLVMBuildCall2(
				llvm_builder,
				wrapped_entry_point_function_type,
				wrapped_entry_point_function_pointer,
				null(),
				0,
				c"function_call_temp".as_ptr() as *const u8,
			)
		};
		unsafe { LLVMBuildRet(llvm_builder, LLVMBuildTrunc(llvm_builder, built_function_call, int_32_type, c"trunc_cast_temp".as_ptr() as *const u8)) };
	}
	// Clean up and return
	unsafe { LLVMDisposeBuilder(llvm_builder) };
	Ok(())
}