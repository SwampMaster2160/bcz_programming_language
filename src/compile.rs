use std::{collections::{HashMap, HashSet}, fs::File, io::{BufRead, BufReader}, path::PathBuf};

use crate::{ast_node::AstNode, error::Error, llvm_c::{LLVMDisposeModule, LLVMDumpModule, LLVMModuleCreateWithNameInContext, LLVMModuleRef, LLVMSetModuleDataLayout, LLVMSetTarget, LLVMValueRef}, parse::parse_tokens, token::Token, MainData};

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
		expression.get_variable_dependencies(&mut variable_dependencies, &mut import_dependencies, &mut HashSet::new())
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
	build_llvm_module(main_data, llvm_module, globals_and_dependencies)
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
	// Build each global in rounds
	let mut built_globals: HashMap<Box<str>, LLVMValueRef> = HashMap::new();
	while !globals_and_dependencies.is_empty() {
		// Build all globals this round in their dependencies are built
		let mut globals_built_this_round = HashSet::new();
		for (name, (global, variable_dependencies)) in globals_and_dependencies.iter() {
			// Make sure that the dependencies are built
			for variable_dependency in variable_dependencies.iter() {
				if !built_globals.contains_key(variable_dependency) {
					continue;
				}
			}
			// Build
			let built_result = global.build_global_assignment(name, llvm_module, main_data, &built_globals)?;
			// Add to list
			built_globals.insert(name.clone(), built_result);
			globals_built_this_round.insert(name.clone());
		}
		//
		if globals_built_this_round.is_empty() {
			return Err((Error::CyclicDependency, globals_and_dependencies.iter().next().unwrap().1.0.start));
		}
		// Remove built globals from the to build list
		for name in globals_built_this_round.iter() {
			globals_and_dependencies.remove(name);
		}
	}
	Ok(())
}