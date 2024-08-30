use std::{fs::File, io::{BufRead, BufReader}, path::PathBuf};

use crate::{error::Error, parse::parse_tokens, token::Token, MainData};

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
		let mut column_number = 1;
		let mut line_content = line_content.as_str();
		loop {
			// Get how many whitespace chars there are untill the next non-whitespace, chars and bytes are the same size sice since we are only looking for ASCII whitespace chars
			let start_whitespace_length = match line_content.find(|chr: char| !chr.is_ascii_whitespace()) {
				Some(start_whitespace_length) => start_whitespace_length,
				None => break,
			};
			// Skip said amount of chars
			column_number += start_whitespace_length;
			line_content = &line_content[start_whitespace_length..];
			// Tokenize a token from the string and push to list of read tokens
			let (token, new_line_content) = Token::tokenize_from_line(main_data, line_content, line_number, column_number)
				.map_err(|error| (error, filepath.clone(), line_number, column_number))?;
			tokens.push(token);
			// Skip over the chars that where consumed by the tokenization
			let bytes_consumed_by_parse = line_content.len() - new_line_content.len();
			let chars_consumed_by_parse = &line_content[..bytes_consumed_by_parse].chars().count();
			column_number += chars_consumed_by_parse;
			line_content = new_line_content;
		}
	}
	// Print tokens if commanded to do so
	if main_data.print_tokens {
		println!("Tokens from tokenizing file {}:", filepath.display());
		for token in tokens.iter() {
			println!("{:?}", token);
		}
	}
	// Parse
	let ast_nodes = parse_tokens(tokens).map_err(|(error, (line, column))| (error, filepath.clone(), line, column))?;
	// Print parsed AST nodes if commanded to do so
	if main_data.print_tokens {
		println!("Tokens from parsing file {}:", filepath.display());
		for ast_node in ast_nodes.iter() {
			ast_node.print_tree(0);
			//println!("{:?}", ast_node);
		}
	}
	// Add to main data
	// Return
	Ok(())
}