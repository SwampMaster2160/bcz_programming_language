use std::{fs::File, io::{BufRead, BufReader}, path::PathBuf};

use crate::{error::Error, token::Token, MainData};

/// Compiles the file at `filepath`.
pub fn compile_file(main_data: &mut MainData, filepath: &PathBuf) -> Result<(), Error> {
	// Open file
	let file = File::open(filepath)
		.map_err(|_| Error::CouldNotOpenFile)?;
	let mut file_reader = BufReader::new(file);
	// Go over each line
	let mut tokens = Vec::new();
	for line_number in 1.. {
		main_data.line = Some(line_number);
		let mut line_content = String::new();
		// Read the line
		match file_reader.read_line(&mut line_content) {
			// End of file encountered
			Ok(0) => break,
			// Normal
			Ok(_) => {},
			// Error
			Err(_) => return Err(Error::CouldNotReadLine),
		}
		// Read tokens from line
		main_data.column = Some(1);
		let mut line_content = line_content.as_str();
		loop {
			// Get how many whitespace chars there are untill the next non-whitespace, chars and bytes are the same size sice since we are only looking for ASCII whitespace chars
			let start_whitespace_length = match line_content.find(|chr: char| !chr.is_ascii_whitespace()) {
				Some(start_whitespace_length) => start_whitespace_length,
				None => break,
			};
			// Skip said amount of chars
			*main_data.column.as_mut().expect("Line number should not be None") += start_whitespace_length;
			line_content = &line_content[start_whitespace_length..];
			// Tokenize a token from the string and push to list of read tokens
			let (token, new_line_content) = Token::tokenize_from_line(line_content)?;
			tokens.push(token);
			// Skip over the chars that where consumed by the tokenization
			let bytes_consumed_by_parse = line_content.len() - new_line_content.len();
			let chars_consumed_by_parse = &line_content[..bytes_consumed_by_parse].chars().count();
			*main_data.column.as_mut().expect("Line number should not be None") += chars_consumed_by_parse;
			line_content = new_line_content;
		}
	}
	// Return
	Ok(())
}