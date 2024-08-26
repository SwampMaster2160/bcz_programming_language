use std::{fs::File, io::{BufRead, BufReader}, path::PathBuf};

use crate::{error::Error, MainData};

/// Compiles the file at `filepath`.
pub fn compile_file(main_data: &mut MainData, filepath: &PathBuf) -> Result<(), Error> {
	// Open file
	let file = File::open(filepath)
		.map_err(|_| Error::CouldNotOpenFile)?;
	let mut file_reader = BufReader::new(file);
	for line_number in 1.. {
		main_data.line = Some(line_number);
		let mut line = String::new();
		match file_reader.read_line(&mut line) {
			Ok(0) => break,
			Ok(_) => {},
			Err(_) => return Err(Error::CouldNotReadLine),
		}
	}
	// Return
	Ok(())
}