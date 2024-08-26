use std::{fs::File, io::BufReader, path::PathBuf};

use crate::{error::Error, MainData};

/// Compiles the file at `filepath`.
pub fn compile_file(main_data: &mut MainData, filepath: &PathBuf) -> Result<(), Error> {
	// Open file
	let file = File::open(filepath)
		.map_err(|_| Error::CouldNotOpenFile)?;
	let file_reader = BufReader::new(file);
	// Return
	Ok(())
}