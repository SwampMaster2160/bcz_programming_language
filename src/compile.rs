use std::path::PathBuf;

use crate::MainData;

/// Compiles the file at `filepath`.
pub fn compile_file(main_data: &mut MainData, filepath: PathBuf) {
	println!("{:?}", filepath);
}