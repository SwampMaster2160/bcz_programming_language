use std::collections::HashMap;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

const BCZ_VERSION: &'static str = env!("CARGO_PKG_VERSION");

#[derive(Clone, Copy)]
enum ArgumentProcessingState {
	Normal,
}

#[derive(Clone, Copy, EnumIter)]
enum CompilerOption {
	Help,
	Version,
}

impl CompilerOption {
	fn short_name(&self) -> Option<&'static str> {
		match self {
			Self::Help => Some("h"),
			Self::Version => Some("v"),
		}
	}

	fn long_name(&self) -> Option<&'static str> {
		match self {
			Self::Help => Some("help"),
			Self::Version => Some("version"),
		}
	}

	fn get_short_options() -> HashMap<&'static str, CompilerOption> {
		Self::iter().map(|variant| (variant.short_name().unwrap(), variant)).collect()
	}

	fn get_long_options() -> HashMap<&'static str, CompilerOption> {
		Self::iter().map(|variant| (variant.long_name().unwrap(), variant)).collect()
	}
}

pub fn process_arguments(arguments: &[&str]) {
	// Process compiler arguments
	let mut argument_processing_state = ArgumentProcessingState::Normal;
	if arguments.is_empty() {
		println!("BCZ compiler version {BCZ_VERSION}, use -h for list of compiler options.");
		return;
	}
	let short_options = CompilerOption::get_short_options();
	let long_options = CompilerOption::get_long_options();
	for argument in arguments.iter() {
		let argument = *argument;
		match argument_processing_state {
			ArgumentProcessingState::Normal => {
				
			}
		}
	}
}