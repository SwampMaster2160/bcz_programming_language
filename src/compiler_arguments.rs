use std::collections::HashMap;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::error::Error;

const BCZ_VERSION: &'static str = env!("CARGO_PKG_VERSION");

#[derive(Clone, Copy)]
enum ArgumentProcessingState {
	Normal,
}

#[derive(Clone, Copy, EnumIter)]
enum CompilerOption {
	Help,
	Version,
	InputFilepath,
}

impl CompilerOption {
	fn short_name(&self) -> Option<&'static str> {
		match self {
			Self::Help => Some("h"),
			Self::Version => Some("v"),
			Self::InputFilepath => None,
		}
	}

	fn long_name(&self) -> Option<&'static str> {
		match self {
			Self::Help => Some("help"),
			Self::Version => Some("version"),
			Self::InputFilepath => None,
		}
	}

	fn get_short_options() -> HashMap<&'static str, Self> {
		Self::iter()
			.map(|variant| (variant.short_name(), variant))
			.filter(|(name, _)| name.is_some())
			.map(|(name, varient)| (name.unwrap(), varient))
			.collect()
	}

	fn get_long_options() -> HashMap<&'static str, Self> {
		Self::iter()
			.map(|variant| (variant.long_name(), variant))
			.filter(|(name, _)| name.is_some())
			.map(|(name, varient)| (name.unwrap(), varient))
			.collect()
	}
}

pub fn process_arguments(arguments: &[&str]) -> Result<(), Error> {
	let mut argument_processing_state = ArgumentProcessingState::Normal;
	// No arguments should result in the version being printed
	if arguments.is_empty() {
		println!("BCZ compiler version {BCZ_VERSION}, use -h for list of compiler options.");
		return Ok(());
	}
	// Process each argument
	let short_options = CompilerOption::get_short_options();
	let long_options = CompilerOption::get_long_options();
	for argument in arguments.iter() {
		let argument = *argument;
		match argument_processing_state {
			// If we are not processing a continuation of the last argument
			ArgumentProcessingState::Normal => {
				// Parse the argument name into it's token
				let option = if argument.starts_with("--") {
					match long_options.get(&argument[2..]) {
						Some(option) => *option,
						None => return Err(Error::InvalidLongArgument(argument.to_string()))
					}
				}
				else if argument.starts_with("-") {
					match short_options.get(&argument[1..]) {
						Some(option) => *option,
						None => return Err(Error::InvalidShortArgument(argument.to_string()))
					}
				}
				// Else if the argument does not begin with a dash, it is an input filepath
				else {
					CompilerOption::InputFilepath
				};
				// Do the action for the token
				match option {
					CompilerOption::Version => println!("BCZ compiler version {BCZ_VERSION}."),
					_ => todo!(),
				}
			}
		}
	}
	Ok(())
}