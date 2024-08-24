use std::collections::HashMap;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::{error::Error, MainData};

/// The version of the BCZ compiler taken from `Cargo.toml`.
const BCZ_VERSION: &'static str = env!("CARGO_PKG_VERSION");

#[derive(Clone, Copy)]
/// A program state that is used while processing compiler arguments that allows arguments to continue previous arguments.
enum ArgumentProcessingState {
	Normal,
}

#[derive(Clone, Copy, EnumIter)]
/// Each compiler option is converted to one of these tokens, the argument processor can then perform the action associated with the option.
enum CompilerOptionToken {
	Help,
	Version,
	/// If the argument is a filepath to a file to be compiled.
	InputFilepath,
	NoLink,
}

impl CompilerOptionToken {
	/// The short name of the option, without the preceding dash.
	fn short_name(&self) -> Option<&'static str> {
		match self {
			Self::Help => Some("h"),
			Self::Version => Some("v"),
			Self::InputFilepath => None,
			Self::NoLink => Some("c"),
		}
	}

	/// The long name of the option, without the preceding double dash.
	fn long_name(&self) -> Option<&'static str> {
		match self {
			Self::Help => Some("help"),
			Self::Version => Some("version"),
			Self::InputFilepath => None,
			Self::NoLink => Some("no-link"),
		}
	}

	/// A description of the option, `None` is returned if the option should not be listed in help.
	fn description(&self) -> Option<&'static str> {
		match self {
			Self::Help => Some("Print this help message"),
			Self::Version => Some("Print the version of the BCZ compiler"),
			Self::InputFilepath => None,
			Self::NoLink => Some("Do not link the resulting object files into an executable"),
		}
	}

	/// Get a short name (without the preceding dash) to token mapping.
	fn get_short_options() -> HashMap<&'static str, Self> {
		Self::iter()
			.map(|variant| (variant.short_name(), variant))
			.filter(|(name, _)| name.is_some())
			.map(|(name, varient)| (name.unwrap(), varient))
			.collect()
	}

	/// Get a long name (without the preceding double dash) to token mapping.
	fn get_long_options() -> HashMap<&'static str, Self> {
		Self::iter()
			.map(|variant| (variant.long_name(), variant))
			.filter(|(name, _)| name.is_some())
			.map(|(name, varient)| (name.unwrap(), varient))
			.collect()
	}
}

pub fn process_arguments(main_data: &mut MainData, arguments: &[&str]) -> Result<(), Error> {
	let mut argument_processing_state = ArgumentProcessingState::Normal;
	// No arguments should result in the version being printed
	if arguments.is_empty() {
		println!("BCZ compiler version {BCZ_VERSION}, use -h for list of compiler options.");
		return Ok(());
	}
	// Process each argument
	let short_options = CompilerOptionToken::get_short_options();
	let long_options = CompilerOptionToken::get_long_options();
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
					CompilerOptionToken::InputFilepath
				};
				// Do the action for the token
				match option {
					CompilerOptionToken::Version => println!("BCZ compiler version {BCZ_VERSION}."),
					CompilerOptionToken::Help => {
						println!("Options:");
						for option in CompilerOptionToken::iter() {
							let description = match option.description() {
								Some(description) => description,
								None => continue,
							};
							let long_name = option.long_name();
							let short_name = option.short_name();
							print!("\t");
							if let Some(long_name) = long_name {
								print!("--{long_name}");
								if short_name.is_some() {
									print!(", ");
								}
							}
							if let Some(short_name) = short_name {
								print!("-{short_name}");
							}
							println!("\t{description}.");
						}
					}
					CompilerOptionToken::NoLink => main_data.do_link = false,
					_ => todo!(),
				}
			}
		}
	}
	Ok(())
}