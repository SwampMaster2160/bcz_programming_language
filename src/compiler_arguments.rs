use std::collections::HashMap;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::{error::Error, MainData};

/// The version of the BCZ compiler taken from `Cargo.toml`.
const BCZ_VERSION: &'static str = env!("CARGO_PKG_VERSION");

#[derive(Clone, Copy, PartialEq, Eq)]
/// A program state that is used while processing compiler arguments that allows arguments to continue previous arguments.
enum ArgumentProcessingState {
	Normal,
	SetPrimaryOutput,
	SetSourceHomeFilepath,
	SetBinaryHomeFilepath,
}

#[derive(Clone, Copy, EnumIter)]
/// Each compiler option is converted to one of these tokens, the argument processor can then perform the action associated with the option.
enum CompilerOptionToken {
	Help,
	Version,
	/// If the argument is a filepath to a file to be compiled.
	InputFilepath,
	NoLink,
	SetPrimaryOutput,
	SetSourceHomeFilepath,
	SetBinaryHomeFilepath,
	PrintTokens,
	PrintAstNodes,
}

impl CompilerOptionToken {
	/// The short name of the option, without the preceding dash.
	const fn short_name(&self) -> Option<&'static str> {
		match self {
			Self::Help => Some("h"),
			Self::Version => Some("v"),
			Self::InputFilepath => None,
			Self::NoLink => Some("c"),
			Self::SetPrimaryOutput => Some("o"),
			Self::SetSourceHomeFilepath => Some("s"),
			Self::SetBinaryHomeFilepath => Some("b"),
			Self::PrintTokens => None,
			Self::PrintAstNodes => None,
		}
	}

	/// The long name of the option, without the preceding double dash.
	const fn long_name(&self) -> Option<&'static str> {
		match self {
			Self::Help => Some("help"),
			Self::Version => Some("version"),
			Self::InputFilepath => None,
			Self::NoLink => Some("no-link"),
			Self::SetPrimaryOutput => Some("primary-output"),
			Self::SetSourceHomeFilepath => Some("source-home"),
			Self::SetBinaryHomeFilepath => Some("binary-home"),
			Self::PrintTokens => Some("print-tokens"),
			Self::PrintAstNodes => Some("print-ast-nodes"),
		}
	}

	/// A description of the option, `None` is returned if the option should not be listed in help.
	const fn description(&self) -> Option<&'static str> {
		match self {
			Self::Help => Some("Print this help message"),
			Self::Version => Some("Print the version of the BCZ compiler"),
			Self::InputFilepath => None,
			Self::NoLink => Some("Do not link the resulting object files into an executable"),
			Self::SetPrimaryOutput => Some("Set the path of the primary output (resulting executable)"),
			Self::SetSourceHomeFilepath => Some("Set the path of the source home directory, input paths are relative to this path"),
			Self::SetBinaryHomeFilepath => Some("Set the path of the binary home directory, output paths are relative to this path"),
			Self::PrintTokens => Some("Print tokens resulting from the lexer"),
			Self::PrintAstNodes => Some("Print AST nodes resulting from the parser"),
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

pub fn process_arguments<'a>(main_data: &mut MainData<'a>, arguments: &[&'a str]) -> Result<(), Error> {
	let mut argument_processing_state = ArgumentProcessingState::Normal;
	// No arguments should result in the version being printed
	if arguments.is_empty() {
		println!("BCZ compiler version {BCZ_VERSION}, use -h for list of compiler options.");
		return Ok(());
	}
	// Process each argument
	let short_options = CompilerOptionToken::get_short_options();
	let long_options = CompilerOptionToken::get_long_options();
	let mut source_path = None;
	let mut binary_path = None;
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
					CompilerOptionToken::SetPrimaryOutput => argument_processing_state = ArgumentProcessingState::SetPrimaryOutput,
					CompilerOptionToken::InputFilepath => main_data.filepaths_to_compile.push(argument),
					CompilerOptionToken::SetSourceHomeFilepath => argument_processing_state = ArgumentProcessingState::SetSourceHomeFilepath,
					CompilerOptionToken::SetBinaryHomeFilepath => argument_processing_state = ArgumentProcessingState::SetBinaryHomeFilepath,
					CompilerOptionToken::PrintTokens => main_data.print_tokens = true,
					CompilerOptionToken::PrintAstNodes => main_data.print_ast_nodes = true,
				}
			}
			ArgumentProcessingState::SetPrimaryOutput => {
				main_data.primary_output_file = Some(argument);
				argument_processing_state = ArgumentProcessingState::Normal;
			}
			ArgumentProcessingState::SetSourceHomeFilepath => {
				source_path = Some(main_data.compiler_working_directory.join(argument));
				argument_processing_state = ArgumentProcessingState::Normal;
			}
			ArgumentProcessingState::SetBinaryHomeFilepath => {
				binary_path = Some(main_data.compiler_working_directory.join(argument));
				argument_processing_state = ArgumentProcessingState::Normal;
			}
		}
	}
	// Make sure that an option that requires a continuation option was not at the end to the argument list
	if argument_processing_state != ArgumentProcessingState::Normal {
		return Err(Error::NoOptionContinuation);
	}
	// Set source path
	main_data.source_path = match source_path {
		Some(source_path) => source_path,
		None => {
			let bcz_source_path = main_data.compiler_working_directory.join("bcz_src");
			if bcz_source_path.is_dir() {
				bcz_source_path
			}
			else {
				main_data.compiler_working_directory.join("src")
			}
		}
	};
	// Set binary path
	main_data.binary_path = match binary_path {
		Some(binary_path) => binary_path,
		None => {
			let bcz_source_path = main_data.compiler_working_directory.join("bcz_bin");
			if bcz_source_path.is_dir() {
				bcz_source_path
			}
			else {
				main_data.compiler_working_directory.join("bin")
			}
		}
	};
	// Return
	Ok(())
}