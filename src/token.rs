use strum_macros::EnumDiscriminants;

use crate::{error::Error, MainData};

pub enum Separator {

}

pub enum Keyword {
	
}

pub enum Operator {
	
}

#[derive(EnumDiscriminants)]
pub enum TokenVariant {
	NumericalLiteral(u64),
	StringLiteral(Box<str>),
	Identifier(Box<str>),
	Keyword(Keyword),
	Separator(Separator),
	Operator(Operator),
}

pub struct Token {
	variant: TokenVariant,
	line: usize,
	column: usize,
	char_length: usize,
}

impl Token {
	/// Takes in a string slice `line_content` and tokenizes the first token in the string. Returns the tokenized token and the input string slice with the tokenized chars removed.
	pub fn tokenize_from_line<'a>(main_data: &mut MainData, line_content: &'a str, line_number: usize, column_number: usize) -> Result<(Self, &'a str), Error> {
		// get the token varient descriminant and length in bytes
		let (token_varient, length_in_bytes) = match line_content.chars().next().expect("Function input should not be empty") {
			first_char if first_char.is_ascii_alphabetic() || first_char == '_' => (
				TokenVariantDiscriminants::Identifier,
				line_content.find(|chr: char| !(chr.is_ascii_alphanumeric() || chr == '_')).unwrap_or_else(|| line_content.len()),
			),
			first_char if first_char.is_ascii_digit() => (
				TokenVariantDiscriminants::NumericalLiteral,
				line_content.find(|chr: char| !(chr.is_ascii_alphanumeric() || chr == '_' || chr == '.')).unwrap_or_else(|| line_content.len()),
			),
			_ => todo!(),
		};
		// Split the input string into the token and the remaining string
		let (token_string, string_without_token) = line_content.split_at(length_in_bytes);
		// Return
		let token = Self {
			variant: todo!(),
			line: line_number,
			column: column_number,
			char_length: token_string.chars().count(),
		};
		Ok((token, string_without_token))
	}
}