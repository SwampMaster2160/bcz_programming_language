use strum::IntoEnumIterator;
use std::collections::{HashMap, HashSet};

use strum_macros::{EnumDiscriminants, EnumIter};

use crate::{error::Error, MainData};

#[derive(EnumIter, Clone, Copy)]
pub enum Separator {
	Semicolon,
	Comma,
	Period,
	OpenParenthesis,
	CloseParenthesis,
	OpenSquareParenthesis,
	CloseSquareParenthesis,
	OpenCurlyParenthesis,
	CloseCurlyParenthesis,
}

impl Separator {
	pub fn get_symbol(&self) -> char {
		match self {
			Self::Semicolon => ';',
			Self::Comma => ',',
			Self::Period => '.',
			Self::OpenParenthesis => '(',
			Self::CloseParenthesis => ')',
			Self::OpenSquareParenthesis => '[',
			Self::CloseSquareParenthesis => ']',
			Self::OpenCurlyParenthesis => '{',
			Self::CloseCurlyParenthesis => '}',
		}
	}

	pub fn get_symbols_map() -> HashMap<char, Self> {
		Self::iter()
			.map(|separator| (separator.get_symbol(), separator))
			.collect()
	}
}

#[derive(EnumIter, Clone, Copy)]
pub enum Keyword {
	EntryPoint,
}

impl Keyword {
	pub fn get_symbol(self) -> &'static str {
		match self {
			Self::EntryPoint => "entry_point",
		}
	}

	pub fn get_symbols_map() -> HashMap<&'static str, Self> {
		Self::iter()
			.map(|keyword| (keyword.get_symbol(), keyword))
			.collect()
	}
}

#[derive(EnumIter, Clone, Copy)]
pub enum Operator {
	AddRead = 1,
	SubtractNegate,
	MultiplyDerefrence,
	DivideReciprocal,
	ModuloPercent,
}

impl Operator {
	pub fn get_symbol(self) -> &'static str {
		match self {
			Self::AddRead => "+",
			Self::SubtractNegate => "-",
			Self::MultiplyDerefrence => "*",
			Self::DivideReciprocal => "/",
			Self::ModuloPercent => "%",
		}
	}

	pub fn get_character_set() -> HashSet<char> {
		"+-*/%=!<>&|~^?#$:".chars().into_iter().collect()
	}

	pub fn get_symbols_map() -> HashMap<&'static str, Self> {
		Self::iter()
			.map(|operator| (operator.get_symbol(), operator))
			.collect()
	}
}

#[derive(EnumIter, Clone, Copy)]
pub enum OperatorType {
	SignedLogicalShortCircuit,
	UnsignedLogicalNotShortCircuit,
	FloatingPointBitwise,
}

impl OperatorType {
	pub fn get_symbol(self) -> Option<char> {
		match self {
			Self::SignedLogicalShortCircuit => None,
			Self::UnsignedLogicalNotShortCircuit => Some('$'),
			Self::FloatingPointBitwise => Some('~'),
		}
	}

	pub fn get_symbols_map() -> HashMap<char, Self> {
		Self::iter()
			.map(|operator_type| (operator_type.get_symbol(), operator_type))
			.filter(|(symbol, operator_type)| symbol.is_some())
			.map(|(symbol, operator_type)| (symbol.unwrap(), operator_type))
			.collect()
	}
}

#[derive(EnumDiscriminants)]
pub enum TokenVariant {
	NumericalLiteral(u64),
	StringLiteral(Box<str>),
	Identifier(Box<str>),
	Keyword(Keyword),
	Separator(Separator),
	Operator(Option<Operator>, OperatorType, bool),
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
			_ if line_content.starts_with("//") => return Err(Error::FeatureNotYetImplemented),
			_ if line_content.starts_with("/*") => return Err(Error::FeatureNotYetImplemented),
			first_char if first_char.is_ascii_alphabetic() || first_char == '_' => (
				TokenVariantDiscriminants::Identifier,
				line_content.find(|chr: char| !(chr.is_ascii_alphanumeric() || chr == '_')).unwrap_or_else(|| line_content.len()),
			),
			first_char if first_char.is_ascii_digit() => (
				TokenVariantDiscriminants::NumericalLiteral,
				line_content.find(|chr: char| !(chr.is_ascii_alphanumeric() || chr == '_' || chr == '.')).unwrap_or_else(|| line_content.len()),
			),
			first_char if main_data.char_to_separator_mapping.contains_key(&first_char) => (TokenVariantDiscriminants::Separator, 1),
			first_char if main_data.operator_character_set.contains(&first_char) => (
				TokenVariantDiscriminants::Operator,
				line_content.find(|chr: char| !main_data.operator_character_set.contains(&chr)).unwrap_or_else(|| line_content.len()),
			),
			'@' => (
				TokenVariantDiscriminants::Keyword,
				&line_content[1..].find(|chr: char| !main_data.operator_character_set.contains(&chr)).unwrap_or_else(|| line_content.len()) + 1,
			),
			'\'' => return Err(Error::FeatureNotYetImplemented),
			'"' => return Err(Error::FeatureNotYetImplemented),
			invalid_char => return Err(Error::InvalidTokenStartChar(invalid_char)),
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