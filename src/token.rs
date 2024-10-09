use strum::IntoEnumIterator;
use std::{collections::{HashMap, HashSet}, num::NonZeroUsize};

use strum_macros::{EnumDiscriminants, EnumIter};

use crate::{error::Error, MainData};

#[derive(EnumIter, Clone, Copy, Debug, PartialEq, Eq)]
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
	pub const fn get_symbol(&self) -> char {
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

	pub const fn is_open_parenthesis(self) -> bool {
		matches!(self, Self::OpenParenthesis | Self::OpenSquareParenthesis | Self::OpenCurlyParenthesis)
	}

	pub const fn is_close_parenthesis(self) -> bool {
		matches!(self, Self::CloseParenthesis | Self::CloseSquareParenthesis | Self::CloseCurlyParenthesis)
	}
}

#[derive(EnumIter, Clone, Copy, Debug)]
pub enum Keyword {
	EntryPoint,
	Link,
}

impl Keyword {
	pub const fn get_symbol(self) -> &'static str {
		match self {
			Self::EntryPoint => "entry_point",
			Self::Link => "link",
		}
	}

	pub fn get_symbols_map() -> HashMap<&'static str, Self> {
		Self::iter()
			.map(|keyword| (keyword.get_symbol(), keyword))
			.collect()
	}
}

#[derive(EnumIter, Clone, Copy, Debug, PartialEq, Eq)]
/// A tokenized operator symbol, the meaning of each symbol depends if it is used as prefix, infix or suffix operator.
pub enum OperatorSymbol {
	AddRead = 1,
	SubtractNegate,
	MultiplyDereference,
	DivideReciprocal,
	ModuloPercent,
	AndTakeRefrence,
	Or,
	Xor,
	Not,
	EqualTo,
	NotEqualTo,
	LessThan,
	LessThanOrEqualTo,
	GreaterThan,
	GreaterThanOrEqualTo,
}

impl OperatorSymbol {
	pub const fn get_symbol(self) -> &'static str {
		match self {
			Self::AddRead => "+",
			Self::SubtractNegate => "-",
			Self::MultiplyDereference => "*",
			Self::DivideReciprocal => "/",
			Self::ModuloPercent => "%",
			Self::AndTakeRefrence => "&",
			Self::Or => "|",
			Self::Xor => "^",
			Self::Not => "!",
			Self::EqualTo => "==",
			Self::NotEqualTo => "!=",
			Self::LessThan => "<",
			Self::LessThanOrEqualTo => "<=",
			Self::GreaterThan => ">",
			Self::GreaterThanOrEqualTo => ">=",
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

#[derive(EnumIter, Clone, Copy, Debug)]
pub enum OperatorType {
	SignedLogicalShortCircuit,
	UnsignedLogicalNotShortCircuit,
	FloatingPointBitwise,
}

impl OperatorType {
	pub const fn get_symbol(self) -> Option<char> {
		match self {
			Self::SignedLogicalShortCircuit => None,
			Self::UnsignedLogicalNotShortCircuit => Some('$'),
			Self::FloatingPointBitwise => Some('~'),
		}
	}

	pub fn get_symbols_map() -> HashMap<char, Self> {
		Self::iter()
			.map(|operator_type| (operator_type.get_symbol(), operator_type))
			.filter(|(symbol, _)| symbol.is_some())
			.map(|(symbol, operator_type)| (symbol.unwrap(), operator_type))
			.collect()
	}
}

#[derive(EnumDiscriminants, Debug)]
pub enum TokenVariant {
	NumericalLiteral(u64),
	StringLiteral(Box<str>),
	Identifier(Box<str>),
	Keyword(Keyword),
	Separator(Separator),
	Operator(Option<OperatorSymbol>, OperatorType, bool, bool),
}

#[derive(Debug)]
pub struct Token {
	pub variant: TokenVariant,
	/// The line and column that this token starts at.
	pub start: (NonZeroUsize, NonZeroUsize),
	/// The line and column of the char after the last char of this token.
	pub end: (NonZeroUsize, NonZeroUsize),
}

/// Reads a single char that may be escaped, returns it and it's source length in bytes.
fn escaped_char_value(sequence: &str) -> Result<(char, usize), Error> {
	let first_char = sequence.chars().next().unwrap();
	if first_char == '\\' {
		if sequence.len() == 1 {
			return Err(Error::NothingEscaped);
		}
		return Ok((match sequence.chars().nth(1).unwrap() {
			'\\' => '\\',
			'\'' => '\'',
			'"' => '"',
			'0' => '\0',
			'n' => '\n',
			'r' => '\r',
			't' => '\t',
			'?' => '?',
			'a' => '\x07',
			'b' => '\x08',
			'e' => '\x1B',
			'f' => '\x0C',
			'v' => '\x0B',
			's' => ' ',
			'd' => '\x7F',
			// Single byte unicode values
			'x' => {
				let code = sequence.get(2..4)
					.ok_or_else(|| Error::InvalidEscapeSequence(sequence.into()))?;
				let value = u8::from_str_radix(&code, 16)
					.map_err(|_| Error::InvalidEscapeSequence(sequence.into()))?;
				return Ok((value as char, 4))
			}
			// Octal char
			'o' => {
				let code = sequence.get(2..5)
					.ok_or_else(|| Error::InvalidEscapeSequence(sequence.into()))?;
				let value = u32::from_str_radix(&code, 8)
					.map_err(|_| Error::InvalidEscapeSequence(sequence.into()))?;
				return Ok((char::from_u32(value).expect("Value should be at most 511"), 5))
			}
			// Unicode values
			'u' => {
				let next_char = match sequence.chars().nth(2) {
					Some(next_char) => next_char,
					None => return Err(Error::InvalidEscapeSequence(sequence.into())),
				};
				// 1-6 digit value
				if next_char == '{' {
					let escape_length = sequence.find('}')
						.ok_or(Error::InvalidEscapeSequence(sequence.into()))? + 1;
					if escape_length > 10 || escape_length < 5 {
						return Err(Error::InvalidEscapeSequence(sequence.into()));
					}
					let digits = &sequence[3..escape_length - 1];
					let value = u32::from_str_radix(digits, 16)
						.map_err(|_| Error::InvalidEscapeSequence(sequence.into()))?;
					let char_value = char::from_u32(value)
						.ok_or_else(|| Error::InvalidEscapeSequence(sequence.into()))?;
					return Ok((char_value, escape_length));
				}
				// 4 digit value
				else {
					let code = sequence.get(2..6)
						.ok_or_else(|| Error::InvalidEscapeSequence(sequence.into()))?;
					let value = u32::from_str_radix(&code, 16)
						.map_err(|_| Error::InvalidEscapeSequence(sequence.into()))?;
					let char_value = char::from_u32(value)
						.ok_or_else(|| Error::InvalidEscapeSequence(sequence.into()))?;
					return Ok((char_value, 6))
				}
			}
			// 6 digit unicode value
			'U' => {
				let code = sequence.get(2..8)
					.ok_or_else(|| Error::InvalidEscapeSequence(sequence.into()))?;
				let value = u32::from_str_radix(&code, 16)
					.map_err(|_| Error::InvalidEscapeSequence(sequence.into()))?;
				let char_value = char::from_u32(value)
					.ok_or_else(|| Error::InvalidEscapeSequence(sequence.into()))?;
				return Ok((char_value, 8))
			}
			_ => return Err(Error::InvalidEscapeSequence(sequence.into())),
		}, 2));
	}
	Ok((first_char, first_char.len_utf8()))
}

impl Token {
	/// Takes in a string slice `line_content` and tokenizes the first token in the string.
	/// Returns the tokenized token and the input string slice with the tokenized chars removed.
	pub fn tokenize_from_line<'a>(main_data: &mut MainData, line_content: &'a str, line_number: NonZeroUsize, column_number: NonZeroUsize)
		-> Result<(Option<Self>, &'a str), Error> {
		// Get the token varient descriminant and length in bytes
		let (token_varient_descriminant, length_in_bytes) = match line_content.chars().next()
			.expect("Function input should not be empty") {
			_ if line_content.starts_with("//") => return Ok((None, "")),
			_ if line_content.starts_with("/*") => return Err(Error::FeatureNotYetImplemented("Block comments".into())),
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
			_ if line_content.starts_with("@=") => match line_content.chars().nth(2) {
				Some(chr) if !main_data.operator_character_set.contains(&chr) => (TokenVariantDiscriminants::Operator, 2),
				_ => return Err(Error::InvalidOperator(line_content.split_at(
					line_content.find(|chr| !main_data.operator_character_set.contains(&chr)).unwrap_or_else(|| line_content.len())
				).0.into())),
			}
			'@' => (
				TokenVariantDiscriminants::Keyword,
				&line_content[1..].find(|chr: char| !(chr.is_ascii_alphanumeric() || chr == '_')).unwrap_or_else(|| line_content.len()) + 1,
			),
			'\'' => (
				TokenVariantDiscriminants::NumericalLiteral,
				{
					if line_content.starts_with("'''") || line_content.starts_with("'\\'") {
						3
					}
					else {
						match (&line_content[1..]).find('\'') {
							Some(length_in_bytes) => length_in_bytes + 2,
							None => return Err(Error::UnterminatedCharLiteral),
						}
					}
				},
			),
			'"' => (
				TokenVariantDiscriminants::StringLiteral,
				'length_found: {
					let mut is_escaped = false;
					for (index, chr) in line_content.chars().skip(1).enumerate() {
						if chr == '"' && !is_escaped {
							break 'length_found index + 2;
						}
						is_escaped = chr == '\\' && !is_escaped;
					}
					return Err(Error::UnterminatedStringLiteral);
				}
			),
			invalid_char => return Err(Error::InvalidTokenStartChar(invalid_char)),
		};
		// Split the input string into the token and the remaining string
		let (token_string, string_without_token) = line_content.split_at(length_in_bytes);
		// Parse the input string to a token varient
		let first_char = token_string.chars().next().expect("Length should be at least 1");
		let token_varient = match token_varient_descriminant {
			TokenVariantDiscriminants::Identifier => TokenVariant::Identifier(token_string.into()),
			TokenVariantDiscriminants::Separator => TokenVariant::Separator(main_data.char_to_separator_mapping[&first_char]),
			TokenVariantDiscriminants::NumericalLiteral => TokenVariant::NumericalLiteral({
				// If we have a char literal
				if first_char == '\'' {
					let content = &token_string[1..token_string.len() - 1];
					let value = match content {
						"\'" => '\'' as u64,
						"\\" => '\\' as u64,
						"" => return Err(Error::EmptyCharLiteral),
						_ => {
							let (value, length) = escaped_char_value(content)?;
							if length != token_string.len() - 2 {
								return Err(Error::MultipleCharsInCharLiteral);
							}
							value as u64
						}
					};
					if value > main_data.int_max_value {
						return Err(Error::NumericalLiteralTooLarge);
					}
					value
				}
				// If we have a numerical literal
				else {
					let (has_prefix, base, is_float) = if first_char == '0' {
						match token_string.chars().nth(1) {
							None => (false, 10, false),
							Some(second_char) if second_char.is_ascii_digit() => (false, 10, false),
							Some('x') => (true, 16, false),
							Some('o') => (true, 8, false),
							Some('b') => (true, 2, false),
							Some('f') => (true, 10, true),
							Some(invalid_char) => return Err(Error::InvalidNumericalLiteralBase(invalid_char)),
						}
					}
					else {
						(false, 10, false)
					};
					// Remove the prefix if it has one
					let string_without_prefix = match has_prefix {
						true => &token_string[2..],
						false => token_string,
					};
					// Parse number
					if is_float {
						return Err(Error::FeatureNotYetImplemented("Float literals".into()));
					}
					else {
						// Parse number char by char
						let mut out = 0u64;
						for chr in string_without_prefix.chars() {
							// Skip underscores
							if chr == '_' {
								continue;
							}
							// Parse digit
							match chr.to_digit(base) {
								Some(digit) => out = match out.checked_mul(base as u64).map(|value| value.checked_add(digit as u64)).flatten() {
									Some(value) if value > main_data.int_max_value => return Err(Error::NumericalLiteralTooLarge),
									Some(value) => value,
									None => return Err(Error::NumericalLiteralTooLarge),
								},
								None => return Err(Error::InvalidDigitForBase(chr, base as u8)),
							}
						}
						out
					}
				}
			}),
			TokenVariantDiscriminants::Keyword => TokenVariant::Keyword(match main_data.str_to_keyword_mapping.get(&token_string[1..]) {
				Some(keyword) => *keyword,
				None => return Err(Error::InvalidKeyword(token_string.to_string()))
			}),
			TokenVariantDiscriminants::Operator => {
				// Parse the l-value assignment operator
				if token_string == "@=" {
					return Ok((Some(Self {
						variant: TokenVariant::Operator(None, OperatorType::UnsignedLogicalNotShortCircuit, true, true),
						start: (line_number, column_number),
						end: (line_number, column_number.saturating_add(2)),
					}), string_without_token));
				}
				// Get operator type
				let operator_type = main_data.char_to_operator_type_mapping.get(&first_char);
				let (operator_type, operator_string_without_type) = match operator_type {
					Some(operator_type) => (*operator_type, &token_string[1..]),
					None => (OperatorType::SignedLogicalShortCircuit, token_string),
				};
				// Get if the operator is an assignment
				let (is_assignment, operator_base_string) =
				match operator_string_without_type.chars().last() == Some('=') && !matches!(operator_string_without_type, "==" | "!=" | "<=" | ">=") {
					true => (true, &operator_string_without_type[..operator_string_without_type.len() - 1]),
					false => (false, operator_string_without_type),
				};
				// Get operator base
				let operator_base = match operator_base_string.is_empty() {
					true => None,
					false => Some(match main_data.str_to_operator_mapping.get(operator_base_string) {
						Some(operator_base) => *operator_base,
						None => return Err(Error::InvalidOperator(token_string.into())),
					}),
				};
				// Create operator token varient
				TokenVariant::Operator(operator_base, operator_type, is_assignment, false)
			}
			TokenVariantDiscriminants::StringLiteral => {
				let mut string_quote_content = &token_string[1..token_string.len() - 1];
				let mut result_string = String::new();
				while !string_quote_content.is_empty() {
					let (char_value, length_in_bytes) = escaped_char_value(string_quote_content)?;
					result_string.push(char_value);
					string_quote_content = &string_quote_content[length_in_bytes..];
				}
				TokenVariant::StringLiteral(result_string.into())
			}
		};
		// Return
		let token = Self {
			variant: token_varient,
			start: (line_number, column_number),
			end: (line_number, column_number.saturating_add(token_string.chars().count())),
		};
		Ok((Some(token), string_without_token))
	}
}