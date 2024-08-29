use std::fmt::Display;

use crate::token::Separator;

pub enum Error {
	InvalidShortArgument(String),
	InvalidLongArgument(String),
	NoOptionContinuation,
	CouldNotOpenFile,
	CouldNotReadLine,
	FeatureNotYetImplemented,
	InvalidTokenStartChar(char),
	InvalidNumericalLiteralBase(char),
	InvalidDigitForBase(char, u8),
	NumericalLiteralTooLarge,
	InvalidKeyword(String),
	InvalidOperator(String),
	TooManyOpenParentheses,
	TooManyCloseParentheses,
	BlankExpression,
	ParenthesisMismatch(Separator, Separator),
}

impl Display for Error {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Error::InvalidShortArgument(arg) => write!(f, "invalid short argument \"{}\"", arg),
			Error::InvalidLongArgument(arg) => write!(f, "invalid long argument \"{}\"", arg),
			Error::NoOptionContinuation => write!(f, "no option continuation"),
			Error::CouldNotOpenFile => write!(f, "could not open file"),
			Error::CouldNotReadLine => write!(f, "could not read line"),
			Error::FeatureNotYetImplemented => write!(f, "feature not yet implemented"),
			Error::InvalidTokenStartChar(c) => write!(f, "invalid token start character '{c}'"),
			Error::InvalidNumericalLiteralBase(c) => write!(f, "invalid numerical literal base \"0{c}\""),
			Error::InvalidDigitForBase(c, base) => write!(f, "invalid digit '{c}' for base {base}"),
			Error::NumericalLiteralTooLarge => write!(f, "numerical literal too large"),
			Error::InvalidKeyword(keyword) => write!(f, "invalid keyword \"{keyword}\""),
			Error::InvalidOperator(operator) => write!(f, "invalid operator \"{operator}\""),
			Error::TooManyCloseParentheses => write!(f, "too many close parentheses"),
			Error::TooManyOpenParentheses => write!(f, "too many open parentheses"),
			Error::BlankExpression => write!(f, "blank expression"),
			Error::ParenthesisMismatch(open, close) => write!(f, "open '{}' mismatched with close '{}'", open.get_symbol(), close.get_symbol()),
		}
	}
}