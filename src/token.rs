use crate::error::Error;

pub enum TokenVariant {
	NumericalLiteral(u64),
}

pub struct Token {
	variant: TokenVariant,
	line: usize,
	column: usize,
}

impl Token {
	pub fn tokenize_from_line(line: &str) -> Result<(Self, &str), Error> {
		todo!()
	}
}