use crate::{error::Error, MainData};

pub enum TokenVariant {
	NumericalLiteral(u64),
}

pub struct Token {
	variant: TokenVariant,
	line: usize,
	column: usize,
}

impl Token {
	pub fn tokenize_from_line<'a>(main_data: &mut MainData, line_content: &'a str, line_number: usize, column_number: usize) -> Result<(Self, &'a str), Error> {
		todo!()
	}
}