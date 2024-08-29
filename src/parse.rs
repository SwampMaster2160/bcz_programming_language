use std::path::PathBuf;

use crate::{ast_node::{AstNode, AstNodeVariant}, error::Error, token::{Separator, Token, TokenVariant}};

enum ParseState {
	Token(Token),
	AstNode(AstNode),
	FunctionArgumentsOrParameters(Box<[AstNode]>, (usize, usize), (usize, usize)),
}

impl ParseState {
	fn is_open_parenthesis(&self) -> bool {
		matches!(self, ParseState::Token(Token { variant: TokenVariant::Separator(Separator::OpenParenthesis | Separator::OpenSquareParenthesis | Separator::OpenCurlyParenthesis), .. }))
	}

	fn is_close_parenthesis(&self) -> bool {
		matches!(self, ParseState::Token(Token { variant: TokenVariant::Separator(Separator::CloseParenthesis | Separator::CloseSquareParenthesis | Separator::CloseCurlyParenthesis), .. }))
	}

	fn get_start(&self) -> (usize, usize) {
		match self {
			ParseState::Token(token) => token.start,
			ParseState::AstNode(ast_node) => ast_node.start,
			ParseState::FunctionArgumentsOrParameters(_, start, _) => *start,
		}
	}

	fn get_end(&self) -> (usize, usize) {
		match self {
			ParseState::Token(token) => token.end,
			ParseState::AstNode(ast_node) => ast_node.end,
			ParseState::FunctionArgumentsOrParameters(_, _, end) => *end,
		}
	}
}

/// Will parse a semi-colon separated expressions into a list of AST nodes if `are_arguments_or_parameters` is `false` or from comma separated function arguments/parameters if `true`.
/// The `bool` returned is `true` if the bracketed area ends in a separator.
fn parse_separated_expressions(items_being_parsed: Vec<ParseState>, are_arguments_or_parameters: bool) -> Result<(Box<[AstNode]>, bool), (Error, usize, usize)> {
	todo!()
}

/// Parses a single expression into an AST node.
fn parse_expression(items_being_parsed: Vec<ParseState>) -> Result<AstNode, (Error, usize, usize)> {
	// Parse bracketed expressions
	let mut index = 0;
	while index < items_being_parsed.len() {
		let item = &items_being_parsed[index];
		if item.is_open_parenthesis() {
			// Find bracketed areas
			let mut bracket_depth = 0usize;
			let mut length = None;
			for (length_so_far, item) in items_being_parsed[index..].iter().enumerate() {
				if item.is_open_parenthesis() {
					bracket_depth += 1;
				}
				if item.is_close_parenthesis() {
					bracket_depth -= 1;
					if length_so_far == 0 {
						length = Some(length_so_far);
					}
				}
			};
			let length = match length {
				Some(length) => length,
				None => {
					let error_location = items_being_parsed.last().expect("Should be at least 1 item").get_end();
					return Err((Error::TooManyOpenParentheses, error_location.0, error_location.1));
				}
			};
			todo!()
		}
		index += 1;
	}
	todo!()
}

/// Takes in the tokens from tokenizing a file and parses each semi-colon separated global expression into a returned AST node.
pub fn parse_tokens(tokens: Vec<Token>) -> Result<Box<[AstNode]>, (Error, usize, usize)> {
	// Wrap all the tokens in a parse state object
	let mut items_being_parsed: Vec<ParseState> = tokens.into_iter()
		.map(|token| match token {
			// Identifier tokens should be converted to identifier AST node parse state object
			Token { variant: TokenVariant::Identifier(name), start, end } => ParseState::AstNode(AstNode {
				variant: AstNodeVariant::Identifier(name),
				start,
				end,
			}),
			// Other tokens should be wrapped in a token parse state object
			other => ParseState::Token(other),
		})
		.collect();
	// Parse semi-colon separated expressions
	Ok(parse_separated_expressions(items_being_parsed, false)?.0)
}