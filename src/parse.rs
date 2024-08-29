use std::path::PathBuf;

use crate::{ast_node::{AstNode, AstNodeVariant}, error::Error, token::{Token, TokenVariant}};

enum ParseState {
	Token(Token),
	AstNode(AstNode),
	FunctionArgumentsOrParameters(Box<[AstNode]>),
}

pub fn parse_tokens(tokens: Vec<Token>) -> Result<Box<[AstNode]>, (Error, PathBuf, usize, usize)> {
	// Wrap all the tokens in a parse state object
	let mut parse_state: Vec<ParseState> = tokens.into_iter()
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
	todo!()
}