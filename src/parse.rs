use std::path::PathBuf;

use crate::{ast_node::AstNode, error::Error, token::Token};

pub fn parse_tokens(tokens: &[Token]) -> Result<Box<[AstNode]>, (Error, PathBuf, usize, usize)> {
	todo!()
}