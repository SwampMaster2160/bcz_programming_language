use std::{mem::take, num::NonZeroUsize};

use auto_const_array::auto_const_array;

use crate::{ast_node::{AstNode, AstNodeVariant, Operation, Operator}, error::Error};
use crate::token::{Keyword, OperatorSymbol, OperatorType, Separator, Token, TokenVariant};

#[derive(Debug)]
enum ParseState {
	Token(Token),
	AstNode(AstNode),
	FunctionArgumentsOrParameters(Box<[AstNode]>, (NonZeroUsize, NonZeroUsize), (NonZeroUsize, NonZeroUsize)),
}

impl ParseState {
	const fn is_open_parenthesis(&self) -> bool {
		matches!(self, ParseState::Token(Token { variant: TokenVariant::Separator(separator), .. }) if separator.is_open_parenthesis())
	}

	const fn is_close_parenthesis(&self) -> bool {
		matches!(self, ParseState::Token(Token { variant: TokenVariant::Separator(separator), .. }) if separator.is_close_parenthesis())
	}

	const fn get_start(&self) -> (NonZeroUsize, NonZeroUsize) {
		match self {
			ParseState::Token(token) => token.start,
			ParseState::AstNode(ast_node) => ast_node.start,
			ParseState::FunctionArgumentsOrParameters(_, start, _) => *start,
		}
	}

	const fn get_end(&self) -> (NonZeroUsize, NonZeroUsize) {
		match self {
			ParseState::Token(token) => token.end,
			ParseState::AstNode(ast_node) => ast_node.end,
			ParseState::FunctionArgumentsOrParameters(_, _, end) => *end,
		}
	}
}

auto_const_array! {
	const BINARY_OPERATOR_PRECEDENCE: [&'static [OperatorSymbol]; _] = [
		&[OperatorSymbol::MultiplyDereference, OperatorSymbol::DivideReciprocal, OperatorSymbol::ModuloPercent],
		&[OperatorSymbol::AddRead, OperatorSymbol::SubtractNegate],
		&[OperatorSymbol::LessThan, OperatorSymbol::LessThanOrEqualTo, OperatorSymbol::GreaterThan, OperatorSymbol::GreaterThanOrEqualTo],
		&[OperatorSymbol::EqualTo, OperatorSymbol::NotEqualTo],
		&[OperatorSymbol::AndTakeRefrence],
		&[OperatorSymbol::Xor],
		&[OperatorSymbol::Or],
	];
}

const fn binary_operator_from_symbol(symbol: OperatorSymbol, operator_type: OperatorType) -> Option<Operation> {
	match (symbol, operator_type) {
		(OperatorSymbol::AddRead, OperatorType::SignedLogicalShortCircuit | OperatorType::UnsignedLogicalNotShortCircuit) => Some(Operation::IntegerAdd),
		(OperatorSymbol::AddRead, OperatorType::FloatingPointBitwise) => Some(Operation::FloatAdd),
		(OperatorSymbol::SubtractNegate, OperatorType::SignedLogicalShortCircuit | OperatorType::UnsignedLogicalNotShortCircuit) => Some(Operation::IntegerSubtract),
		(OperatorSymbol::SubtractNegate, OperatorType::FloatingPointBitwise) => Some(Operation::FloatSubtract),
		(OperatorSymbol::MultiplyDereference, OperatorType::SignedLogicalShortCircuit | OperatorType::UnsignedLogicalNotShortCircuit) =>
			Some(Operation::IntegerMultiply),
		(OperatorSymbol::MultiplyDereference, OperatorType::FloatingPointBitwise) => Some(Operation::FloatMultiply),
		(OperatorSymbol::DivideReciprocal, OperatorType::SignedLogicalShortCircuit) => Some(Operation::SignedDivide),
		(OperatorSymbol::DivideReciprocal, OperatorType::UnsignedLogicalNotShortCircuit) => Some(Operation::UnsignedDivide),
		(OperatorSymbol::DivideReciprocal, OperatorType::FloatingPointBitwise) => Some(Operation::FloatDivide),
		(OperatorSymbol::ModuloPercent, OperatorType::SignedLogicalShortCircuit) => Some(Operation::SignedTruncatedModulo),
		(OperatorSymbol::ModuloPercent, OperatorType::UnsignedLogicalNotShortCircuit) => Some(Operation::UnsignedModulo),
		(OperatorSymbol::ModuloPercent, OperatorType::FloatingPointBitwise) => Some(Operation::FloatTruncatedModulo),
		(OperatorSymbol::AndTakeRefrence, OperatorType::FloatingPointBitwise) => Some(Operation::BitwiseAnd),
		(OperatorSymbol::Or, OperatorType::FloatingPointBitwise) => Some(Operation::BitwiseOr),
		(OperatorSymbol::Xor, OperatorType::FloatingPointBitwise) => Some(Operation::BitwiseXor),
		(OperatorSymbol::AndTakeRefrence, OperatorType::UnsignedLogicalNotShortCircuit) => Some(Operation::LogicalNotShortCircuitAnd),
		(OperatorSymbol::Or, OperatorType::UnsignedLogicalNotShortCircuit) => Some(Operation::LogicalNotShortCircuitOr),
		(OperatorSymbol::Xor, OperatorType::UnsignedLogicalNotShortCircuit | OperatorType::SignedLogicalShortCircuit) => Some(Operation::LogicalXor),
		(OperatorSymbol::AndTakeRefrence, OperatorType::SignedLogicalShortCircuit) => Some(Operation::LogicalShortCircuitAnd),
		(OperatorSymbol::Or, OperatorType::SignedLogicalShortCircuit) => Some(Operation::LogicalShortCircuitOr),
		(OperatorSymbol::EqualTo, OperatorType::UnsignedLogicalNotShortCircuit | OperatorType::SignedLogicalShortCircuit) => Some(Operation::IntegerEqualTo),
		(OperatorSymbol::NotEqualTo, OperatorType::UnsignedLogicalNotShortCircuit | OperatorType::SignedLogicalShortCircuit) => Some(Operation::IntegerNotEqualTo),
		(OperatorSymbol::LessThan, OperatorType::UnsignedLogicalNotShortCircuit) => Some(Operation::UnsignedLessThan),
		(OperatorSymbol::LessThanOrEqualTo, OperatorType::UnsignedLogicalNotShortCircuit) => Some(Operation::UnsignedLessThanOrEqualTo),
		(OperatorSymbol::GreaterThan, OperatorType::UnsignedLogicalNotShortCircuit) => Some(Operation::UnsignedGreaterThan),
		(OperatorSymbol::GreaterThanOrEqualTo, OperatorType::UnsignedLogicalNotShortCircuit) => Some(Operation::UnsignedGreaterThanOrEqualTo),
		(OperatorSymbol::LessThan, OperatorType::SignedLogicalShortCircuit) => Some(Operation::SignedLessThan),
		(OperatorSymbol::LessThanOrEqualTo, OperatorType::SignedLogicalShortCircuit) => Some(Operation::SignedLessThanOrEqualTo),
		(OperatorSymbol::GreaterThan, OperatorType::SignedLogicalShortCircuit) => Some(Operation::SignedGreaterThan),
		(OperatorSymbol::GreaterThanOrEqualTo, OperatorType::SignedLogicalShortCircuit) => Some(Operation::SignedGreaterThanOrEqualTo),
		(OperatorSymbol::EqualTo, OperatorType::FloatingPointBitwise) => Some(Operation::FloatEqualTo),
		(OperatorSymbol::NotEqualTo, OperatorType::FloatingPointBitwise) => Some(Operation::FloatNotEqualTo),
		(OperatorSymbol::LessThan, OperatorType::FloatingPointBitwise) => Some(Operation::FloatLessThan),
		(OperatorSymbol::LessThanOrEqualTo, OperatorType::FloatingPointBitwise) => Some(Operation::FloatLessThanOrEqualTo),
		(OperatorSymbol::GreaterThan, OperatorType::FloatingPointBitwise) => Some(Operation::FloatGreaterThan),
		(OperatorSymbol::GreaterThanOrEqualTo, OperatorType::FloatingPointBitwise) => Some(Operation::FloatGreaterThanOrEqualTo),
		(OperatorSymbol::Not, _) => None,
		(OperatorSymbol::TernaryFirst | OperatorSymbol::TernarySecond, _) => None,
		//_ => None,
	}
}

const fn prefix_operator_from_symbol(symbol: OperatorSymbol, operator_type: OperatorType) -> Option<Operation> {
	match (symbol, operator_type) {
		(OperatorSymbol::AddRead, _) => Some(Operation::Read),
		(OperatorSymbol::MultiplyDereference, _) => Some(Operation::Dereference),
		(OperatorSymbol::AndTakeRefrence, _) => Some(Operation::TakeReference),
		(OperatorSymbol::SubtractNegate, OperatorType::SignedLogicalShortCircuit | OperatorType::UnsignedLogicalNotShortCircuit) => Some(Operation::IntegerNegate),
		(OperatorSymbol::SubtractNegate, OperatorType::FloatingPointBitwise) => Some(Operation::FloatNegate),
		(OperatorSymbol::Not, OperatorType::FloatingPointBitwise) => Some(Operation::BitwiseNot),
		(OperatorSymbol::Not, OperatorType::SignedLogicalShortCircuit | OperatorType::UnsignedLogicalNotShortCircuit) => Some(Operation::LogicalNot),
		_ => None,
	}
}

const fn postfix_operator_from_symbol(symbol: OperatorSymbol, operator_type: OperatorType) -> Option<Operation> {
	match (symbol, operator_type) {
		_ => None,
	}
}

/// Will parse a semi-colon separated expressions into a list of AST nodes if `are_arguments_or_parameters` is `false`
/// or from comma separated function arguments/parameters if `true`.
/// The `bool` returned is `true` if the bracketed area ends in a separator.
fn parse_separated_expressions(mut items_being_parsed: Vec<ParseState>, are_arguments_or_parameters: bool)
	-> Result<(Box<[AstNode]>, bool), (Error, (NonZeroUsize, NonZeroUsize))> {
	let mut ast_nodes_out: Vec<AstNode> = Vec::new();
	loop {
		let mut parenthesis_depth = 0usize;
		// Get the length to the next separator
		let mut length = None;
		for (length_so_far, item) in items_being_parsed.iter().enumerate() {
			if let ParseState::Token(Token { variant: TokenVariant::Separator(separator), .. }) = item {
				if separator.is_open_parenthesis() {
					parenthesis_depth += 1;
				}
				if separator.is_close_parenthesis() {
					parenthesis_depth = parenthesis_depth.checked_sub(1).ok_or_else(|| (Error::TooManyCloseParentheses, item.get_start()))?;
				}
				if parenthesis_depth == 0 && (
					((!are_arguments_or_parameters) && *separator == Separator::Semicolon) || (are_arguments_or_parameters && *separator == Separator::Comma)
				) {
					length = Some(length_so_far);
					break;
				}
			}
		}
		let (length, is_last) = match length {
			Some(length) => (length, false),
			None => (items_being_parsed.len(), true),
		};
		// Remove items to separator and parse them
		let split_off = items_being_parsed.split_off(length);
		let expression_items = items_being_parsed;
		items_being_parsed = split_off;
		if !is_last {
			items_being_parsed.remove(0);
		}
		if length == 0 {
			if are_arguments_or_parameters && !is_last {
				return Err((Error::BlankExpression, items_being_parsed.first().unwrap().get_start()));
			}
		}
		else {
			ast_nodes_out.push(parse_expression(expression_items)?);
		}
		// Return if at the end
		if is_last {
			return Ok((ast_nodes_out.into(), length == 0));
		}
	}
}

/// Parses a single expression into an AST node.
fn parse_expression(mut items_being_parsed: Vec<ParseState>) -> Result<AstNode, (Error, (NonZeroUsize, NonZeroUsize))> {
	// Parse bracketed expressions
	let mut index = 0;
	while index < items_being_parsed.len() {
		let item = &items_being_parsed[index];
		if item.is_open_parenthesis() {
			// Find parenthesised areas
			let mut bracket_depth = 0usize;
			let mut length = None;
			for (length_so_far, item) in items_being_parsed[index..].iter().enumerate() {
				if item.is_open_parenthesis() {
					bracket_depth += 1;
				}
				if item.is_close_parenthesis() {
					bracket_depth -= 1;
					if bracket_depth == 0 {
						length = Some(length_so_far);
						break;
					}
				}
			};
			let length = match length {
				Some(length) => length,
				None => return Err((Error::TooManyOpenParentheses, items_being_parsed.last().unwrap().get_end())),
			};
			// Remove parenthesised area into vec
			let mut parenthesised_items: Vec<ParseState> = items_being_parsed.drain(index..index + length + 1).collect();
			let open_parenthesis = parenthesised_items.remove(0);
			let close_parenthesis = parenthesised_items.pop().unwrap();
			let open_separator = match open_parenthesis {
				ParseState::Token(Token { variant: TokenVariant::Separator(open_separator), ..}) => open_separator,
				_ => unreachable!(),
			};
			let close_separator = match close_parenthesis {
				ParseState::Token(Token { variant: TokenVariant::Separator(close_separator), ..}) => close_separator,
				_ => unreachable!(),
			};
			// Make sure the parentheses match
			if (open_separator == Separator::OpenParenthesis && close_separator != Separator::CloseParenthesis) ||
				(open_separator == Separator::OpenCurlyParenthesis && close_separator != Separator::CloseCurlyParenthesis) ||
				(open_separator == Separator::OpenSquareParenthesis && close_separator != Separator::CloseSquareParenthesis) {
				return Err((Error::ParenthesisMismatch(open_separator, close_separator), close_parenthesis.get_start()));
			}
			// Parse bracketed area
			let result_of_parse = match open_separator {
				Separator::OpenParenthesis => {
					let (arguments_or_parameters, _) = parse_separated_expressions(parenthesised_items, true)?;
					ParseState::FunctionArgumentsOrParameters(arguments_or_parameters, open_parenthesis.get_start(), close_parenthesis.get_end())
				}
				Separator::OpenCurlyParenthesis => {
					let (expressions, result_is_undefined) = parse_separated_expressions(parenthesised_items, false)?;
					ParseState::AstNode(AstNode {
						start: open_parenthesis.get_start(), end: close_parenthesis.get_end(), variant: AstNodeVariant::Block(expressions, result_is_undefined)
					})
				},
				Separator::OpenSquareParenthesis => return Err((Error::FeatureNotYetImplemented("Index operator".into()), open_parenthesis.get_start())),
				_ => unreachable!(),
			};
			// Insert result of parse back into list
			items_being_parsed.insert(index, result_of_parse);
		}
		index += 1;
	}
	// Parse function calls
	let mut index = 1;
	'w: while index < items_being_parsed.len() {
		// Make sure the item is a function arguments/parameters item
		if let ParseState::FunctionArgumentsOrParameters(_, arguments_start, arguments_end) = &items_being_parsed[index] {
			let (_, arguments_end) = (*arguments_start, *arguments_end);
			// Make sure the item to the left is not a parsed expression
			match &items_being_parsed[index - 1] {
				// User defined functions
				ParseState::AstNode(..) => {
					// Get function pointer
					let function_pointer = items_being_parsed.remove(index - 1);
					let function_pointer = match function_pointer {
						ParseState::AstNode(ast_node) => ast_node,
						_ => {
							index += 1;
							continue;
						}
					};
					// Get function parameters
					let parameters = match &mut items_being_parsed[index - 1] {
						ParseState::FunctionArgumentsOrParameters(parameters, _, _) => take(parameters),
						_ => unreachable!(),
					};
					// Construct function call node
					let operator_ast_node = AstNode {
						start: function_pointer.start,
						end: arguments_end,
						variant: AstNodeVariant::FunctionCall(Box::new(function_pointer), parameters),
					};
					// Insert back into list
					items_being_parsed[index - 1] = ParseState::AstNode(operator_ast_node);
					index -= 1;
					continue;
				}
				// Build in functions
				ParseState::Token(Token { start, end: _, variant: TokenVariant::Keyword(keyword) }) => 'a: {
					let start = *start;
					// Get built in function
					//let function = match keyword {
					//	Keyword::Write | Keyword::Stack => {
					//		match keyword {
					//			Keyword::Write => BuiltInFunctionCall::Write,
					//			Keyword::Stack => BuiltInFunctionCall::Stack,
					//			_ => unreachable!(),
					//		}
					//	}
					//	Keyword::EntryPoint | Keyword::Link | Keyword::Loop => break 'a,
					//};
					let keyword = match keyword {
						Keyword::EntryPoint | Keyword::Link => break 'a,
						keyword => *keyword
					};
					items_being_parsed.remove(index - 1);
					// Get function parameters
					let parameters = match &mut items_being_parsed[index - 1] {
						ParseState::FunctionArgumentsOrParameters(parameters, _, _) => take(parameters),
						_ => unreachable!(),
					};
					// Construct function call node
					let operator_ast_node = AstNode {
						start,
						end: arguments_end,
						variant: AstNodeVariant::Keyword(keyword, parameters, None)//AstNodeVariant::BuiltInFunctionCall(function, parameters),
					};
					// Insert back into list
					items_being_parsed[index - 1] = ParseState::AstNode(operator_ast_node);
					index -= 1;
					continue 'w;
				}
				_ => {}
			}
		};
		index += 1;
	}
	// Parse built in functions without arguments
	for item in items_being_parsed.iter_mut() {
		let (keyword, start, end) = match item {
			ParseState::Token(Token { variant: TokenVariant::Keyword(keyword), start, end }) => (*keyword, *start, *end),
			_ => continue,
		};
		*item = ParseState::AstNode(AstNode { variant: AstNodeVariant::Keyword(keyword, Box::new([]), None), start, end })
	}
	// Parse unary prefix operators
	for index in (0..items_being_parsed.len().saturating_sub(1)).rev() {
		// Make sure the item is an operator token
		let (operator_symbol, operator_type, is_assignment, start) = match &items_being_parsed[index] {
			ParseState::Token(Token {
				variant: TokenVariant::Operator(operator_symbol, operator_type, is_assignment, _), start, end: _
			}) => (*operator_symbol, *operator_type, *is_assignment, *start),
			_ => continue,
		};
		// Make sure the item to the left is not a parsed expression
		match index.checked_sub(1) {
			None => {},
			Some(index) => match &items_being_parsed[index] {
				ParseState::AstNode(..) => continue,
				_ => {},
			},
		}
		// Make sure it's not an assignment
		if is_assignment {
			return Err((Error::FeatureNotYetImplemented("Augmented prefix operators".into()), start));
		}
		// Make sure the base operator is Some
		let operator_symbol = match operator_symbol {
			Some(operator_symbol) => operator_symbol,
			None => return Err((Error::NoOperatorBase, start)),
		};
		// Get operator
		let operator = match prefix_operator_from_symbol(operator_symbol, operator_type) {
			Some(operator) => operator,
			None => return Err((Error::InvalidPrefixOperatorSymbol(operator_symbol), start)),
		};
		// Get operand
		let operand = items_being_parsed.remove(index + 1);
		let operand = match operand {
			ParseState::AstNode(ast_node) => ast_node,
			_ => return Err((Error::BinaryOperatorNotUsedOnExpressions, operand.get_start())),
		};
		// Construct operator node
		let operator_ast_node = AstNode {
			start,
			end: operand.end,
			variant: AstNodeVariant::Operator(Operator::Normal(operator), [operand].into()),
		};
		// Insert back into list
		items_being_parsed[index] = ParseState::AstNode(operator_ast_node);
	}
	// Parse unary postfix operators
	let mut index = 1;
	while index < items_being_parsed.len().saturating_sub(1) {
		// Make sure the item is an operator token
		if let ParseState::Token(Token {
			variant: TokenVariant::Operator(operator_symbol, operator_type, is_assignment, _), start, end
		}) = &items_being_parsed[index] {
			let (operator_symbol, operator_type, is_assignment, start, end) =
			(*operator_symbol, *operator_type, *is_assignment, *start, *end);
			// Make sure the item to the right is not a parsed expression
			if !matches!(
				&items_being_parsed[index + 1], ParseState::AstNode(..) | ParseState::FunctionArgumentsOrParameters(..) |
				ParseState::Token(Token { variant: TokenVariant::Keyword(..), .. })
			) {
				// Assignments not yet implemented
				if is_assignment {
					return Err((Error::FeatureNotYetImplemented("Augmented suffix operators".into()), start));
				}
				// Make sure the base operator is Some
				let operator_symbol = match operator_symbol {
					Some(operator_symbol) => operator_symbol,
					None => return Err((Error::NoOperatorBase, start)),
				};
				// Get operator
				let operator = match postfix_operator_from_symbol(operator_symbol, operator_type) {
					Some(operator) => operator,
					None => return Err((Error::InvalidPrefixOperatorSymbol(operator_symbol), start)),
				};
				// Get operand
				let operand = items_being_parsed.remove(index - 1);
				let operand = match operand {
					ParseState::AstNode(ast_node) => ast_node,
					_ => return Err((Error::BinaryOperatorNotUsedOnExpressions, operand.get_start())),
				};
				// Construct operator node
				let operator_ast_node = AstNode {
					start: operand.start,
					end,
					variant: AstNodeVariant::Operator(Operator::Normal(operator), [operand].into()),
				};
				// Insert back into list
				items_being_parsed[index - 1] = ParseState::AstNode(operator_ast_node);
				continue;
			}
		};
		index += 1;
	}
	// Parse non-augmented binary operators
	for operator_precedence_level in BINARY_OPERATOR_PRECEDENCE {
		// Search for operators in the precedence level
		let mut index = 1;
		while index < items_being_parsed.len().saturating_sub(1) {
			if let ParseState::Token(Token {
				variant: TokenVariant::Operator(operator_symbol, operator_type, false, false), start, end: _
			}) = &items_being_parsed[index] {
				let operator_symbol = match operator_symbol {
					Some(operator_symbol) => *operator_symbol,
					None => return Err((Error::NoOperatorBase, *start)),
				};
				if operator_precedence_level.contains(&operator_symbol) {
					// If we find one
					// Convert to AST operator
					let operator = match binary_operator_from_symbol(operator_symbol, *operator_type) {
						Some(operator) => operator,
						None => return Err((Error::InvalidInfixOperatorSymbol(operator_symbol), *start)),
					};
					// Get left and right operands
					let left_operand = items_being_parsed.remove(index - 1);
					items_being_parsed.remove(index - 1);
					let right_operand = items_being_parsed.remove(index - 1);
					let left_operand = match left_operand {
						ParseState::AstNode(ast_node) => ast_node,
						_ => return Err((Error::BinaryOperatorNotUsedOnExpressions, left_operand.get_start())),
					};
					let right_operand = match right_operand {
						ParseState::AstNode(ast_node) => ast_node,
						_ => return Err((Error::BinaryOperatorNotUsedOnExpressions, right_operand.get_start())),
					};
					// Construct operator node
					let operator_ast_node = AstNode {
						start: left_operand.start,
						end: right_operand.end,
						variant: AstNodeVariant::Operator(Operator::Normal(operator), [left_operand, right_operand].into()),
					};
					// Insert back into list
					items_being_parsed.insert(index - 1, ParseState::AstNode(operator_ast_node));
					continue;
				}
			}
			index += 1;
		}
	}
	// TODO: Parse ternary operators
	let mut index = items_being_parsed.len().saturating_sub(2);
	while index > 0 {
		if let ParseState::Token(Token {
			variant: TokenVariant::Operator(operator_symbol, operator_type, false, _), start, end: _
		}) = &items_being_parsed[index] { 'a: {
			// Get the AST operator
			let operator = match operator_symbol {
				Some(OperatorSymbol::TernaryFirst) => match operator_type {
					OperatorType::SignedLogicalShortCircuit => Operation::ShortCircuitTernary,
					OperatorType::UnsignedLogicalNotShortCircuit => Operation::NotShortCircuitTernary,
					OperatorType::FloatingPointBitwise => return Err((Error::InvalidTernaryOperator, *start)),
				},
				_ => break 'a,
			};
			// Get the index of the ":" operator
			let second_operator_index = items_being_parsed.iter()
				.skip(index)
				.position(|item| matches!(item, ParseState::Token(Token {
					variant: TokenVariant::Operator(Some(OperatorSymbol::TernarySecond), OperatorType::SignedLogicalShortCircuit, false, _),
					start: _,
					end: _,
				}
				)))
				.ok_or_else(|| (Error::UnmatchedTernary, *start))?;
			// Remove operators and operands
			let left_operand = items_being_parsed.remove(index - 1);
			items_being_parsed.remove(index - 1);
			let right_operand = items_being_parsed.remove(index + second_operator_index - 1);
			items_being_parsed.remove(index + second_operator_index - 2);
			let center_operand = items_being_parsed.drain(index - 1..index + second_operator_index - 2).collect();
			// Parse expression between the "?" and ":" operators
			let center_operand = parse_expression(center_operand)?;
			// Get left and right operands
			let left_operand = match left_operand {
				ParseState::AstNode(ast_node) => ast_node,
				_ => return Err((Error::TernaryOperatorNotUsedOnExpressions, left_operand.get_start())),
			};
			let right_operand = match right_operand {
				ParseState::AstNode(ast_node) => ast_node,
				_ => return Err((Error::TernaryOperatorNotUsedOnExpressions, right_operand.get_start())),
			};
			// Construct operator node
			let operator_ast_node = AstNode {
				start: left_operand.start,
				end: right_operand.end,
				variant: AstNodeVariant::Operator(Operator::Normal(operator), [left_operand, center_operand, right_operand].into()),
			};
			// Insert back into list
			items_being_parsed.insert(index - 1, ParseState::AstNode(operator_ast_node));
		}}
		index -= 1;
	}
	// Parse function definitions
	for index in (0..items_being_parsed.len()).rev() {
		// Make sure the item is a function arguments/parameters item
		let (parameters_start, parameters_end) = match &items_being_parsed[index] {
			ParseState::FunctionArgumentsOrParameters(_, parameters_start, parameters_end) =>
				(*parameters_start, *parameters_end),
				_ => continue,
		};
		// Get function body
		if index == items_being_parsed.len().saturating_sub(1) {
			return Err((Error::FunctionParametersWithoutBody, parameters_end));
		}
		let function_body = items_being_parsed.remove(index + 1);
		let function_body_ast_node = match function_body {
			ParseState::AstNode(ast_node) => ast_node,
			_ => return Err((Error::FunctionParametersWithoutBody, function_body.get_start())),
		};
		// Get function parameters
		let function_parameters = match &mut items_being_parsed[index] {
			ParseState::FunctionArgumentsOrParameters(arguments_or_parameters, _, _) => take(arguments_or_parameters),
			_ => unreachable!(),
		};
		// Construct function node
		let function_ast_node = AstNode {
			start: parameters_start,
			end: function_body_ast_node.end,
			variant: AstNodeVariant::FunctionDefinition(function_parameters, Box::new(function_body_ast_node)),
		};
		// Insert back into list
		items_being_parsed[index] = ParseState::AstNode(function_ast_node);
	}
	// Parse some metadata items
	for index in (0..items_being_parsed.len()).rev() {
		// Make sure we have a keyword
		let (keyword, arguments, child, start, keyword_end) = match &mut items_being_parsed[index] {
			ParseState::AstNode(AstNode { variant: AstNodeVariant::Keyword(keyword, arguments, child), start, end: keyword_end }) => {
					match keyword {
						Keyword::EntryPoint | Keyword::Link | Keyword::Loop | Keyword::Break | Keyword::Continue | Keyword::Export => {},
						Keyword::Write | Keyword::Stack | Keyword::Import => continue,
					};
					(*keyword, take(arguments), take(child), *start, *keyword_end)
				}
			_ => continue,
		};
		if child.is_some() {
			return Err((Error::KeywordWithTwoChildren, start));
		}
		// Take child node
		let child_node = match (index + 1) < items_being_parsed.len() {
			true => Some(match items_being_parsed.remove(index + 1) {
				ParseState::AstNode(ast_node) => ast_node,
				_ => return Err((Error::MetadataItemWithoutChildNode, start)),
			}),
			false => None,
		};
		// Construct new node
		let metadata_ast_node = AstNode {
			start,
			end: match &child_node {
				Some(child_node) => child_node.end,
				None => match arguments.last() {
					Some(argument) => argument.end,
					None => keyword_end,
				}
			},
			variant: AstNodeVariant::Keyword(keyword, arguments, child_node.map(|child_node| Box::new(child_node))),
		};
		// Insert back into list
		items_being_parsed[index] = ParseState::AstNode(metadata_ast_node);
	}
	// Parse augmented binary operators
	let mut index = items_being_parsed.len().saturating_sub(2);
	while index > 0 {
		if let ParseState::Token(Token {
			variant: TokenVariant::Operator(operator_symbol, operator_type, true, is_l_value_assignment), start, end: _
		})
		= &items_being_parsed[index] {
			// If we find one
			// Convert to AST operator
			let operator = match is_l_value_assignment {
				false => match operator_symbol {
					Some(operator_symbol) => Operator::Augmented(match binary_operator_from_symbol(*operator_symbol, *operator_type) {
						Some(operator) => operator,
						None => return Err((Error::BinaryOperatorNotUsedOnExpressions, *start)),
					}),
					None => Operator::Assignment,
				}
				true => Operator::LValueAssignment,
			};
			// Get left and right operands
			let left_operand = items_being_parsed.remove(index - 1);
			items_being_parsed.remove(index - 1);
			let right_operand = items_being_parsed.remove(index - 1);
			let left_operand = match left_operand {
				ParseState::AstNode(ast_node) => ast_node,
				_ => return Err((Error::BinaryOperatorNotUsedOnExpressions, left_operand.get_start())),
			};
			let right_operand = match right_operand {
				ParseState::AstNode(ast_node) => ast_node,
				_ => return Err((Error::BinaryOperatorNotUsedOnExpressions, right_operand.get_start())),
			};
			// Construct operator node
			let operator_ast_node = AstNode {
				start: left_operand.start,
				end: right_operand.end,
				variant: AstNodeVariant::Operator(operator, [left_operand, right_operand].into()),
			};
			// Insert back into list
			items_being_parsed.insert(index - 1, ParseState::AstNode(operator_ast_node));
			index -= 1;
			continue;
		}
		index -= 1;
	}
	// Return
	if items_being_parsed.len() == 1 && matches!(&items_being_parsed[0], ParseState::AstNode(..)) {
		match items_being_parsed.into_iter().next() {
			Some(ParseState::AstNode(ast_node)) => return Ok(ast_node),
			_ => unreachable!(),
		}
	}
	for item in items_being_parsed.iter() {
		match item {
			ParseState::Token(Token { variant: TokenVariant::Operator(..), start, .. }) =>
				return Err((Error::OperatorUsedOnNothing, *start)),
			ParseState::Token(Token { variant: TokenVariant::Separator(..), start, .. }) =>
				return Err((Error::OperatorUsedOnNothing, *start)),
			_ => {},
		}
	}
	let start = items_being_parsed.first().unwrap().get_start();
	return Err((Error::FeatureNotYetImplemented("Feature".into()), start));
}

/// Takes in the tokens from tokenizing a file and parses each semi-colon separated global expression into a returned AST node.
pub fn parse_tokens(tokens: Vec<Token>) -> Result<Box<[AstNode]>, (Error, (NonZeroUsize, NonZeroUsize))> {
	// Wrap all the tokens in a parse state object
	let items_being_parsed: Vec<ParseState> = tokens.into_iter()
		.map(|token| match token {
			// Identifier tokens should be converted to identifier AST node parse state objects
			Token { variant: TokenVariant::Identifier(name), start, end } => ParseState::AstNode(AstNode {
				variant: AstNodeVariant::Identifier(name),
				start,
				end,
			}),
			// String tokens should be converted to string AST node parse state objects
			Token { variant: TokenVariant::StringLiteral(string), start, end } => ParseState::AstNode(AstNode {
				variant: AstNodeVariant::String(string),
				start,
				end,
			}),
			// Numerical tokens should be converted to constant AST node parse state objects
			Token { variant: TokenVariant::NumericalLiteral(number), start, end } => ParseState::AstNode(AstNode {
				variant: AstNodeVariant::Constant(number),
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