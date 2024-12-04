use std::{cmp::Ordering, collections::{HashMap, HashSet}, hash::{DefaultHasher, Hash, Hasher}, iter::{repeat, repeat_n}, mem::{swap, take}, num::NonZeroUsize, path::PathBuf};

use strum_macros::EnumDiscriminants;

use crate::{built_value::{BuiltLValue, BuiltRValue}, compile::relative_filepath_to_absolute, error::Error, file_build_data::FileBuildData, function_building_data::{BlockLevel, FunctionBuildData}, token::Keyword, MainData};
use llvm_nhb::{builder::Builder, enums::{CallingConvention, Comparison, Linkage}, module::Module, types::Type, value::Value};

#[derive(Debug, Clone)]
pub enum Operation {
	IntegerAdd,
	FloatAdd,
	IntegerSubtract,
	FloatSubtract,
	IntegerMultiply,
	FloatMultiply,
	SignedDivide,
	UnsignedDivide,
	FloatDivide,
	SignedTruncatedModulo,
	UnsignedModulo,
	FloatTruncatedModulo,
	Read,
	IntegerNegate,
	FloatNegate,
	Dereference,
	TakeReference,
	BitwiseAnd,
	BitwiseOr,
	BitwiseXor,
	BitwiseNot,
	LogicalNotShortCircuitAnd,
	LogicalNotShortCircuitOr,
	LogicalShortCircuitAnd,
	LogicalShortCircuitOr,
	LogicalXor,
	LogicalNot,
	IntegerEqualTo,
	IntegerNotEqualTo,
	UnsignedLessThan,
	UnsignedLessThanOrEqualTo,
	UnsignedGreaterThan,
	UnsignedGreaterThanOrEqualTo,
	UnsignedThreeWayCompare,
	SignedLessThan,
	SignedLessThanOrEqualTo,
	SignedGreaterThan,
	SignedGreaterThanOrEqualTo,
	SignedThreeWayCompare,
	FloatEqualTo,
	FloatNotEqualTo,
	FloatLessThan,
	FloatLessThanOrEqualTo,
	FloatGreaterThan,
	FloatGreaterThanOrEqualTo,
	FloatThreeWayCompare,
	NotShortCircuitTernary,
	ShortCircuitTernary,
	PrefixIntegerIncrement,
	SuffixIntegerIncrement,
	PrefixIntegerDecrement,
	SuffixIntegerDecrement,
	LogicalLeftBitShift,
	LogicalRightBitShift,
	ArithmeticRightBitShift,
}

#[derive(Debug, Clone)]
pub enum Operator {
	Assignment,
	Normal(Operation),
	Augmented(Operation),
	LValueAssignment,
}

#[derive(Debug, EnumDiscriminants, Clone)]
pub enum AstNodeVariant {
	/// A constant.
	Constant(u64),
	/// An operator with its operands.
	Operator(Operator, Box<[AstNode]>),
	/// For an identifier such as `my_var` or `myFunc`.
	Identifier(Box<str>),
	/// A semi-colon separated list of expressions that where between curly brackets and if the result is undefined.
	Block(Box<[AstNode]>, bool),
	/// A function pointer to call and the arguments passed in.
	FunctionCall(Box<AstNode>, Box<[AstNode]>),
	/// A keyword, arguments and an optional child node
	Keyword(Keyword, Box<[AstNode]>, Option<Box<AstNode>>),
	/// A list of parameters for a function definition and the function body.
	FunctionDefinition(Box<[AstNode]>, Box<AstNode>),
	/// A string literal.
	String(Box<str>),
}

#[derive(Debug, Clone)]
pub struct AstNode {
	pub variant: AstNodeVariant,
	/// The line and column that this node starts at.
	pub start: (NonZeroUsize, NonZeroUsize),
	/// The line and column of the char after the last char of this node.
	pub end: (NonZeroUsize, NonZeroUsize),
}

impl AstNode {
	pub fn print_tree(&self, level: usize) {
		for _ in 0..level {
			print!("-");
		}
		print!("{} {}:{} to {}:{} {:?}", '{', self.start.0, self.start.1, self.end.0, self.end.1, AstNodeVariantDiscriminants::from(&self.variant));
		match &self.variant {
			AstNodeVariant::Block(_, result_is_undefined) => print!(", result_is_undefined: {result_is_undefined:?}"),
			AstNodeVariant::Constant(value) => print!(", value: {value}"),
			AstNodeVariant::FunctionCall(_, _) => {},
			AstNodeVariant::FunctionDefinition(_, _) => {},
			AstNodeVariant::Identifier(name) => print!(", name: {name}"),
			AstNodeVariant::String(string_value) => print!(", string_value: {string_value:?}"),
			AstNodeVariant::Operator(operator, _) => print!(", operator: {operator:?}"),
			AstNodeVariant::Keyword(keyword, _, _) => print!(", keyword: {keyword:?}"),
		}
		println!(" {}", '}');
		match &self.variant {
			AstNodeVariant::Block(nodes, _) => for node in nodes {
				node.print_tree(level + 1);
			}
			AstNodeVariant::FunctionCall(function, arguments) => {
				print!("p");
				function.print_tree(level + 1);
				for argument in arguments {
					print!("a");
					argument.print_tree(level + 1);
				}
			},
			AstNodeVariant::Keyword(_, arguments, child) => {
				for argument in arguments {
					print!("a");
					argument.print_tree(level + 1);
				}
				if let Some(child) = child {
					print!("c");
					child.print_tree(level + 1);
				}
			}
			AstNodeVariant::FunctionDefinition(parameters, body) => {
				for parameter in parameters {
					print!("p");
					parameter.print_tree(level + 1);
				}
				print!("b");
				body.print_tree(level + 1);
			}
			AstNodeVariant::Operator(_, operands) => for operand in operands {
				operand.print_tree(level + 1);
			}
			AstNodeVariant::Constant(..) => {}
			AstNodeVariant::Identifier(..) => {}
			AstNodeVariant::String(..) => {}
		}
	}

	/// Removes global assignments nodes and puts them into a `(name, node)` hash map, replacing them with an identifier node.
	pub fn separate_globals(&mut self, global_list: &mut HashMap<Box<str>, (Self, bool)>, will_be_discarded: bool, can_be_exported: bool) -> Result<bool, (Error, (NonZeroUsize, NonZeroUsize))> {
		let start = self.start;
		match &mut self.variant {
			AstNodeVariant::Operator(operator, operands) => match operator {
				Operator::Assignment => {
					// Separate operands
					let dummy_number = NonZeroUsize::new(1).unwrap();
					let mut identifier_node = AstNode {
						start: (dummy_number, dummy_number), end: (dummy_number, dummy_number), variant: AstNodeVariant::Constant(0)
					};
					let mut operand_node = AstNode {
						start: (dummy_number, dummy_number), end: (dummy_number, dummy_number), variant: AstNodeVariant::Constant(0)
					};
					swap(&mut operands[0], &mut identifier_node);
					swap(&mut operands[1], &mut operand_node);
					let is_exported = identifier_node.separate_globals(global_list, false, true)?;
					operand_node.separate_globals(global_list, false, false)?;
					// Get name to assign to
					let AstNode {
						start: _,
						end:_,
						variant,
					} = &identifier_node;
					let name = match variant {
						AstNodeVariant::Identifier(name) => name.clone(),
						_ => return Err((Error::GlobalAssignmentToNonIdentifier, start)),
					};
					// Pop out global assignment into global variable list
					match global_list.insert(name, (operand_node, is_exported)) {
						Some(..) => return Err((Error::GlobalVariableConflict(match variant {
							AstNodeVariant::Identifier(name) => name.clone().into(),
							_ => return Err((Error::GlobalAssignmentToNonIdentifier, start)),
						}), start)),
						None => {}
					};
					// Replace node with the identifier node
					*self = identifier_node;
				}
				Operator::Normal(..) => for operand in operands {
					operand.separate_globals(global_list, will_be_discarded, false)?;
				}
				Operator::Augmented(..) => return Err((Error::GlobalAugmentedOperator, start)),
				Operator::LValueAssignment => return Err((Error::GlobalLValueAssignment, start)),
			}
			AstNodeVariant::Constant(..) => {}
			AstNodeVariant::FunctionCall(..) => if will_be_discarded {
				return Err((Error::DiscardedGlobalFunctionCall, start));
			}
			AstNodeVariant::Block(children, is_result_undefined) => {
				if *is_result_undefined && children.is_empty() {
					return Ok(false);
				}
				if children.len() != 1 || (*is_result_undefined && children.len() != 0) {
					return Err((Error::FeatureNotYetImplemented("Global blocks".into()), start));
				}
				let dummy_number = NonZeroUsize::new(1).unwrap();
				let mut child = AstNode { start: (dummy_number, dummy_number), end: (dummy_number, dummy_number), variant: AstNodeVariant::Constant(0) };
				swap(&mut children[0], &mut child);
				child.separate_globals(global_list, will_be_discarded, false)?;
				*self = child;
			}
			AstNodeVariant::FunctionDefinition(..) => {}
			AstNodeVariant::Identifier(..) => {}
			AstNodeVariant::String(..) => {}
			AstNodeVariant::Keyword(keyword, arguments, child) => match keyword {
				Keyword::Export => {
					if !arguments.is_empty() {
						return Err((Error::InvalidBuiltInFunctionArgumentCount, start));
					}
					let child = match child {
						Some(child) => child,
						None => return Err((Error::InvalidBuiltInFunctionArgumentCount, start)),
					};
					if !can_be_exported {
						return Err((Error::InvalidExport, start));
					}
					child.separate_globals(global_list, will_be_discarded, false)?;
					let dummy_number = NonZeroUsize::new(1).unwrap();
					let mut child_taken = AstNode { start: (dummy_number, dummy_number), end: (dummy_number, dummy_number), variant: AstNodeVariant::Constant(0) };
					swap(&mut **child, &mut child_taken);
					*self = child_taken;
					return Ok(true);
				}
				_ => {
					for argument in arguments {
						argument.separate_globals(global_list, will_be_discarded, false)?;
					}
					if let Some(child) = child {
						child.separate_globals(global_list, will_be_discarded, false)?;
					}
				}
			}
		}
		Ok(false)
	}

	/// Will search a global node and its children for global variable dependencies that need to be compiled before this node is.
	///
	/// Appends imported filepaths that need to be compiled before this global variable to `import_dependencies`.
	///
	/// Appends the name of global variables that need to be compiled before this global variable to `variable_dependencies`.
	pub fn get_variable_dependencies(
		&self,
		main_data: &MainData,
		filepath: &PathBuf,
		variable_dependencies: &mut HashSet<Box<str>>,
		import_dependencies: &mut HashSet<PathBuf>,
		local_variables: &mut Vec<HashSet<Box<str>>>,
		is_l_value: bool,
	) -> Result<(), (Error, (NonZeroUsize, NonZeroUsize))> {
		// Unpack
		let AstNode {
			variant,
			start,
			end: _,
		} = self;
		// Search depends on type of node
		match variant {
			// For a block we search each sub-expression in the block
			AstNodeVariant::Block(sub_expressions, _) => {
				match is_l_value {
					false => {
						local_variables.push(HashSet::new());
						for expression in sub_expressions {
							expression.get_variable_dependencies(main_data, filepath, variable_dependencies, import_dependencies, local_variables, false)?;
						}
						local_variables.pop();
					}
					true => return Err((Error::FeatureNotYetImplemented("L-value blocks".into()), *start)),
				};
			}
			// Constants can't have dependencies
			AstNodeVariant::Constant(..) => {}
			// For a function call we search the expression yeilding the function pointer and the function argument expressions
			AstNodeVariant::FunctionCall(function, arguments) => {
				if is_l_value {
					return Err((Error::LValueFunctionCall, *start));
				}
				function
					.get_variable_dependencies(main_data, filepath, variable_dependencies, import_dependencies, &mut local_variables.clone(), false)?;
				for argument in arguments {
					argument.get_variable_dependencies(
						main_data, filepath, variable_dependencies, import_dependencies, local_variables, false
					)?;
				}
			}
			AstNodeVariant::Keyword(keyword, arguments, child) => {
				match keyword {
					Keyword::Write | Keyword::Stack => for argument in arguments {
						argument.get_variable_dependencies(
							main_data, filepath, variable_dependencies, import_dependencies, local_variables, false
						)?;
					}
					Keyword::EntryPoint => child.as_ref().unwrap().get_variable_dependencies(main_data, filepath, variable_dependencies, import_dependencies, local_variables, is_l_value)?,
					Keyword::Link | Keyword::SystemConstant => for argument in arguments {
						argument.get_variable_dependencies(
							main_data, filepath, variable_dependencies, import_dependencies, local_variables, false
						)?;
					}
					Keyword::Export => unreachable!(),
					Keyword::Loop => child.as_ref().unwrap().get_variable_dependencies(main_data, filepath, variable_dependencies, import_dependencies, local_variables, false)?,
					Keyword::Break | Keyword::Continue => if !arguments.is_empty() {
						return Err((Error::FeatureNotYetImplemented("Arguments for @break and @continue".into()), *start));
					}
					Keyword::Import => {
						for argument in arguments {
							argument.get_variable_dependencies(main_data, filepath, variable_dependencies, import_dependencies, local_variables, false)?;
						}
						let import_name_node = &arguments[0];
						let import_name = match &import_name_node.variant {
							AstNodeVariant::Identifier(import_name) => &**import_name,
							AstNodeVariant::String(import_name) => &**import_name,
							_ => return Err((Error::ExpectedIdentifier, import_name_node.start)),
						};
						let absolute_filepath = relative_filepath_to_absolute(main_data, filepath, import_name)
							.map_err(|error| (error, import_name_node.start))?;
						import_dependencies.insert(absolute_filepath);
					}
				}
			}
			AstNodeVariant::FunctionDefinition(parameters, body) => {
				if is_l_value {
					return Err((Error::LValueFunctionDefinition, *start));
				}
				let mut local_variables_top = HashSet::new();
				for parameter in parameters {
					match &parameter.variant {
						AstNodeVariant::Identifier(name) => {
							local_variables_top.insert(name.clone());
						}
						_ => return Err((Error::ExpectedIdentifier, parameter.start)),
					}
				}
				let mut local_variables = vec![local_variables_top];
				body.get_variable_dependencies(
					main_data, filepath, variable_dependencies, import_dependencies, &mut local_variables, false
				)?;
			}
			AstNodeVariant::Identifier(name) => match is_l_value {
				// An identifier being used as a r-value should have its name added to the the global variable list unless it's in the local variable list
				false => 'a: {
					for local_variable_level in local_variables.iter() {
						if local_variable_level.contains(name) {
							break 'a;
						}
					}
					variable_dependencies.insert(name.clone());
				}
				// An identifier being used as an l-value should be added to the local variable list
				// so that it is not added to the global variable list if used later
				true => 'a: {
					for local_variable_level in local_variables.iter() {
						if local_variable_level.contains(name) {
							break 'a;
						}
					}
					local_variables.last_mut().unwrap().insert(name.clone());
				}
			}
			AstNodeVariant::Operator(operator, operands) => match operator {
				// For an assignment, we search the the l-value and r-value
				Operator::Assignment => {
					operands[0].get_variable_dependencies(main_data, filepath, variable_dependencies, import_dependencies, local_variables, true)?;
					operands[1].get_variable_dependencies(main_data, filepath, variable_dependencies, import_dependencies, local_variables, false)?;
				}
				// For an augmented assignment, we search the the l-value and r-value
				Operator::Augmented(operation) => match operation {
					Operation::IntegerAdd | Operation::IntegerSubtract | Operation::IntegerMultiply | Operation::SignedDivide | Operation::SignedTruncatedModulo |
					Operation::UnsignedDivide | Operation::UnsignedModulo |
					Operation::FloatAdd | Operation::FloatSubtract | Operation::FloatMultiply | Operation::FloatDivide | Operation::FloatTruncatedModulo |
					Operation::BitwiseAnd | Operation::BitwiseOr | Operation::BitwiseXor | Operation::LogicalNotShortCircuitAnd |
					Operation::LogicalNotShortCircuitOr | Operation::LogicalXor | Operation::LogicalShortCircuitAnd | Operation::LogicalShortCircuitOr |
					Operation::IntegerEqualTo | Operation::IntegerNotEqualTo | Operation::UnsignedLessThanOrEqualTo | Operation::SignedLessThanOrEqualTo |
					Operation::UnsignedGreaterThan | Operation::UnsignedGreaterThanOrEqualTo | Operation::UnsignedLessThan |
					Operation::SignedGreaterThan | Operation::SignedGreaterThanOrEqualTo | Operation::SignedLessThan |
					Operation::FloatEqualTo | Operation::FloatNotEqualTo | Operation::FloatLessThanOrEqualTo |
					Operation::FloatGreaterThan | Operation::FloatGreaterThanOrEqualTo | Operation::FloatLessThan |
					Operation::LogicalLeftBitShift | Operation::LogicalRightBitShift | Operation::ArithmeticRightBitShift |
					Operation::UnsignedThreeWayCompare | Operation::SignedThreeWayCompare | Operation::FloatThreeWayCompare => {
						operands[0]
							.get_variable_dependencies(main_data, filepath, variable_dependencies, import_dependencies, local_variables, true)?;
						operands[1]
							.get_variable_dependencies(main_data, filepath, variable_dependencies, import_dependencies, local_variables, false)?;
					}
					Operation::Dereference | Operation::IntegerNegate | Operation::FloatNegate | Operation::Read | Operation::TakeReference |
					Operation::BitwiseNot | Operation::LogicalNot | Operation::PrefixIntegerDecrement | Operation::PrefixIntegerIncrement | Operation::SuffixIntegerDecrement |
					Operation::SuffixIntegerIncrement
						=> return Err((Error::FeatureNotYetImplemented("Augmented unary operators".into()), *start)),
					Operation::ShortCircuitTernary | Operation::NotShortCircuitTernary => unreachable!(),
				}
				// For normal operators we search the operands
				Operator::Normal(operation) => match operation {
					// Operators that only have r-values as operands
					Operation::IntegerAdd | Operation::IntegerSubtract | Operation::IntegerMultiply | Operation::SignedDivide | Operation::SignedTruncatedModulo |
					Operation::UnsignedDivide | Operation::UnsignedModulo |
					Operation::FloatAdd | Operation::FloatSubtract | Operation::FloatMultiply | Operation::FloatDivide | Operation::FloatTruncatedModulo |
					Operation::Dereference | Operation::IntegerNegate | Operation::FloatNegate |
					Operation::BitwiseAnd | Operation::BitwiseOr | Operation::BitwiseXor | Operation::LogicalNotShortCircuitAnd |
					Operation::LogicalNotShortCircuitOr | Operation::LogicalXor | Operation::LogicalShortCircuitAnd |
					Operation::LogicalShortCircuitOr | Operation::TakeReference | Operation::BitwiseNot | Operation::LogicalNot |
					Operation::IntegerEqualTo | Operation::IntegerNotEqualTo  | Operation::UnsignedLessThanOrEqualTo | Operation::SignedLessThanOrEqualTo |
					Operation::UnsignedGreaterThan | Operation::UnsignedGreaterThanOrEqualTo | Operation::UnsignedLessThan |
					Operation::SignedGreaterThan | Operation::SignedGreaterThanOrEqualTo | Operation::SignedLessThan |
					Operation::FloatEqualTo | Operation::FloatNotEqualTo  | Operation::FloatLessThanOrEqualTo |
					Operation::FloatGreaterThan | Operation::FloatGreaterThanOrEqualTo | Operation::FloatLessThan |
					Operation::ArithmeticRightBitShift | Operation::LogicalLeftBitShift | Operation::LogicalRightBitShift |
					Operation::UnsignedThreeWayCompare | Operation::SignedThreeWayCompare | Operation::FloatThreeWayCompare
						=> for operand in operands {
						operand.get_variable_dependencies(main_data, filepath, variable_dependencies, import_dependencies, local_variables, false)?;
					}
					// Operators that only have l-values as operands
					Operation::Read | Operation::PrefixIntegerDecrement | Operation::PrefixIntegerIncrement | Operation::SuffixIntegerDecrement | Operation::SuffixIntegerIncrement => for operand in operands {
						operand.get_variable_dependencies(main_data, filepath, variable_dependencies, import_dependencies, local_variables, true)?;
					}
					// Ternary operator
					Operation::ShortCircuitTernary | Operation::NotShortCircuitTernary => {
						operands[0].get_variable_dependencies(main_data, filepath, variable_dependencies, import_dependencies, local_variables, false)?;
						operands[1].get_variable_dependencies(main_data, filepath, variable_dependencies, import_dependencies, local_variables, is_l_value)?;
						operands[2].get_variable_dependencies(main_data, filepath, variable_dependencies, import_dependencies, local_variables, is_l_value)?;
					}
				}
				// For l-value assignments, we search the operands
				Operator::LValueAssignment => for operand in operands {
					operand.get_variable_dependencies(main_data, filepath, variable_dependencies, import_dependencies, local_variables, true)?;
				}
			}
			// Strings, just like constants, can't have dependencies
			AstNodeVariant::String(..) => {}
		}
		Ok(())
	}

	pub fn build_function_signature<'a>(
		&'a self,
		main_data: &'a MainData,
		file_build_data: &mut FileBuildData<'a, 'a>,
		llvm_module: &'a Module,
		llvm_builder: &'a Builder,
		name: &str,
	) -> Result<Value<'a, 'a>, (Error, (NonZeroUsize, NonZeroUsize))> {
		// Unpack node
		let Self {
			start,
			end: _,
			variant,
		} = self;
		match variant {
			AstNodeVariant::FunctionDefinition(parameters, _) => {
				// Create function parameter type
				if parameters.len() > u16::MAX as usize {
					return Err((Error::TooManyFunctionParameters, *start));
				}
				let parameter_types: Box<[Type]> = repeat(main_data.int_type).take(parameters.len()).collect();
				let function_type = main_data.int_type.function_type(&*parameter_types, false);
				// Build function value
				let function = llvm_module.add_function(function_type, &*name);
				function.set_linkage(Linkage::Internal);
				// Return
				Ok(function)
			}
			AstNodeVariant::Keyword(keyword, _, child_node) => match keyword {
				Keyword::EntryPoint =>
					child_node.as_ref().unwrap().build_function_signature(main_data, file_build_data, llvm_module, llvm_builder, name),
				_ => unreachable!(),
			}
			_ => unreachable!(),
		}
	}

	/// Build a function definition into LLVM IR code and return the built value.
	fn build_function_definition<'a>(
		&'a self,
		main_data: &'a MainData,
		file_build_data: &mut FileBuildData<'a, 'a>,
		llvm_module: &'a Module,
		llvm_builder: &'a Builder,
		name: &str,
		is_entry_point: bool,
	) -> Result<Value<'a, 'a>, (Error, (NonZeroUsize, NonZeroUsize))> {
		// Unpack function definition node
		let Self {
			start,
			end: _,
			variant,
		} = self;
		// If we have a metadata node, then build the child node
		let (parameters, function_body) = match variant {
			AstNodeVariant::FunctionDefinition(function_parameters, function_body) => (function_parameters, function_body),
			AstNodeVariant::Keyword(keyword, _, child) => match keyword {
				Keyword::EntryPoint =>
					return child.as_ref().unwrap().build_function_definition(main_data, file_build_data, llvm_module, llvm_builder, name, true),
				_ => unreachable!(),
			}
			_ => unreachable!(),
		};
		let function = match file_build_data.built_global_function_signatures.get(name) {
			Some(function) => function.clone(),
			None => {
				// Create function parameter type
				if parameters.len() > u16::MAX as usize {
					return Err((Error::TooManyFunctionParameters, *start));
				}
				let parameter_types: Box<[Type]> = repeat(main_data.int_type).take(parameters.len()).collect();
				let function_type = main_data.int_type.function_type(&*parameter_types, false);
				// Build function value
				let function = llvm_module.add_function(function_type, &*name);
				function.set_linkage(Linkage::Internal);
				function
			}
		};
		// Build function body
		// Build the entry basic block where allocas will be added to
		let entry_basic_block = function.append_basic_block(&main_data.llvm_context, "entry");
		llvm_builder.position_at_end(&entry_basic_block);
		// Build the first basic block that we will be building the function body on
		let body_basic_block = function.append_basic_block(&main_data.llvm_context, "function_body");
		llvm_builder.position_at_end(&body_basic_block);
		// Build the function info struct
		let mut inner_block_stack: Vec<BlockLevel<'_,>> = vec![BlockLevel {
			local_variables: HashMap::new(),
			basic_blocks: vec![body_basic_block.clone()],
			allocas_in_use: HashSet::new(),
			array_allocas_in_use: HashMap::new(),
			is_loop: false,
		}];
		let mut function_info = FunctionBuildData {
			function: function.clone(),
			block_stack: &mut inner_block_stack,
			allocas_not_in_use: &mut HashSet::new(),
			alloca_block: &entry_basic_block,
			array_allocas_not_in_use: &mut HashMap::new(),
		};
		// Build function parameters
		for (parameter_index, parameter) in parameters.iter().enumerate() {
			// Get parameter name
			let parameter_name = match &parameter.variant {
				AstNodeVariant::Identifier(name) => name,
				_ => return Err((Error::ExpectedIdentifier, parameter.start)),
			};
			// Add parameter to local scope
			let parameter_value = function.get_parameter(parameter_index);
			let parameter_variable = function_info.get_alloca(main_data, llvm_builder, parameter_name);
			parameter_variable.build_store(&parameter_value, llvm_builder);
			function_info.block_stack.last_mut().unwrap().local_variables.insert(parameter_name.clone(), BuiltLValue::AllocaVariable(parameter_variable));
		}
		// Build function body
		let function_body_built = function_body.build_r_value(main_data, file_build_data, llvm_module, llvm_builder, Some(&mut function_info))?;
		// Build branch from entry block to first body block
		llvm_builder.position_at_end(&entry_basic_block);
		llvm_builder.build_branch(&body_basic_block);
		// Build return
		llvm_builder.position_at_end(function_info.block_stack.last().unwrap().last_block());
		BuiltRValue::Value(function_body_built.get_value(main_data, llvm_builder).build_return(llvm_builder));
		// Return
		let result = function.build_ptr_to_int(llvm_builder, main_data.int_type, "fn_ptr_to_int");
		if is_entry_point {
			if file_build_data.entrypoint.is_some() {
				return Err((Error::MultipleEntryPoints, *start));
			}
			file_build_data.entrypoint = Some(result.clone())
		}
		Ok(result)
	}

	/// Build an r-value into LLVM IR code and return the built value.
	pub fn build_r_value<'a, 'b>(
		&'a self,
		main_data: &'a MainData<'a>,
		file_build_data: &mut FileBuildData<'a, 'a>,
		llvm_module: &'a Module,
		llvm_builder: &'a Builder<'a, 'a>,
		function_build_data: Option<&mut FunctionBuildData<'a, 'b>>,
	) -> Result<BuiltRValue<'a>, (Error, (NonZeroUsize, NonZeroUsize))> {
		// Unpack
		let Self {
			start,
			end: _,
			variant,
		} = self;
		// Use the `build_function_definition()` method to build the node if it is a function.
		if self.is_function() {
			// Build function
			let out = self.build_function_definition(
				main_data, file_build_data, llvm_module, llvm_builder, "__bcz__unnamedFunction", false/*, false*/
			)?;
			// The function will have positioned the builder pos to one of it's basic blocks, so re-position it back
			if let Some(function_info) = function_build_data {
				llvm_builder.position_at_end(function_info.block_stack.last().unwrap().last_block());
			}
			// Return
			return Ok(BuiltRValue::Value(out));
		}
		// Building depends on node variant
		Ok(match variant {
			// Constants build an int constant
			AstNodeVariant::Constant(value) => BuiltRValue::Value(main_data.int_type.const_int(*value as u128, false)),
			// For an identifier, we load the value stored in the variable it represents
			AstNodeVariant::Identifier(name) => get_variable_by_name(main_data, file_build_data, llvm_builder, function_build_data, &*name),
			AstNodeVariant::Operator(operator, operands) => {
				let function_build_data = match function_build_data {
					Some(function_build_data) => function_build_data,
					None => return Err((Error::GlobalOperatorNotConstEvaluated, *start))
				};
				match operator {
					// For an assignment, we build the l and r-values and then build a store instruction
					Operator::Assignment => {
						let r_value = operands[1].build_r_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?;
						let l_value = operands[0].build_l_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?;
						l_value.set_value(main_data, llvm_builder, &r_value.get_value(main_data, llvm_builder));
						return Ok(r_value);
					}
					// For a normal operator, we build the operands then build the operator instruction
					Operator::Normal(operation) => match operation {
						Operation::IntegerAdd | Operation::IntegerSubtract | Operation::IntegerMultiply |
						Operation::UnsignedDivide | Operation::UnsignedModulo | Operation::SignedDivide | Operation::SignedTruncatedModulo |
						Operation::BitwiseAnd | Operation::BitwiseOr | Operation::BitwiseXor | Operation::LogicalNotShortCircuitOr |
						Operation::LogicalNotShortCircuitAnd | Operation::LogicalXor |
						Operation::IntegerEqualTo | Operation::IntegerNotEqualTo | Operation::UnsignedLessThanOrEqualTo |
						Operation::UnsignedGreaterThan | Operation::UnsignedGreaterThanOrEqualTo | Operation::UnsignedLessThan |
						Operation::SignedLessThanOrEqualTo | Operation::SignedGreaterThan | Operation::SignedGreaterThanOrEqualTo | Operation::SignedLessThan => {
							let left_value = operands[0].build_r_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?
								.get_value(main_data, llvm_builder);
							let right_value = operands[1].build_r_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?
								.get_value(main_data, llvm_builder);
							let result = match operation {
								Operation::IntegerAdd => left_value.build_add(&right_value, llvm_builder, "add_temp"),
								Operation::IntegerSubtract => left_value.build_sub(&right_value, llvm_builder, "sub_temp"),
								Operation::IntegerMultiply => left_value.build_mult(&right_value, llvm_builder, "mult_temp"),
								Operation::UnsignedDivide => left_value.build_unsigned_div(&right_value, llvm_builder, "udiv_temp"),
								Operation::UnsignedModulo => left_value.build_unsigned_modulo(&right_value, llvm_builder, "umod_temp"),
								Operation::SignedDivide => left_value.build_signed_div(&right_value, llvm_builder, "sdiv_temp"),
								Operation::SignedTruncatedModulo => left_value.build_signed_truncated_modulo(&right_value, llvm_builder, "stmod_temp"),
								Operation::BitwiseAnd => left_value.build_bitwise_and(&right_value, llvm_builder, "band_temp"),
								Operation::BitwiseOr | Operation::LogicalNotShortCircuitOr =>
									left_value.build_bitwise_or(&right_value, llvm_builder, "bor_temp"),
								Operation::BitwiseXor => left_value.build_bitwise_xor(&right_value, llvm_builder, "bxor_temp"),
								Operation::LogicalNotShortCircuitAnd => {
									let zero_const = main_data.int_type.const_int(0, false);
									let left_value_bool =
										left_value.build_compare(&zero_const, Comparison::NotEqual, llvm_builder, "itbneq_temp");
									let right_value_bool =
										right_value.build_compare(&zero_const, Comparison::NotEqual, llvm_builder, "itbneq_temp");
									left_value_bool.build_bitwise_and(&right_value_bool, llvm_builder, "band_temp")
										.build_zero_extend(llvm_builder, main_data.int_type, "bool_to_int_temp")
								}
								Operation::LogicalXor => {
									let zero_const = main_data.int_type.const_int(0, false);
									let left_value_bool =
										left_value.build_compare(&zero_const, Comparison::NotEqual, llvm_builder, "itbneq_temp");
									let right_value_bool =
										right_value.build_compare(&zero_const, Comparison::NotEqual, llvm_builder, "itbneq_temp");
									left_value_bool.build_bitwise_xor(&right_value_bool, llvm_builder, "bxor_temp")
										.build_zero_extend(llvm_builder, main_data.int_type, "bool_to_int_temp")
								}
								Operation::IntegerEqualTo =>
									left_value.build_compare(&right_value, Comparison::Equal, llvm_builder, "eq_temp")
										.build_zero_extend(llvm_builder, main_data.int_type, "bool_to_int_temp"),
								Operation::IntegerNotEqualTo =>
									left_value.build_compare(&right_value, Comparison::NotEqual, llvm_builder, "neq_temp")
										.build_zero_extend(llvm_builder, main_data.int_type, "bool_to_int_temp"),
								Operation::UnsignedLessThan =>
									left_value.build_compare(&right_value, Comparison::UnsignedLessThan, llvm_builder, "ult_temp")
										.build_zero_extend(llvm_builder, main_data.int_type, "bool_to_int_temp"),
								Operation::UnsignedLessThanOrEqualTo =>
									left_value.build_compare(&right_value, Comparison::UnsignedLessThanOrEqualTo, llvm_builder, "ulteq_temp")
										.build_zero_extend(llvm_builder, main_data.int_type, "bool_to_int_temp"),
								Operation::UnsignedGreaterThan =>
									left_value.build_compare(&right_value, Comparison::UnsignedGreaterThan, llvm_builder, "ugt_temp")
										.build_zero_extend(llvm_builder, main_data.int_type, "bool_to_int_temp"),
								Operation::UnsignedGreaterThanOrEqualTo =>
									left_value.build_compare(&right_value, Comparison::UnsignedGreaterThanOrEqualTo, llvm_builder, "ugteq_temp")
										.build_zero_extend(llvm_builder, main_data.int_type, "bool_to_int_temp"),
								Operation::SignedLessThan =>
									left_value.build_compare(&right_value, Comparison::SignedLessThan, llvm_builder, "slt_temp")
										.build_zero_extend(llvm_builder, main_data.int_type, "bool_to_int_temp"),
								Operation::SignedLessThanOrEqualTo =>
									left_value.build_compare(&right_value, Comparison::SignedLessThanOrEqualTo, llvm_builder, "slteq_temp")
										.build_zero_extend(llvm_builder, main_data.int_type, "bool_to_int_temp"),
								Operation::SignedGreaterThan =>
									left_value.build_compare(&right_value, Comparison::SignedGreaterThan, llvm_builder, "sgt_temp")
										.build_zero_extend(llvm_builder, main_data.int_type, "bool_to_int_temp"),
								Operation::SignedGreaterThanOrEqualTo =>
									left_value.build_compare(&right_value, Comparison::SignedGreaterThanOrEqualTo, llvm_builder, "sgteq_temp")
										.build_zero_extend(llvm_builder, main_data.int_type, "bool_to_int_temp"),
								_ => unreachable!(),
							};
							BuiltRValue::Value(result)
						}
						Operation::LogicalShortCircuitAnd | Operation::LogicalShortCircuitOr => {
							// Get the left value
							let left_value = operands[0].build_r_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?
								.get_value(main_data, llvm_builder);
							// Get if we should skip
							let skip_condition = match operation {
								Operation::LogicalShortCircuitAnd
									=> left_value.build_compare(&main_data.int_type.const_int(0, false), Comparison::Equal, llvm_builder, "should_skip_temp"),
								Operation::LogicalShortCircuitOr
									=> left_value.build_compare(&main_data.int_type.const_int(0, false), Comparison::NotEqual, llvm_builder, "should_skip_temp"),
								_ => unreachable!()
							};
							// Get the alloca to write the result to
							let result_alloca = function_build_data.get_alloca(main_data, llvm_builder, "logical_short_circuit_value");
							function_build_data.block_stack.last_mut().unwrap().allocas_in_use.insert(result_alloca.clone());
							result_alloca.build_store(&left_value, llvm_builder);
							let get_right_value_basic_block = function_build_data.function.append_basic_block(&main_data.llvm_context, "get_right_value");
							let end_basic_block = function_build_data.function.append_basic_block(&main_data.llvm_context, "skip");
							// Build a conditional branch that may or may not skip getting the right value
							skip_condition.build_conditional_branch(&end_basic_block, &get_right_value_basic_block, &main_data.llvm_context, llvm_builder);
							// Build getting right value
							llvm_builder.position_at_end(&get_right_value_basic_block);
							function_build_data.block_stack.last_mut().unwrap().basic_blocks.push(get_right_value_basic_block);
							let right_value = operands[1].build_r_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?;
							let operation_result = match operation {
								Operation::LogicalShortCircuitAnd => right_value,
								Operation::LogicalShortCircuitOr => right_value,
								_ => unreachable!()
							};
							result_alloca.build_store(&operation_result.get_value(main_data, llvm_builder), llvm_builder);
							llvm_builder.build_branch(&end_basic_block);
							// Re-position builder at end
							llvm_builder.position_at_end(&end_basic_block);
							function_build_data.block_stack.last_mut().unwrap().basic_blocks.push(end_basic_block);
							// Read result
							let result = result_alloca.build_load(main_data.int_type, llvm_builder, "read_result");
							function_build_data.surrender_alloca(result_alloca);
							BuiltRValue::Value(result)
						}
						Operation::NotShortCircuitTernary => {
							// Build operands
							let condition = operands[0].build_r_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?
								.get_value(main_data, llvm_builder)
								.build_compare(&main_data.int_type.const_int(0, false), Comparison::NotEqual, llvm_builder, "int_to_bool_temp");
							let then_case = operands[1].build_r_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?
								.get_value(main_data, llvm_builder);
							let else_case = operands[2].build_r_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?
								.get_value(main_data, llvm_builder);
							// Build the basic blocks for the then and else cases and an end basic block to jump to when they have been executed
							let then_basic_block = function_build_data.function.append_basic_block(&main_data.llvm_context, "ternary_then");
							let else_basic_block = function_build_data.function.append_basic_block(&main_data.llvm_context, "ternary_else");
							let end_basic_block = function_build_data.function.append_basic_block(&main_data.llvm_context, "ternary_end");
							// Build the alloca to write the ternary result to
							let result_alloca = function_build_data.get_alloca(main_data, llvm_builder, "non_short_circuit_result");
							function_build_data.block_stack.last_mut().unwrap().allocas_in_use.insert(result_alloca.clone());
							// Build the conditional branch to the then and else branches depending on the condition
							condition.build_conditional_branch(&then_basic_block, &else_basic_block, &main_data.llvm_context, llvm_builder);
							// Build then case
							llvm_builder.position_at_end(&then_basic_block);
							function_build_data.block_stack.last_mut().unwrap().basic_blocks.push(then_basic_block);
							result_alloca.build_store(&then_case, llvm_builder);
							llvm_builder.build_branch(&end_basic_block);
							// Build else case
							llvm_builder.position_at_end(&else_basic_block);
							function_build_data.block_stack.last_mut().unwrap().basic_blocks.push(else_basic_block);
							result_alloca.build_store(&else_case, llvm_builder);
							llvm_builder.build_branch(&end_basic_block);
							// Re-position builder at end
							llvm_builder.position_at_end(&end_basic_block);
							function_build_data.block_stack.last_mut().unwrap().basic_blocks.push(end_basic_block);
							// Read ternary result
							let result = result_alloca.build_load(main_data.int_type, llvm_builder, "read_ternary_result");
							function_build_data.surrender_alloca(result_alloca);
							BuiltRValue::Value(result)
						}
						Operation::ShortCircuitTernary => {
							// Build the condition to an i1
							let condition = operands[0].build_r_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?
								.get_value(main_data, llvm_builder)
								.build_compare(&main_data.int_type.const_int(0, false), Comparison::NotEqual, llvm_builder, "int_to_bool_temp");
							// Build the basic blocks for the then and else cases and an end basic block to jump to when they have been executed
							let then_basic_block = function_build_data.function.append_basic_block(&main_data.llvm_context, "ternary_then");
							let else_basic_block = function_build_data.function.append_basic_block(&main_data.llvm_context, "ternary_else");
							let end_basic_block = function_build_data.function.append_basic_block(&main_data.llvm_context, "ternary_end");
							// Build the alloca to write the ternary result to
							let result_alloca = function_build_data.get_alloca(main_data, llvm_builder, "short_circuit_result");
							function_build_data.block_stack.last_mut().unwrap().allocas_in_use.insert(result_alloca.clone());
							// Build the conditional branch to the then and else branches depending on the condition
							condition.build_conditional_branch(&then_basic_block, &else_basic_block, &main_data.llvm_context, llvm_builder);
							// Build then case
							llvm_builder.position_at_end(&then_basic_block);
							function_build_data.block_stack.last_mut().unwrap().basic_blocks.push(then_basic_block);
							let then_case = operands[1].build_r_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?
								.get_value(main_data, llvm_builder);
							result_alloca.build_store(&then_case, llvm_builder);
							llvm_builder.build_branch(&end_basic_block);
							// Build else case
							llvm_builder.position_at_end(&else_basic_block);
							function_build_data.block_stack.last_mut().unwrap().basic_blocks.push(else_basic_block);
							let else_case = operands[2].build_r_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?
								.get_value(main_data, llvm_builder);
							result_alloca.build_store(&else_case, llvm_builder);
							llvm_builder.build_branch(&end_basic_block);
							// Re-position builder at end
							llvm_builder.position_at_end(&end_basic_block);
							function_build_data.block_stack.last_mut().unwrap().basic_blocks.push(end_basic_block);
							// Read ternary result
							let result = result_alloca.build_load(main_data.int_type, llvm_builder, "read_ternary_result");
							function_build_data.surrender_alloca(result_alloca);
							BuiltRValue::Value(result)
						}
						Operation::IntegerNegate | Operation::Dereference | Operation::BitwiseNot => {
							let operand = operands[0].build_r_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?
								.get_value(main_data, llvm_builder);
							let result = match operation {
								Operation::IntegerNegate => operand.build_negate(llvm_builder, "neg_temp"),
								Operation::Dereference =>
									operand.build_int_to_ptr(llvm_builder, main_data.int_type.pointer_to(), "int_to_ptr_for_deref_temp")
										.build_load(main_data.int_type, llvm_builder, "load_for_deref_temp"),
								Operation::BitwiseNot => operand.build_bitwise_not(llvm_builder, "bnot_temp"),
								_ => unreachable!()
							};
							BuiltRValue::Value(result)
						}
						Operation::TakeReference | Operation::Read => {
							let value = operands[0].build_l_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?;
							match operation {
								Operation::TakeReference => BuiltRValue::Value(value
									.get_pointer(main_data, llvm_builder)
									.build_ptr_to_int(llvm_builder, main_data.int_type, "ptr_to_int_for_take_ref_temp")),
								Operation::Read => BuiltRValue::Value(value.get_value(main_data, llvm_builder)),
								_ => unreachable!(),
							}
						}
						_ => return Err((Error::FeatureNotYetImplemented("This operator".into()), *start)),
					}
					// TODO
					Operator::Augmented(..) => return Err((Error::FeatureNotYetImplemented("Augmented assignments".into()), self.start)),
					Operator::LValueAssignment => return Err((Error::FeatureNotYetImplemented("L-value assignments".into()), self.start)),
				}
			}
			// We built function definitions at the start of this function
			AstNodeVariant::FunctionDefinition(..) => unreachable!(),
			// For blocks, we build the sub-expressions
			AstNodeVariant::Block(block_expressions, is_result_undefined) => {
				// If we are in the global scope
				if *is_result_undefined && block_expressions.is_empty() {
					return Ok(BuiltRValue::Value(main_data.int_type.undefined()));
				}
				let function_build_data = match function_build_data {
					Some(function_build_data) => function_build_data,
					None => return Err((Error::FeatureNotYetImplemented("Blocks in global scope".into()), self.start)),
				};
				// Create the first inner basic block for the BCZ block, then branch from the current basic block to it, then re-position the builder to the new basic block
				let inner_basic_block = function_build_data.function.append_basic_block(&main_data.llvm_context, "block_start");
				llvm_builder.build_branch(&inner_basic_block);
				llvm_builder.position_at_end(&inner_basic_block);
				// Create a basic block to branch to after we are done with the BCZ block we are building
				function_build_data.block_stack.last_mut().unwrap().basic_blocks.push(function_build_data.function.append_basic_block(&main_data.llvm_context, "block_end"));
				// Push a new block level onto the block stack
				function_build_data.block_stack.push(BlockLevel {
					basic_blocks: vec![inner_basic_block],
					local_variables: HashMap::new(),
					allocas_in_use: HashSet::new(),
					array_allocas_in_use: HashMap::new(),
					is_loop: false,
				});
				// Build each expression
				let mut last_built_expression = None;
				for expression in block_expressions {
					last_built_expression = Some(expression.build_r_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?);
				}
				// Move all allocas to the unused alloca lists
				let top_block = function_build_data.block_stack.last().unwrap();
				for alloca in top_block.allocas_in_use.iter() {
					function_build_data.allocas_not_in_use.insert(alloca.clone());
				}
				for (array_type, arrays) in top_block.array_allocas_in_use.iter() {
					let to_append_to = match function_build_data.array_allocas_not_in_use.get_mut(array_type) {
						Some(to_append_to) => to_append_to,
						None => {
							function_build_data.array_allocas_not_in_use.insert(array_type.clone(), HashSet::new());
							function_build_data.array_allocas_not_in_use.get_mut(array_type).unwrap()
						}
					};
					for array in arrays {
						to_append_to.insert(array.clone());
					}
				}
				// Pop the block level we pushed
				function_build_data.block_stack.pop();
				// Branch to the basic block that was created before to branch to after the BCZ block was built and position the builder to it
				llvm_builder.build_branch(function_build_data.block_stack.last().unwrap().last_block());
				llvm_builder.position_at_end(function_build_data.block_stack.last().unwrap().last_block());
				// Return
				match (is_result_undefined, last_built_expression) {
					(true, _) | (false, None) => BuiltRValue::Value(main_data.int_type.undefined()),
					(false, Some(last_built_expression)) => last_built_expression,
				}
			}
			// For a function call, we build the expression that yeilds the function pointer and the ones that yeild the function arguments and then build the call.
			AstNodeVariant::FunctionCall(function_to_call, arguments) => {
				let function_build_data = match function_build_data {
					Some(function_build_data) => function_build_data,
					None => return Err((Error::FeatureNotYetImplemented("Global function calls".into()), self.start))
				};
				if arguments.len() > u16::MAX as usize {
					return Err((Error::TooManyFunctionArguments, self.start))
				}
				// Build function body and arguments
				let function_pointer_built = function_to_call
					.build_r_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?;
				let mut arguments_built = Vec::with_capacity(arguments.len());
				for argument in arguments {
					arguments_built.push(argument.build_r_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?.get_value(main_data, llvm_builder));
				}
				// Build types
				let argument_types: Box<[Type]> = repeat(main_data.int_type).take(arguments.len()).collect();
				let function_type = main_data.int_type.function_type(&*argument_types, false);
				let function_pointer_type = function_type.pointer_to();
				// Build function call
				let function_pointer = function_pointer_built
					.get_value(main_data, llvm_builder)
					.build_int_to_ptr(llvm_builder, function_pointer_type, "int_to_ptr_temp");
				let built_function_call = function_pointer
					.build_call(arguments_built.as_slice(), function_type, llvm_builder, "function_call_temp");
				BuiltRValue::Value(built_function_call)
			}
			// For a built in function, building depends on the function
			AstNodeVariant::Keyword(keyword, arguments, child) => {
				match keyword {
					Keyword::Write => {
						let function_build_data = match function_build_data {
							Some(function_build_data) => function_build_data,
							None => return Err((Error::GlobalOperatorNotConstEvaluated, self.start))
						};
						// Get arguments
						let (address_to_write_to, (write_type, is_signed), value_to_write) = match arguments.len() {
							2 => {
								(&arguments[0], (main_data.int_type, false), &arguments[1])
							}
							3 => (&arguments[0], *(&arguments[1].type_from_width(main_data)?), &arguments[2]),
							_ => return Err((Error::InvalidBuiltInFunctionArgumentCount, self.start)),
						};
						if write_type.is_void() {
							return Err((Error::VoidParameter, self.start))
						}
						let write_type_ptr = write_type.pointer_to();
						// Build arguments
						let address_to_write_to_built = address_to_write_to
							.build_r_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?
							.get_value(main_data, llvm_builder)
							.build_int_to_ptr(llvm_builder, write_type_ptr, "int_to_ptr_temp");
						let value_to_write_built = value_to_write
							.build_r_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?
							.get_value(main_data, llvm_builder);
						let value_to_write_built_cast = match main_data.int_bit_width
							.cmp(&(write_type.size_in_bits(&main_data.llvm_data_layout) as u8)) {
							Ordering::Greater => value_to_write_built.clone().build_truncate(llvm_builder, write_type, "truncate_temp"),
							Ordering::Equal => value_to_write_built.clone(),
							Ordering::Less => match is_signed {
								false => value_to_write_built.clone().build_zero_extend(llvm_builder, write_type, "zero_extend_temp"),
								true => value_to_write_built.clone().build_sign_extend(llvm_builder, write_type, "sign_extend_temp"),
							}
						};
						// Build write
						address_to_write_to_built.build_store(&value_to_write_built_cast, llvm_builder);
						// Expression yeilds the written value
						BuiltRValue::Value(value_to_write_built)
					}
					Keyword::Stack => {
						let function_build_data = match function_build_data {
							Some(function_build_data) => function_build_data,
							None => return Err((Error::GlobalOperatorNotConstEvaluated, self.start))
						};
						// Get arguments
						let (count, entry_width) = match arguments.len() {
							0 => (None, None),
							1 => (Some(&arguments[0]), None),
							2 => (Some(&arguments[0]), Some(&arguments[1])),
							_ => return Err((Error::InvalidBuiltInFunctionArgumentCount, self.start)),
						};
						// Get entry count
						let count = match count {
							Some(count) => match count.variant {
								AstNodeVariant::Constant(count) => count,
								_ => return Err((Error::ConstValueRequired, count.start)),
							}
							None => 1,
						};
						// Get entry type
						let entry_type = match entry_width {
							Some(entry_width) => {
								let entry_type = entry_width.type_from_width(main_data)?.0;
								if entry_type.is_void() {
									return Err((Error::VoidParameter, self.start));
								}
								entry_type
							}
							None => main_data.int_type,
						};
						// Get alloca
						BuiltRValue::Value(function_build_data.get_array_alloca(entry_type, count, llvm_builder, "stack"))
					}
					Keyword::EntryPoint | Keyword::Export => unreachable!(),
					Keyword::Link => {
						if function_build_data.is_some() {
							return Err((Error::FeatureNotYetImplemented("Link in function".into()), self.start));
						}
						if arguments.len() < 2 {
							return Err((Error::InvalidBuiltInFunctionArgumentCount, self.start));
						}
						if arguments.len() > u16::MAX as usize {
							return Err((Error::TooManyFunctionParameters, *start));
						}
						// Get wrapped function name
						let wrapped_function_name = &arguments[0];
						let wrapped_function_name: &str = match &wrapped_function_name.variant {
							AstNodeVariant::String(link_function_name) => &**link_function_name,
							AstNodeVariant::Identifier(link_function_name) => &**link_function_name,
							_ => return Err((Error::ConstValueRequired, wrapped_function_name.start)),
						};
						// Create wrapped function type
						let parameter_count = arguments.len() - 2;
						let mut wrapped_parameter_types = Vec::with_capacity(parameter_count);
						for parameter in &arguments[2..] {
							let parameter_type = parameter.type_from_width(main_data)?.0;
							if parameter_type.is_void() {
								return Err((Error::VoidParameter, self.start));
							}
							wrapped_parameter_types.push(parameter_type);
						}
						let (wrapped_function_return_type, wrapped_function_return_type_is_signed) = arguments[1].type_from_width(main_data)?;
						let wrapped_function_type = wrapped_function_return_type.function_type(&*wrapped_parameter_types, false);
						// Create wrapped function
						let wrapped_function = llvm_module.add_function(wrapped_function_type, &*wrapped_function_name);
						wrapped_function.set_linkage(Linkage::DLLImport);
						wrapped_function.set_calling_convention(CallingConvention::Win64);
						// Create wrapper function type
						let wrapper_function_parameter_types: Box<[Type]> = repeat_n(main_data.int_type, parameter_count).collect();
						let wrapper_function_type = main_data.int_type.function_type(&wrapper_function_parameter_types, false);
						// Create wrapper function
						let wrapper_function = llvm_module.add_function(wrapper_function_type, &format!("__link__{wrapped_function_name}"));
						wrapper_function.set_linkage(Linkage::Internal);
						// Build casts
						let basic_block = wrapper_function.append_basic_block(&main_data.llvm_context, "entry");
						llvm_builder.position_at_end(&basic_block);
						let mut arguments_converted = Vec::new();
						for (parameter_index, parameter) in (&arguments[2..]).iter().enumerate() {
							let (parameter_type, is_signed) = parameter.type_from_width(main_data)?;
							let argument = wrapper_function.get_parameter(parameter_index);
								let argument_converted = match main_data.int_bit_width
									.cmp(&(parameter_type.size_in_bits(&main_data.llvm_data_layout) as u8)) {
								Ordering::Less => match is_signed {
									false => argument.build_zero_extend(llvm_builder, parameter_type, "z_extend_temp"),
									true => argument.build_sign_extend(llvm_builder, parameter_type, "s_extend_temp"),
								}
								Ordering::Equal => argument,
								Ordering::Greater => argument.build_truncate(llvm_builder, parameter_type, "truncate_temp"),
							};
							arguments_converted.push(argument_converted);
						}
						let call_result = wrapped_function.build_call(arguments_converted.as_slice(), wrapped_function_type, llvm_builder, "wrapped_function_call_temp");
						// Build return
						if wrapped_function_return_type.is_void() {
							llvm_builder.build_return_void();
						}
						else {
							let call_result_converted = match main_data.int_bit_width
								.cmp(&(wrapped_function_return_type.size_in_bits(&main_data.llvm_data_layout) as u8)) {
								Ordering::Less => call_result.build_truncate(llvm_builder, main_data.int_type, "truncate_temp"),
								Ordering::Equal => call_result,
								Ordering::Greater => match wrapped_function_return_type_is_signed {
									false => call_result.build_zero_extend(llvm_builder, main_data.int_type, "zero_extend_temp"),
									true => call_result.build_sign_extend(llvm_builder, main_data.int_type, "sign_extend_temp"),
								}
							};
							call_result_converted.build_return(llvm_builder);
						}
						// Return wrapper function as int
						BuiltRValue::Value(wrapper_function.build_ptr_to_int(llvm_builder, main_data.int_type, "link_fn_to_int_temp"))
					}
					Keyword::Loop => {
						let function_build_data = match function_build_data {
							Some(function_build_data) => function_build_data,
							None => return Err((Error::GlobalOperatorNotConstEvaluated, self.start))
						};
						if !arguments.is_empty() {
							return Err((Error::FeatureNotYetImplemented("Loop arguments".into()), self.start));
						}
						// Get the alloca for the loop result
						let result_alloca = function_build_data.get_alloca(main_data, llvm_builder, "loop_result");
						// Create the first inner basic block for the BCZ block, then branch from the current basic block to it, then re-position the builder to the new basic block
						let inner_basic_block = &mut function_build_data.function.append_basic_block(&main_data.llvm_context, "loop_start");
						llvm_builder.build_branch(&inner_basic_block);
						llvm_builder.position_at_end(&inner_basic_block);
						// Create a basic block to branch to after we are done with the BCZ block we are building
						function_build_data.block_stack.last_mut().unwrap().basic_blocks.push(function_build_data.function.append_basic_block(&main_data.llvm_context, "loop_end"));
						// Push a new block level onto the block stack
						function_build_data.block_stack.push(BlockLevel {
							basic_blocks: vec![inner_basic_block.clone()],
							local_variables: HashMap::new(),
							allocas_in_use: HashSet::new(),
							array_allocas_in_use: HashMap::new(),
							is_loop: true,
						});
						// Build child expression
						child.as_ref().unwrap().build_r_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?;
						// Build branch from end of loop to start of loop
						llvm_builder.build_branch(&function_build_data.block_stack.last().unwrap().basic_blocks[0]);
						// Pop the scope we pushed
						function_build_data.block_stack.pop();
						// Branch to the basic block that was created before to branch to after the BCZ block was built and position the builder to it
						llvm_builder.position_at_end(function_build_data.block_stack.last().unwrap().last_block());
						// Return
						let result = result_alloca.build_load(main_data.int_type, llvm_builder, "loop_result_temp");
						function_build_data.surrender_alloca(result_alloca);
						BuiltRValue::Value(result)
					}
					Keyword::Break => {
						let function_build_data = match function_build_data {
							Some(function_build_data) => function_build_data,
							None => return Err((Error::FeatureNotYetImplemented("Blocks in global scope".into()), self.start)),
						};
						let mut last_was_loop = false;
						for block_level in function_build_data.block_stack.iter().rev() {
							if last_was_loop {
								llvm_builder.build_branch(&block_level.last_block());
								let unreachable_basic_block = function_build_data.function.append_basic_block(&main_data.llvm_context, "break_unreachable");
								llvm_builder.position_at_end(&unreachable_basic_block);
								function_build_data.block_stack.last_mut().unwrap().basic_blocks.push(unreachable_basic_block);
								return Ok(BuiltRValue::Value(main_data.int_type.undefined()));
							}
							last_was_loop = block_level.is_loop;
						}
						return Err((Error::NotUsedInsideLoop, self.start));
					}
					Keyword::Continue => {
						let function_build_data = match function_build_data {
							Some(function_build_data) => function_build_data,
							None => return Err((Error::FeatureNotYetImplemented("Blocks in global scope".into()), self.start)),
						};
						for block_level in function_build_data.block_stack.iter().rev() {
							if block_level.is_loop {
								llvm_builder.build_branch(&block_level.basic_blocks[0]);
								let unreachable_basic_block = function_build_data.function.append_basic_block(&main_data.llvm_context, "continue_unreachable");
								llvm_builder.position_at_end(&unreachable_basic_block);
								function_build_data.block_stack.last_mut().unwrap().basic_blocks.push(unreachable_basic_block);
								return Ok(BuiltRValue::Value(main_data.int_type.undefined()));
							}
						}
						return Err((Error::NotUsedInsideLoop, self.start));
					}
					Keyword::Import => {
						// Get arguments
						let (filepath, global_variable_name) = match arguments.len() {
							2 => (&arguments[0], &arguments[1]),
							_ => return Err((Error::InvalidBuiltInFunctionArgumentCount, self.start)),
						};
						// Get filepath
						let filepath = match &filepath.variant {
							AstNodeVariant::String(filepath) => &**filepath,
							AstNodeVariant::Identifier(filepath) => &**filepath,
							_ => return Err((Error::ConstValueRequired, filepath.start)),
						};
						let filepath_buff = relative_filepath_to_absolute(main_data, file_build_data.filepath, filepath)
							.map_err(|error| (error, *start))?;
						let mut hasher = DefaultHasher::new();
						filepath_buff.hash(&mut hasher);
						let hash = hasher.finish();
						// Get global variable name
						let global_variable_name = match &global_variable_name.variant {
							AstNodeVariant::String(global_variable_name) => &**global_variable_name,
							AstNodeVariant::Identifier(global_variable_name) => &**global_variable_name,
							_ => return Err((Error::ConstValueRequired, global_variable_name.start)),
						};
						let global = llvm_module.add_global(main_data.int_type, &format!("__export__{hash}__{global_variable_name}"));
						global.set_linkage(Linkage::External);
						global.set_is_constant(true);
						BuiltRValue::ImportedConstant(global)
					}
					Keyword::SystemConstant => unreachable!(),
				}
			}
			// Build strings
			AstNodeVariant::String(text) => {
				let string = llvm_module.add_global(main_data.int_8_type.array_type(text.len() + 1), "string");
				string.set_linkage(Linkage::Internal);
				string.set_is_constant(true);
				string.set_initializer(&main_data.llvm_context.const_string(text, true));
				BuiltRValue::Value(string.build_ptr_to_int(llvm_builder, main_data.int_type, "str_ptr_to_int"))
			}
		})
	}

	/// Build an l-value into LLVM IR code and return the built l-value.
	pub fn build_l_value<'a, 'b>(
		&'a self,
		main_data: &'a MainData<'a>,
		file_build_data: &mut FileBuildData<'a, 'a>,
		llvm_module: &'a Module,
		llvm_builder: &'a Builder<'a, 'a>,
		function_build_data: Option<&mut FunctionBuildData<'a, 'b>>,
	) -> Result<BuiltLValue<'a>, (Error, (NonZeroUsize, NonZeroUsize))> {
		// Unpack
		let Self {
			start: _,
			end: _,
			variant,
		} = self;
		// Action depends on variant
		Ok(match variant {
			// For an identifier, we create or return a local variable
			AstNodeVariant::Identifier(name) => {
				let function_build_data = match function_build_data {
					Some(function_build_data) => function_build_data,
					None => return Err((Error::GlobalOperatorNotConstEvaluated, self.start)),
				};
				// Get local variable if it exists
				for scope_level in function_build_data.block_stack.iter().rev() {
					if let Some(variable) = scope_level.local_variables.get(name) {
						return Ok(variable.clone());
					}
				}
				// Get alloca for variable
				let alloca = function_build_data.get_alloca(main_data, llvm_builder, name);
				// Insert variable into list
				function_build_data.block_stack.last_mut().unwrap().local_variables.insert(name.clone(), BuiltLValue::AllocaVariable(alloca.clone()));
				// Return variable
				BuiltLValue::AllocaVariable(alloca)
			}
			AstNodeVariant::Constant(..) => return Err((Error::InvalidLValue, self.start)),
			AstNodeVariant::String(..) => return Err((Error::InvalidLValue, self.start)),
			AstNodeVariant::FunctionCall(..) => return Err((Error::InvalidLValue, self.start)),
			AstNodeVariant::FunctionDefinition(..) => return Err((Error::InvalidLValue, self.start)),
			AstNodeVariant::Keyword(keyword, _arguments, _child) => {
				match keyword {
					Keyword::Link | Keyword::EntryPoint | Keyword::Import | Keyword::Export => return Err((Error::InvalidLValue, self.start)),
					Keyword::Write => return Err((Error::FeatureNotYetImplemented("L-value write".into()), self.start)),
					Keyword::Stack => return Err((Error::FeatureNotYetImplemented("L-value stack".into()), self.start)),
					Keyword::Loop => return Err((Error::FeatureNotYetImplemented("L-value loop".into()), self.start)),
					Keyword::Break => return Err((Error::FeatureNotYetImplemented("L-value break".into()), self.start)),
					Keyword::Continue => return Err((Error::FeatureNotYetImplemented("L-value continue".into()), self.start)),
					Keyword::SystemConstant => unreachable!(),
				}
			}
			AstNodeVariant::Block(..) => return Err((Error::FeatureNotYetImplemented("L-value blocks".into()), self.start)),
			AstNodeVariant::Operator(operator, operands) => match operator {
				Operator::Assignment => return Err((Error::FeatureNotYetImplemented("L-value assignments".into()), self.start)),
				Operator::Augmented(..) => return Err((Error::FeatureNotYetImplemented("L-value agumented assignments".into()), self.start)),
				Operator::LValueAssignment => return Err((Error::InvalidLValue, self.start)),
				Operator::Normal(operation) => match operation {
					Operation::Dereference => {
						let function_build_data = match function_build_data {
							Some(function_build_data) => function_build_data,
							None => return Err((Error::FeatureNotYetImplemented("Blocks in global scope".into()), self.start)),
						};
						let pointer =  operands[0]
							.build_r_value(main_data, file_build_data, llvm_module, llvm_builder, Some(function_build_data))?
							.get_value(main_data, llvm_builder)
							.build_int_to_ptr(llvm_builder, main_data.int_type, "int_to_ptr_for_deref");
						BuiltLValue::DereferencedPointer(pointer)
					}
					_ => return Err((Error::FeatureNotYetImplemented("L-value operator".into()), self.start)),
				}
			}
		})
	}

	/// Build a global variable into LLVM IR code.
	pub fn build_global_assignment<'a>(
		&'a self, main_data: &'a MainData, llvm_module: &'a Module<'a>, llvm_builder: &'a Builder<'a, 'a>, file_build_data: &mut FileBuildData<'a, 'a>, name: &str,
		is_exported: bool,
	) -> Result<BuiltRValue<'a>, (Error, (NonZeroUsize, NonZeroUsize))> {
		// Build r-value/function
		let r_value = if self.is_function() {
			let function =
				self.build_function_definition(main_data, file_build_data, llvm_module, llvm_builder, name, false/*, false*/)?;
			BuiltRValue::Value(function)
		}
		else {
			let r_value = self.build_r_value(main_data, file_build_data, llvm_module, llvm_builder, None)?;
			// Assign to global variable
			match &r_value {
				BuiltRValue::Value(value) => {
					let global = llvm_module.add_global(main_data.int_type, name);
					global.set_linkage(Linkage::Internal);
					global.set_is_constant(true);
					global.set_initializer(value);
				}
				BuiltRValue::ImportedConstant(..) => {}
			}
			r_value
		};
		if is_exported {
			let mut hasher = DefaultHasher::new();
			file_build_data.filepath.hash(&mut hasher);
			let hash = hasher.finish();
			let global = llvm_module.add_global(main_data.int_type, &format!("__export__{hash}__{name}"));
			match &r_value {
				BuiltRValue::Value(value) => {
					global.set_linkage(Linkage::External);
					global.set_is_constant(true);
					global.set_initializer(value);
				}
				BuiltRValue::ImportedConstant(..) => return Err((Error::FeatureNotYetImplemented("Re-exporting".into()), self.start))
			}
		}
		// Return
		Ok(r_value)
	}

	/// Returns if the expression can be built into a function.
	pub fn is_function(&self) -> bool {
		match &self.variant {
			AstNodeVariant::FunctionDefinition(..) => true,
			AstNodeVariant::Keyword(keyword, _arguments, child) => match keyword {
				Keyword::EntryPoint => child.as_ref().unwrap().is_function(),
				_ => false,
			}
			_ => false,
		}
	}

	/// Get a int/void type form a byte width.
	pub fn type_from_width<'a>(&'a self, main_data: &'a MainData) -> Result<(Type<'a>, bool), (Error, (NonZeroUsize, NonZeroUsize))> {
		let Self {
			start,
			end: _,
			variant,
		} = self;
		Ok(match variant {
			AstNodeVariant::Constant(value) => {
				let is_negative = (main_data.sign_bit_mask & *value) != 0;
				let byte_width = match is_negative {
					false => *value,
					true => (*value ^ main_data.int_max_value).wrapping_add(1),
				};
				(match byte_width {
					0 => main_data.llvm_context.void_type(),
					1 => main_data.llvm_context.int_8_type(),
					2 => main_data.llvm_context.int_16_type(),
					4 => main_data.llvm_context.int_32_type(),
					8 => main_data.llvm_context.int_64_type(),
					16 => main_data.llvm_context.int_128_type(),
					_ => return Err((Error::InvalidTypeWidth, *start)),
				}, is_negative)
			}
			_ => return Err((Error::ConstValueRequired, *start)),
		})
	}

	/// Const evaluate expressions that can be const evaluated.
	pub fn const_evaluate(
		&mut self,
		main_data: &mut MainData,
		const_evaluated_globals: &HashMap<Box<str>, (AstNode, bool, HashSet<Box<str>>)>,
		variable_dependencies: &mut HashSet<Box<str>>,
		local_variables: &mut Vec<HashMap<Box<str>, Option<u64>>>,
		is_link_function: bool,
		is_l_value: bool,
		is_standard_library: bool,
	) -> Result<(), (Error, (NonZeroUsize, NonZeroUsize))> {
		// Unpack
		let Self {
			start,
			end,
			variant,
		} = self;
		// Action depends on variant
		match variant {
			AstNodeVariant::Operator(operator, operands) => {
				// Const evaluate operands
				match operator {
					Operator::Assignment => {
						operands[0]
							.const_evaluate(main_data, const_evaluated_globals, variable_dependencies, local_variables, is_link_function, true, is_standard_library)?;
						operands[1]
							.const_evaluate(main_data, const_evaluated_globals, variable_dependencies, local_variables, is_link_function, false, is_standard_library)?;
					}
					Operator::Augmented(..) => return Err((Error::FeatureNotYetImplemented("Augmented assignments".into()), *start)),
					Operator::LValueAssignment => return Err((Error::FeatureNotYetImplemented("Augmented assignments".into()), *start)),
					Operator::Normal(operation) => match operation {
						Operation::BitwiseAnd | Operation::BitwiseOr | Operation::BitwiseXor | Operation::FloatAdd | Operation::FloatDivide |
						Operation::FloatMultiply | Operation::FloatSubtract | Operation::FloatNegate | Operation::FloatTruncatedModulo |
						Operation::IntegerAdd | Operation::IntegerMultiply | Operation::IntegerNegate | Operation::IntegerSubtract |
						Operation::LogicalNotShortCircuitAnd | Operation::LogicalNotShortCircuitOr | Operation::LogicalXor |
						Operation::LogicalShortCircuitAnd | Operation::LogicalShortCircuitOr |
						Operation::SignedDivide | Operation::SignedTruncatedModulo | Operation::UnsignedDivide | Operation::UnsignedModulo |
						Operation::Dereference | Operation::BitwiseNot | Operation::LogicalNot |
						Operation::IntegerEqualTo | Operation::IntegerNotEqualTo  | Operation::UnsignedLessThanOrEqualTo | Operation::SignedLessThanOrEqualTo |
						Operation::UnsignedGreaterThan | Operation::UnsignedGreaterThanOrEqualTo | Operation::UnsignedLessThan |
						Operation::SignedGreaterThan | Operation::SignedGreaterThanOrEqualTo | Operation::SignedLessThan |
						Operation::FloatEqualTo | Operation::FloatNotEqualTo  | Operation::FloatLessThanOrEqualTo |
						Operation::FloatGreaterThan | Operation::FloatGreaterThanOrEqualTo | Operation::FloatLessThan |
						Operation::ArithmeticRightBitShift | Operation::LogicalRightBitShift | Operation::LogicalLeftBitShift |
						Operation::UnsignedThreeWayCompare | Operation::SignedThreeWayCompare | Operation::FloatThreeWayCompare => {
							for operand in operands.iter_mut() {
								operand.const_evaluate(
									main_data, const_evaluated_globals, variable_dependencies, local_variables, is_link_function, false, is_standard_library
								)?;
							}
						}
						Operation::Read | Operation::TakeReference | Operation::SuffixIntegerIncrement | Operation::SuffixIntegerDecrement |
						Operation::PrefixIntegerIncrement | Operation::PrefixIntegerDecrement=> {
							for operand in operands.iter_mut() {
								operand.const_evaluate(
									main_data, const_evaluated_globals, variable_dependencies, local_variables, is_link_function, true, is_standard_library
								)?;
							}
						}
						Operation::ShortCircuitTernary | Operation::NotShortCircuitTernary => {
							operands[0].const_evaluate(main_data, const_evaluated_globals, variable_dependencies, local_variables, is_link_function, false, is_standard_library)?;
							operands[0].const_evaluate(main_data, const_evaluated_globals, variable_dependencies, local_variables, is_link_function, is_l_value, is_standard_library)?;
							operands[0].const_evaluate(main_data, const_evaluated_globals, variable_dependencies, local_variables, is_link_function, is_l_value, is_standard_library)?;
						}
					}
				}
				// Const evaluate self
				match operator {
					Operator::Normal(operation) => match operation {
						// If we have a unary operator or a constant
						Operation::IntegerNegate | Operation::BitwiseNot | Operation::LogicalNot
						=> if let AstNode { variant: AstNodeVariant::Constant(value), .. } = operands[0] {
							let new_value = match operation {
								Operation::IntegerNegate => ((value ^ main_data.int_max_value).wrapping_add(1)) & main_data.int_max_value,
								Operation::BitwiseNot | Operation::LogicalNot => value ^ main_data.int_max_value,
								_ => unreachable!(),
							};
							*self = AstNode { variant: AstNodeVariant::Constant(new_value), start: *start, end: *end };
						}
						// If we have a binary operator with constant operands
						Operation::UnsignedModulo | Operation::SignedTruncatedModulo | Operation::IntegerEqualTo | Operation::IntegerNotEqualTo |
						Operation::UnsignedLessThan | Operation::UnsignedLessThanOrEqualTo | Operation::UnsignedGreaterThan |
						Operation::UnsignedGreaterThanOrEqualTo | Operation::SignedLessThan | Operation::SignedLessThanOrEqualTo |
						Operation::SignedGreaterThan | Operation::SignedGreaterThanOrEqualTo => {
							if let (
								AstNode { variant: AstNodeVariant::Constant(left_value), .. },
								AstNode { variant: AstNodeVariant::Constant(right_value), .. }
							) = (&operands[0], &operands[1]) {
								let new_value = match operation {
									Operation::UnsignedModulo => left_value.checked_rem(*right_value)
										.ok_or_else(|| (Error::ModuloByZero, *start))? & main_data.int_max_value,
									Operation::SignedTruncatedModulo => {
										let left_value = main_data.value_to_signed(*left_value);
										let right_value = main_data.value_to_signed(*right_value);
										if right_value == 0 {
											return Err((Error::ModuloByZero, *start));
										}
										main_data.signed_to_value(left_value.wrapping_rem(right_value))
									}
									Operation::IntegerEqualTo => (left_value == right_value) as u64,
									Operation::IntegerNotEqualTo => (left_value != right_value) as u64,
									Operation::UnsignedLessThan => (left_value < right_value) as u64,
									Operation::UnsignedLessThanOrEqualTo => (left_value <= right_value) as u64,
									Operation::UnsignedGreaterThan => (left_value > right_value) as u64,
									Operation::UnsignedGreaterThanOrEqualTo => (left_value >= right_value) as u64,
									Operation::SignedLessThan => {
										let left_value = main_data.value_to_signed(*left_value);
										let right_value = main_data.value_to_signed(*right_value);
										(left_value < right_value) as u64
									}
									Operation::SignedLessThanOrEqualTo => {
										let left_value = main_data.value_to_signed(*left_value);
										let right_value = main_data.value_to_signed(*right_value);
										(left_value <= right_value) as u64
									}
									Operation::SignedGreaterThan => {
										let left_value = main_data.value_to_signed(*left_value);
										let right_value = main_data.value_to_signed(*right_value);
										(left_value > right_value) as u64
									}
									Operation::SignedGreaterThanOrEqualTo => {
										let left_value = main_data.value_to_signed(*left_value);
										let right_value = main_data.value_to_signed(*right_value);
										(left_value >= right_value) as u64
									}
									_ => unreachable!(),
								};
								*self = AstNode { variant: AstNodeVariant::Constant(new_value), start: *start, end: *end };
							}
						}
						// Make sure constant null pointers are not dereferenced
						Operation::Dereference => {
							if let AstNode { variant: AstNodeVariant::Constant(0), .. } = operands[0] {
								return Err((Error::NullPointerDereference, *start));
							}
						}
						Operation::Read => {
							if let AstNode { variant: AstNodeVariant::Identifier(name), .. } = &mut operands[0] {
								*self = AstNode { variant: AstNodeVariant::Identifier(take(name)), start: *start, end: *end };
								self.const_evaluate(main_data, const_evaluated_globals, variable_dependencies, local_variables, is_link_function, is_l_value, is_standard_library)?;
							}
						}
						Operation::NotShortCircuitTernary => {
							if let AstNode { variant: AstNodeVariant::Constant(left_value), .. } = operands[0] {
								if left_value != 0 {
									*self = AstNode { variant: operands[1].variant.clone(), start: *start, end: *end };
								}
								else {
									*self = AstNode { variant: operands[2].variant.clone(), start: *start, end: *end };
								}
							}
						}
						Operation::ShortCircuitTernary => {
							if let AstNode { variant: AstNodeVariant::Constant(left_value), .. } = operands[0] {
								if left_value != 0 {
									if let AstNode { variant: AstNodeVariant::Constant(_), .. } = operands[2] {
										*self = AstNode { variant: operands[1].variant.clone(), start: *start, end: *end };
									}
								}
								else {
									if let AstNode { variant: AstNodeVariant::Constant(_), .. } = operands[1] {
										*self = AstNode { variant: operands[2].variant.clone(), start: *start, end: *end };
									}
								}
							}
						}
						// x & MAX = x
						Operation::BitwiseAnd => {
							if let AstNode { variant: AstNodeVariant::Constant(left_value), .. } = operands[0] {
								if left_value == main_data.int_max_value {
									*self = AstNode { variant: operands[1].variant.clone(), start: *start, end: *end };
								}
								else if let AstNode { variant: AstNodeVariant::Constant(right_value), .. } = operands[1] {
									let new_value = left_value & right_value & main_data.int_max_value;
									*self = AstNode { variant: AstNodeVariant::Constant(new_value), start: *start, end: *end };
								}
							}
							else if let AstNode { variant: AstNodeVariant::Constant(right_value), .. } = operands[1] {
								if right_value == main_data.int_max_value {
									*self = AstNode { variant: operands[0].variant.clone(), start: *start, end: *end };
								}
							}
						}
						// x | 0 = x
						Operation::BitwiseOr => {
							if let AstNode { variant: AstNodeVariant::Constant(left_value), .. } = operands[0] {
								if left_value == 0 {
									*self = AstNode { variant: operands[1].variant.clone(), start: *start, end: *end };
								}
								else if let AstNode { variant: AstNodeVariant::Constant(right_value), .. } = operands[1] {
									let new_value = (left_value | right_value) & main_data.int_max_value;
									*self = AstNode { variant: AstNodeVariant::Constant(new_value), start: *start, end: *end };
								}
							}
							else if let AstNode { variant: AstNodeVariant::Constant(right_value), .. } = operands[1] {
								if right_value == 0 {
									*self = AstNode { variant: operands[0].variant.clone(), start: *start, end: *end };
								}
							}
						}
						// x ^ 0 = x
						// x ^ MAX = !x
						Operation::BitwiseXor => {
							if let AstNode { variant: AstNodeVariant::Constant(left_value), .. } = operands[0] {
								if left_value == 0 {
									*self = AstNode { variant: operands[1].variant.clone(), start: *start, end: *end };
								}
								else if left_value == main_data.int_max_value {
									if let AstNodeVariant::Operator(operator, operands) = &mut self.variant {
										*operator = Operator::Normal(Operation::BitwiseNot);
										*operands = Box::new([operands[1].clone()]);
										self.const_evaluate(
											main_data, const_evaluated_globals, variable_dependencies, local_variables, is_link_function, is_l_value, is_standard_library
										)?;
									}
								}
								else if let AstNode { variant: AstNodeVariant::Constant(right_value), .. } = operands[1] {
									let new_value = (left_value ^ right_value) & main_data.int_max_value;
									*self = AstNode { variant: AstNodeVariant::Constant(new_value), start: *start, end: *end };
								}
							}
							else if let AstNode { variant: AstNodeVariant::Constant(right_value), .. } = operands[1] {
								if right_value == 0 {
									*self = AstNode { variant: operands[0].variant.clone(), start: *start, end: *end };
								}
								else if right_value == main_data.int_max_value {
									if let AstNodeVariant::Operator(operator, operands) = &mut self.variant {
										*operator = Operator::Normal(Operation::BitwiseNot);
										*operands = Box::new([operands[0].clone()]);
										self.const_evaluate(
											main_data, const_evaluated_globals, variable_dependencies, local_variables, is_link_function, is_l_value, is_standard_library
										)?;
									}
								}
							}
						}
						// x + 0 = x
						Operation::IntegerAdd => {
							if let AstNode { variant: AstNodeVariant::Constant(left_value), .. } = operands[0] {
								if left_value == 0 {
									*self = AstNode { variant: operands[1].variant.clone(), start: *start, end: *end };
								}
								else if let AstNode { variant: AstNodeVariant::Constant(right_value), .. } = operands[1] {
									let new_value = left_value.wrapping_add(right_value) & main_data.int_max_value;
									*self = AstNode { variant: AstNodeVariant::Constant(new_value), start: *start, end: *end };
								}
							}
							else if let AstNode { variant: AstNodeVariant::Constant(right_value), .. } = operands[1] {
								if right_value == 0 {
									*self = AstNode { variant: operands[0].variant.clone(), start: *start, end: *end };
								}
							}
						}
						// x - 0 = x
						// 0 - x = -x
						Operation::IntegerSubtract => {
							if let AstNode { variant: AstNodeVariant::Constant(left_value), .. } = operands[0] {
								if left_value == 0 {
									if let AstNodeVariant::Operator(operator, operands) = &mut self.variant {
										*operator = Operator::Normal(Operation::IntegerNegate);
										*operands = Box::new([operands[1].clone()]);
										self.const_evaluate(
											main_data, const_evaluated_globals, variable_dependencies, local_variables, is_link_function, is_l_value, is_standard_library
										)?;
									}
								}
								else if let AstNode { variant: AstNodeVariant::Constant(right_value), .. } = operands[1] {
									let new_value = left_value.wrapping_sub(right_value) & main_data.int_max_value;
									*self = AstNode { variant: AstNodeVariant::Constant(new_value), start: *start, end: *end };
								}
							}
							else if let AstNode { variant: AstNodeVariant::Constant(right_value), .. } = operands[1] {
								if right_value == 0 {
									*self = AstNode { variant: operands[0].variant.clone(), start: *start, end: *end };
								}
							}
						}
						// x * 1 = x
						Operation::IntegerMultiply => {
							if let AstNode { variant: AstNodeVariant::Constant(left_value), .. } = operands[0] {
								if left_value == 1 {
									*self = AstNode { variant: operands[1].variant.clone(), start: *start, end: *end };
								}
								else if let AstNode { variant: AstNodeVariant::Constant(right_value), .. } = operands[1] {
									let new_value = left_value.wrapping_mul(right_value) & main_data.int_max_value;
									*self = AstNode { variant: AstNodeVariant::Constant(new_value), start: *start, end: *end };
								}
							}
							else if let AstNode { variant: AstNodeVariant::Constant(right_value), .. } = operands[1] {
								if right_value == 1 {
									*self = AstNode { variant: operands[0].variant.clone(), start: *start, end: *end };
								}
							}
						}
						// x / 1 = x
						// x / 0 = Error
						Operation::UnsignedDivide => {
							if let AstNode { variant: AstNodeVariant::Constant(right_value), .. } = operands[0] {
								if right_value == 1 {
									*self = AstNode { variant: operands[0].variant.clone(), start: *start, end: *end };
								}
								else if right_value == 0 {
									return Err((Error::DivisionByZero, *start));
								}
								else if let AstNode { variant: AstNodeVariant::Constant(left_value), .. } = operands[1] {
									let new_value = left_value.wrapping_div(right_value) & main_data.int_max_value;
									*self = AstNode { variant: AstNodeVariant::Constant(new_value), start: *start, end: *end };
								}
							}
						}
						// x / 1 = x
						// x / 0 = Error
						Operation::SignedDivide => {
							if let AstNode { variant: AstNodeVariant::Constant(right_value), .. } = operands[0] {
								if right_value == 1 {
									*self = AstNode { variant: operands[0].variant.clone(), start: *start, end: *end };
								}
								else if right_value == 0 {
									return Err((Error::DivisionByZero, *start));
								}
								else if let AstNode { variant: AstNodeVariant::Constant(left_value), .. } = operands[1] {
									let left_value = main_data.value_to_signed(left_value);
									let right_value = main_data.value_to_signed(right_value);
									let new_value = main_data.signed_to_value(left_value.wrapping_div(right_value)) & main_data.int_max_value;
									*self = AstNode { variant: AstNodeVariant::Constant(new_value), start: *start, end: *end };
								}
							}
						}
						// true & x = x
						// false & x = false
						Operation::LogicalShortCircuitAnd => {
							if let AstNode { variant: AstNodeVariant::Constant(left_value), .. } = operands[0] {
								if left_value != 0 {
									*self = AstNode { variant: operands[1].variant.clone(), start: *start, end: *end };
								}
								else {
									*self = AstNode { variant: operands[0].variant.clone(), start: *start, end: *end };
								}
							}
							else if let AstNode { variant: AstNodeVariant::Constant(right_value), .. } = operands[1] {
								if right_value != 0 {
									*self = AstNode { variant: operands[0].variant.clone(), start: *start, end: *end };
								}
							}
						}
						// false | x = x
						// true | x = true
						Operation::LogicalShortCircuitOr => {
							if let AstNode { variant: AstNodeVariant::Constant(left_value), .. } = operands[0] {
								if left_value == 0 {
									*self = AstNode { variant: operands[1].variant.clone(), start: *start, end: *end };
								}
								else {
									*self = AstNode { variant: operands[0].variant.clone(), start: *start, end: *end };
								}
							}
							else if let AstNode { variant: AstNodeVariant::Constant(right_value), .. } = operands[1] {
								if right_value == 0 {
									*self = AstNode { variant: operands[0].variant.clone(), start: *start, end: *end };
								}
							}
						}
						// true & x = x
						Operation::LogicalNotShortCircuitAnd => {
							if let AstNode { variant: AstNodeVariant::Constant(left_value), .. } = operands[0] {
								if left_value != 0 {
									*self = AstNode { variant: operands[1].variant.clone(), start: *start, end: *end };
								}
							}
							else if let AstNode { variant: AstNodeVariant::Constant(right_value), .. } = operands[1] {
								if right_value != 0 {
									*self = AstNode { variant: operands[0].variant.clone(), start: *start, end: *end };
								}
							}
						}
						// false | x = x
						Operation::LogicalNotShortCircuitOr => {
							if let AstNode { variant: AstNodeVariant::Constant(left_value), .. } = operands[0] {
								if left_value == 0 {
									*self = AstNode { variant: operands[1].variant.clone(), start: *start, end: *end };
								}
							}
							else if let AstNode { variant: AstNodeVariant::Constant(right_value), .. } = operands[1] {
								if right_value == 0 {
									*self = AstNode { variant: operands[0].variant.clone(), start: *start, end: *end };
								}
							}
						}
						// false ^ x = x
						// true ^ x = !x
						Operation::LogicalXor => {
							if let AstNode { variant: AstNodeVariant::Constant(left_value), .. } = operands[0] {
								if left_value == 0 {
									*self = AstNode { variant: operands[1].variant.clone(), start: *start, end: *end };
								}
								else {
									match &mut self.variant {
										AstNodeVariant::Operator(operator, operands) => {
											*operator = Operator::Normal(Operation::LogicalNot);
											*operands = Box::new([operands[1].clone()]);
										}
										_ => unreachable!(),
									}
									self.const_evaluate(main_data, const_evaluated_globals, variable_dependencies, local_variables, is_link_function, is_l_value, is_standard_library)?;
								}
							}
							else if let AstNode { variant: AstNodeVariant::Constant(right_value), .. } = operands[1] {
								if right_value == 0 {
									*self = AstNode { variant: operands[0].variant.clone(), start: *start, end: *end };
								}
								else {
									match &mut self.variant {
										AstNodeVariant::Operator(operator, operands) => {
											*operator = Operator::Normal(Operation::LogicalNot);
											*operands = Box::new([operands[0].clone()]);
										}
										_ => unreachable!(),
									}
									self.const_evaluate(main_data, const_evaluated_globals, variable_dependencies, local_variables, is_link_function, is_l_value, is_standard_library)?;
								}
								
							}
						}
						// TODO
						_ => {}
					}
					Operator::Assignment => if let (AstNodeVariant::Identifier(name), AstNodeVariant::Constant(value)) =
						(&operands[0].variant, &operands[1].variant) {
						for local_variable_level in local_variables.iter_mut().rev() {
							if let Some(variable) = local_variable_level.get_mut(name) {
								*variable = Some(*value);
								return Ok(());
							}
						}
					}
					// TODO
					_ => {}
				}
			}
			AstNodeVariant::FunctionDefinition(parameters, body) => {
				let mut inner_local_variables = vec![HashMap::new()];
				if !is_link_function {
					for parameter in parameters.iter() {
						let name = match &parameter.variant {
							AstNodeVariant::Identifier(name) => name,
							_ => return Err((Error::ExpectedIdentifier, parameter.start)),
						}.clone();
						inner_local_variables[0].insert(name, None);
					}
				}
				body.const_evaluate(
					main_data, const_evaluated_globals, variable_dependencies, &mut inner_local_variables, false, false, is_standard_library
				)?;
				if is_link_function {
					for parameter in parameters {
						parameter.const_evaluate(
							main_data, const_evaluated_globals, variable_dependencies, local_variables, false, false, is_standard_library
						)?;
					}
				}
			}
			AstNodeVariant::Block(sub_expressions, ..) => {
				local_variables.push(HashMap::new());
				if is_l_value {
					return Err((Error::FeatureNotYetImplemented("L-value blocks".into()), *start));
				}
				for sub_expression in sub_expressions {
					sub_expression
						.const_evaluate(main_data, const_evaluated_globals, variable_dependencies, local_variables, false, false, is_standard_library)?;
				}
				local_variables.pop();
			}
			AstNodeVariant::Constant(..) => {}
			AstNodeVariant::FunctionCall(function_pointer, arguments) => {
				function_pointer
					.const_evaluate(main_data, const_evaluated_globals, variable_dependencies, local_variables, false, false, is_standard_library)?;
				for argument in arguments {
					argument
						.const_evaluate(main_data, const_evaluated_globals, variable_dependencies, local_variables, false, false, is_standard_library)?;
				}
			}
			AstNodeVariant::Keyword(keyword, arguments, child) => {
				match keyword {
					Keyword::Write | Keyword::Stack | Keyword::Loop | Keyword::Import | Keyword::Link => {
						for argument in arguments {
							argument.const_evaluate(
								main_data, const_evaluated_globals, variable_dependencies, local_variables, false, false, is_standard_library
							)?;
						}
					}
					Keyword::EntryPoint => child.as_mut().unwrap()
						.const_evaluate(main_data, const_evaluated_globals, variable_dependencies, local_variables, is_link_function, is_l_value, is_standard_library)?,
					Keyword::Break | Keyword::Continue => if !arguments.is_empty() {
						return Err((Error::FeatureNotYetImplemented("Arguments for @break and @continue".into()), *start));
					}
					Keyword::Export => unreachable!(),
					Keyword::SystemConstant => {
						if !is_standard_library {
							return Err((Error::OnlyUsableInStandardLibrary, *start));
						}
						match child {
							Some(child) => return Err((Error::ShouldNotHaveChild, child.start)),
							None => {}
						}
						for argument in arguments.iter_mut() {
							argument.const_evaluate(
								main_data, const_evaluated_globals, variable_dependencies, local_variables, false, false, is_standard_library
							)?;
						}
						if arguments.len() != 1 {
							return Err((Error::InvalidBuiltInFunctionArgumentCount, *start));
						}
						let constant_id = match arguments[0] {
							AstNode { variant: AstNodeVariant::Constant(value), .. } => value,
							_ => return Err((Error::ConstValueRequired, arguments[0].start)),
						};
						let constant_value = match constant_id {
							0 => (main_data.int_bit_width / 8) as u64, // T_WORD
							_ => return Err((Error::InvalidSystemConstant, arguments[0].start)),
						};
						self.variant = AstNodeVariant::Constant(constant_value);
					}
				}
			}
			AstNodeVariant::String(..) => {}
			AstNodeVariant::Identifier(name) => 'a: {
				if is_l_value {
					for local_variable_level in local_variables.iter_mut().rev() {
						if local_variable_level.contains_key(name) {
							break 'a;
						}
					}
					let top_local_variable_level = local_variables.last_mut().unwrap();
					top_local_variable_level.insert(name.clone(), None);
				}
				else {
					for local_variable_level in local_variables.iter_mut().rev() {
						if let Some(value) = local_variable_level.get_mut(name) {
							if let Some(value) = value {
								self.variant = AstNodeVariant::Constant(*value);
							}
							return Ok(());
						}
					}
					if let Some(value) = const_evaluated_globals.get(name) {
						if let AstNodeVariant::Constant(value) = value.0.variant {
							self.variant = AstNodeVariant::Constant(value);
							return Ok(());
						}
					}
				}
			}
		}
		Ok(())
	}
}

/// Get a local or global variable.
fn get_variable_by_name<'a, 'b>(
	main_data: &MainData<'a>,
	file_build_data: &mut FileBuildData<'a, 'a>,
	llvm_builder: &Builder<'a, 'a>,
	function_build_data: Option<&mut FunctionBuildData<'a, 'b>>,
	name: &str
) -> BuiltRValue<'a> {
	if let Some(function_build_data) = function_build_data {
		for scope_level in function_build_data.block_stack.iter().rev() {
			if let Some(variable) = scope_level.local_variables.get(name) {
				return BuiltRValue::Value(variable.get_value(main_data, llvm_builder));
			}
		}
	}
	if let Some(built_global) = file_build_data.built_globals.get(name) {
		return built_global.clone();
	}
	BuiltRValue::Value(file_build_data.built_global_function_signatures[name].build_ptr_to_int(llvm_builder, main_data.int_type, "fn_ptr_to_int_temp"))
}