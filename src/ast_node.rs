use std::{collections::{HashMap, HashSet}, ffi::{c_uint, c_ulonglong}, iter::{repeat, once}, mem::swap};

use strum_macros::EnumDiscriminants;

use crate::{built_value::{BuiltLValue, BuiltRValue}, error::Error, file_build_data::FileBuildData, llvm_c::{LLVMAddFunction, LLVMAddGlobal, LLVMAppendBasicBlockInContext, LLVMBasicBlockRef, LLVMBool, LLVMBuildAdd, LLVMBuildAlloca, LLVMBuildCall2, LLVMBuildIntToPtr, LLVMBuildMul, LLVMBuildNeg, LLVMBuildRet, LLVMBuildSDiv, LLVMBuildSRem, LLVMBuildStore, LLVMBuildSub, LLVMBuildUDiv, LLVMBuildURem, LLVMConstInt, LLVMFunctionType, LLVMGetParam, LLVMGetUndef, LLVMPointerType, LLVMPositionBuilderAtEnd, LLVMSetInitializer, LLVMTypeRef}, MainData};

#[derive(Debug)]
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
}

#[derive(Debug)]
pub enum Operator {
	Assignment,
	Normal(Operation),
	Augmented(Operation),
	LValueAssignment,
}

#[derive(Debug)]
pub enum Metadata {
	EntryPoint,
	Link,
}

#[derive(Debug, EnumDiscriminants)]
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
	/// A list of parameters for a function definition and the function body.
	FunctionDefinition(Box<[AstNode]>, Box<AstNode>),
	/// A string literal.
	String(Box<str>),
	/// Metadata about a child node.
	Metadata(Metadata, Box<AstNode>),
}

#[derive(Debug)]
pub struct AstNode {
	pub variant: AstNodeVariant,
	/// The line and column that this node starts at.
	pub start: (usize, usize),
	/// The line and column of the char after the last char of this node.
	pub end: (usize, usize),
}

impl AstNode {
	pub fn print_tree(&self, level: usize) {
		for _ in 0..level {
			print!("-");
		}
		print!("{} {}:{} to {}:{} {:?}", '{', self.start.0, self.start.1, self.end.0, self.end.1, AstNodeVariantDiscriminants::from(&self.variant));
		match &self.variant {
			AstNodeVariant::Block(_, result_is_undefined) => print!(", result_is_undefined: {:?}", result_is_undefined),
			AstNodeVariant::Constant(value) => print!(", value: {}", value),
			AstNodeVariant::FunctionCall(_, _) => {},
			AstNodeVariant::FunctionDefinition(_, _) => {},
			AstNodeVariant::Identifier(name) => print!(", name: {name}"),
			AstNodeVariant::String(string_value) => print!(", string_value: {string_value:?}"),
			AstNodeVariant::Operator(operator, _) => print!(", operator: {:?}", operator),
			AstNodeVariant::Metadata(metadata, _) => print!(", metadata: {:?}", metadata),
		}
		println!(" {}", '}');
		match &self.variant {
			AstNodeVariant::Block(nodes, _) => for node in nodes {
				node.print_tree(level + 1);
			}
			AstNodeVariant::FunctionCall(function, arguments) => {
				function.print_tree(level + 1);
				for argument in arguments {
					argument.print_tree(level + 1);
				}
			},
			AstNodeVariant::FunctionDefinition(parameters, body) => {
				for parameter in parameters {
					parameter.print_tree(level + 1);
				}
				body.print_tree(level + 1);
			},
			AstNodeVariant::Operator(_, operands) => for operand in operands {
				operand.print_tree(level + 1);
			}
			AstNodeVariant::Metadata(_, child) => child.print_tree(level + 1),
			AstNodeVariant::Constant(..) => {}
			AstNodeVariant::Identifier(..) => {}
			AstNodeVariant::String(..) => {}
		}
	}

	/// Removes global assignments nodes and puts them into a `(name, node)` hash map, replacing them with an identifier node.
	pub fn separate_globals(&mut self, global_list: &mut HashMap<Box<str>, Self>, will_be_discarded: bool) -> Result<(), (Error, (usize, usize))> {
		let start = self.start;
		match &mut self.variant {
			AstNodeVariant::Operator(operator, operands) => match operator {
				Operator::Assignment => {
					// Separate operands
					let mut identifier_node = AstNode { start: (0, 0), end: (0, 0), variant: AstNodeVariant::Constant(0) };
					let mut operand_node = AstNode { start: (0, 0), end: (0, 0), variant: AstNodeVariant::Constant(0) };
					swap(&mut operands[0], &mut identifier_node);
					swap(&mut operands[1], &mut operand_node);
					operand_node.separate_globals(global_list, false)?;
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
					match global_list.insert(name, operand_node) {
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
					operand.separate_globals(global_list, will_be_discarded)?;
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
					return Ok(());
				}
				if children.len() != 1 || (*is_result_undefined && children.len() != 0) {
					return Err((Error::FeatureNotYetImplemented("global blocks".into()), start));
				}
				let mut child = AstNode { start: (0, 0), end: (0, 0), variant: AstNodeVariant::Constant(0) };
				swap(&mut children[0], &mut child);
				child.separate_globals(global_list, will_be_discarded)?;
				*self = child;
			}
			AstNodeVariant::FunctionDefinition(..) => {}
			AstNodeVariant::Identifier(..) => {}
			AstNodeVariant::Metadata(_, child) => child.separate_globals(global_list, will_be_discarded)?,
			AstNodeVariant::String(..) => {}
		}
		Ok(())
	}

	/// Will search a global node and its children for global variable dependencies that need to be compiled before this node is.
	pub fn get_variable_dependencies(
		&self, variable_dependencies: &mut HashSet<Box<str>>,
		import_dependencies: &mut HashSet<Box<str>>,
		local_variables: &mut HashSet<Box<str>>,
		is_l_value: bool
	) -> Result<(), (Error, (usize, usize))> {
		let AstNode {
			variant,
			start,
			end: _,
		} = self;
		match variant {
			AstNodeVariant::Block(sub_expressions, _) => for expression in sub_expressions {
				match is_l_value {
					false => expression.get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, false)?,
					true => return Err((Error::FeatureNotYetImplemented("l-value blocks".into()), *start)),
				};
			}
			AstNodeVariant::Constant(..) => {}
			AstNodeVariant::FunctionCall(function, arguments) => {
				if is_l_value {
					return Err((Error::LValueFunctionCall, *start));
				}
				function.get_variable_dependencies(variable_dependencies, import_dependencies, &mut local_variables.clone(), false)?;
				for argument in arguments {
					argument.get_variable_dependencies(variable_dependencies, import_dependencies, &mut local_variables.clone(), false)?;
				}
			}
			AstNodeVariant::FunctionDefinition(parameters, body) => {
				if is_l_value {
					return Err((Error::LValueFunctionDefinition, *start));
				}
				for parameter in parameters {
					match &parameter.variant {
						AstNodeVariant::Identifier(name) => local_variables.insert(name.clone()),
						_ => return Err((Error::ExpectedIdentifier, *start)),
					};
				}
				body.get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, false)?;
			}
			AstNodeVariant::Identifier(name) => match is_l_value {
				false => if !local_variables.contains(name) {
					variable_dependencies.insert(name.clone());
				}
				true => {
					local_variables.insert(name.clone());
				}
			}
			AstNodeVariant::Metadata(_, child) => child.get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, is_l_value)?,
			AstNodeVariant::Operator(operator, operands) => match operator {
				Operator::Assignment => {
					operands[0].get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, true)?;
					operands[1].get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, false)?;
				}
				Operator::Augmented(operation) => match operation {
					Operation::IntegerAdd | Operation::IntegerSubtract | Operation::IntegerMultiply | Operation::SignedDivide | Operation::SignedTruncatedModulo |
					Operation::UnsignedDivide | Operation::UnsignedModulo |
					Operation::FloatAdd | Operation::FloatSubtract | Operation::FloatMultiply | Operation::FloatDivide | Operation::FloatTruncatedModulo => {
						operands[0].get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, true)?;
						operands[1].get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, false)?;
					}
					Operation::Dereference | Operation::IntegerNegate | Operation::FloatNegate | Operation::Read => return Err((Error::FeatureNotYetImplemented("augmented unary operators".into()), *start)),
				}
				Operator::Normal(operation) => match operation {
					// Operators that only have r-values as operands
					Operation::IntegerAdd | Operation::IntegerSubtract | Operation::IntegerMultiply | Operation::SignedDivide | Operation::SignedTruncatedModulo |
					Operation::UnsignedDivide | Operation::UnsignedModulo |
					Operation::FloatAdd | Operation::FloatSubtract | Operation::FloatMultiply | Operation::FloatDivide | Operation::FloatTruncatedModulo |
					Operation::Dereference | Operation::IntegerNegate | Operation::FloatNegate => for operand in operands {
						operand.get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, false)?;
					}
					// Operators that only have l-values as operands
					Operation::Read => for operand in operands {
						operand.get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, true)?;
					}
				}
				Operator::LValueAssignment => for operand in operands {
					operand.get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, true)?;
				}
			}
			AstNodeVariant::String(..) => {}
		}
		Ok(())
	}

	fn build_function_definition(&self, main_data: &mut MainData, file_build_data: &mut FileBuildData, name: &str, is_link_function: bool) -> Result<BuiltRValue, (Error, (usize, usize))> {
		// Unpack function definition node
		let Self {
			start,
			end: _,
			variant,
		} = self;
		let (parameters, function_body) = match variant {
			AstNodeVariant::FunctionDefinition(function_parameters, function_body) => (function_parameters, function_body),
			AstNodeVariant::Metadata(metadata, child) => match metadata {
				Metadata::EntryPoint => {
					let child_built = child.build_function_definition(main_data, file_build_data, name, is_link_function)?;
					if file_build_data.entrypoint.is_some() {
						return Err((Error::MultipleEntryPoints, *start));
					}
					file_build_data.entrypoint = Some(child_built.clone());
					return Ok(child_built);
				}
				Metadata::Link => return child.build_function_definition(main_data, file_build_data, name, true),
			}
			_ => unreachable!(),
		};
		// Create function parameter type
		if parameters.len() > u16::MAX as usize {
			return Err((Error::TooManyFunctionParameters, *start));
		}
		// TODO: Link functions
		let parameter_types: Box<[LLVMTypeRef]> = repeat(main_data.int_type).take(parameters.len()).collect();
		let function_type = unsafe { LLVMFunctionType(main_data.int_type, parameter_types.as_ptr(), parameter_types.len() as c_uint, false as LLVMBool) };
		// Build function value
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		let function = unsafe { LLVMAddFunction(file_build_data.llvm_module, name.as_ptr(), function_type) };
		// Build function body
		let basic_block = unsafe { LLVMAppendBasicBlockInContext(main_data.llvm_context, function, c"entry".as_ptr() as *const u8) };
		unsafe { LLVMPositionBuilderAtEnd(file_build_data.llvm_builder, basic_block) };
		// Create local scope with the scope that contains the function parameters
		let mut function_parameter_variables = HashMap::new();
		for (parameter_index, parameter) in parameters.iter().enumerate() {
			// Get parameter name
			let parameter_name = match &parameter.variant {
				AstNodeVariant::Identifier(name) => name,
				_ => return Err((Error::ExpectedIdentifier, parameter.start)),
			};
			// Add parameter to local scope
			let parameter_value = unsafe { LLVMGetParam(function, parameter_index as c_uint) };
			let parameter_name_c: Box<[u8]> = parameter_name.bytes().chain(once(0)).collect();
			let parameter_variable = unsafe { LLVMBuildAlloca(file_build_data.llvm_builder, main_data.int_type, parameter_name_c.as_ptr()) };
			unsafe { LLVMBuildStore(file_build_data.llvm_builder, parameter_value, parameter_variable) };
			function_parameter_variables.insert(parameter_name.clone(), BuiltLValue::AllocaVariable(parameter_variable));
		}
		let mut inner_local_variables = vec![function_parameter_variables];
		// Build function body
		let function_body_built = function_body.build_r_value(main_data, file_build_data, &mut inner_local_variables, Some(basic_block))?;
		unsafe { LLVMBuildRet(file_build_data.llvm_builder, function_body_built.get_value(main_data, file_build_data.llvm_builder)) };
		// Return
		Ok(BuiltRValue::Function(function))
	}

	pub fn build_r_value(&self, main_data: &mut MainData, file_build_data: &mut FileBuildData, local_variables: &mut Vec<HashMap<Box<str>, BuiltLValue>>, basic_block: Option<LLVMBasicBlockRef>)
	-> Result<BuiltRValue, (Error, (usize, usize))> {
		let Self {
			start,
			end: _,
			variant,
		} = self;
		if self.is_function() {
			let out = self.build_function_definition(main_data, file_build_data, "unnamedFunction", false)?;
			if let Some(basic_block) = basic_block {
				unsafe { LLVMPositionBuilderAtEnd(file_build_data.llvm_builder, basic_block) };
			}
			return Ok(out);
		}
		Ok(match variant {
			AstNodeVariant::Constant(value) => BuiltRValue::NumericalValue(unsafe {
				LLVMConstInt(main_data.int_type, *value as c_ulonglong, false as LLVMBool)
			}),
			AstNodeVariant::Identifier(name) => get_variable_by_name(main_data, file_build_data, local_variables, &*name),
			AstNodeVariant::Operator(operator, operands) => match operator {
				Operator::Assignment => {
					let l_value = operands[0].build_l_value(main_data, file_build_data, local_variables, basic_block)?;
					let r_value = operands[1].build_r_value(main_data, file_build_data, local_variables, basic_block)?;
					l_value.set_value(main_data, file_build_data.llvm_builder, r_value.clone());
					return Ok(r_value);
				}
				Operator::Normal(operation) => match operation {
					Operation::IntegerAdd | Operation::IntegerSubtract | Operation::IntegerMultiply |
					Operation::UnsignedDivide | Operation::UnsignedModulo | Operation::SignedDivide | Operation::SignedTruncatedModulo => {
						let left_value = operands[0].build_r_value(main_data, file_build_data, local_variables, basic_block)?.get_value(main_data, file_build_data.llvm_builder);
						let right_value = operands[1].build_r_value(main_data, file_build_data, local_variables, basic_block)?.get_value(main_data, file_build_data.llvm_builder);
						let result = match operation {
							Operation::IntegerAdd => unsafe { LLVMBuildAdd(file_build_data.llvm_builder, left_value, right_value, c"add_temp".as_ptr() as *const u8) },
							Operation::IntegerSubtract => unsafe { LLVMBuildSub(file_build_data.llvm_builder, left_value, right_value, c"sub_temp".as_ptr() as *const u8) },
							Operation::IntegerMultiply => unsafe { LLVMBuildMul(file_build_data.llvm_builder, left_value, right_value, c"mul_temp".as_ptr() as *const u8) },
							Operation::UnsignedDivide => unsafe { LLVMBuildUDiv(file_build_data.llvm_builder, left_value, right_value, c"udiv_temp".as_ptr() as *const u8) },
							Operation::UnsignedModulo => unsafe { LLVMBuildURem(file_build_data.llvm_builder, left_value, right_value, c"umod_temp".as_ptr() as *const u8) },
							Operation::SignedDivide => unsafe { LLVMBuildSDiv(file_build_data.llvm_builder, left_value, right_value, c"sdiv_temp".as_ptr() as *const u8) },
							Operation::SignedTruncatedModulo => unsafe { LLVMBuildSRem(file_build_data.llvm_builder, left_value, right_value, c"stmod_temp".as_ptr() as *const u8) },
							_ => unreachable!(),
						};
						BuiltRValue::NumericalValue(result)
					}
					Operation::IntegerNegate => {
						let operand = operands[0].build_r_value(main_data, file_build_data, local_variables, basic_block)?.get_value(main_data, file_build_data.llvm_builder);
						let result = match operation {
							Operation::IntegerNegate => unsafe { LLVMBuildNeg(file_build_data.llvm_builder, operand, c"neg_temp".as_ptr() as *const u8) },
							_ => unreachable!()
						};
						BuiltRValue::NumericalValue(result)
					}
					_ => return Err((Error::FeatureNotYetImplemented("this operator".into()), *start)),
				}
				Operator::Augmented(..) => return Err((Error::FeatureNotYetImplemented("augmented assignments".into()), self.start)),
				Operator::LValueAssignment => return Err((Error::FeatureNotYetImplemented("l-value assignments".into()), self.start)),
			}
			AstNodeVariant::FunctionDefinition(..) => unreachable!(),
			AstNodeVariant::Block(block_expressions, is_result_undefined) => {
				// If we are in the global scope
				if *is_result_undefined && block_expressions.is_empty() {
					return Ok(BuiltRValue::NumericalValue(unsafe { LLVMGetUndef(main_data.int_type) }));
				}
				if local_variables.is_empty() {
					return Err((Error::FeatureNotYetImplemented("blocks in global scope".into()), self.start));
				}
				// Push block scope
				local_variables.push(HashMap::new());
				// Build each expression
				let mut last_built_expression = None;
				for expression in block_expressions {
					last_built_expression = Some(expression.build_r_value(main_data, file_build_data, local_variables, basic_block)?);
				}
				// Pop the scope we pushed
				local_variables.pop();
				// Return
				match (is_result_undefined, last_built_expression) {
					(true, _) | (false, None) => BuiltRValue::NumericalValue(unsafe { LLVMGetUndef(main_data.int_type) }),
					(false, Some(last_built_expression)) => last_built_expression,
				}
			}
			AstNodeVariant::FunctionCall(function, arguments) => {
				if local_variables.is_empty() {
					return Err((Error::FeatureNotYetImplemented("global function calls".into()), self.start))
				}
				if arguments.len() > u16::MAX as usize {
					return Err((Error::TooManyFunctionArguments, self.start))
				}
				// Build function body and arguments
				let function_pointer_built = function.build_r_value(main_data, file_build_data, local_variables, basic_block)?.get_value(main_data, file_build_data.llvm_builder);
				let mut arguments_built = Vec::with_capacity(arguments.len());
				for argument in arguments {
					arguments_built.push(argument.build_r_value(main_data, file_build_data, local_variables, basic_block)?.get_value(main_data, file_build_data.llvm_builder));
				}
				// Build types
				let argument_types: Box<[LLVMTypeRef]> = repeat(main_data.int_type).take(arguments.len()).collect();
				let function_type = unsafe { LLVMFunctionType(main_data.int_type, argument_types.as_ptr(), argument_types.len() as c_uint, false as LLVMBool) };
				let function_pointer_type = unsafe { LLVMPointerType(function_type, 0) };
				// Build function call
				let function_pointer = unsafe {
					LLVMBuildIntToPtr(file_build_data.llvm_builder, function_pointer_built, function_pointer_type, c"int_to_ptr_temp".as_ptr() as *const u8)
				};
				let built_function_call = unsafe {
					LLVMBuildCall2(
						file_build_data.llvm_builder,
						function_type,
						function_pointer,
						arguments_built.as_ptr(),
						arguments_built.len() as c_uint,
						c"function_call_temp".as_ptr() as *const u8
					)
				};
				BuiltRValue::NumericalValue(built_function_call)
			}
			AstNodeVariant::String(_text) => return Err((Error::FeatureNotYetImplemented("string literals".into()), self.start)),
			AstNodeVariant::Metadata(metadata, _child) => match metadata {
				Metadata::EntryPoint => unreachable!(),
				Metadata::Link => unreachable!(),
			}
		})
	}

	pub fn build_l_value(&self, main_data: &mut MainData, file_build_data: &mut FileBuildData, local_variables: &mut Vec<HashMap<Box<str>, BuiltLValue>>, _basic_block: Option<LLVMBasicBlockRef>)
	-> Result<BuiltLValue, (Error, (usize, usize))> {
		let Self {
			start: _,
			end: _,
			variant,
		} = self;
		Ok(match variant {
			AstNodeVariant::Identifier(name) => {
				// Get local variable if it exists
				for scope_level in local_variables.iter().rev() {
					if let Some(variable) = scope_level.get(name) {
						return Ok(variable.clone());
					}
				}
				// Else create local variable
				let variable_name_c: Box<[u8]> = name.bytes().chain(once(0)).collect();
				let variable = unsafe { LLVMBuildAlloca(file_build_data.llvm_builder, main_data.int_type, variable_name_c.as_ptr()) };
				local_variables.last_mut().unwrap().insert(name.clone(), BuiltLValue::AllocaVariable(variable));
				BuiltLValue::AllocaVariable(variable)
			}
			_ => return Err((Error::FeatureNotYetImplemented("building feature".into()), self.start)),
		})
	}

	pub fn build_global_assignment(&self, main_data: &mut MainData, file_build_data: &mut FileBuildData, name: &str) -> Result<BuiltRValue, (Error, (usize, usize))> {
		if self.is_function() {
			let function = self.build_function_definition(main_data, file_build_data, name, false)?;
			return Ok(function);
		}
		let name_c: Box<[u8]> = name.bytes().chain(once(0)).collect();
		let r_value = self.build_r_value(main_data, file_build_data, &mut Vec::new(), None)?;
		let global = unsafe { LLVMAddGlobal(file_build_data.llvm_module, main_data.int_type, name_c.as_ptr()) };
		unsafe { LLVMSetInitializer(global, r_value.get_value(main_data, file_build_data.llvm_builder)) };
		return Ok(BuiltRValue::GlobalVariable(global));
	}

	pub fn is_function(&self) -> bool {
		match &self.variant {
			AstNodeVariant::FunctionDefinition(..) => true,
			AstNodeVariant::Metadata(metadata, child) => match metadata {
				Metadata::EntryPoint => child.is_function(),
				Metadata::Link => child.is_function(),
			}
			_ => false,
		}
	}
}

fn get_variable_by_name(main_data: &MainData, file_build_data: &mut FileBuildData, local_variables: &mut Vec<HashMap<Box<str>, BuiltLValue>>, name: &str) -> BuiltRValue {
	for scope_level in local_variables.iter().rev() {
		if let Some(variable) = scope_level.get(name) {
			return variable.get_value(main_data, file_build_data.llvm_builder);
		}
	}
	file_build_data.built_globals[name].clone()
}