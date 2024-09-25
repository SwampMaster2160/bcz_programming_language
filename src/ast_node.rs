use std::{cmp::Ordering, collections::{HashMap, HashSet}, ffi::{c_uint, c_ulonglong}, iter::{once, repeat}, mem::swap};

use strum_macros::EnumDiscriminants;

use crate::{built_value::BuiltLValue, error::Error, file_build_data::FileBuildData, llvm::{builder::Builder, llvm_c::{LLVMAddFunction, LLVMAddGlobal, LLVMAppendBasicBlockInContext, LLVMBasicBlockRef, LLVMBool, LLVMBuildAlloca, LLVMBuildRet, LLVMBuildStore, LLVMConstInt, LLVMDLLImportLinkage, LLVMGetParam, LLVMPointerType, LLVMPositionBuilderAtEnd, LLVMSetFunctionCallConv, LLVMSetInitializer, LLVMSetLinkage, LLVMSizeOfTypeInBits, LLVMWin64CallConv}, llvm_type::Type, module::Module, traits::WrappedReference, value::Value}, MainData};

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
}

#[derive(Debug, Clone)]
pub enum Operator {
	Assignment,
	Normal(Operation),
	Augmented(Operation),
	LValueAssignment,
}

#[derive(Debug, Clone)]
pub enum Metadata {
	EntryPoint,
	Link,
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
	/// A list of parameters for a function definition and the function body.
	FunctionDefinition(Box<[AstNode]>, Box<AstNode>),
	/// A string literal.
	String(Box<str>),
	/// Metadata about a child node.
	Metadata(Metadata, Box<AstNode>),
}

#[derive(Debug, Clone)]
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
		is_l_value: bool,
		is_link_function: bool,
	) -> Result<(), (Error, (usize, usize))> {
		let AstNode {
			variant,
			start,
			end: _,
		} = self;
		if is_link_function && !matches!(variant, AstNodeVariant::FunctionDefinition(..) | AstNodeVariant::Metadata(..)) {
			return Err((Error::LinkNotUsedOnFunction, *start))
		}
		match variant {
			AstNodeVariant::Block(sub_expressions, _) => for expression in sub_expressions {
				match is_l_value {
					false => expression.get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, false, false)?,
					true => return Err((Error::FeatureNotYetImplemented("l-value blocks".into()), *start)),
				};
			}
			AstNodeVariant::Constant(..) => {}
			AstNodeVariant::FunctionCall(function, arguments) => {
				if is_l_value {
					return Err((Error::LValueFunctionCall, *start));
				}
				function.get_variable_dependencies(variable_dependencies, import_dependencies, &mut local_variables.clone(), false, false)?;
				for argument in arguments {
					argument.get_variable_dependencies(variable_dependencies, import_dependencies, &mut local_variables.clone(), false, false)?;
				}
			}
			AstNodeVariant::FunctionDefinition(parameters, body) => {
				if is_l_value {
					return Err((Error::LValueFunctionDefinition, *start));
				}
				for parameter in parameters {
					match (&parameter.variant, is_link_function) {
						(AstNodeVariant::Identifier(name), false) => {
							local_variables.insert(name.clone());
						}
						(_, false) => return Err((Error::ExpectedIdentifier, parameter.start)),
						(_, true) => parameter.get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, false, false)?,
					};
				}
				match is_link_function {
					false => body.get_variable_dependencies(variable_dependencies, import_dependencies, &mut HashSet::new(), false, false)?,
					true => body.get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, false, false)?,
				}
			}
			AstNodeVariant::Identifier(name) => match is_l_value {
				false => if !local_variables.contains(name) {
					variable_dependencies.insert(name.clone());
				}
				true => {
					local_variables.insert(name.clone());
				}
			}
			AstNodeVariant::Metadata(metadata, child) => match metadata {
				Metadata::EntryPoint => child.get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, is_l_value, is_link_function)?,
				Metadata::Link => child.get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, is_l_value, true)?,
			},
			AstNodeVariant::Operator(operator, operands) => match operator {
				Operator::Assignment => {
					operands[0].get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, true, false)?;
					operands[1].get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, false, false)?;
				}
				Operator::Augmented(operation) => match operation {
					Operation::IntegerAdd | Operation::IntegerSubtract | Operation::IntegerMultiply | Operation::SignedDivide | Operation::SignedTruncatedModulo |
					Operation::UnsignedDivide | Operation::UnsignedModulo |
					Operation::FloatAdd | Operation::FloatSubtract | Operation::FloatMultiply | Operation::FloatDivide | Operation::FloatTruncatedModulo => {
						operands[0].get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, true, false)?;
						operands[1].get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, false, false)?;
					}
					Operation::Dereference | Operation::IntegerNegate | Operation::FloatNegate | Operation::Read => return Err((Error::FeatureNotYetImplemented("augmented unary operators".into()), *start)),
				}
				Operator::Normal(operation) => match operation {
					// Operators that only have r-values as operands
					Operation::IntegerAdd | Operation::IntegerSubtract | Operation::IntegerMultiply | Operation::SignedDivide | Operation::SignedTruncatedModulo |
					Operation::UnsignedDivide | Operation::UnsignedModulo |
					Operation::FloatAdd | Operation::FloatSubtract | Operation::FloatMultiply | Operation::FloatDivide | Operation::FloatTruncatedModulo |
					Operation::Dereference | Operation::IntegerNegate | Operation::FloatNegate => for operand in operands {
						operand.get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, false, false)?;
					}
					// Operators that only have l-values as operands
					Operation::Read => for operand in operands {
						operand.get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, true, false)?;
					}
				}
				Operator::LValueAssignment => for operand in operands {
					operand.get_variable_dependencies(variable_dependencies, import_dependencies, local_variables, true, false)?;
				}
			}
			AstNodeVariant::String(..) => {}
		}
		Ok(())
	}

	fn build_function_definition<'a>(&'a self, main_data: &'a MainData, file_build_data: &mut FileBuildData<'a, 'a>, llvm_module: &'a Module, llvm_builder: &Builder, name: &str, is_link_function: bool, is_entry_point: bool)
	-> Result<Value<'a, 'a>, (Error, (usize, usize))> {
		// Unpack function definition node
		let Self {
			start,
			end: _,
			variant,
		} = self;
		let (parameters, function_body) = match variant {
			AstNodeVariant::FunctionDefinition(function_parameters, function_body) => (function_parameters, function_body),
			AstNodeVariant::Metadata(metadata, child) => match metadata {
				Metadata::EntryPoint => return child.build_function_definition(main_data, file_build_data, llvm_module, llvm_builder, name, is_link_function, true),
				Metadata::Link => return child.build_function_definition(main_data, file_build_data, llvm_module, llvm_builder, name, true, is_entry_point),
			}
			_ => unreachable!(),
		};
		// Create function parameter type
		if parameters.len() > u16::MAX as usize {
			return Err((Error::TooManyFunctionParameters, *start));
		}
		let parameter_types: Box<[Type]> = repeat(main_data.int_type).take(parameters.len()).collect();
		let function_type = main_data.int_type.function_type(&*parameter_types, false);
		// Build function value
		let name_bytes: Box<[u8]> = match is_link_function {
			false => name.bytes().chain(once(0)).collect(),
			true => "__bcz__link__".bytes().chain(name.bytes()).chain(once(0)).collect(),
		};
		let function = unsafe { Value::from_ref(LLVMAddFunction(llvm_module.get_ref(), name_bytes.as_ptr(), function_type.get_ref())) };
		// Build function body
		let basic_block = unsafe { LLVMAppendBasicBlockInContext(main_data.llvm_context.get_ref(), function.get_ref(), c"entry".as_ptr() as *const u8) };
		unsafe { LLVMPositionBuilderAtEnd(llvm_builder.get_ref(), basic_block) };
		match is_link_function {
			false => {
				let mut function_parameter_variables = HashMap::new();
				for (parameter_index, parameter) in parameters.iter().enumerate() {
					// Get parameter name
					let parameter_name = match &parameter.variant {
						AstNodeVariant::Identifier(name) => name,
						_ => return Err((Error::ExpectedIdentifier, parameter.start)),
					};
					// Add parameter to local scope
					let parameter_value = unsafe { Value::from_ref(LLVMGetParam(function.get_ref(), parameter_index as c_uint)) };
					let parameter_name_c: Box<[u8]> = parameter_name.bytes().chain(once(0)).collect();
					let parameter_variable = unsafe { Value::from_ref(LLVMBuildAlloca(llvm_builder.get_ref(), main_data.int_type.get_ref(), parameter_name_c.as_ptr())) };
					unsafe { LLVMBuildStore(llvm_builder.get_ref(), parameter_value.get_ref(), parameter_variable.get_ref()) };
					function_parameter_variables.insert(parameter_name.clone(), BuiltLValue::AllocaVariable(parameter_variable));
				}
				let mut inner_local_variables: Vec<HashMap<Box<str>, BuiltLValue<'a>>> = vec![function_parameter_variables];
				// Build function body
				let function_body_built: Value<'a, 'a> = function_body.build_r_value(main_data, file_build_data, llvm_module, llvm_builder, &mut inner_local_variables, Some(basic_block))?;
				unsafe { LLVMBuildRet(llvm_builder.get_ref(), function_body_built.get_ref()) };
			}
			true => {
				// Get wrapped function type
				let mut wrapped_function_parameter_types = Vec::with_capacity(parameters.len());
				for parameter in parameters.iter() {
					let (parameter_type, _) = parameter.type_from_width(main_data)?;
					wrapped_function_parameter_types.push(parameter_type);
				}
				let (wrapped_function_return_type, wrapped_function_return_type_is_signed) = function_body.type_from_width(main_data)?;
				let wrapped_function_type = wrapped_function_return_type.function_type(wrapped_function_parameter_types.as_slice(), false);
				// Link to wrapped function
				let wrapped_name: Box<[u8]> = name.bytes().chain(once(0)).collect();
				let wrapped_function = unsafe { Value::from_ref(LLVMAddFunction(llvm_module.get_ref(), wrapped_name.as_ptr(), wrapped_function_type.get_ref())) };
				unsafe { LLVMSetLinkage(wrapped_function.get_ref(), LLVMDLLImportLinkage) };
				unsafe { LLVMSetFunctionCallConv(wrapped_function.get_ref(), LLVMWin64CallConv) };
				// Cast arguments to the types of the wrapped function parameters
				let mut arguments = Vec::with_capacity(parameters.len());
				for (parameter_index, parameter) in parameters.iter().enumerate() {
					let (parameter_type, is_signed) = parameter.type_from_width(main_data)?;
					let argument = unsafe { Value::from_ref(LLVMGetParam(function.get_ref(), parameter_index as c_uint)) };
					let argument_converted = match main_data.int_bit_width.cmp(&(unsafe { LLVMSizeOfTypeInBits(main_data.llvm_data_layout, parameter_type.get_ref()) } as u8)) {
						Ordering::Less => match is_signed {
							false => argument.build_zero_extend(llvm_builder, parameter_type, "z_extend_temp"),
							true => argument.build_sign_extend(llvm_builder, parameter_type, "s_extend_temp"),
						}
						Ordering::Equal => argument,
						Ordering::Greater => argument.build_truncate(llvm_builder, parameter_type, "truncate_temp"),
					};
					arguments.push(argument_converted);
				}
				// Call wrapped function
				let call_result = unsafe { wrapped_function.build_call(arguments.as_slice(), wrapped_function_type, llvm_builder, name) };
				//let call_result = unsafe {
				//	Value::from_ref(LLVMBuildCall2(
				//		file_build_data.llvm_builder.get_ref(),
				//		wrapped_function_type.get_ref(),
				//		wrapped_function,
				//		transmute(arguments.as_ptr()),
				//		arguments.len() as c_uint,
				//		c"func_call_temp".as_ptr() as *const u8,
				//	))
				//};
				// Build return
				let call_result_converted = match main_data.int_bit_width.cmp(&(unsafe { LLVMSizeOfTypeInBits(main_data.llvm_data_layout, wrapped_function_return_type.get_ref()) } as u8)) {
					Ordering::Less => call_result.build_truncate(llvm_builder, main_data.int_type, "truncate_temp"),
					Ordering::Equal => call_result,
					Ordering::Greater => match wrapped_function_return_type_is_signed {
						false => call_result.build_zero_extend(llvm_builder, main_data.int_type, "zero_extend_temp"),
						true => call_result.build_sign_extend(llvm_builder, main_data.int_type, "sign_extend_temp"),
					}
				};
				unsafe { LLVMBuildRet(llvm_builder.get_ref(), call_result_converted.get_ref()) };
			}
		}
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

	pub fn build_r_value<'a>(&'a self, main_data: &'a MainData<'a>, file_build_data: &mut FileBuildData<'a, 'a>, llvm_module: &'a Module, llvm_builder: &Builder, local_variables: &mut Vec<HashMap<Box<str>, BuiltLValue<'a>>>, basic_block: Option<LLVMBasicBlockRef>)
	-> Result<Value, (Error, (usize, usize))> {
		let Self {
			start,
			end: _,
			variant,
		} = self;
		if self.is_function() {
			let out: Value<'a, 'a> = self.build_function_definition(main_data, file_build_data, llvm_module, llvm_builder, "__bcz__unnamedFunction", false, false)?;
			if let Some(basic_block) = basic_block {
				unsafe { LLVMPositionBuilderAtEnd(llvm_builder.get_ref(), basic_block) };
			}
			return Ok(out);
		}
		Ok(match variant {
			AstNodeVariant::Constant(value) => unsafe { Value::from_ref(LLVMConstInt(main_data.int_type.get_ref(), *value as c_ulonglong, false as LLVMBool)) },
			AstNodeVariant::Identifier(name) => get_variable_by_name(main_data, file_build_data, llvm_builder, local_variables, &*name),
			AstNodeVariant::Operator(operator, operands) => match operator {
				Operator::Assignment => {
					let r_value = operands[1].build_r_value(main_data, file_build_data, llvm_module, llvm_builder, local_variables, basic_block)?;
					let l_value = operands[0].build_l_value(main_data, file_build_data, llvm_module, llvm_builder, local_variables, basic_block)?;
					l_value.set_value(main_data, llvm_builder, r_value.clone());
					return Ok(r_value);
				}
				Operator::Normal(operation) => match operation {
					Operation::IntegerAdd | Operation::IntegerSubtract | Operation::IntegerMultiply |
					Operation::UnsignedDivide | Operation::UnsignedModulo | Operation::SignedDivide | Operation::SignedTruncatedModulo => {
						let left_value = operands[0].build_r_value(main_data, file_build_data, llvm_module, llvm_builder, local_variables, basic_block)?;
						let right_value = operands[1].build_r_value(main_data, file_build_data, llvm_module, llvm_builder, local_variables, basic_block)?;
						let result = match operation {
							Operation::IntegerAdd => left_value.build_add(&right_value, llvm_builder, "add_temp"),
							Operation::IntegerSubtract => left_value.build_sub(&right_value, llvm_builder, "sub_temp"),
							Operation::IntegerMultiply => left_value.build_mult(&right_value, llvm_builder, "mult_temp"),
							Operation::UnsignedDivide => left_value.build_unsigned_div(&right_value, llvm_builder, "udiv_temp"),
							Operation::UnsignedModulo => left_value.build_unsigned_modulo(&right_value, llvm_builder, "umod_temp"),
							Operation::SignedDivide => left_value.build_signed_div(&right_value, llvm_builder, "sdiv_temp"),
							Operation::SignedTruncatedModulo => left_value.build_signed_truncated_modulo(&right_value, llvm_builder, "stmod_temp"),
							_ => unreachable!(),
						};
						result
					}
					Operation::IntegerNegate => {
						let operand = operands[0].build_r_value(main_data, file_build_data, llvm_module, llvm_builder, local_variables, basic_block)?;
						let result = match operation {
							Operation::IntegerNegate => operand.build_negate(llvm_builder, "neg_temp"),
							_ => unreachable!()
						};
						result
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
					return Ok(main_data.int_type.undefined());
				}
				if local_variables.is_empty() {
					return Err((Error::FeatureNotYetImplemented("blocks in global scope".into()), self.start));
				}
				// Push block scope
				local_variables.push(HashMap::new());
				// Build each expression
				let mut last_built_expression = None;
				for expression in block_expressions {
					last_built_expression = Some(expression.build_r_value(main_data, file_build_data, llvm_module, llvm_builder, local_variables, basic_block)?);
				}
				// Pop the scope we pushed
				local_variables.pop();
				// Return
				match (is_result_undefined, last_built_expression) {
					(true, _) | (false, None) => main_data.int_type.undefined(),
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
				let function_pointer_built = function.build_r_value(main_data, file_build_data, llvm_module, llvm_builder, local_variables, basic_block)?;
				let mut arguments_built = Vec::with_capacity(arguments.len());
				for argument in arguments {
					arguments_built.push(argument.build_r_value(main_data, file_build_data, llvm_module, llvm_builder, local_variables, basic_block)?);
				}
				// Build types
				let argument_types: Box<[Type]> = repeat(main_data.int_type).take(arguments.len()).collect();
				let function_type = main_data.int_type.function_type(&*argument_types, false);
				let function_pointer_type = unsafe { Type::from_ref(LLVMPointerType(function_type.get_ref(), 0)) };
				// Build function call
				let function_pointer = function_pointer_built.build_int_to_ptr(llvm_builder, function_pointer_type, "int_to_ptr_temp");
				let built_function_call = unsafe { function_pointer.build_call(arguments_built.as_slice(), function_type, llvm_builder, "function_call_temp") };
				//let built_function_call = unsafe {
				//	Value::from_ref(LLVMBuildCall2(
				//		file_build_data.llvm_builder.get_ref(),
				//		function_type.get_ref(),
				//		function_pointer.get_ref(),
				//		transmute(arguments_built.as_ptr()),
				//		arguments_built.len() as c_uint,
				//		c"function_call_temp".as_ptr() as *const u8
				//	))
				//};
				built_function_call
			}
			AstNodeVariant::String(_text) => return Err((Error::FeatureNotYetImplemented("string literals".into()), self.start)),
			AstNodeVariant::Metadata(metadata, _child) => match metadata {
				Metadata::EntryPoint => unreachable!(),
				Metadata::Link => unreachable!(),
			}
		})
	}

	pub fn build_l_value<'a>(&'a self, main_data: &MainData, _file_build_data: &mut FileBuildData, _llvm_module: &Module, llvm_builder: &Builder, local_variables: &'a mut Vec<HashMap<Box<str>, BuiltLValue>>, _basic_block: Option<LLVMBasicBlockRef>)
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
				let variable = unsafe { Value::from_ref(LLVMBuildAlloca(llvm_builder.get_ref(), main_data.int_type.get_ref(), variable_name_c.as_ptr())) };
				local_variables.last_mut().unwrap().insert(name.clone(), BuiltLValue::AllocaVariable(variable.clone()));
				BuiltLValue::AllocaVariable(variable)
			}
			_ => return Err((Error::FeatureNotYetImplemented("building feature".into()), self.start)),
		})
	}

	pub fn build_global_assignment<'a>(&'a self, main_data: &'a MainData, llvm_module: &'a Module<'a>, llvm_builder: &Builder<'a>, file_build_data: &mut FileBuildData<'a, 'a>, name: &str)
	-> Result<Value, (Error, (usize, usize))> {
		if self.is_function() {
			let function = self.build_function_definition(main_data, file_build_data, llvm_module, llvm_builder, name, false, false)?;
			return Ok(function);
		}
		let name_c: Box<[u8]> = name.bytes().chain(once(0)).collect();
		let r_value = self.build_r_value(main_data, file_build_data, llvm_module, llvm_builder, &mut Vec::new(), None)?;
		let global = unsafe { Value::from_ref(LLVMAddGlobal(llvm_module.get_ref(), main_data.int_type.get_ref(), name_c.as_ptr())) };
		unsafe { LLVMSetInitializer(global.get_ref(), r_value.get_ref()) };
		return Ok(r_value);
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

	pub fn type_from_width<'a>(&'a self, main_data: &'a MainData) -> Result<(Type, bool), (Error, (usize, usize))> {
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
					//0 => unsafe { LLVMVoidTypeInContext(main_data.llvm_context) },
					1 => main_data.llvm_context.int_8_type(),
					2 => main_data.llvm_context.int_16_type(),
					4 => main_data.llvm_context.int_32_type(),
					8 => main_data.llvm_context.int_64_type(),
					16 => main_data.llvm_context.int_128_type(),
					_ => return Err((Error::InvalidTypeWidth, *start)),
				}, is_negative)
			}
			_ => return Err((Error::InvalidType, *start)),
		})
	}

	pub fn const_evaluate(&mut self, main_data: &mut MainData, const_evaluated_globals: &HashMap<Box<str>, (AstNode, HashSet<Box<str>>)>, variable_dependencies: &mut HashSet<Box<str>>, is_link_function: bool)
	-> Result<(), (Error, (usize, usize))> {
		let Self {
			start,
			end,
			variant,
		} = self;
		match variant {
			AstNodeVariant::Operator(operator, operands) => {
				for operand in operands.iter_mut() {
					operand.const_evaluate(main_data, const_evaluated_globals, variable_dependencies, is_link_function)?;
				}
				match operator {
					Operator::Normal(operation) => match operation {
						Operation::IntegerNegate => if let AstNode { variant: AstNodeVariant::Constant(value), .. } = operands[0] {
							let new_value = ((value ^ main_data.int_max_value).wrapping_add(1)) & main_data.int_max_value;
							*self = AstNode { variant: AstNodeVariant::Constant(new_value), start: *start, end: *end };
						}
						// TODO
						_ => {}
					}
					// TODO
					_ => {}
				}
			}
			AstNodeVariant::FunctionDefinition(parameters, body) => {
				body.const_evaluate(main_data, const_evaluated_globals, variable_dependencies, false)?;
				if is_link_function {
					for parameter in parameters {
						parameter.const_evaluate(main_data, const_evaluated_globals, variable_dependencies, false)?;
					}
				}
			}
			AstNodeVariant::Metadata(metadata, child) => match metadata {
				Metadata::EntryPoint => child.const_evaluate(main_data, const_evaluated_globals, variable_dependencies, is_link_function)?,
				Metadata::Link => child.const_evaluate(main_data, const_evaluated_globals, variable_dependencies, true)?,
			}
			AstNodeVariant::Block(sub_expressions, ..) => for sub_expression in sub_expressions {
				sub_expression.const_evaluate(main_data, const_evaluated_globals, variable_dependencies, is_link_function)?;
			}
			AstNodeVariant::Constant(..) => {}
			AstNodeVariant::FunctionCall(function_pointer, arguments) => {
				function_pointer.const_evaluate(main_data, const_evaluated_globals, variable_dependencies, is_link_function)?;
				for argument in arguments {
					argument.const_evaluate(main_data, const_evaluated_globals, variable_dependencies, is_link_function)?;
				}
			}
			AstNodeVariant::String(..) => {}
			// TODO
			AstNodeVariant::Identifier(..) => {}
		}
		Ok(())
	}
}

fn get_variable_by_name<'a>(main_data: &MainData, file_build_data: &mut FileBuildData<'a, 'a>, llvm_builder: &Builder, local_variables: &Vec<HashMap<Box<str>, BuiltLValue<'a>>>, name: &str) -> Value<'a, 'a> {
	for scope_level in local_variables.iter().rev() {
		if let Some(variable) = scope_level.get(name) {
			return variable.get_value(main_data, llvm_builder);
		}
	}
	file_build_data.built_globals[name].clone()
}