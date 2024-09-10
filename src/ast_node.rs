use std::{collections::{HashMap, HashSet}, ffi::{c_uint, c_ulonglong}, iter::{repeat, once}, mem::swap};

use strum_macros::EnumDiscriminants;

use crate::{built_value::BuiltValue, error::Error, llvm_c::{LLVMAddFunction, LLVMAddGlobal, LLVMAppendBasicBlockInContext, LLVMBool, LLVMBuildAdd, LLVMBuildAlloca, LLVMBuildMul, LLVMBuildRet, LLVMBuildSDiv, LLVMBuildSRem, LLVMBuildStore, LLVMBuildSub, LLVMBuildUDiv, LLVMBuildURem, LLVMBuilderRef, LLVMConstInt, LLVMFunctionType, LLVMGetParam, LLVMModuleRef, LLVMPositionBuilderAtEnd, LLVMSetInitializer, LLVMTypeRef}, MainData};

#[derive(Debug)]
pub enum Operator {
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
	FloatModulo,
	Read,
	IntegerNegate,
	FloatNegate,
	Dereference,
}

#[derive(Debug)]
pub enum Metadata {
	EntryPoint,
}

#[derive(Debug, EnumDiscriminants)]
pub enum AstNodeVariant {
	/// A constant.
	Constant(u64),
	/// An operator with its operands and if is an assignment.
	Operator(Option<Operator>, Box<[AstNode]>, bool),
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
			AstNodeVariant::Operator(operator, _, is_assignment) => print!(", operator: {:?}, is_assignment: {:?}", operator, is_assignment),
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
			AstNodeVariant::Operator(_, operands, _) => for operand in operands {
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
			AstNodeVariant::Operator(operator, operands, is_assignment) => match is_assignment {
				true => {
					// Make sure the assignment is not augmented
					if !matches!(operator, None) {
						return Err((Error::GlobalAugmentedOperator, start));
					}
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
				false => for operand in operands {
					operand.separate_globals(global_list, will_be_discarded)?;
				}
			}
			AstNodeVariant::Constant(..) => {}
			AstNodeVariant::FunctionCall(..) => if will_be_discarded {
				return Err((Error::DiscardedGlobalFunctionCall, start));
			}
			AstNodeVariant::Block(children, is_result_undefined) => {
				if *is_result_undefined || children.len() != 1 {
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
		local_variables: &mut HashSet<Box<str>>
	) -> Result<(), (Error, (usize, usize))> {
		let AstNode {
			variant,
			start,
			end: _,
		} = self;
		match variant {
			AstNodeVariant::Block(sub_expressions, _) => for expression in sub_expressions {
				expression.get_variable_dependencies(variable_dependencies, import_dependencies, local_variables)?;
			}
			AstNodeVariant::Constant(..) => {}
			AstNodeVariant::FunctionCall(function, arguments) => {
				function.get_variable_dependencies(variable_dependencies, import_dependencies, &mut local_variables.clone())?;
				for argument in arguments {
					argument.get_variable_dependencies(variable_dependencies, import_dependencies, &mut local_variables.clone())?;
				}
			}
			AstNodeVariant::FunctionDefinition(parameters, body) => {
				for parameter in parameters {
					match &parameter.variant {
						AstNodeVariant::Identifier(name) => local_variables.insert(name.clone()),
						_ => return Err((Error::ExpectedIdentifier, *start)),
					};
				}
				body.get_variable_dependencies(variable_dependencies, import_dependencies, local_variables)?;
			}
			AstNodeVariant::Identifier(name) => if !local_variables.contains(name) {
				variable_dependencies.insert(name.clone());
			}
			AstNodeVariant::Metadata(_, child) => child.get_variable_dependencies(variable_dependencies, import_dependencies, local_variables)?,
			AstNodeVariant::Operator(_, operands, is_assignment) => {
				for (index, operand) in operands.iter().enumerate() {
					if !(*is_assignment && index == 0 && matches!(&operands[0].variant, AstNodeVariant::Identifier(..))) {
						operand.get_variable_dependencies(variable_dependencies, import_dependencies, &mut local_variables.clone())?;
					}
				}
				if *is_assignment {
					match &operands[0].variant {
						AstNodeVariant::Identifier(name) => {
							local_variables.insert(name.clone());
						}
						_ => {}
					};
				}
			}
			AstNodeVariant::String(..) => {}
		}
		Ok(())
	}

	fn build_function_definition(
		&self, main_data: &mut MainData, llvm_module: LLVMModuleRef, llvm_builder: LLVMBuilderRef,
		built_globals: &HashMap<Box<str>, BuiltValue>, local_variables: &mut Vec<HashMap<Box<str>, BuiltValue>>, name: &str,
	) -> Result<BuiltValue, (Error, (usize, usize))> {
		// Unpack function definition node
		let Self {
			start,
			end: _,
			variant,
		} = self;
		let (parameters, function_body) = match variant {
			AstNodeVariant::FunctionDefinition(function_parameters, function_body) => (function_parameters, function_body),
			_ => todo!(),
		};
		// Create function parameter type
		if parameters.len() > u16::MAX as usize {
			return Err((Error::TooManyFunctionParameters, *start));
		}
		let parameter_types: Box<[LLVMTypeRef]> = repeat(main_data.int_type).take(parameters.len()).collect();
		let function_type = unsafe { LLVMFunctionType(main_data.int_type, parameter_types.as_ptr(), parameter_types.len() as c_uint, false as LLVMBool) };
		// Build function value
		let name: Box<[u8]> = name.bytes().chain(once(0)).collect();
		let function = unsafe { LLVMAddFunction(llvm_module, name.as_ptr(), function_type) };
		// Build function body
		let basic_block = unsafe { LLVMAppendBasicBlockInContext(main_data.llvm_context, function, c"entry".as_ptr() as *const u8) };
		unsafe { LLVMPositionBuilderAtEnd(llvm_builder, basic_block) };
		// Add a local scope to the scope stack that contains the function parameters
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
			let parameter_variable = unsafe { LLVMBuildAlloca(llvm_builder, main_data.int_type, parameter_name_c.as_ptr()) };
			unsafe { LLVMBuildStore(llvm_builder, parameter_value, parameter_variable) };
			function_parameter_variables.insert(parameter_name.clone(), BuiltValue::AllocaVariable(parameter_variable));
		}
		local_variables.push(function_parameter_variables);
		// Build function body
		let function_body_built = function_body.build_r_value(main_data, llvm_module, llvm_builder, built_globals, local_variables)?;
		unsafe { LLVMBuildRet(llvm_builder, function_body_built.get_value(main_data, llvm_builder)) };
		// Pop the local scope that we pushed before
		local_variables.pop();
		// Return
		Ok(BuiltValue::Function(function))
	}

	pub fn build_r_value(
		&self, main_data: &mut MainData, llvm_module: LLVMModuleRef, llvm_builder: LLVMBuilderRef, built_globals: &HashMap<Box<str>, BuiltValue>, local_variables: &mut Vec<HashMap<Box<str>, BuiltValue>>
	) -> Result<BuiltValue, (Error, (usize, usize))> {
		let Self {
			start: _,
			end: _,
			variant,
		} = self;
		Ok(match variant {
			AstNodeVariant::Constant(value) => BuiltValue::NumericalValue(unsafe {
				LLVMConstInt(main_data.int_type, *value as c_ulonglong, false as LLVMBool)
			}),
			AstNodeVariant::Identifier(name) => get_variable_by_name(built_globals, local_variables, &*name),
			AstNodeVariant::Operator(operator, operands, is_assignment) => {
				if *is_assignment {
					return Err((Error::FeatureNotYetImplemented("local assignments".into()), self.start));
				}
				let operator = match operator {
					Some(operator) => operator,
					None => return Err((Error::FeatureNotYetImplemented("no operator".into()), self.start)),
				};
				match operator {
					Operator::IntegerAdd | Operator::IntegerSubtract | Operator::IntegerMultiply |
					Operator::UnsignedDivide | Operator::UnsignedModulo | Operator::SignedDivide | Operator::SignedTruncatedModulo => {
						let left_value = operands[0].build_r_value(main_data, llvm_module, llvm_builder, built_globals, local_variables)?.get_value(main_data, llvm_builder);
						let right_value = operands[1].build_r_value(main_data, llvm_module, llvm_builder, built_globals, local_variables)?.get_value(main_data, llvm_builder);
						let result = match operator {
							Operator::IntegerAdd => unsafe { LLVMBuildAdd(llvm_builder, left_value, right_value, c"add_temp".as_ptr() as *const u8) },
							Operator::IntegerSubtract => unsafe { LLVMBuildSub(llvm_builder, left_value, right_value, c"sub_temp".as_ptr() as *const u8) },
							Operator::IntegerMultiply => unsafe { LLVMBuildMul(llvm_builder, left_value, right_value, c"mul_temp".as_ptr() as *const u8) },
							Operator::UnsignedDivide => unsafe { LLVMBuildUDiv(llvm_builder, left_value, right_value, c"udiv_temp".as_ptr() as *const u8) },
							Operator::UnsignedModulo => unsafe { LLVMBuildURem(llvm_builder, left_value, right_value, c"umod_temp".as_ptr() as *const u8) },
							Operator::SignedDivide => unsafe { LLVMBuildSDiv(llvm_builder, left_value, right_value, c"sdiv_temp".as_ptr() as *const u8) },
							Operator::SignedTruncatedModulo => unsafe { LLVMBuildSRem(llvm_builder, left_value, right_value, c"stmod_temp".as_ptr() as *const u8) },
							_ => unreachable!(),
						};
						BuiltValue::NumericalValue(result)
					}
					_ => return Err((Error::FeatureNotYetImplemented("operator".into()), self.start)),
				}
			}
			AstNodeVariant::FunctionDefinition(..) => {
				self.build_function_definition(main_data, llvm_module, llvm_builder, built_globals, local_variables, "unnamedFunction")?
			}
			_ => return Err((Error::FeatureNotYetImplemented("building feature".into()), self.start)),
		})
	}

	pub fn build_global_assignment(&self, name: &str, llvm_module: LLVMModuleRef, llvm_builder: LLVMBuilderRef, main_data: &mut MainData, built_globals: &HashMap<Box<str>, BuiltValue>) ->
		Result<BuiltValue, (Error, (usize, usize))> {
		// TODO: functions
		let r_value = self.build_r_value(main_data, llvm_module, llvm_builder, built_globals, &mut Vec::new())?;
		let mut name: Vec<u8> = name.bytes().collect();
		name.push(0);
		let global = unsafe { LLVMAddGlobal(llvm_module, main_data.int_type, name.as_ptr()) };
		unsafe { LLVMSetInitializer(global, r_value.get_value(main_data, llvm_builder)) };
		return Ok(BuiltValue::GlobalVariable(global));
	}
}

fn get_variable_by_name(built_globals: &HashMap<Box<str>, BuiltValue>, local_variables: &mut Vec<HashMap<Box<str>, BuiltValue>>, name: &str) -> BuiltValue {
	for scope_level in local_variables.iter().rev() {
		if let Some(scope_level) = scope_level.get(name) {
			return scope_level.clone();
		}
	}
	built_globals[name].clone()
}