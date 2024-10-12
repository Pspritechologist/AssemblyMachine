pub mod ast;
pub mod span;
mod compiling;

use std::ops::Deref;

pub use grammar::scriptParser as Parser;

// lalrpop_mod!(grammar);
mod grammar;
// use lalrpop_util::lalrpop_mod;

use ast::*;
use span::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error<'a> {
	BadType {
		expected: Option<Type>,
		got: Type,
		span: &'a Span,
	},
	BadAddType {
		a: Type,
		b: Type,
		span: &'a Span,
	},
	RecursiveFunction {
		name: &'a str,
		span: &'a Span,
	},
	UnknownFunction {
		name: &'a str,
		span: &'a Span,
	},
	UnknownVariable {
		name: &'a str,
		span: &'a Span,
	},
	WrongNumberOfArgs {
		expected: usize,
		got: usize,
		span: &'a Span,
	},
	DuplicateMapKey {
		key: &'a Constant,
		span: &'a Span,
	},
	LoopStatementOutsideLoop {
		span: &'a Span,
	},
	UnknownLoopLabel {
		label: &'a str,
		span: &'a Span,
	},
	CantBeIndexed {
		ty: Type,
		span: &'a Span,
	},
	UntypedArray {
		span: &'a Span,
	},
}

impl<'a> Error<'a> {
	pub fn span(&self) -> &'a Span {
		match self {
			Error::BadType { span, .. } => span,
			Error::BadAddType { span, .. } => span,
			Error::RecursiveFunction { span, .. } => span,
			Error::UnknownFunction { span, .. } => span,
			Error::UnknownVariable { span, .. } => span,
			Error::WrongNumberOfArgs { span, .. } => span,
			Error::DuplicateMapKey { span, .. } => span,
			Error::LoopStatementOutsideLoop { span } => span,
			Error::UnknownLoopLabel { span, .. } => span,
			Error::CantBeIndexed { span, .. } => span,
			Error::UntypedArray { span } => span,
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Warning<'a> {
	UnusedFunction { name: &'a str, span: &'a Span },
	UnusedVariable { name: &'a str, span: &'a Span },
}

impl<'a> Warning<'a> {
	pub fn span(&self) -> &'a Span {
		match self {
			Warning::UnusedFunction { span, .. } => span,
			Warning::UnusedVariable { span, .. } => span,
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Context<'a> {
	functions: Vec<FuncData<'a>>,
	variables: Vec<VarData<'a>>,

	is_loop: bool,
	labels: Vec<&'a str>,
}

impl<'a> Context<'a> {

}

pub struct NonsemblyValidator<'a> {
	script: &'a Script,

	context_stack: Vec<Context<'a>>,
	old_context: Vec<Context<'a>>,

	errors: Vec<Error<'a>>,
}

impl<'a> NonsemblyValidator<'a> {
	pub fn new(script: &'a Script) -> Self {
		Self {
			script,
			errors: Vec::new(),
			context_stack: vec![Context::default()],
			old_context: Vec::new(),
		}
	}

	pub fn validate(mut self) -> Result<Vec<Warning<'a>>, (Vec<Error<'a>>, Vec<Warning<'a>>)> {
		for instr in self.script {
			self.validate_instruction(instr);
		}

		self.pop_context(); // We pop our last context.

		let mut warnings = vec![];

		for func in self.old_context.iter().flat_map(|ctx| ctx.functions.iter()) {
			if !func.used {
				warnings.push(Warning::UnusedFunction { name: func.name, span: func.span });
			}
		}

		for var in self.old_context.iter().flat_map(|ctx| ctx.variables.iter()) {
			if !var.used {
				warnings.push(Warning::UnusedVariable { name: var.name, span: var.span });
			}
		}

		if self.errors.is_empty() {
			Ok(warnings)
		} else {
			Err((self.errors, warnings))
		}
	}

	fn validate_instruction(&mut self, instr: &'a Instruction) {
		match instr {
			Instruction::FunctionDef { name, value, args, body, span } => {
				self.validate_function(name, body, value, args, span);
			},
			Instruction::Assign { name, value, span } => {
				self.validate_value(value, span);
				let ret_type = self.resolve_type(value);
				self.push_var(VarData::new(name, ret_type.unwrap_or(Type::NonTyped(NonTypeReason::ValidateError)), span));
			},
			Instruction::AssignOp { name, value, op, span } => {
				self.validate_value(value, span);
				let Some(var) = self.find_var(name) else {
					self.errors.push(Error::UnknownVariable { name, span });
					return;
				};
				if matches!(var.value, Type::NonTyped(_)) {
					return;
				}

				let Ok(assign_type) = self.resolve_type(value) else {
					return;
				};
				
				match op {
					AssignOp::Add => {
						match (&var.value, &assign_type) {
							(Type::Array(i), t) | (t, Type::Array(i)) if *t == **i => (),
							(Type::String, Type::Num) | (Type::Num, Type::String) => (),
							(a, b) if a == b => (),
							(a, b) => self.errors.push(Error::BadAddType { a: a.clone(), b: b.clone(), span }),
						}
					},
					_ => {
						if var.value != Type::Num {
							self.errors.push(Error::BadType { expected: Some(Type::Num), got: var.value.clone(), span });
						}
						if assign_type != Type::Num {
							self.errors.push(Error::BadType { expected: Some(Type::Num), got: assign_type, span });
						}
					}
				}
			},
			Instruction::Value { value, span } => {
				self.validate_value(value, span);
			},
			Instruction::ControlFlow { control_flow, span } => {
				self.validate_control_flow(control_flow, span);
			},
		}
	}

	fn validate_function(&mut self, func_name: &'a String, body: &'a [Instruction], func_value: &'a Type, args: &'a [(String, Type, Span)], span: &'a Span) {
		self.push_ctx_func();

		for (name, ty, span) in args {
			self.push_var(VarData::new(name, ty.clone(), span));
		}

		for instr in body {
			match instr {
				Instruction::Value { value: Value::FunctionCall { name, .. }, span }
					if name == func_name => self.errors.push(Error::RecursiveFunction { name, span }),
				i => self.validate_instruction(i),
			}
		}

		for instr in body {
			if let Instruction::ControlFlow { control_flow: ControlFlow::Return { value: return_type }, span } = instr {
				let return_type = match return_type.as_ref().map(|value| self.resolve_type(&value)).unwrap_or(Ok(Type::Null)) {
					Ok(value) => value,
					Err(_) => continue,
				};

				if func_value != &return_type {
					self.errors.push(Error::BadType { expected: Some(func_value.clone()), got: return_type.clone(), span });
				}
			}
		}

		self.pop_context();

		self.push_func(FuncData::new(func_name, args.iter().map(|a| &a.1).collect(), func_value, span));
	}

	fn validate_value(&mut self, value: &'a Value, span: &'a Span) {
		match value {
			Value::Variable { id } => {
				if !self.use_var(&id) {
					self.errors.push(Error::UnknownVariable { name: &id, span });
				}
			},
			Value::FunctionCall { name, args } => {
				self.use_func(&name);
				
				if let Some(func) = self.find_func(&name) {
					if func.args.len() != args.len() {
						self.errors.push(Error::WrongNumberOfArgs { expected: func.args.len(), got: args.len(), span });
					} else {
						let mut errors = vec![];

						for (arg, ty) in args.iter().zip(func.args.iter()) {
							let Ok(arg_type) = self.resolve_type(arg) else {
								continue;
							};

							if &arg_type != *ty {
								errors.push(Error::BadType { expected: Some((*ty).clone()), got: arg_type, span });
							}
						}

						self.errors.extend(errors);
					}
				} else {
								if name != "print" { //FIXME: REMOVE THIS LATER.
					self.errors.push(Error::UnknownFunction { name: &name, span });
								}
				}

				for arg in args {
					self.validate_value(arg, span);
				}
			},
			Value::Literal { value } => {
				match value {
					Constant::Map(map, inner) => {
						{
							let mut keys = vec![];

							for (k, v) in map {
								self.validate_value(&k, span);
								self.validate_value(&v, span);

								// Check for duplicate literal keys.
								if let Value::Literal { value } = k {
									if keys.contains(&value) {
										self.errors.push(Error::DuplicateMapKey { key: value, span });
									} else {
										keys.push(&value);
									}
								}
							}
						}

						// Ensure the inner type is valid.
						match (map.is_empty(), inner.is_some()) {
							(true, true) => (),
							(true, false) => self.errors.push(Error::UntypedArray { span }),
							(false, false) => {
								let (k1, v1) = map.first().unwrap();
								let (Ok(k1_ty), Ok(v1_ty)) = (self.resolve_type(k1), self.resolve_type(v1)) else {
									return;
								};

								for (k, v) in map.iter().skip(1) {
									let (Ok(k_ty), Ok(v_ty)) = (self.resolve_type(k), self.resolve_type(v)) else {
										continue;
									};
									
									if k_ty != k1_ty {
										self.errors.push(Error::BadType { expected: Some(k1_ty.clone()), got: k_ty, span });
									}
									if v_ty != v1_ty {
										self.errors.push(Error::BadType { expected: Some(v1_ty.clone()), got: v_ty, span });
									}
								}
							},
							// This is currently unreachable, but it's here for future-proofing.
							(false, true) => {
								let (ex_k_ty, ex_v_ty) = inner.as_ref().cloned().unwrap();
								for (k, v) in map {
									if let (Ok(k_ty), Ok(v_ty)) = (self.resolve_type(&k), self.resolve_type(&v)) {
										if k_ty != ex_k_ty {
											self.errors.push(Error::BadType { expected: Some(ex_k_ty.clone()), got: k_ty, span });
										}
										if v_ty != ex_v_ty {
											self.errors.push(Error::BadType { expected: Some(ex_v_ty.clone()), got: v_ty, span });
										}
									}
								}
							},
						}
					},
					Constant::Array(array, inner) => {
						{
							for item in array {
								self.validate_value(item, span);
							}
						}

						// Ensure the inner type is valid.
						match (array.is_empty(), inner.is_some()) {
							(true, true) => (),
							(true, false) => self.errors.push(Error::UntypedArray { span }),
							(false, false) => {
								let Ok(first_ty) = self.resolve_type(array.first().unwrap()) else {
									return;
								};

								for item in array.iter().skip(1) {
									let Ok(item_ty) = self.resolve_type(item) else {
										continue;
									};

									if item_ty != first_ty {
										self.errors.push(Error::BadType { expected: Some(first_ty.clone()), got: item_ty, span });
									}
								}
							},
							// This is currently unreachable, but it's here for future-proofing.
							(false, true) => {
								let ex_ty = inner.as_ref().cloned().unwrap();
								for item in array {
									if let Ok(item_ty) = self.resolve_type(item) {
										if item_ty != ex_ty {
											self.errors.push(Error::BadType { expected: Some(ex_ty.clone()), got: item_ty, span });
										}
									}
								}
							},
						}
					},
					_ => (),
				}
			},
			Value::Operation(op) => {
				let (a, b) = op.get_values();
				self.validate_value(a, span);
				if let Some(b) = b {
					self.validate_value(b, span);
				}

				let (a, b) = match (self.resolve_type(a), b.map(|b| self.resolve_type(b))) {
					(Ok(a), Some(Ok(b))) => (a, b),
					(Ok(a), None) => (a, Type::Null),
					_ => return,
				};

				match (op.deref(), (a, b)) {
					(Operation::And(..), (bt, Type::Bool)) |
					(Operation::Or(..), (bt, Type::Bool)) |
					(Operation::And(..), (Type::Bool, bt)) |
					(Operation::Or(..), (Type::Bool, bt))
						if bt != Type::Bool => self.errors.push(Error::BadType { expected: Some(Type::Bool), got: bt, span }),
					
					(Operation::Not(..), (bt, _))
						if bt != Type::Bool => self.errors.push(Error::BadType { expected: Some(Type::Bool), got: bt, span }),

					(Operation::Eq(..), (a, b)) |
					(Operation::Neq(..), (a, b)) |
					(Operation::Lt(..), (a, b)) |
					(Operation::Gt(..), (a, b)) |
					(Operation::Lte(..), (a, b)) |
					(Operation::Gte(..), (a, b))
						if a != b => self.errors.push(Error::BadType { expected: Some(a.clone()), got: b.clone(), span }),

					(Operation::Sub(..), (a, b)) |
					(Operation::Pwr(..), (a, b)) |
					(Operation::Mul(..), (a, b)) |
					(Operation::Div(..), (a, b)) |
					(Operation::Mod(..), (a, b))
						if a != Type::Num || b != Type::Num =>
						self.errors.push(Error::BadType { expected: Some(Type::Num), got: if a != Type::Num { a } else { b }, span }),

					(Operation::Incr(..), (bt, _)) |
					(Operation::Decr(..), (bt, _))
						if bt != Type::Num => self.errors.push(Error::BadType { expected: Some(Type::Num), got: bt, span }),
					
					(Operation::Add(..), (a, b)) =>
						match (a, b) {
							(a, b) if a == b => (),
							(Type::Array(i), t) | (t, Type::Array(i)) if t == *i => (),
							(Type::String, Type::Num) | (Type::Num, Type::String) => (),
							(a, b) => self.errors.push(Error::BadAddType { a, b, span }),
						},
					(Operation::Index(..), (Type::Array(_), i))
						if i != Type::Num => self.errors.push(Error::BadType { expected: Some(Type::Num), got: i, span }),
					(Operation::Index(..), (Type::Map(kv), i))
						if (*kv).0 != i => self.errors.push(Error::BadType { expected: Some((*kv).0.clone()), got: i, span }),
					(Operation::Index(..), (t, _))
						if !matches!(t, Type::Array(_) | Type::Map(_)) => self.errors.push(Error::CantBeIndexed { ty: t, span }),
					_ => (),
				}
			},
		}
	}

	fn validate_control_flow(&mut self, control_flow: &'a ControlFlow, span: &'a Span) {
		match control_flow {
			ControlFlow::Return { value } => {
				if let Some(value) = value {
					self.validate_value(value, span);
				}
			},
			ControlFlow::Break { id } => {
				if !self.in_loop() {
					self.errors.push(Error::LoopStatementOutsideLoop { span });
				}

				if let Some(id) = id {
					if !self.find_label(id) {
						self.errors.push(Error::UnknownLoopLabel { label: id, span });
					}
				}
			},
			ControlFlow::Continue { id } => {
				if !self.in_loop() {
					self.errors.push(Error::LoopStatementOutsideLoop { span });
				}

				if let Some(id) = id {
					if !self.find_label(id) {
						self.errors.push(Error::UnknownLoopLabel { label: id, span });
					}
				}
			},
			ControlFlow::If { cond, body, else_body } => {
				self.validate_value(cond, span);
				self.push_ctx_loop();
				for instr in body {
					self.validate_instruction(instr);
				}
				self.pop_context();

				if let Some(else_body) = else_body {
					self.push_ctx_loop();
					for instr in else_body {
						self.validate_instruction(instr);
					}
					self.pop_context();
				}
			},
			ControlFlow::While { cond, body, label } => {
				self.push_ctx_loop();
				if let Some(label) = label {
					self.push_label(label);
				}

				self.validate_value(cond, span);
				if let Ok(cond_type) = self.resolve_type(&cond) && cond_type != Type::Bool {
					self.errors.push(Error::BadType { expected: Some(Type::Bool), got: cond_type, span });
				}

				for instr in body {
					self.validate_instruction(instr);
				}

				self.pop_context();
			},
			ControlFlow::For { iter, ident, body, label } => {
				self.push_ctx_loop();
				if let Some(label) = label {
					self.push_label(label);
				}

				self.validate_value(iter, span);
				if let Ok(iter_type) = self.resolve_type(&iter) {
					if let Type::Array(inner) = iter_type {
						self.push_var(VarData::new(ident, *inner, span));

						for instr in body {
							self.validate_instruction(instr);
						}
					} else {
						self.errors.push(Error::CantBeIndexed { ty: iter_type, span });
					}
				}

				self.pop_context();
			},
			ControlFlow::Loop { body, label } => {
				self.push_ctx_loop();
				if let Some(label) = label {
					self.push_label(label);
				}

				for instr in body {
					self.validate_instruction(instr);
				}

				self.pop_context();
			},
		}
	}

	fn in_loop(&self) -> bool {
		self.context_stack.last().unwrap().is_loop
	}

	fn push_var(&mut self, var: VarData<'a>) {
		self.context_stack.last_mut().unwrap().variables.push(var);
	}

	fn push_func(&mut self, func: FuncData<'a>) {
		self.context_stack.last_mut().unwrap().functions.push(func);
	}

	fn push_label(&mut self, label: &'a str) {
		self.context_stack.last_mut().unwrap().labels.push(label);
	}

	fn push_ctx_func(&mut self) {
		self.context_stack.push(Context::default());
	}

	fn push_ctx_loop(&mut self) {
		self.context_stack.push(Context { is_loop: true, ..Context::default() });
	}

	fn pop_context(&mut self) {
		self.old_context.push(self.context_stack.pop().unwrap()); //TODO: Unwrap.
	}

	fn find_var(&self, name: &str) -> Option<&VarData> {
		self.context_stack.iter().rev().find_map(|ctx| {
			ctx.variables.iter().rev().find(|var| *var == name)
		})
	}

	fn find_func(&self, name: &str) -> Option<&FuncData> {
		self.context_stack.iter().rev().find_map(|ctx| {
			ctx.functions.iter().rev().find(|func| *func == name)
		})
	}

	fn find_label(&self, name: &str) -> bool {
		let mut iter = self.context_stack.iter().rev();
		while let Some(ctx) = iter.next() && ctx.is_loop {
			if ctx.labels.contains(&name) {
				return true;
			}
		}

		false
	}

	/// # Safety
	/// This is a bad idea, but it's the only way I can think of to get around the lifetime issues right now.
	fn find_var_mut(&mut self, name: &str) -> Option<&mut VarData> {
		self.context_stack.iter_mut().rev().find_map(|ctx| {
			// ctx.variables.iter_mut().rev().find(|var| *var == name)
			unsafe { std::mem::transmute(ctx.variables.iter_mut().rev().find(|var| *var == name)) }
		})
	}

	/// # Safety
	/// This is a bad idea, but it's the only way I can think of to get around the lifetime issues right now.
	fn find_func_mut(&mut self, name: &str) -> Option<&mut FuncData> {
		self.context_stack.iter_mut().rev().find_map(|ctx| {
			// ctx.functions.iter_mut().rev().find(|func| *func == name)
			unsafe { std::mem::transmute(ctx.functions.iter_mut().rev().find(|func| *func == name)) }
		})
	}

	fn use_var(&mut self, name: &str) -> bool {
		if let Some(var) = self.find_var_mut(name) {
			var.used = true;
			true
		} else {
			false
		}
	}

	fn use_func(&mut self, name: &str) -> bool {
		if let Some(func) = self.find_func_mut(name) {
			func.used = true;
			true
		} else {
			false
		}
	}

	fn resolve_type(&self, value: &Value) -> Result<Type, NonTypeReason> {
		match value {
			Value::Variable { id } => match self.find_var(&id).map(|var| var.value.clone()).ok_or(NonTypeReason::CouldNotFind) {
				Ok(Type::NonTyped(r)) => Err(r),
				t => t,
			},
			Value::FunctionCall { name, .. } => match self.find_func(&name).map(|func| func.value.clone()).ok_or(NonTypeReason::CouldNotFind) {
				Ok(Type::NonTyped(r)) => Err(r),
				t => t,
			},
			Value::Literal { value: Constant::Array(items, inner) } => match (items.is_empty(), inner.is_some()) {
				(true, true) => Ok(Type::Array(inner.as_ref().unwrap().clone().into())),
				(false, false) => Result::map(self.resolve_type(items.first().unwrap()), |t| Type::Array(Box::new(t))),
				(true, false) => Err(NonTypeReason::ValidateError),
				(false, true) => unreachable!("Cannot have items and explicit type"),
			},
			Value::Literal { value: Constant::Map(items, inner) } => match (items.is_empty(), inner.is_some()) {
				(_, true) => Ok(Type::Map(inner.as_ref().unwrap().clone().into())),
				(false, false) => {
					let (k, v) = items.first().unwrap();
					let (k, v) = (self.resolve_type(k)?, self.resolve_type(v)?);
					Ok(Type::Map((k, v).into()))
				},
				(true, false) => Err(NonTypeReason::ValidateError),
			},
			Value::Literal { value } => Ok(value.get_type()),
			Value::Operation(op) => if let Operation::Add(a, b) = op.deref() {
				let (a, b) = match (self.resolve_type(a), self.resolve_type(b)) {
					(Err(e), _) | (_, Err(e)) => return Err(e),
					(Ok(a), Ok(b)) => (a, b),
				};

				match (a, b) {
					(a, b) if a == b => Ok(a),
					(Type::Array(t), _) | (_, Type::Array(t)) => Ok(Type::Array(Box::new(*t))),
					(Type::String, Type::Num) | (Type::Num, Type::String) => Ok(Type::String),
					(_, _) => Err(NonTypeReason::Add),
				}
			} else if let Operation::Index(t, _) = op.deref() {
				let t = self.resolve_type(t)?;

				match t {
					Type::Array(t) => Ok(*t),
					Type::Map(kv) => Ok((*kv).0),
					_ => Err(NonTypeReason::ValidateError),
				}
			} else {
				Ok(op.get_type())
			}
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct FuncData<'a> {
	name: &'a str,
	args: Vec<&'a Type>,
	value: &'a Type,
	used: bool,
	span: &'a Span,
}

impl<'a> FuncData<'a> {
	pub fn new(name: &'a str, args: Vec<&'a Type>, value: &'a Type, span: &'a Span) -> Self {
		Self { name, args, value, used: false, span }
	}
}

impl<'a> PartialEq<str> for FuncData<'a> {
	fn eq(&self, other: &str) -> bool {
		self.name == other
	}
}

impl<'a> PartialEq<Type> for FuncData<'a> {
	fn eq(&self, other: &Type) -> bool {
		self.value == other
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct VarData<'a> {
	name: &'a str,
	value: Type,
	used: bool,
	span: &'a Span,
}

impl<'a> VarData<'a> {
	pub fn new(name: &'a str, value: Type, span: &'a Span) -> Self {
		Self { name, value, used: false, span }
	}
}

impl<'a> PartialEq<str> for VarData<'a> {
	fn eq(&self, other: &str) -> bool {
		self.name == other
	}
}

impl<'a> PartialEq<Type> for VarData<'a> {
	fn eq(&self, other: &Type) -> bool {
		self.value == *other
	}
}
