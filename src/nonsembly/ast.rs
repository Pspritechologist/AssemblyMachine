use std::fmt::write;

use assembly_machine::{objects::{Num, Object}, UnwrapUnsafeSafe};
use super::span::Span;

pub type Script = Vec<Instruction>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instruction {
	FunctionDef {
		span: Span,
		name: String,
		value: Type,
		args: Vec<(String, Type, Span)>,
		body: Vec<Instruction>,
	},
	Assign {
		name: String,
		value: Box<Value>,
		span: Span,
	},
	AssignOp {
		name: String,
		value: Value,
		op: AssignOp,
		span: Span,
	},
	Value {
		value: Value,
		span: Span,
	},
	ControlFlow {
		control_flow: ControlFlow,
		span: Span,
	},
}

impl Instruction {
	pub fn new_function_def(name: String, value: Type, args: Vec<(String, Type, Span)>, body: Vec<Instruction>, span: Span) -> Self {
		Instruction::FunctionDef {
			name,
			value,
			args,
			body,
			span,
		}
	}

	pub fn new_assign(name: String, value: Value, span: Span) -> Self {
		Instruction::Assign {
			name,
			value: Box::new(value),
			span,
		}
	}

	pub fn new_assign_op(name: String, value: Value, op: AssignOp, span: Span) -> Self {
		Instruction::AssignOp {
			name,
			value,
			op,
			span,
		}
	}

	pub fn new_value(value: Value, span: Span) -> Self {
		Instruction::Value {
			value,
			span,
		}
	}

	pub fn new_control_flow(control_flow: ControlFlow, span: Span) -> Self {
		Instruction::ControlFlow {
			control_flow,
			span,
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOp {
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	Pwr,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
	Operation(Box<Operation>),
	Variable { id: String },
	Literal { value: Constant },
	FunctionCall {
		name: String,
		args: Vec<Value>,
	},
}

impl Value {
	pub fn new_variable(id: String) -> Self {
		Value::Variable { id }
	}

	pub fn new_literal(value: Constant) -> Self {
		Value::Literal { value }
	}

	pub fn new_function_call(name: String, args: Vec<Value>) -> Self {
		Value::FunctionCall { name, args }
	}
}

impl From<Constant> for Value {
	fn from(constant: Constant) -> Self {
		Value::new_literal(constant)
	}
}

impl From<Operation> for Value {
	fn from(operation: Operation) -> Self {
		Value::Operation(Box::new(operation))
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constant {
	Num(Num),
	Bool(bool),
	String(String),
	Array(Vec<Value>, Option<Type>),
	Map(Vec<(Value, Value)>, Option<(Type, Type)>),
}

impl Constant {
	pub fn new_num(value: impl Into<Num>) -> Self {
		Constant::Num(value.into())
	}

	pub fn new_bool(value: bool) -> Self {
		Constant::Bool(value)
	}

	pub fn new_string_raw(value: String) -> Self {
		Constant::String(value)
	}

	pub fn new_string_escaped(value: &str) -> Self {
		Constant::String(unescape::unescape(value).unwrap_or_else(|| value.to_string()))
	}

	pub fn new_string_multiline(value: &str) -> Self {
		let value = value.strip_prefix('\n').unwrap_or(value);

		let count_indent = |value: &str| value.chars().take_while(|i| matches!(i, '\t' | ' ')).count();

		let indent = value.lines().filter_map(|l| match count_indent(l) { i if i > 0 => Some(i), _ => None }).min().unwrap();
		let value = value
			.lines()
			.map(|l| l.chars().skip(indent).collect::<String>())
			.collect::<Vec<_>>()
			.join("\n");

		Constant::String(unescape::unescape(&value).unwrap_or(value))
	}

	pub fn new_array(value: Vec<Value>) -> Self {
		Constant::Array(value, None)
	}

	pub fn new_array_empty(inner: Type) -> Self {
		Constant::Array(Vec::new(), Some(inner))
	}

	pub fn new_map(value: Vec<(Value, Value)>) -> Self {
		Constant::Map(value, None)
	}

	pub fn new_map_empty(inner: (Type, Type)) -> Self {
		Constant::Map(Vec::new(), Some(inner))
	}
}

impl Typed for Constant {
	fn get_type(&self) -> Type {
		match self {
			Constant::Num(_) => Type::Num,
			Constant::Bool(_) => Type::Bool,
			Constant::String(_) => Type::String,
			Constant::Array(..) => Type::Array(Type::NonTyped(NonTypeReason::InnerType).into()),
			Constant::Map(..) => Type::Map((Type::NonTyped(NonTypeReason::InnerType), Type::NonTyped(NonTypeReason::InnerType)).into()),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ControlFlow {
	Return { value: Option<Value> },
	If {
		cond: Box<Value>,
		body: Vec<Instruction>,
		else_body: Option<Vec<Instruction>>,
	},
	While {
		cond: Box<Value>,
		body: Vec<Instruction>,
		label: Option<String>,
	},
	For {
		iter: Value,
		ident: String,
		body: Vec<Instruction>,
		label: Option<String>,
	},
	Loop {
		body: Vec<Instruction>,
		label: Option<String>,
	},
	Continue { id: Option<String> },
	Break { id: Option<String> },
}

impl ControlFlow {
	pub fn new_return(value: Option<Value>) -> Self {
		ControlFlow::Return { value }
	}

	pub fn new_if(cond: Value, body: Vec<Instruction>, else_body: Option<Vec<Instruction>>) -> Self {
		ControlFlow::If {
			cond: Box::new(cond),
			body,
			else_body,
		}
	}

	pub fn new_while(cond: Value, body: Vec<Instruction>, label: Option<String>) -> Self {
		ControlFlow::While {
			cond: Box::new(cond),
			body,
			label,
		}
	}

	pub fn new_for(iter: Value, ident: String, body: Vec<Instruction>, label: Option<String>) -> Self {
		ControlFlow::For {
			iter,
			ident,
			body,
			label,
		}
	}

	pub fn new_loop(body: Vec<Instruction>, label: Option<String>) -> Self {
		ControlFlow::Loop {
			body,
			label,
		}
	}
	
	pub fn new_continue(id: Option<String>) -> Self {
		ControlFlow::Continue { id }
	}

	pub fn new_break(id: Option<String>) -> Self {
		ControlFlow::Break { id }
	}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
	Num,
	Bool,
	String,
	Array(Box<Type>),
	Map(Box<(Type, Type)>),
	Null,
	Custom(String),
	NonTyped(NonTypeReason),
}

impl Default for Type {
	fn default() -> Self {
		Self::Null
	}
}

impl std::str::FromStr for Type {
	type Err = !;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		Ok(match s.to_lowercase().trim() {
			"num" => Type::Num,
			"bool" => Type::Bool,
			"string" => Type::String,
			"null" => Type::Null,
			a if a.starts_with("array[") && a.ends_with(']') => Type::Array(Box::new(a[6..a.len() - 1].trim().parse()?)),
			m if m.starts_with("map{") && m.ends_with('}') && m.contains(':') => {
				let mut split = m[4..m.len() - 1].split(':');
				Type::Map((split.next().unwrap().trim().parse()?, split.next().unwrap().trim().parse()?).into())
			}
			_ => Type::Custom(s.to_string()),
		})
	}
}

impl std::fmt::Display for Type {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Type::Num => write!(f, "Num"),
			Type::Bool => write!(f, "Bool"),
			Type::String => write!(f, "String"),
			Type::Array(i) => write!(f, "Array<{i}>"),
			Type::Map(i) => write!(f, "Map<{}, {}>", i.0, i.1),
			Type::Null => write!(f, "Null"),
			Type::Custom(s) => write!(f, "{}", s),
			Type::NonTyped(reason) => write!(f, "NonTyped({reason:?})"),
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NonTypeReason {
	/// Means this value couldn't be found during validation- an error.
	CouldNotFind,
	/// 'Add' accepts values of multiple types and requires
	/// validation-time information to understand its type.
	Add,
	/// 'Index' requires a value to be an array or map.
	/// This is validation-time information.
	Index,
	/// Determining the inner type of an array or map may require
	/// validation-time information.
	InnerType,
	/// An unknown error, usually from further up in validation.
	ValidateError,
	/// Some type, likely supported by the VM but not valid for use in the language.  
	/// Usually stems from a function pointer.
	InvalidType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operation {
	/// ==
	Eq(Value, Value),
	/// !=
	Neq(Value, Value),
	/// <
	Lt(Value, Value),
	/// >
	Gt(Value, Value),
	/// <=
	Lte(Value, Value),
	/// >=
	Gte(Value, Value),
	/// +
	Add(Value, Value),
	/// -
	Sub(Value, Value),
	/// ^
	Pwr(Value, Value),
	/// *
	Mul(Value, Value),
	/// /
	Div(Value, Value),
	/// %
	Mod(Value, Value),
	/// ++
	Incr(Value),
	/// --
	Decr(Value),
	/// &&
	And(Value, Value),
	/// ||
	Or(Value, Value),
	/// !
	Not(Value),
	/// array\[4] or map\["key"]
	Index(Value, Value),
}

impl Operation {
	pub fn new_eq(lhs: Value, rhs: Value) -> Self {
		Operation::Eq(lhs, rhs)
	}
	pub fn new_neq(lhs: Value, rhs: Value) -> Self {
		Operation::Neq(lhs, rhs)
	}
	pub fn new_lt(lhs: Value, rhs: Value) -> Self {
		Operation::Lt(lhs, rhs)
	}
	pub fn new_gt(lhs: Value, rhs: Value) -> Self {
		Operation::Gt(lhs, rhs)
	}
	pub fn new_lte(lhs: Value, rhs: Value) -> Self {
		Operation::Lte(lhs, rhs)
	}
	pub fn new_gte(lhs: Value, rhs: Value) -> Self {
		Operation::Gte(lhs, rhs)
	}
	pub fn new_add(lhs: Value, rhs: Value) -> Self {
		Operation::Add(lhs, rhs)
	}
	pub fn new_sub(lhs: Value, rhs: Value) -> Self {
		Operation::Sub(lhs, rhs)
	}
	pub fn new_pwr(lhs: Value, rhs: Value) -> Self {
		Operation::Pwr(lhs, rhs)
	}
	pub fn new_mul(lhs: Value, rhs: Value) -> Self {
		Operation::Mul(lhs, rhs)
	}
	pub fn new_div(lhs: Value, rhs: Value) -> Self {
		Operation::Div(lhs, rhs)
	}
	pub fn new_mod(lhs: Value, rhs: Value) -> Self {
		Operation::Mod(lhs, rhs)
	}
	pub fn new_incr(value: Value) -> Self {
		Operation::Incr(value)
	}
	pub fn new_decr(value: Value) -> Self {
		Operation::Decr(value)
	}
	pub fn new_and(lhs: Value, rhs: Value) -> Self {
		Operation::And(lhs, rhs)
	}
	pub fn new_or(lhs: Value, rhs: Value) -> Self {
		Operation::Or(lhs, rhs)
	}
	pub fn new_not(value: Value) -> Self {
		Operation::Not(value)
	}
	pub fn new_index(target: Value, index: Value) -> Self {
		Operation::Index(target, index)
	}

	pub fn get_values(&self) -> (&Value, Option<&Value>) {
		match self {
			Operation::Eq(a, b) |
			Operation::Neq(a, b) |
			Operation::Lt(a, b) |
			Operation::Gt(a, b) |
			Operation::Lte(a, b) |
			Operation::Gte(a, b) |
			Operation::Add(a, b) |
			Operation::Sub(a, b) |
			Operation::Pwr(a, b) |
			Operation::Mul(a, b) |
			Operation::Div(a, b) |
			Operation::Mod(a, b) |
			Operation::And(a, b) |
			Operation::Or(a, b) => (a, Some(b)),
			Operation::Incr(a) |
			Operation::Decr(a) |
			Operation::Not(a) => (a, None),
			Operation::Index(a, b) => (a, Some(b)),
		}
	}
}

impl Typed for Operation {
	fn get_type(&self) -> Type {
		match self {
			Operation::Add(_, _) => Type::NonTyped(NonTypeReason::Add),
			Operation::Eq(_, _) |
			Operation::Neq(_, _) |
			Operation::Lt(_, _) |
			Operation::Gt(_, _) |
			Operation::Lte(_, _) |
			Operation::Gte(_, _) |
			Operation::And(_, _) |
			Operation::Or(_, _) |
			Operation::Not(_) => Type::Bool,
			Operation::Sub(_, _) |
			Operation::Pwr(_, _) |
			Operation::Mul(_, _) |
			Operation::Div(_, _) |
			Operation::Mod(_, _) |
			Operation::Incr(_) |
			Operation::Decr(_) => Type::Num,
			Operation::Index(_, _) => Type::NonTyped(NonTypeReason::Index),
		}
	}
}

pub trait Typed {
	fn get_type(&self) -> Type;
}

// impl Typed for Object {
// 	fn get_type(&self) -> Type {
// 		match self {
// 			Object::Num(_) => Type::Num,
// 			Object::Bool(_) => Type::Bool,
// 			Object::String(_) => Type::String,
// 			Object::Array(_) => Type::Array,
// 			Object::Map(_) => Type::Map,
// 			Object::Null => Type::Null,
// 			Object::Fn(_) => Type::NonTyped(NonTypeReason::InvalidType)
// 		}
// 	}
// }
