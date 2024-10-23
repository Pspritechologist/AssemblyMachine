use assembly_machine_macros::SpanData;

use super::*;

#[derive(Debug, Clone, PartialEq, Eq, SpanData)]
#[enum_from]
pub enum Value {
	Variable(Ident),
	FunctionCall(FunctionCall),
	Literal(Constant),
	Operation(Box<Spanned<Operation>>),
}

#[derive(Debug, Clone, PartialEq, Eq, SpanData)]
pub struct FunctionCall {
	pub span: Span,
	pub name: Ident,
	pub args: Vec<Value>,
}

impl FunctionCall {
	pub fn new(name: Ident, args: Vec<Value>, span: Span) -> Self {
		Self { name, args, span }
	}
}

#[derive(Debug, Clone, PartialEq, Eq, SpanData)]
pub enum Constant {
	Num(Spanned<Num>),
	Bool(Spanned<bool>),
	String(Spanned<String>),
	Array(Spanned<(Vec<Value>, Option<Spanned<TypeDef>>)>),
	Map(Spanned<(Vec<(Value, Value)>, Option<(Spanned<TypeDef>, Spanned<TypeDef>)>)>),
}

impl Constant {
	pub fn new_num(value: impl Into<Num>, span: Span) -> Self {
		Self::Num(Spanned::new(value.into(), span))
	}

	pub fn new_bool(value: bool, span: Span) -> Self {
		Self::Bool(Spanned::new(value, span))
	}

	pub fn new_string_raw(value: String, span: Span) -> Self {
		Self::String(Spanned::new(value, span))
	}

	pub fn new_string_escaped(value: &str, span: Span) -> Self {
		Constant::String(Spanned::new(unescape::unescape(value).unwrap_or_else(|| value.to_string()), span))
	}

	pub fn new_string_multiline(value: &str, span: Span) -> Self {
		let value = value.strip_prefix('\n').unwrap_or(value);

		let count_indent = |value: &str| value.chars().take_while(|i| matches!(i, '\t' | ' ')).count();

		let indent = value.lines().filter_map(|l| match count_indent(l) { i if i > 0 => Some(i), _ => None }).min().unwrap();
		let value = value
			.lines()
			.map(|l| l.chars().skip(indent).collect::<String>())
			.collect::<Vec<_>>()
			.join("\n");

		Constant::String(Spanned::new(unescape::unescape(&value).unwrap_or(value), span))
	}

	pub fn new_array(value: Vec<Value>, span: Span) -> Self {
		Constant::Array(Spanned::new((value, None), span))
	}

	pub fn new_array_empty(inner: Spanned<TypeDef>, span: Span) -> Self {
		Constant::Array(Spanned::new((Vec::new(), Some(inner)), span))
	}

	pub fn new_map(value: Vec<(Value, Value)>, span: Span) -> Self {
		Constant::Map(Spanned::new((value, None), span))
	}

	pub fn new_map_empty(inner: (Spanned<TypeDef>, Spanned<TypeDef>), span: Span) -> Self {
		Constant::Map(Spanned::new((Vec::new(), Some(inner)), span))
	}
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
	fn get_type(&self) -> TypeDef {
		match self {
			Operation::Add(_, _) => TypeDef::NonTyped(NonTypeReason::Add),
			Operation::Eq(_, _) |
			Operation::Neq(_, _) |
			Operation::Lt(_, _) |
			Operation::Gt(_, _) |
			Operation::Lte(_, _) |
			Operation::Gte(_, _) |
			Operation::And(_, _) |
			Operation::Or(_, _) |
			Operation::Not(_) => TypeDef::Bool,
			Operation::Sub(_, _) |
			Operation::Pwr(_, _) |
			Operation::Mul(_, _) |
			Operation::Div(_, _) |
			Operation::Mod(_, _) |
			Operation::Incr(_) |
			Operation::Decr(_) => TypeDef::Num,
			Operation::Index(_, _) => TypeDef::NonTyped(NonTypeReason::Index),
		}
	}
}
