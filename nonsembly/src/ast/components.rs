use super::*;
use assembly_machine_macros::SpanData;
use std::ops::{Deref, DerefMut};

pub trait SpanData {
	fn get_span(&self) -> Span;
}

#[derive(PartialEq, Eq)]
pub struct Spanned<T>(T, Span);

impl<T> Spanned<T> {
	pub fn new(value: T, span: Span) -> Self {
		Spanned(value, span)
	}
}

impl<T> From<(T, Span)> for Spanned<T> {
	fn from((value, span): (T, Span)) -> Self {
		Spanned(value, span)
	}
}

impl<T> std::ops::Deref for Spanned<T> {
	type Target = T;
	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

impl<T> std::ops::DerefMut for Spanned<T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.0
	}
}

impl<T> SpanData for Spanned<T> {
	fn get_span(&self) -> Span {
		self.1
	}
}

impl<T: ToString> Spanned<T> {
	fn clone_string(&self) -> Spanned<String> {
		Spanned(self.0.to_string(), self.1)
	}
}

impl<T: Clone> Clone for Spanned<T> {
	fn clone(&self) -> Self {
		Spanned(self.0.clone(), self.1)
	}
}

impl<T: Copy> Copy for Spanned<T> {}

impl<T: std::fmt::Debug> std::fmt::Debug for Spanned<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{:?} {:?}", self.0, self.1)
	}
}

impl<T: std::fmt::Display> std::fmt::Display for Spanned<T> {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{}", self.0)
	}
}

#[derive(Clone, Debug, PartialEq, Eq, SpanData)]
pub struct Ident {
	id: Spanned<String>,
}

impl Deref for Ident {
	type Target = String;
	fn deref(&self) -> &Self::Target {
		&self.id
	}
}

impl DerefMut for Ident {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.id
	}
}

impl Ident {
	pub fn new(id: String, span: Span) -> Self {
		Ident { id: Spanned::new(id, span) }
	}
}

impl std::fmt::Display for Ident {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "{}", self.id.0)
	}
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AssignOperator {
	// Set,
	Add,
	Sub,
	Mul,
	Div,
	Mod,
	Pwr,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeDef {
	Num,
	Bool,
	String,
	Array(Box<Spanned<TypeDef>>),
	Map(Box<(Spanned<TypeDef>, Spanned<TypeDef>)>),
	Null,
	Custom(Spanned<String>),
	NonTyped(NonTypeReason),
}

impl Default for TypeDef {
	fn default() -> Self {
		Self::Null
	}
}

impl TypeDef {
	pub fn new(s: Spanned<&str>) -> Self {
		match s.to_lowercase().trim() {
			"num" => TypeDef::Num,
			"bool" => TypeDef::Bool,
			"string" => TypeDef::String,
			"null" => TypeDef::Null,
			_ => TypeDef::Custom(s.clone_string()),
		}
	}

	pub fn new_array(inner: Spanned<TypeDef>) -> Self {
		TypeDef::Array(Box::new(inner))
	}

	pub fn new_map(k: Spanned<TypeDef>, v: Spanned<TypeDef>) -> Self {
		TypeDef::Map(Box::new((k, v)))
	}

	// type Err = !;

	// fn from_str(s: &str) -> Result<Self, Self::Err> {
	// 	Ok(match s.to_lowercase().trim() {
	// 		"num" => TypeDef::Num,
	// 		"bool" => TypeDef::Bool,
	// 		"string" => TypeDef::String,
	// 		"null" => TypeDef::Null,
	// 		a if a.starts_with("array<") && a.ends_with('>') => TypeDef::Array(Box::new(a[6..a.len() - 1].trim().parse()?)),
	// 		m if m.starts_with("map<") && m.ends_with('>') && m.contains(':') => {
	// 			let mut split = m[4..m.len() - 1].split(':');
	// 			TypeDef::Map((split.next().unwrap().trim().parse()?, split.next().unwrap().trim().parse()?).into())
	// 		}
	// 		_ => TypeDef::Custom(s),
	// 	})
	// }
}

impl std::fmt::Display for TypeDef {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			TypeDef::Num => write!(f, "Num"),
			TypeDef::Bool => write!(f, "Bool"),
			TypeDef::String => write!(f, "String"),
			TypeDef::Array(i) => write!(f, "Array<{}>", i.0),
			TypeDef::Map(i) => write!(f, "Map<{}, {}>", i.0.0, i.1.0),
			TypeDef::Null => write!(f, "Null"),
			TypeDef::Custom(s) => write!(f, "{}", s.0),
			TypeDef::NonTyped(reason) => write!(f, "NonTyped({reason:?})"),
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

pub trait Typed {
	fn get_type(&self) -> TypeDef;
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
