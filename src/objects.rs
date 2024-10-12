use std::sync::Arc;
use std::collections::HashMap as Map;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Object {
	Num(Num),
	String(String),
	Bool(bool),
	Array(Arc<Vec<Object>>),
	Map(Arc<Map<String, Object>>),
	Fn(crate::FnPointer),
	Null,
}

impl Object {
	pub fn add(&self, other: &Self) -> Self {
		match (self, other) {
			(Object::Num(a), Object::Num(b)) => Object::Num(*a + *b),
			(Object::String(a), Object::String(b)) => Object::String(format!("{}{}", a, b)),
			(Object::Bool(a), Object::Bool(b)) => Object::Bool(*a || *b),
			(Object::Array(a), Object::Array(b)) => {
				let mut array = (**a).clone();
				array.extend_from_slice(b);
				Object::Array(Arc::new(array))
			},
			(Object::Map(a), Object::Map(b)) => {
				let mut map = (**a).clone();
				map.extend(b.iter().map(|(k, v)| (k.clone(), v.clone())));
				Object::Map(Arc::new(map))
			},
			(non_null, Object::Null) | (Object::Null, non_null) => non_null.clone(),
			_ => panic!("Types not supported for addition: {self:?} and {other:?}"),
		}
	}

	pub fn as_num(&self) -> Num {
		match self {
			Object::Num(num) => *num,
			_ => panic!("Expected a number, got {:?}", self),
		}
	}

	pub fn as_bool(&self) -> bool {
		match self {
			Object::Bool(boolean) => *boolean,
			Object::Num(num) => *num != Num::Int(0),
			Object::String(string) => !string.is_empty(),
			Object::Array(array) => !array.is_empty(),
			Object::Map(map) => !map.is_empty(),
			Object::Null => false,
			Object::Fn(_) => false,
		}
	}

	pub fn as_fn_pointer(&self) -> crate::FnPointer {
		match self {
			Object::Fn(func) => *func,
			_ => panic!("Expected a function, got {:?}", self),
		}
	}
}

// impl Ord for Object {
// 	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
// 		match (self, other) {
// 			(Object::Num(a), Object::Num(b)) => a.cmp(b),
// 			(Object::String(a), Object::String(b)) => a.cmp(b),
// 			(Object::Bool(a), Object::Bool(b)) => a.cmp(b),
// 			(Object::Array(a), Object::Array(b)) => a.cmp(b),
// 			(Object::Map(a), Object::Map(b)) => a.len().cmp(&b.len()),
// 			(Object::Null, Object::Null) => std::cmp::Ordering::Equal,
// 			(Object::Null, _) => std::cmp::Ordering::Less,
// 			(_, Object::Null) => std::cmp::Ordering::Greater,
// 			_ => panic!("Types not supported for comparison: {:?} and {:?}", self, other),
// 		}
// 	}
// }

impl PartialOrd for Object {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		match (self, other) {
			(Object::Num(a), Object::Num(b)) => a.partial_cmp(b),
			(Object::String(a), Object::String(b)) => a.partial_cmp(b),
			(Object::Bool(a), Object::Bool(b)) => a.partial_cmp(b),
			(Object::Array(a), Object::Array(b)) => a.partial_cmp(b),
			(Object::Map(a), Object::Map(b)) => a.len().partial_cmp(&b.len()),
			(Object::Null, Object::Null) => Some(std::cmp::Ordering::Equal),
			(Object::Null, _) => Some(std::cmp::Ordering::Less),
			(_, Object::Null) => Some(std::cmp::Ordering::Greater),
			_ => None,
		}
	}
}

impl serde::Serialize for Object {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: serde::Serializer,
	{
		match self {
			Object::Num(num) => num.serialize(serializer),
			Object::String(string) => string.serialize(serializer),
			Object::Bool(boolean) => boolean.serialize(serializer),
			Object::Array(array) => array.serialize(serializer),
			Object::Map(map) => map.serialize(serializer),
			Object::Null => serializer.serialize_unit(),
			Object::Fn(_) => serializer.serialize_unit(),
		}
	}
}

impl<'de> serde::Deserialize<'de> for Object {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: serde::Deserializer<'de>,
	{
		#[derive(serde::Deserialize)]
		#[serde(untagged)]
		enum ObjectEnum {
			Num(Num),
			String(String),
			Array(Vec<Object>),
			Map(Map<String, Object>),
			Null,
		}

		let object = ObjectEnum::deserialize(deserializer)?;

		Ok(match object {
			ObjectEnum::Num(num) => Object::Num(num),
			ObjectEnum::String(string) => Object::String(string),
			ObjectEnum::Array(array) => Object::Array(Arc::new(array)),
			ObjectEnum::Map(map) => Object::Map(Arc::new(map)),
			ObjectEnum::Null => Object::Null,
		})
	}
}

impl std::fmt::Display for Object {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Object::Num(num) => write!(f, "{}", num),
			Object::String(string) => write!(f, "{}", string),
			Object::Bool(boolean) => write!(f, "{}", boolean),
			Object::Array(array) => {
				write!(f, "[ ")?;
				for item in array[..array.len().saturating_sub(1)].iter() {
					write!(f, "{}, ", item)?;
				}
				if let Some(item) = array.last() {
					write!(f, "{} ", item)?;
				}
				write!(f, "]")
			},
			Object::Map(map) => {
				let mut iter = map.iter();
				write!(f, "{{ ")?;
				for _ in [0..iter.len().saturating_sub(1)] {
					let (key, value) = iter.next().unwrap();
					write!(f, "{}: {}, ", key, value)?;
				}
				if let Some((key, value)) = iter.next() {
					write!(f, "{}: {} ", key, value)?;
				}
				write!(f, "}}")
			},
			Object::Null => write!(f, "null"),
			Object::Fn(_) => write!(f, "function"),
		}
	}
}

impl Default for Object {
	fn default() -> Self {
		Object::Null
	}
}

impl<T: Into<Num>> From<T> for Object {
	fn from(num: T) -> Self {
		Object::Num(num.into())
	}
}

impl From<String> for Object {
	fn from(string: String) -> Self {
		Object::String(string)
	}
}

impl From<&str> for Object {
	fn from(string: &str) -> Self {
		Object::String(string.to_string())
	}
}

impl From<bool> for Object {
	fn from(boolean: bool) -> Self {
		Object::Bool(boolean)
	}
}

impl From<Vec<Object>> for Object {
	fn from(array: Vec<Object>) -> Self {
		Object::Array(Arc::new(array))
	}
}

impl From<Map<String, Object>> for Object {
	fn from(map: Map<String, Object>) -> Self {
		Object::Map(Arc::new(map))
	}
}

impl From<crate::FnPointer> for Object {
	fn from(func: crate::FnPointer) -> Self {
		Object::Fn(func)
	}
}

/// A number that can be either an integer or a float.
#[derive(serde::Deserialize, serde::Serialize)]
#[derive(Clone, Copy, Debug)]
pub enum Num {
	Int(i64),
	Float(f64),
}

impl Num {
	pub fn as_i64(&self) -> i64 {
		match self {
			Num::Int(value) => *value,
			Num::Float(value) => *value as i64,
		}
	}

	pub fn as_f64(&self) -> f64 {
		match self {
			Num::Int(value) => *value as f64,
			Num::Float(value) => *value,
		}
	}
}

impl From<i64> for Num {
	fn from(value: i64) -> Self {
		Num::Int(value)
	}
}

impl From<f64> for Num {
	fn from(value: f64) -> Self {
		Num::Float(value)
	}
}

impl Default for Num {
	fn default() -> Self {
		Num::Int(0)
	}
}

impl std::fmt::Display for Num {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Num::Int(value) => write!(f, "{}", value),
			Num::Float(value) => write!(f, "{}", value),
		}
	}
}

impl Eq for Num {}
impl PartialEq for Num {
	fn eq(&self, other: &Self) -> bool {
		fn compare_i_f(a: &i64, b: &f64) -> bool {
			compare_f(&(*a as f64), b)
		}

		fn compare_f(a: &f64, b: &f64) -> bool {
			(a - b).abs() < f64::EPSILON
		}

		match (self, other) {
			(Num::Int(a), Num::Int(b)) => a == b,
			(Num::Float(a), Num::Float(b)) => {
				match (a.is_nan(), b.is_nan()) {
					(true, true) => true,
					(false, false) => compare_f(a, b),
					_ => false,
				}
			},
			(Num::Int(a), Num::Float(b)) => compare_i_f(a, b),
			(Num::Float(a), Num::Int(b)) => compare_i_f(b, a),
		}
	}
}

impl Ord for Num {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		match (self, other) {
			(Num::Int(a), Num::Int(b)) => a.cmp(b),
			(Num::Float(a), Num::Float(b)) => a.partial_cmp(b).unwrap(),
			(Num::Int(a), Num::Float(b)) => (*a as f64).partial_cmp(b).unwrap(),
			(Num::Float(a), Num::Int(b)) => a.partial_cmp(&(*b as f64)).unwrap(),
		}
	}
}

impl PartialOrd for Num {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.cmp(other))
	}
}

impl std::ops::Neg for Num {
	type Output = Num;

	fn neg(self) -> Num {
		match self {
			Num::Int(value) => Num::Int(-value),
			Num::Float(value) => Num::Float(-value),
		}
	}
}

macro_rules! impl_math_ops {
	($($trait:ident $fn:ident),*) => {
		$(
			impl std::ops::$trait for Num {
				type Output = Num;

				fn $fn(self, other: Num) -> Num {
					match (self, other) {
						(Num::Int(a), Num::Int(b)) => Num::from(a.$fn(b)),
						(Num::Float(a), Num::Float(b)) => Num::from(a.$fn(b)),
						(Num::Int(a), Num::Float(b)) => Num::from((a as f64).$fn(b)),
						(Num::Float(a), Num::Int(b)) => Num::from(a.$fn(b as f64)),
					}
				}
			}
		)*
	};
}

impl_math_ops!(Add add, Sub sub, Mul mul, Div div, Rem rem);

// macro_rules! impl_cmp_ops {
// 	($($trait:ident $fn:ident),*) => {
// 		$(
// 			impl std::cmp::$trait for Num {
// 				fn $fn(&self, other: &Num) -> bool {
// 					match (self, other) {
// 						(Num::Int(a), Num::Int(b)) => a.$fn(b),
// 						(Num::Float(a), Num::Float(b)) => a.$fn(b),
// 						(Num::Int(a), Num::Float(b)) => (*a as f64).$fn(*b),
// 						(Num::Float(a), Num::Int(b)) => *a.$fn(*b as f64),
// 					}
// 				}
// 			}
// 		)*
// 	};
// }

// impl_cmp_ops!(PartialEq eq, PartialOrd partial_cmp);
