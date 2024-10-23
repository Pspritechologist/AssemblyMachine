use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
	valid: bool,
	pub l: usize,
	pub r: usize,
}

impl Span {
	pub const INVALID: Span = Span { valid: false, l: 0, r: 0 };
	
	/// Returns a new span from l to r.
	pub fn new(l: usize, r: usize) -> Self {
		Span { valid: true, l, r }
	}

	/// Returns a new span from the start of the first span to the end of the second span.
	pub fn expand(self, second: &Span) -> Self {
		Span::new(self.l, second.r)
	}
	
	/// Returns an invalid span.
	pub const fn new_invalid() -> Self {
		Self::INVALID
	}

	/// Returns whether the span is valid.
	pub fn is_valid(&self) -> bool {
		self.valid
	}

	/// Returns the span as a tuple of (left, right) if it is valid.
	pub fn get(&self) -> Option<(usize, usize)> {
		if self.valid {
			Some((self.l, self.r))
		} else {
			None
		}
	}
}

impl From<(usize, usize)> for Span {
	fn from((l, r): (usize, usize)) -> Self {
		Span::new(l, r)
	}
}

impl SpanData for Span {
	fn get_span(&self) -> Span {
		*self
	}
}
