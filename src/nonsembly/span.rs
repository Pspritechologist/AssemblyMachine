#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
	pub l: usize,
	pub r: usize,
}

impl Span {
	pub fn new(l: usize, r: usize) -> Self {
		Span { l, r }
	}
}

impl From<(usize, usize)> for Span {
	fn from((l, r): (usize, usize)) -> Self {
		Span::new(l, r)
	}
}
