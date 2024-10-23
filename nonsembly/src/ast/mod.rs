use assembly_machine::objects::Num;
use span::Span;
use components::*;

pub mod instructions;
pub mod components;
pub mod span;

pub type InstrBlock = Vec<instructions::Instruction>;

impl<T: SpanData> SpanData for Vec<T> {
	fn get_span(&self) -> Span {
		if self.is_empty() {
			Span::new_invalid()
		} else {
			Span::expand(self.first().unwrap().get_span(), &self.last().unwrap().get_span())
		}
	}
}
