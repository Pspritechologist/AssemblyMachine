use super::*;

#[derive(Debug, Clone, PartialEq, Eq, SpanData)]
pub struct Return {
	#[span_start]
	pub span: Span,
	#[span_end]
	pub value: Option<Value>,
}

impl Return {
	pub fn new(value: Option<Value>, span: Span) -> Self {
		Self { span, value }
	}
}

#[derive(Debug, Clone, PartialEq, Eq, SpanData)]
pub struct IfBlock {
	pub span: Span,
	pub cond: Value,
	pub body: Spanned<InstrBlock>,
	pub else_body: Option<Spanned<InstrBlock>>,
}

impl IfBlock {
	pub fn new(cond: Value, body: Spanned<InstrBlock>, else_body: Option<Spanned<InstrBlock>>, span: Span) -> Self {
		Self { span, cond, body, else_body }
	}
}

#[derive(Debug, Clone, PartialEq, Eq, SpanData)]
pub struct WhileBlock {
	pub span: Span,
	pub cond: Value,
	pub body: InstrBlock,
	pub label: Option<Ident>,
}

impl WhileBlock {
	pub fn new(cond: Value, body: InstrBlock, label: Option<Ident>, span: Span) -> Self {
		Self { span, cond, body, label }
	}
}

#[derive(Debug, Clone, PartialEq, Eq, SpanData)]
pub struct ForBlock {
	pub span: Span,
	pub iter: Value,
	pub ident: Ident,
	pub body: InstrBlock,
	pub label: Option<Ident>,
}

impl ForBlock {
	pub fn new(iter: Value, ident: Ident, body: InstrBlock, label: Option<Ident>, span: Span) -> Self {
		Self { span, iter, ident, body, label }
	}
}

#[derive(Debug, Clone, PartialEq, Eq, SpanData)]
pub struct LoopBlock {
	pub span: Span,
	pub body: InstrBlock,
	pub label: Option<Ident>,
}

impl LoopBlock {
	pub fn new(body: InstrBlock, label: Option<Ident>, span: Span) -> Self {
		Self { span, body, label }
	}
}

#[derive(Debug, Clone, PartialEq, Eq, SpanData)]
pub struct Continue {
	#[span_start]
	pub span: Span,
	#[span_end]
	pub id: Option<Ident>,
}

impl Continue {
	pub fn new(id: Option<Ident>, span: Span) -> Self {
		Self { span, id }
	}
}

#[derive(Debug, Clone, PartialEq, Eq, SpanData)]
pub struct Break {
	#[span_start]
	pub span: Span,
	#[span_end]
	pub id: Option<Ident>,
}

impl Break {
	pub fn new(id: Option<Ident>, span: Span) -> Self {
		Self { span, id }
	}
}
