pub mod value;
pub mod control_flow;

use super::*;
use assembly_machine_macros::{enum_from, SpanData};
use control_flow::Return;
use value::*;
use control_flow::*;

#[derive(Debug, Clone, PartialEq, Eq, SpanData)]
#[enum_from]
pub enum Instruction {
	FunctionDef(FunctionDef),
	Assign(Assignment),
	AssignOp(AssignOp),
	Value(value::Value),
	ControlFlow(ControlFlow),
}

#[derive(Debug, Clone, PartialEq, Eq, SpanData)]
pub struct FunctionDef {
	pub span: Span,
	pub name: Ident,
	pub type_def: Spanned<TypeDef>,
	pub args: Vec<(Ident, Spanned<TypeDef>)>,
	pub body: Vec<Instruction>,
}

impl FunctionDef {
	pub fn new(name: Ident, type_def: Spanned<TypeDef>, args: Vec<(Ident, Spanned<TypeDef>)>, body: Vec<Instruction>, span: Span) -> Self {
		Self { span, name, type_def, args, body }
	}
}

#[derive(Debug, Clone, PartialEq, Eq, SpanData)]
pub struct Assignment {
	#[span_start]
	pub name: Ident,
	#[span_end]
	pub value: Value,
}

impl Assignment {
	pub fn new(name: Ident, value: Value) -> Self {
		Self { name, value }
	}
}

#[derive(Debug, Clone, PartialEq, Eq, SpanData)]
pub struct AssignOp {
	#[span_start]
	pub name: Ident,
	#[span_end]
	pub value: Value,
	pub op: AssignOperator,
}

impl AssignOp {
	pub fn new(name: Ident, value: Value, op: AssignOperator) -> Self {
		Self { name, value, op }
	}
}

#[derive(Debug, Clone, PartialEq, Eq, SpanData)]
#[enum_from]
pub enum ControlFlow {
	Return(Return),
	If(IfBlock),
	While(WhileBlock),
	For(ForBlock),
	Loop(LoopBlock),
	Continue(Continue),
	Break(Break),
}
