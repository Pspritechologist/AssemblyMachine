use std::collections::HashMap;

use assembly_machine::{objects::Object, FnPointer};

use super::{Constant, ControlFlow, Instruction, Script, Value};

#[derive(Debug, Clone)]
pub struct ByteCode {
	pub ops: Vec<Instruction>,
	pub data: Vec<Object>,
	pub start_index: usize,
}

#[derive(Debug, Clone)]
pub struct NonsemblyCompiler {
	ops: Vec<Instruction>,

	func_ops: Vec<Instruction>,
	func_arg_index: usize,
	functions: HashMap<String, FuncData>,

	data: Vec<Constant>,
}

impl NonsemblyCompiler {
	pub fn compile(mut self, input: Script) -> ByteCode {

		for instr in input {
			self.handle_instruction(instr);
		}

		ByteCode {
			ops: self.ops,
			data: self.data.into_iter().map(const_to_obj).collect(),
			start_index: 0,
		}
	}

	fn handle_instruction(&mut self, instr: Instruction) {
		
		match instr {
			Instruction::Assign { name, value, .. } => {
				
			},
			Instruction::AssignOp { name, value, op, .. } => {

			},
			Instruction::FunctionDef { name, args, body, .. } => {
				self.generate_function(name, args.into_iter().map(|(s, ..)| s).collect(), body);
			},
			Instruction::Value { value, .. } => {

			},
			Instruction::ControlFlow { control_flow, .. } => {

			},
		}
	}

	fn generate_function(&mut self, name: String, args: Vec<String>, body: Vec<Instruction>) {
		let ret_i = self.functions.len();
	}

	fn get_data_index(&mut self, data: Constant) -> usize {
		self.data.iter().position(|d| d == &data).unwrap_or_else(|| {
			let index = self.data.len();
			self.data.push(data);
			index
		})
	}
}

fn const_to_obj(constant: Constant) -> Object {
	match constant {
		Constant::Num(num) => Object::Num(num),
		Constant::String(string) => Object::String(string),
		Constant::Bool(boolean) => Object::Bool(boolean),
		_ => panic!("This function should never be passed a runtime-only constant"),
	}
}

#[derive(Debug, Clone)]
enum DataConst {
	Constant(Constant),
	NativeFn(FnPointer),
}

#[derive(Debug, Clone)]
struct FuncData {
	arg_index: usize,
	args: usize,
	index: usize,
}
