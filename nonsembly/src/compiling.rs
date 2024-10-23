// use std::collections::HashMap;

// use assembly_machine::{objects::Object, ByteCode, FnPointer, Operation as Op};

// use super::{Constant, ControlFlow, Instruction, Operation, Script, Value};

// #[derive(Debug, Clone)]
// pub struct NonsemblyCompiler {
// 	ops: Vec<Op>,

// 	func_ops: Vec<Op>,
// 	func_arg_index: usize,
// 	functions: HashMap<String, FuncData>,

// 	data: Vec<Constant>,
// }

// impl NonsemblyCompiler {
// 	pub fn compile(mut self, input: Script) -> ByteCode {

// 		for instr in input {
// 			self.handle_instruction(instr);
// 		}

// 		ByteCode {
// 			ops: self.ops,
// 			data: self.data.into_iter().map(const_to_obj).collect(),
// 			start_index: 0,
// 		}
// 	}

// 	fn handle_instruction(&mut self, instr: Instruction) {
		
// 		match instr {
// 			Instruction::Assign { name, value, .. } => {
				
// 			},
// 			Instruction::AssignOp { name, value, op, .. } => {

// 			},
// 			Instruction::FunctionDef { name, args, body, .. } => {
// 				// self.generate_function(name, args.into_iter().map(|(s, ..)| s).collect(), body);
// 			},
// 			Instruction::Value { value, .. } => {

// 			},
// 			Instruction::ControlFlow { control_flow, .. } => {

// 			},
// 		}
// 	}

// 	fn generate_function(&mut self, name: String, args: Vec<String>, body: Vec<Instruction>) {

// 	}

// 	fn generate_value_ops(&mut self, value: Value) -> Vec<Op> {
// 		match value {
// 			Value::Literal { value } => {
// 				vec![]
// 			},
// 			Value::Variable { id } => {
// 				vec![]
// 			},
// 			Value::Operation(op) => self.generate_operation_ops(*op),
// 			Value::FunctionCall { name, args } => {
// 				vec![]
// 			},
// 		}
// 	}

// 	fn generate_operation_ops(&mut self, op: Operation) -> Vec<Op> {
// 		match op {
// 			Operation::Add(a, b) => {
// 				vec![]
// 			},
// 			Operation::Eq(a, b) => {
// 				vec![]
// 			},
// 			Operation::Neq(a, b) => {
// 				vec![]
// 			},
// 			Operation::Lt(a, b) => {
// 				vec![]
// 			},
// 			Operation::Gt(a, b) => {
// 				vec![]
// 			},
// 			Operation::Lte(a, b) => {
// 				vec![]
// 			},
// 			Operation::Gte(a, b) => {
// 				vec![]
// 			},
// 			Operation::Sub(a, b) => {
// 				vec![]
// 			},
// 			Operation::Pwr(a, b) => {
// 				vec![]
// 			},
// 			Operation::Mul(a, b) => {
// 				vec![]
// 			},
// 			Operation::Div(a, b) => {
// 				vec![]
// 			},
// 			Operation::Mod(a, b) => {
// 				vec![]
// 			},
// 			Operation::Incr(n) => {
// 				vec![]
// 			},
// 			Operation::Decr(n) => {
// 				vec![]
// 			},
// 			Operation::And(a, b) => {
// 				vec![]
// 			},
// 			Operation::Or(a, b) => {
// 				vec![]
// 			},
// 			Operation::Not(b) => {
// 				vec![]
// 			},
// 			Operation::Index(t, i) => {
// 				vec![]
// 			},
// 		}
// 	}

// 	fn get_data_index(&mut self, data: Constant) -> usize {
// 		self.data.iter().position(|d| d == &data).unwrap_or_else(|| {
// 			let index = self.data.len();
// 			self.data.push(data);
// 			index
// 		})
// 	}
// }

// fn const_to_obj(constant: Constant) -> Object {
// 	match constant {
// 		Constant::Num(num) => Object::Num(num),
// 		Constant::String(string) => Object::String(string),
// 		Constant::Bool(boolean) => Object::Bool(boolean),
// 		_ => panic!("This function should never be passed a runtime-only constant"),
// 	}
// }

// #[derive(Debug, Clone)]
// enum DataConst {
// 	Constant(Constant),
// 	NativeFn(FnPointer),
// }

// #[derive(Debug, Clone)]
// struct FuncData {
// 	arg_index: usize,
// 	args: usize,
// 	index: usize,
// }

// #[derive(Debug, Clone)]
// struct VarData {
// 	index: usize,
	
// }
