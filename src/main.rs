#![feature(never_type)]
#![feature(let_chains)]

use std::{env::current_dir, hint::black_box};

use assembly_machine::{objects::{Num, Object}, FunctionData};
use nonsembly::ast::instructions::{*, control_flow::*, value::*};
// use nonsembly::ast::{Constant, ControlFlow, Instruction, Operation, Value};

mod nonsembly;

fn main() {
	// let ops: Vec<_> = match ron::from_str(&std::fs::read_to_string(current_dir().unwrap().join("assembly_machine/src/script.ron")).unwrap()) {
	// 	Ok(ops) => ops,
	// 	Err(e) => {
	// 		eprintln!("Failed to parse script: {}", e);
	// 		return;
	// 	},
	// };

	// let print = |args: &[Object]| {
	// 	if let Some(txt) = args.first() {
	// 		// black_box(
	// 		println!("{}",
	// 			indent_lines(&txt.to_string())
	// 		);
	// 	};

	// 	return None;
	// };

	// let random = |args: Vec<Object>| {
	// 	if args.len() < 2 {
	// 		return Object::Null;
	// 	}

	// 	let min = args[0].as_num().as_i64();
	// 	let max = args[1].as_num().as_i64();

	// 	return fastrand::i64(min..max).into();
	// };

	// use assembly_machine::Operation::*;

	// let ops = black_box(vec![
	// 	Halt { },
	// 	// hello
	// 	Call { arg: 0, fnc: 0 },
	// 	Exit { },
	// 	// get_greeting
	// 	Set { dst: 0, src: 1 },
	// 	Ret { ret: 0 },
	// 	// hello_1
	// 	Set { dst: 0, src: 1 },
	// 	Call { arg: 0, fnc: 0 },
	// 	// awa
	// 	Enter { arg: 0, add: 2 },
	// 	Exit { },
	// 	// _start
	// 	Reserve { cnt: 4 },
	// 	Enter { arg: 0, add: 1 },
	// 	Enter { arg: 0, add: 0 },
	// 	// Line 21
	// 	Set { dst: 0, src: 0 },
	// 	Enter { arg: 1, add: 1 },
	// 	ArPush { arr: 0, src: 1 },
	// 	Set { dst: 1, src: 2 },
	// 	ArPush { arr: 0, src: 1 },
	// 	Set { dst: 1, src: 3 },
	// 	ArPush { arr: 0, src: 1 },
	// 	// Line 27
	// 	ArIdxI { dst: 1, arr: 0, idx: 0 },
	// 	Enter { arg: 1, add: 0 },
	// 	ArIdxI { dst: 1, arr: 0, idx: 1 },
	// 	Enter { arg: 1, add: 0 },
	// 	ArIdxI { dst: 1, arr: 0, idx: 2 },
	// 	Enter { arg: 1, add: 0 },
	// 	// Line 31
	// 	Len { dst: 1, arr: 0 },
	// 	Seti { dst: 2, val: 0 },
	// 	ArIdx { dst: 3, arr: 0, idx: 2 },
	// 	Enter { arg: 3, add: 0 },
	// 	AddI { dst: 2, lhs: 2, rhs: 1 },
	// 	Neq { dst: 3, lhs: 2, rhs: 1 },
	// 	CJmpI { cnd: 3, off: -4 },
	// 	VoidA { dst: 1 },
	// 	// Line 35
	// 	Seti { dst: 1, val: 12 },
	// 	Seti { dst: 2, val: 0 },
	// 	AddI { dst: 2, lhs: 2, rhs: 1 },
	// 	Gt { dst: 3, lhs: 2, rhs: 1 },
	// 	CJmpI { cnd: 3, off: 4 },
	// 	GteI { dst: 3, lhs: 2, rhs: 50 },
	// 	CJmpI { cnd: 3, off: 4 },
	// 	JmpI { off: -5 },
	// 	AddI { dst: 1, lhs: 1, rhs: 1 },
	// 	JmpI { off: -8 },
	// 	VoidA { dst: 2 },
	// ]);

	// let data = black_box(vec![
	// 	Object::Array(assembly_machine::objects::Array::default()),
	// 	Object::String("Hello, World!".to_string()),
	// 	Object::String("Hello, Universe!".to_string()),
	// 	Object::String("Hello, Multiverse!".to_string()),
	// ]);

	// let native_funcs = black_box(vec![
	// 	assembly_machine::NativeFunctionData {
	// 		arg_count: 1, pointer: print
	// 	},
	// ]);

	// let script_funcs = black_box(vec![
	// 	FunctionData {
	// 		arg_count: 1,
	// 		reg_size: 1,
	// 		address: 1,
	// 	},
	// 	FunctionData {
	// 		arg_count: 0,
	// 		reg_size: 1,
	// 		address: 3,
	// 	},
	// 	FunctionData {
	// 		arg_count: 0,
	// 		reg_size: 1,
	// 		address: 5,
	// 	},
	// 	FunctionData {
	// 		arg_count: 0,
	// 		reg_size: 0,
	// 		address: 7,
	// 	},
	// ]);

	// let code = assembly_machine::ByteCode {
	// 	data,
	// 	native_funcs,
	// 	script_funcs,
	// 	ops,
	// 	start_index: 9,
	// };

	// let vm = assembly_machine::VM::new_from_bytecode(&code);

	// let time = std::time::Instant::now();

	// if vm.run().is_ok() {
	// 	println!(":)");
	// }

	// println!("Took: {:?}", time.elapsed());

	let script = match nonsembly::Parser::new().parse(include_str!("script.ass")) {
		Ok(s) => s,
		Err(e) => return println!("{e}"),
	};

	for instr in script.iter() {
		println!("{}\n", format_instr(instr))
		// black_box(format_instr(instr));
	}

	// let result = nonsembly::NonsemblyValidator::new(&script).validate();
	// match result {
	// 	Err((errors, warns)) => {
	// 		for w in warns {
	// 			println!("{w:?}\n")
	// 		}

	// 		for e in errors {
	// 			println!("{e:?}\n")
	// 		}
	// 	},
	// 	Ok(warns) if warns.is_empty() => println!("No warnings :D"),
	// 	Ok(warns) => {
	// 		for w in warns {
	// 			println!("{w:?}\n")
	// 		}

	// 		println!("No errors :)")
	// 	},
	// }
}

fn format_instr(instr: &Instruction) -> String {
	use nonsembly::ast::components::AssignOperator::*;
	match instr {
		Instruction::Value(value) => format_value(value),
		Instruction::Assign(Assignment { name, value }) => format!("Assigning \x1b[36m{name}\x1b[0m < {}", format_value(value)),
		Instruction::AssignOp(AssignOp { name, op, value }) => match op {
			Add => format!("Adding to \x1b[36m{name}\x1b[0m with {}", format_value(value)),
			Sub => format!("Subbing from \x1b[36m{name}\x1b[0m with {}", format_value(value)),
			Mul => format!("Multiplying \x1b[36m{name}\x1b[0m with {}", format_value(value)),
			Div => format!("Dividing \x1b[36m{name}\x1b[0m with {}", format_value(value)),
			Mod => format!("Getting rem of \x1b[36m{name}\x1b[0m with {}", format_value(value)),
			Pwr => format!("Raising \x1b[36m{name}\x1b[0m with {}", format_value(value)),
		},
		Instruction::ControlFlow(control_flow) => format_control_flow(control_flow),
		Instruction::FunctionDef(FunctionDef { name, type_def: value, args, body, .. }) => {
			let mut result = format!("fn \x1b[1;96m{name}\x1b[0m(");
			result.push_str(&args.into_iter().map(|(name, ty)| format!("\x1b[36m{name}\x1b[0m: \x1b[1;32m{ty}\x1b[0m")).collect::<Vec<String>>().join(", "));
			result.push_str(&format!(") -> \x1b[1;32m{value}\x1b[0m {{"));
			for instr in body {
				result.push_str("\n");
				result.push_str(&indent_lines(&format_instr(instr)));
			}
			result.push_str("}");
			result
		},
	}
}

fn format_value(value: &Value) -> String {
	match value {
		Value::Literal(literal) => format_const(literal),
		Value::Variable(id) => format!("\x1b[36m{id}\x1b[0m"),
		Value::FunctionCall(FunctionCall { name, args, .. }) => format!("\x1b[1;96m{name}\x1b[0m({})", args.into_iter().map(|v| format_value(v)).collect::<Vec<String>>().join(", ")),
		Value::Operation(op) => format_op(op),
	}
}

fn format_const(literal: &Constant) -> String {
	match literal {
		Constant::String(s) => format!("\x1b[32m\"{s}\"\x1b[0m"),
		Constant::Num(num) => match **num {
			Num::Int(i) => format!("\x1b[34m{i}i\x1b[0m"),
			Num::Float(f) => format!("\x1b[34m{f}f\x1b[0m"),
		},
		Constant::Bool(b) => format!("\x1b[33m{b}\x1b[0m"),
		Constant::Array(a) => {
			let (a, ty) = (&a.0, &a.1);
			let mut result = String::new();
			result.push_str("\x1b[33m[\x1b[0m");
			result.push_str(" ");

			if a.is_empty() {
				result.push_str(&ty.as_ref().expect("Empty array must have a type").to_string());
				result.push_str("\x1b[33m]\x1b[0m");
				return result;
			}
			
			result.push_str(&a.iter().map(|item| format_value(item)).collect::<Vec<String>>().join(", "));

			result.push_str("\x1b[33m ]\x1b[0m");

			if result.len() > 200 {
				result.clear();
				result.push_str("\x1b[33m[\x1b[0m");

				result.push_str("\n");
				for item in a {
					result.push_str(&indent_lines(&format_value(&item)));
					result.push_str("\n");
				}

				result.push_str("\x1b[33m]\x1b[0m");
			}

			result
		},
		Constant::Map(m) => {
			let (m, ty) = (&m.0, &m.1);
			let mut result = String::new();
			result.push_str("\x1b[33m{\x1b[0m");
			result.push_str(" ");

			if m.is_empty() {
				let (k, v) = ty.as_ref().expect("Empty map must have a type");
				result.push_str(&format!("{}: {}", k, v));
				result.push_str("\x1b[33m}\x1b[0m");
				return result;
			}

			result.push_str(&m.iter().map(|(k, v)| format_value(k) + ": " + &format_value(v)).collect::<Vec<String>>().join(", "));

			result.push_str("\x1b[33m }\x1b[0m");
			if result.len() > 200 {
				result.clear();
				result.push_str("\x1b[33m{\x1b[0m");

				result.push_str("\n");
				for (key, value) in m {
					result.push_str(&indent_lines(&format_value(&key)));
					result.push_str(": ");
					result.push_str(&indent_lines(&format_value(&value)));
					result.push_str("\n");
				}

				result.push_str("\x1b[33m}\x1b[0m");
			}

			result
		}
	}
}

fn format_op(op: &Operation) -> String {
	use Operation::*;
	match op {
		Add(a, b) => format!("({} \x1b[31m+\x1b[0m {})", format_value(a), format_value(b)),
		Sub(a, b) => format!("({} \x1b[31m-\x1b[0m {})", format_value(a), format_value(b)),
		Pwr(a, b) => format!("({} \x1b[31m^\x1b[0m {})", format_value(a), format_value(b)),
		Mul(a, b) => format!("({} \x1b[31m*\x1b[0m {})", format_value(a), format_value(b)),
		Div(a, b) => format!("({} \x1b[31m/\x1b[0m {})", format_value(a), format_value(b)),
		Mod(a, b) => format!("({} \x1b[31m%\x1b[0m {})", format_value(a), format_value(b)),
		And(a, b) => format!("({} \x1b[31m&&\x1b[0m {})", format_value(a), format_value(b)),
		Or(a, b) => format!("({} \x1b[31m||\x1b[0m {})", format_value(a), format_value(b)),
		Not(a) => format!("\x1b[31m!\x1b[0m{}", format_value(a)),
		Eq(a, b) => format!("({} \x1b[31m==\x1b[0m {})", format_value(a), format_value(b)),
		Neq(a, b) => format!("({} \x1b[31m!=\x1b[0m {})", format_value(a), format_value(b)),
		Gt(a, b) => format!("({} \x1b[31m>\x1b[0m {})", format_value(a), format_value(b)),
		Lt(a, b) => format!("({} \x1b[31m<\x1b[0m {})", format_value(a), format_value(b)),
		Gte(a, b) => format!("({} \x1b[31m>=\x1b[0m {})", format_value(a), format_value(b)),
		Lte(a, b) => format!("({} \x1b[31m<=\x1b[0m {})", format_value(a), format_value(b)),
		Incr(a) => format!("{}\x1b[31m++\x1b[0m", format_value(a)),
		Decr(a) => format!("{}\x1b[31m--\x1b[0m", format_value(a)),
		Index(t, i) => format!("{}\x1b[36m[\x1b[0m{}\x1b[36m]\x1b[0m", format_value(t), format_value(i)),
	}
}

fn format_control_flow(cf: &ControlFlow) -> String {
	match cf {
		ControlFlow::Break(br) => format!("break{}", br.id.as_ref().map(|id| format!(" \x1b[35m{id}\x1b[0m")).unwrap_or_default()),
		ControlFlow::Continue(con) => format!("continue{}", con.id.as_ref().map(|id| format!(" \x1b[35m{id}\x1b[0m")).unwrap_or_default()),
		ControlFlow::Return(ret) => format!("return{}", ret.value.as_ref().map(|v| format!(" {}", format_value(&v))).unwrap_or_default()),
		ControlFlow::If(IfBlock { cond, body, else_body, .. }) => {
			let mut result = format!("if ({}) {{", format_value(cond));
			for instr in body.iter() {
				result.push_str("\n");
				result.push_str(&indent_lines(&format_instr(&instr)));
			}
			result.push_str("}");
			if let Some(else_body) = else_body {
				result.push_str(" else {");
				for instr in else_body.iter() {
					result.push_str("\n");
					result.push_str(&indent_lines(&format_instr(&instr)));
				}
				result.push_str("}");
			}
			result
		},
		ControlFlow::While(WhileBlock { cond, body, label, .. }) => {
			let mut result = format!("while{} ({}) {{", label.as_ref().map(|l| format!(" '\x1b[35m{l}\x1b[0m")).unwrap_or_default(), format_value(cond));
			for instr in body {
				result.push_str("\n");
				result.push_str(&indent_lines(&format_instr(instr)));
			}
			result.push_str("}");
			result
		},
		ControlFlow::For(ForBlock { iter, ident, body, label, .. }) => {
			let mut result = format!("for{} (\x1b[36m{ident}\x1b[0m in {}) {{", label.as_ref().map(|l| format!(" '\x1b[35m{l}\x1b[0m")).unwrap_or_default(), format_value(iter));
			for instr in body {
				result.push_str("\n");
				result.push_str(&indent_lines(&format_instr(instr)));
			}
			result.push_str("}");
			result
		},
		ControlFlow::Loop(LoopBlock { body, label, .. }) => {
			let mut result = format!("loop{} {{", label.as_ref().map(|l| format!(" '\x1b[35m{l}\x1b[0m")).unwrap_or_default());
			for instr in body {
				result.push_str("\n");
				result.push_str(&indent_lines(&format_instr(instr)));
			}
			result.push_str("}");
			result
		},
	}
}

fn indent_lines(s: &str) -> String {
	let mut result = String::new();
	for l in s.lines() {
		result += &format!("\t{l}\n");
	}
	result
}
