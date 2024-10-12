#![feature(never_type)]
#![feature(let_chains)]

use std::hint::black_box;

use assembly_machine::objects::{Num, Object};
use nonsembly::ast::{Constant, ControlFlow, Instruction, Operation, Value};

mod nonsembly;

fn main() {
	// let ops: Vec<_> = match ron::from_str(include_str!("script.ron")) {
	// 	Ok(ops) => ops,
	// 	Err(e) => {
	// 		eprintln!("Failed to parse script: {}", e);
	// 		return;
	// 	},
	// };
	// let vm = assembly_machine::VM::<16>::new(&ops, vec![ 50.into(), 27.into(), 1000000.into(), 1.into(), 0.into() ]);

	let print = |args: Vec<&Object>| {
		if let Some(txt) = args.first() {
			black_box(indent_lines(&txt.to_string()));
		};

		return Object::Null;
	};

	let random = |args: Vec<&Object>| {
		if args.len() < 2 {
			return Object::Null;
		}

		let min = args[0].as_num().as_i64();
		let max = args[1].as_num().as_i64();

		return fastrand::i64(min..max).into();
	};

	use assembly_machine::Operations::*;

	// Reg 00: Print
	// Reg 01: Print arg
	// Reg 02: Random
	// Reg 03: Random arg min
	// Reg 04: Random arg max
	// Reg 05: Random result
	// Reg 06: 0
	// Reg 07: 1
	// Reg 08: Counter
	// Reg 09: Counter test
	let ops = vec![
		// Setup constants and such.
		Set { dst: 0, src: 0 },
		Set { dst: 2, src: 1 },
		Set { dst: 6, src: 2 },
		Set { dst: 7, src: 3 },
		Set { dst: 8, src: 4 },

		// Add three numbers function.
		// Three args starting at 11.
		// Return offset at 10.
		

		// Timer increment.
		Sub { dst: 8, a: 8, b: 7 },
		Le { dst: 9, a: 8, b: 6 },
		TFJmp { tst: 9, off: 999 },

		// Generate random number.
		Copy { dst: 3, src: 6 },
		Copy { dst: 4, src: 8 },
		Call { dst: 5, src: 2, arg: 2 },
		Void { dst: 3 },
		Void { dst: 4 },

		// Print random number.
		Copy { dst: 1, src: 5 },
		Call { dst: 10, src: 0, arg: 1 },
		Void { dst: 1 },

		// Jump back to timer increment.
		BwJmp { off: 11 },
	];

	let data = vec![
		Object::Fn(print),
		Object::Fn(random),
		Object::Num(0.into()),
		Object::Num(1.into()),
		Object::Num(100_000.into()),
	];

	let vm = assembly_machine::VM::<128>::new(&ops, data);

	if vm.run().is_ok() {
		println!(":)");
	}

	// let script = match nonsembly::Parser::new().parse(include_str!("script.ass")) {
	// 	Ok(s) => s,
	// 	Err(e) => return println!("{e}"),
	// };

	// for instr in script.iter() {
	// 	println!("{}\n", format_instr(instr))
	// 	// black_box(format_instr(instr));
	// }

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
	use nonsembly::ast::AssignOp;
	match instr {
		Instruction::Value { value, .. } => format_value(value),
		Instruction::Assign { name, value, .. } => format!("Assigning \x1b[36m{name}\x1b[0m < {}", format_value(value)),
		Instruction::AssignOp { name, value, op, .. } => match op {
			AssignOp::Add => format!("Adding to \x1b[36m{name}\x1b[0m with {}", format_value(value)),
			AssignOp::Sub => format!("Subbing from \x1b[36m{name}\x1b[0m with {}", format_value(value)),
			AssignOp::Mul => format!("Multiplying \x1b[36m{name}\x1b[0m with {}", format_value(value)),
			AssignOp::Div => format!("Dividing \x1b[36m{name}\x1b[0m with {}", format_value(value)),
			AssignOp::Mod => format!("Getting rem of \x1b[36m{name}\x1b[0m with {}", format_value(value)),
			AssignOp::Pwr => format!("Raising \x1b[36m{name}\x1b[0m with {}", format_value(value)),
		},
		Instruction::ControlFlow { control_flow, .. } => format_control_flow(control_flow),
		Instruction::FunctionDef { name, value, args, body, .. } => {
			let mut result = format!("fn \x1b[1;96m{name}\x1b[0m(");
			result.push_str(&args.into_iter().map(|(name, ty, _)| format!("\x1b[36m{name}\x1b[0m: \x1b[1;32m{ty}\x1b[0m")).collect::<Vec<String>>().join(", "));
			result.push_str(&format!(") -> \x1b[1;32m{value}\x1b[0m"));
			result.push_str(" {\n");
			for instr in body {
				result.push_str(&indent_lines(&format_instr(instr)));
				result.push_str("\n");
			}
			result.push_str("}");
			result
		},
	}
}

fn format_value(value: &Value) -> String {
	match value {
		Value::Literal { value: literal } => format_const(literal),
		Value::Variable { id } => format!("\x1b[36m{id}\x1b[0m"),
		Value::FunctionCall { name, args} => format!("\x1b[1;96m{name}\x1b[0m({})", args.into_iter().map(|v| format_value(v)).collect::<Vec<String>>().join(", ")),
		Value::Operation(op) => format_op(op),
	}
}

fn format_const(literal: &Constant) -> String {
	match literal {
		Constant::String(s) => format!("\x1b[32m\"{s}\"\x1b[0m"),
		Constant::Num(Num::Float(f)) => format!("\x1b[34m{f}f\x1b[0m"),
		Constant::Num(Num::Int(i)) =>  format!("\x1b[34m{i}i\x1b[0m"),
		Constant::Bool(b) => format!("\x1b[33m{b}\x1b[0m"),
		Constant::Array(a, ty) => {
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
					result.push_str(&indent_lines(&format_value(item)));
					result.push_str("\n");
				}

				result.push_str("\x1b[33m]\x1b[0m");
			}

			result
		},
		Constant::Map(m, ty) => {
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
					result.push_str(&indent_lines(&format_value(key)));
					result.push_str(": ");
					result.push_str(&indent_lines(&format_value(value)));
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
		ControlFlow::Break { id } => format!("break{}", id.as_ref().map(|id| format!(" \x1b[35m{id}\x1b[0m")).unwrap_or_default()),
		ControlFlow::Continue { id } => format!("continue{}", id.as_ref().map(|id| format!(" \x1b[35m{id}\x1b[0m")).unwrap_or_default()),
		ControlFlow::Return { value } => format!("return{}", value.as_ref().map(|v| format!(" {}", format_value(&v))).unwrap_or_default()),
		ControlFlow::If { cond, body, else_body } => {
			let mut result = format!("if ({})", format_value(cond));
			result.push_str(" {\n");
			for instr in body {
				result.push_str(&indent_lines(&format_instr(instr)));
				result.push_str("\n");
			}
			result.push_str("}");
			if let Some(else_body) = else_body {
				result.push_str(" else {\n");
				for instr in else_body {
					result.push_str(&indent_lines(&format_instr(instr)));
					result.push_str("\n");
				}
				result.push_str("}");
			}
			result
		},
		ControlFlow::While { cond, body, label } => {
			let mut result = format!("while{} ({})", label.as_ref().map(|l| format!(" '\x1b[36m{l}\x1b[0m")).unwrap_or_default(), format_value(cond));
			result.push_str(" {\n");
			for instr in body {
				result.push_str(&indent_lines(&format_instr(instr)));
				result.push_str("\n");
			}
			result.push_str("}");
			result
		},
		ControlFlow::For { iter, ident, body, label } => {
			let mut result = format!("for{} (\x1b[36m{ident}\x1b[0m in {})", label.as_ref().map(|l| format!(" '\x1b[36m{l}\x1b[0m")).unwrap_or_default(), format_value(iter));
			result.push_str(" {\n");
			for instr in body {
				result.push_str(&indent_lines(&format_instr(instr)));
				result.push_str("\n");
			}
			result.push_str("}");
			result
		},
		ControlFlow::Loop { body, label } => {
			let mut result = format!("loop{}", label.as_ref().map(|l| format!(" '\x1b[36m{l}\x1b[0m")).unwrap_or_default());
			result.push_str(" {\n");
			for instr in body {
				result.push_str(&indent_lines(&format_instr(instr)));
				result.push_str("\n");
			}
			result.push_str("}");
			result
		},
	}
}

fn indent_lines(s: &str) -> String {
	s.lines().map(|l| "\t".to_string() + l).collect::<Vec<String>>().join("\n")
}
