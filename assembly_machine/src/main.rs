use std::{env::current_dir, hint::black_box};

fn main() {
	// let ops: Vec<_> = match ron::from_str(&std::fs::read_to_string(current_dir().unwrap().join("script.ron")).unwrap()) {
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
}
