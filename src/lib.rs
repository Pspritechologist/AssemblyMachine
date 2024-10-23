#![feature(debug_closure_helpers)]
#![feature(never_type)]
#![feature(ptr_as_ref_unchecked)]

pub mod objects;

use objects::Object;

#[repr(C)]
pub struct MyType {
	pub vec: Vec<i32>,
	pub string: u128,
}

pub type FnPointer = fn(Vec<Object>) -> Object;

#[derive(serde::Deserialize, serde::Serialize)]
#[derive(Debug, Clone)]
pub enum Operation {
	/// Gracefully halts the program, returning the top value of the stack to the caller.
	Halt	{ },
	/// Pops a number of items from the stack.
	Void	{ cnt: i32 },
	/// Pushes a constant from the data array at `src` to the stack.
	Push	{ src: i32 },
	/// Copies the an amount of items at the top of the stack to the top of the stack.
	Copy	{ cnt: i32 },
	/// Clones a value an amount down from the top of the stack and pushes it to the stack.
	Retr	{ cnt: i32 },
	/// Swaps the top two values of the stack.
	Swap	{ },
	/// Adds the top two values of the stack and pushes the result to the stack.
	Add		{ },
	/// Subtracts the top value of the stack from the second value and pushes the result to the stack.
	Sub		{ },
	/// Multiplies the top two values of the stack and pushes the result to the stack.
	Mul		{ },
	/// Divides the second value of the stack by the top value and pushes the result to the stack.
	Div		{ },
	/// Mods the second value of the stack by the top value and pushes the result to the stack.
	Mod		{ },
	/// Negates the top value of the stack and pushes the result to the stack.
	Neg		{ },
	/// Compares if the top two values of the stack are equal and pushes the result to the stack.
	Eq		{ },
	/// Compares if the second value of the stack is less than the top value and pushes the result to the stack.
	Lt		{ },
	/// Compares if the second value of the stack is greater than the top value and pushes the result to the stack.
	Gt		{ },
	/// Compares if the second value of the stack is less than or equal to the top value and pushes the result to the stack.
	Le		{ },
	/// Compares if the second value of the stack is greater than or equal to the top value and pushes the result to the stack.
	Ge		{ },
	/// Calls a function pointer from the function table at `src` with arguments from the top of the stack and pushes the result to the stack.  
	/// The table keeps track of how many arguments the function takes.
	Call	{ src: i32 },
	/// Jumps forward a static number of instructions.
	FwJmp	{ off: i32 },
	/// Jumps backward a static number of instructions.
	BwJmp	{ off: i32 },
	/// Jumps a number of instructions taken from the top of the stack.
	VarJmp	{ },
	/// Jumps forward a static number of instructions if the value at the top of the stack is true.
	TFJmp	{ off: i32 },
	/// Jumps backward a static number of instructions if the value at the top of the stack is true.
	TBJmp	{ off: i32 },
	/// Jumps a number of instructions taken from the top of the stack if the second value of the stack is true.
	TVJmp	{ },
	/// Prints the value at the top of the stack without popping it.
	Print	{ },
}

struct MyStruct<'a> {
	my_data: VM<'a>,
}

impl<'a> MyStruct<'_> {
	pub fn get_data(&self) -> &'a VM {
		&self.my_data
	}
}

pub struct VM<'vm> {
	data: &'vm [Object],
	stack: std::cell::UnsafeCell<Vec<Object>>,
	operations: &'vm [Operation],
	index: usize,
}

#[derive(Debug, Clone)]
pub struct ByteCode {
	pub ops: Vec<Operation>,
	pub data: Vec<Object>,
	pub start_index: usize,
}

impl<'vm> VM<'vm> {
	pub fn new(operations: &'vm [Operation], data: &'vm [Object]) -> Self {
		VM {
			data,
			operations,
			index: 0,
			stack: std::cell::UnsafeCell::new(Vec::with_capacity(1024)),
		}
	}

	pub fn new_with_cap(operations: &'vm [Operation], data: &'vm [Object], cap: usize) -> Self {
		VM {
			data,
			operations,
			index: 0,
			stack: std::cell::UnsafeCell::new(Vec::with_capacity(cap)),
		}
	}

	pub fn new_from_bytecode(bytecode: &'vm ByteCode) -> Self {
		VM {
			data: &bytecode.data,
			operations: &bytecode.ops,
			index: bytecode.start_index,
			stack: std::cell::UnsafeCell::new(Vec::with_capacity(1024)),
		}
	}

	pub fn run(mut self) -> Result<Option<Object>, ()> {
		use Operation::*;

		let mut out = std::io::stdout().lock();

		while self.index < self.operations.len() {
			// unsafe { println!("{:?}", self.stack.get().as_ref_unchecked()); }

			match self.operations[self.index] {
				Halt { } => return Ok(Some(self.pop())),
				Void { cnt } => for _ in 0..cnt { self.pop(); },
				Push { src } => self.push(self.data[src as usize].clone()),
				Copy { cnt } => for i in 0..cnt { self.push(self.get(self.stack_len() - i as usize - 1).clone()); },
				Retr { cnt } => self.push(self.get(self.stack_len() - cnt as usize).clone()),
				Swap { } => {
					let a = self.pop();
					let b = self.pop();
					self.push(a);
					self.push(b);
				},
				Add { } => self.push(self.pop().add(self.pop())),
				Sub { } => self.push((self.pop().as_num() - self.pop().as_num()).into()),
				Mul { } => self.push((self.pop().as_num() * self.pop().as_num()).into()),
				Div { } => self.push((self.pop().as_num() / self.pop().as_num()).into()),
				Mod { } => self.push((self.pop().as_num() % self.pop().as_num()).into()),
				Eq { } => self.push((self.pop() == self.pop()).into()),
				Neg { } => self.push((-self.pop().as_num()).into()),
				Lt { } => self.push((self.pop().as_num() < self.pop().as_num()).into()),
				Gt { } => self.push((self.pop().as_num() > self.pop().as_num()).into()),
				Le { } => self.push((self.pop().as_num() <= self.pop().as_num()).into()),
				Ge { } => self.push((self.pop().as_num() >= self.pop().as_num()).into()),
				Call { src } => {
					// let args: Vec<_> = (1..=args).map(|i| self.pop()).collect();
					// self.push(self.pop().as_fn_pointer()(args));
					let (fn_ptr, args) = self.data[src as usize].as_fn_pointer();
					let args = (0..args).map(|_| self.pop()).collect();
					self.push(fn_ptr(args));
				},
				FwJmp { off } => self.index += off as usize - 1,
				BwJmp { off } => self.index -= off as usize + 1,
				VarJmp { } => self.index = self.pop().as_num().as_i64() as usize,
				TFJmp { off } => if self.pop().as_bool() { self.index += off as usize - 1; },
				TBJmp { off } => if self.pop().as_bool() { self.index -= off as usize + 1; },
				TVJmp { } => if self.pop().as_bool() { self.index = self.pop().as_num().as_i64() as usize; } else { self.pop(); },
				Print { } => {
					// println!("{ }", self.registers[src]);
					std::io::Write::write_all(&mut out, &self.pop().to_string().into_bytes()).unwrap();
					// std::io::Write::write_all(&mut out, b"\n").unwrap();
					// std::hint::black_box(&self.get(src));
				},
			}

			self.index += 1;
		}

		Ok(None)
	}

	fn stack_len(&self) -> usize {
		#[cfg(debug_assertions)]
		unsafe { self.stack.get().as_ref().expect("Failed to get stack mutably").len() }

		#[cfg(not(debug_assertions))]
		unsafe { self.stack.get().as_ref_unchecked().len() }
	}

	fn push(&self, value: Object) {
		#[cfg(debug_assertions)]
		unsafe { self.stack.get().as_mut().expect("Failed to get stack mutably").push(value); }

		#[cfg(not(debug_assertions))]
		unsafe { self.stack.get().as_mut_unchecked().push(value); }
	}

	fn pop(&self) -> Object {
		#[cfg(debug_assertions)]
		unsafe { self.stack.get().as_mut().expect("Failed to get stack mutably").pop().expect("Attempted to pop from an empty stack") }

		#[cfg(not(debug_assertions))]
		unsafe { self.stack.get().as_mut_unchecked().pop().unwrap_unchecked() }
	}

	fn get(&self, index: usize) -> &Object {
		#[cfg(debug_assertions)]
		unsafe { self.stack.get().as_mut().expect("Failed to get stack mutably").get(index).unwrap_or_else(|| panic!("Attempted to access an out-of-bounds register")) }

		#[cfg(not(debug_assertions))]
		unsafe { self.stack.get().as_mut_unchecked().get_unchecked(index) }
	}

	// // #[inline(always)]
	// pub fn set(&mut self, index: usize, value: Object) {
	// 	unsafe { *self.registers.get_unchecked_mut(index) = value; }
	// }

	// // #[inline(always)]
	// pub fn get(&self, index: usize) -> &Object {
	// 	unsafe { self.registers.get_unchecked(index) }
	// }
}

pub trait UnwrapUnsafeSafe<T> {
	fn unwrap_safe(self) -> T;
}

impl<T> UnwrapUnsafeSafe<T> for Result<T, !> {
	fn unwrap_safe(self) -> T {
		unsafe { self.unwrap_unchecked() }
	}
}
