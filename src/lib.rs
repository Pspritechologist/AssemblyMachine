#![feature(debug_closure_helpers)]
#![feature(never_type)]

pub mod objects;

use objects::Object;

#[repr(C)]
pub struct MyType {
	pub vec: Vec<i32>,
	pub string: u128,
}

pub type FnPointer = fn(Vec<&Object>) -> Object;

#[derive(serde::Deserialize, serde::Serialize)]
pub enum Operations {
	/// Gracefully halts the program, returning the value of a register to the caller.
	Halt	{ src: usize },
	/// Sets the value of a register to null, voiding it.
	Void	{ dst: usize },
	/// Sets the value of a register to a constant from the data array at `src`.
	Set		{ dst: usize, src: usize },
	/// Copies the value of a register to another register.
	Copy	{ dst: usize, src: usize },
	/// Adds the values of two registers and stores the result in another register.
	Add		{ dst: usize, a: usize, b: usize },
	/// Subtracts the value of one register from another and stores the result in another register.
	Sub		{ dst: usize, a: usize, b: usize },
	/// Multiplies the values of two registers and stores the result in another register.
	Mul		{ dst: usize, a: usize, b: usize },
	/// Divides the value of one register by another and stores the result in another register.
	Div		{ dst: usize, a: usize, b: usize },
	/// Mods the value of one register by another and stores the result in another register.
	Mod		{ dst: usize, a: usize, b: usize },
	/// Negates the value of a register and stores the result in another register.
	Neg		{ dst: usize, src: usize },
	/// Compares the values of two registers and stores the result in another register.
	Eq		{ dst: usize, a: usize, b: usize },
	/// Compares the values of two registers and stores the result in another register.
	Lt		{ dst: usize, a: usize, b: usize },
	/// Compares the values of two registers and stores the result in another register.
	Gt		{ dst: usize, a: usize, b: usize },
	/// Compares the values of two registers and stores the result in another register.
	Le		{ dst: usize, a: usize, b: usize },
	/// Compares the values of two registers and stores the result in another register.
	Ge		{ dst: usize, a: usize, b: usize },
	/// Calls a function pointer in one register and stores the result in another register.  
	/// Args are taken from a number of registers following `src` based on `args`.
	Call	{ dst: usize, src: usize, arg: usize },
	/// Jumps forward a static number of instructions.
	FwJmp	{ off: usize },
	/// Jumps backward a static number of instructions.
	BwJmp	{ off: usize },
	/// Jumps a number of instructions specified by a register.
	RegJmp	{ src: usize },
	/// Jumps forward a static number of instructions if a register is true.
	TFJmp	{ tst: usize, off: usize },
	/// Jumps backward a static number of instructions if a register is true.
	TBJmp	{ tst: usize, off: usize },
	/// Jumps a number of instructions specified by a register if another register is true.
	TRJmp	{ tst: usize, src: usize },
	/// Prints the value of a register.
	Print	{ src: usize },
}

pub struct VM<'vm, const REGS: usize = { usize::MAX / 2 }> {
	data: Vec<Object>,
	registers: [Object; REGS],
	operations: &'vm [Operations],
	index: usize,
}

impl<'vm, const REGS: usize> VM<'vm, REGS> {
	pub fn new(operations: &'vm [Operations], data: Vec<Object>) -> VM<'vm, REGS> {
		VM {
			data,
			operations,
			registers: [const { Object::Null }; REGS],
			index: 0,
		}
	}

	pub fn run(mut self) -> Result<Option<Object>, ()> {
		use Operations::*;

		let mut out = std::io::stdout().lock();

		while self.index < self.operations.len() {
			match self.operations[self.index] {
				Halt { src } => return Ok(Some(self.get(src).clone())),
				Void { dst } => self.set(dst, Object::Null),
				Set { dst, src } => self.set(dst, self.data[src].clone()),
				Copy { dst, src } => self.set(dst, self.get(src).clone()),
				Add { dst, a, b } => self.set(dst, self.get(a).add(self.get(b))),
				Sub { dst, a, b } => self.set(dst, (self.get(a).as_num() - self.get(b).as_num()).into()),
				Mul { dst, a, b } => self.set(dst, (self.get(a).as_num() * self.get(b).as_num()).into()),
				Div { dst, a, b } => self.set(dst, (self.get(a).as_num() / self.get(b).as_num()).into()),
				Mod { dst, a, b } => self.set(dst, (self.get(a).as_num() % self.get(b).as_num()).into()),
				Eq { dst, a, b } => self.set(dst, (self.get(a) == self.get(b)).into()),
				Neg { dst, src } => self.set(dst, (-self.get(src).as_num()).into()),
				Lt { dst, a, b } => self.set(dst, (self.get(a).as_num() < self.get(b).as_num()).into()),
				Gt { dst, a, b } => self.set(dst, (self.get(a).as_num() > self.get(b).as_num()).into()),
				Le { dst, a, b } => self.set(dst, (self.get(a).as_num() <= self.get(b).as_num()).into()),
				Ge { dst, a, b } => self.set(dst, (self.get(a).as_num() >= self.get(b).as_num()).into()),
				Call { dst, src, arg: args } => {
					let args: Vec<_> = (1..=args).map(|i| self.get(src + i)).collect();
					self.set(dst, self.get(src).as_fn_pointer()(args));
				},
				FwJmp { off: offset } => self.index += offset as usize - 1,
				BwJmp { off: offset } => self.index -= offset as usize + 1,
				RegJmp { src } => self.index = self.get(src).as_num().as_i64() as usize,
				TFJmp { tst, off: offset } => if self.get(tst).as_bool() { self.index += offset as usize - 1; },
				TBJmp { tst, off: offset } => if self.get(tst).as_bool() { self.index -= offset as usize + 1; },
				TRJmp { tst, src } => if self.get(tst).as_bool() { self.index = self.get(src).as_num().as_i64() as usize; },
				Print { src } => {
					// println!("{}", self.registers[src]);
					std::io::Write::write_all(&mut out, &self.registers[src].to_string().into_bytes()).unwrap();
					// std::io::Write::write_all(&mut out, b"\n").unwrap();
					// std::hint::black_box(&self.get(src));
				},
			}

			self.index += 1;
		}

		Ok(None)
	}

	// #[inline(always)]
	pub fn get(&self, index: usize) -> &Object {
		unsafe { self.registers.get_unchecked(index) }
	}

	// #[inline(always)]
	pub fn set(&mut self, index: usize, value: Object) {
		unsafe { *self.registers.get_unchecked_mut(index) = value; }
	}
}

pub trait UnwrapUnsafeSafe<T> {
	fn unwrap_safe(self) -> T;
}

impl<T> UnwrapUnsafeSafe<T> for Result<T, !> {
	fn unwrap_safe(self) -> T {
		unsafe { self.unwrap_unchecked() }
	}
}
