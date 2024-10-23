#![feature(debug_closure_helpers)]
#![feature(never_type)]
#![feature(ptr_as_ref_unchecked)]

pub mod objects;

pub type FnPointer = fn(&[Object]) -> Option<Object>;

use objects::Object;

/// Native function index
type FnNI = u16;
/// Script function index
type FnSI = u16;
/// Constant value index
type ConI = u16;
/// Register index.
type RegI = u8;
/// Eight bit immediate value
type Imm1 = u8;
// Sixteen bit immediate value
type Imm2 = i16;

#[derive(serde::Deserialize, serde::Serialize)]
#[derive(Debug, Clone)]
pub enum Operation {
	Halt	{ },
	HaltR	{ src: RegI },
	Set		{ dst: RegI, src: ConI },
	Seti	{ dst: RegI, val: Imm2 },
	Void 	{ dst: RegI },
	VoidA	{ dst: RegI },
	VoidR	{ beg: RegI, cnt: Imm1 },
	Add		{ dst: RegI, lhs: RegI, rhs: RegI },
	AddI	{ dst: RegI, lhs: RegI, rhs: Imm1 },
	Sub		{ dst: RegI, lhs: RegI, rhs: RegI },
	SubI	{ dst: RegI, lhs: RegI, rhs: Imm1 },
	Mul		{ dst: RegI, lhs: RegI, rhs: RegI },
	MulI	{ dst: RegI, lhs: RegI, rhs: Imm1 },
	Div		{ dst: RegI, lhs: RegI, rhs: RegI },
	DivI	{ dst: RegI, lhs: RegI, rhs: Imm1 },
	Mod		{ dst: RegI, lhs: RegI, rhs: RegI },
	ModI	{ dst: RegI, lhs: RegI, rhs: Imm1 },
	Neg		{ dst: RegI, src: RegI },
	Eq		{ dst: RegI, lhs: RegI, rhs: RegI },
	EqI		{ dst: RegI, lhs: RegI, rhs: Imm1 },
	Neq		{ dst: RegI, lhs: RegI, rhs: RegI },
	NeqI	{ dst: RegI, lhs: RegI, rhs: Imm1 },
	Lt		{ dst: RegI, lhs: RegI, rhs: RegI },
	LtI		{ dst: RegI, lhs: RegI, rhs: Imm1 },
	Gt		{ dst: RegI, lhs: RegI, rhs: RegI },
	GtI		{ dst: RegI, lhs: RegI, rhs: Imm1 },
	Lte		{ dst: RegI, lhs: RegI, rhs: RegI },
	LteI	{ dst: RegI, lhs: RegI, rhs: Imm1 },
	Gte		{ dst: RegI, lhs: RegI, rhs: RegI },
	GteI	{ dst: RegI, lhs: RegI, rhs: Imm1 },
	Jmp		{ off: RegI },
	JmpI 	{ off: Imm2 },
	CJmp	{ cnd: RegI, off: RegI },
	CJmpI	{ cnd: RegI, off: Imm2 },
	Reserve	{ cnt: Imm1 },
	Enter	{ arg: RegI, add: FnSI },
	Ret		{ ret: RegI },
	Exit	{ },
	Call	{ arg: RegI, fnc: FnNI },
	ArIdx	{ dst: RegI, arr: RegI, idx: RegI },
	ArIdxI	{ dst: RegI, arr: RegI, idx: Imm1 },
	ArSet	{ arr: RegI, src: RegI, idx: RegI },
	ArSetI	{ arr: RegI, src: RegI, idx: Imm1 },
	ArPush	{ arr: RegI, src: RegI },
	Len		{ dst: RegI, arr: RegI },
}

pub struct VM<'vm> {
	data: &'vm [Object],
	script_funcs: &'vm [FunctionData],
	native_funcs: &'vm [NativeFunctionData],
	regs: std::cell::UnsafeCell<Vec<Object>>,
	reg_base: usize,
	operations: &'vm [Operation],
	index: usize,
}

#[derive(Debug, Clone)]
pub struct ByteCode {
	pub ops: Vec<Operation>,
	pub data: Vec<Object>,
	pub script_funcs: Vec<FunctionData>,
	pub native_funcs: Vec<NativeFunctionData>,
	pub start_index: usize,
}

#[derive(Debug, Clone)]
pub struct FunctionData {
	pub address: usize,
	pub arg_count: u8,
	pub reg_size: u8,
}

#[derive(Debug, Clone)]
pub struct NativeFunctionData {
	pub pointer: FnPointer,
	pub arg_count: u8,
}

impl<'vm> VM<'vm> {
	pub fn new(
		operations: &'vm [Operation],
		data: &'vm [Object],
		native_funcs: &'vm [NativeFunctionData],
		script_funcs: &'vm [FunctionData]
	) -> Self {
		VM {
			data,
			native_funcs,
			script_funcs,
			operations,
			index: 0,
			regs: std::cell::UnsafeCell::new(Vec::with_capacity(128)),
			reg_base: 0,
		}
	}

	pub fn new_with_cap(
		operations: &'vm [Operation],
		data: &'vm [Object],
		native_funcs: &'vm [NativeFunctionData],
		script_funcs: &'vm [FunctionData],
		cap: usize
	) -> Self {
		VM {
			data,
			native_funcs,
			script_funcs,
			operations,
			index: 0,
			regs: std::cell::UnsafeCell::new(Vec::with_capacity(cap)),
			reg_base: 0,
		}
	}

	pub fn new_from_bytecode(bytecode: &'vm ByteCode) -> Self {
		VM {
			data: &bytecode.data,
			native_funcs: &bytecode.native_funcs,
			script_funcs: &bytecode.script_funcs,
			operations: &bytecode.ops,
			index: bytecode.start_index,
			regs: std::cell::UnsafeCell::new(Vec::with_capacity(1024)),
			reg_base: 0,
		}
	}

	pub fn run(mut self) -> Result<Option<Object>, ()> {
		while self.index <= self.operations.len() {
			self.index += 1;

			use Operation::*;
			match self.operations[self.index - 1] {
				Halt { } => return Ok(None),
				HaltR { src } => return Ok(Some(self.get(src).clone())),
				Set { dst, src } => self.set(dst, self.data[src as usize].clone()),
				Seti { dst, val } => self.set(dst, Object::Num((val as i64).into())),
				Void { dst } => self.set(dst, Object::Null),
				VoidA { dst } => (dst..self.reg_len()).for_each(|i| self.set(i, Object::Null)),
				VoidR { beg, cnt } => (beg..(beg + cnt)).for_each(|i| self.set(i, Object::Null)),

				Add { dst, lhs, rhs } => self.set(dst, (self.get(lhs).clone().add(self.get(rhs))).into()),
				AddI { dst, lhs, rhs } => self.set(dst, (self.get(lhs).as_num() + (rhs as i64).into()).into()),
				Sub { dst, lhs, rhs } => self.set(dst, (self.get(lhs).as_num() - self.get(rhs).as_num()).into()),
				SubI { dst, lhs, rhs } => self.set(dst, (self.get(lhs).as_num() - (rhs as i64).into()).into()),
				Mul { dst, lhs, rhs } => self.set(dst, (self.get(lhs).as_num() * self.get(rhs).as_num()).into()),
				MulI { dst, lhs, rhs } => self.set(dst, (self.get(lhs).as_num() * (rhs as i64).into()).into()),
				Div { dst, lhs, rhs } => self.set(dst, (self.get(lhs).as_num() / self.get(rhs).as_num()).into()),
				DivI { dst, lhs, rhs } => self.set(dst, (self.get(lhs).as_num() / (rhs as i64).into()).into()),
				Mod { dst, lhs, rhs } => self.set(dst, (self.get(lhs).as_num() % self.get(rhs).as_num()).into()),
				ModI { dst, lhs, rhs } => self.set(dst, (self.get(lhs).as_num() % (rhs as i64).into()).into()),
				Neg { dst, src } => self.set(dst, (-self.get(src).as_num()).into()),
				Eq { dst, lhs, rhs } => self.set(dst, (self.get(lhs) == self.get(rhs)).into()),
				EqI { dst, lhs, rhs } => self.set(dst, (self.get(lhs).as_num() == (rhs as i64).into()).into()),
				Neq { dst, lhs, rhs } => self.set(dst, (self.get(lhs) != self.get(rhs)).into()),
				NeqI { dst, lhs, rhs } => self.set(dst, (self.get(lhs).as_num() != (rhs as i64).into()).into()),
				Lt { dst, lhs, rhs } => self.set(dst, (self.get(lhs).as_num() < self.get(rhs).as_num()).into()),
				LtI { dst, lhs, rhs } => self.set(dst, (self.get(lhs).as_num() < (rhs as i64).into()).into()),
				Gt { dst, lhs, rhs } => self.set(dst, (self.get(lhs).as_num() > self.get(rhs).as_num()).into()),
				GtI { dst, lhs, rhs } => self.set(dst, (self.get(lhs).as_num() > (rhs as i64).into()).into()),
				Lte { dst, lhs, rhs } => self.set(dst, (self.get(lhs).as_num() <= self.get(rhs).as_num()).into()),
				LteI { dst, lhs, rhs } => self.set(dst, (self.get(lhs).as_num() <= (rhs as i64).into()).into()),
				Gte { dst, lhs, rhs } => self.set(dst, (self.get(lhs).as_num() >= self.get(rhs).as_num()).into()),
				GteI { dst, lhs, rhs } => self.set(dst, (self.get(lhs).as_num() >= (rhs as i64).into()).into()),

				Jmp { off } => self.index = (self.index as i64 + self.get(off).as_num().as_i64()) as usize,
				JmpI { off } => self.index = (self.index as i64 + off as i64) as usize,
				CJmp { cnd, off } => if self.get(cnd).as_bool() { self.index = (self.index as i64 + self.get(off).as_num().as_i64()) as usize },
				CJmpI { cnd, off } => if self.get(cnd).as_bool() { self.index = (self.index as i64 + off as i64) as usize },
				Reserve { cnt } => {
					let regs = self.regs.get_mut();

					// regs.reserve(cnt as usize + 1);

					// Pushes the old base to the register stack so it can be restored later.
					// regs.push(Object::Num((self.reg_base as i64).into()));

					// Adjusts the reg base so that the new Registers are seen as 0..cnt.
					self.reg_base = regs.len();

					// Pushes a number of empty registers to the register stack.
					(0..cnt).for_each(|_| regs.push(Object::Null));	
				},
				Enter { arg: arg_index, add } => {
					#[cfg(debug_assertions)]
					let func = self.script_funcs.get(add as usize).expect("Function not found");
					#[cfg(not(debug_assertions))]
					let func = unsafe { self.script_funcs.get_unchecked(add as usize) };

					//FIXME: Cleanup
					let arg_start = self.reg_base + arg_index as usize;
					let args = unsafe { &self.regs.get().as_ref().expect("awa")[arg_start..arg_start + func.arg_count as usize] };

					let regs = self.regs.get_mut();

					regs.reserve(func.reg_size as usize + 3);

					// Pushes the return address for the function.
					regs.push(Object::Num(((arg_index + func.arg_count) as i64).into()));
					// Pushes the current execution index.
					regs.push(Object::Num((self.index as i64).into()));
					// Pushes the old base to the register stack so it can be restored later.
					regs.push(Object::Num((self.reg_base as i64).into()));

					// Sets the new execution index to the start of the function.
					self.index = func.address;

					// Adjusts the reg base so that the new Registers are seen as 0..cnt.
					self.reg_base = regs.len();

					// Pushes a number of empty registers to the register stack as specified by the function.
					(0..func.reg_size).for_each(|_| regs.push(Object::Null));

					// Set the arguments to the new registers.
					args.into_iter().cloned().enumerate().for_each(|(i, arg)| self.set(i as u8, arg));
				},
				Ret { ret } => {
					let value = self.get(ret).clone();

					let regs = self.regs.get_mut();

					// Restores the old base.
					regs.truncate(self.reg_base as usize);
					self.reg_base = regs.pop().unwrap().as_num().as_i64() as usize;
					// Restores the old execution index.
					self.index = regs.pop().unwrap().as_num().as_i64() as usize;
					// Sets the return register to the value of the function.
					let ret = regs.pop().unwrap().as_num().as_i64() as u8;
					self.set(ret, value);
				},
				Exit { } => {
					let regs = self.regs.get_mut();

					// Restores the old base.
					regs.truncate(self.reg_base as usize);
					self.reg_base = regs.pop().unwrap().as_num().as_i64() as usize;
					// Restores the old execution index.
					self.index = regs.pop().unwrap().as_num().as_i64() as usize;
					// Pop the return address.
					regs.pop();
				},
				Call { arg, fnc } => {
					#[cfg(debug_assertions)]
					let func = self.native_funcs.get(fnc as usize).expect("Function not found");
					#[cfg(not(debug_assertions))]
					let func = unsafe { self.native_funcs.get_unchecked(fnc as usize) };

					//FIXME: Cleanup
					let args = unsafe { &self.regs.get().as_ref().expect("awa")[self.reg_base..self.reg_base + func.arg_count as usize] };

					if let Some(ret) = (func.pointer)(args) {
						self.set(arg + func.arg_count, ret);
					}
				},

				ArIdx { dst, arr, idx } => self.set(dst, self.get(arr).as_array().as_ref().borrow()[self.get(idx).as_num().as_i64() as usize].clone()),
				ArIdxI { dst, arr, idx } => self.set(dst, self.get(arr).as_array().as_ref().borrow()[idx as usize].clone()),
				ArSet { arr, src, idx } => self.get(arr).as_array().borrow_mut()[self.get(idx).as_num().as_i64() as usize] = self.get(src).clone(),
				ArSetI { arr, src, idx } => self.get(arr).as_array().borrow_mut()[idx as usize] = self.get(src).clone(),
				ArPush { arr, src } => self.get(arr).as_array().borrow_mut().push(self.get(src).clone()),
				Len { dst, arr } => self.set(dst, Object::Num((self.get(arr).as_array().as_ref().borrow().len() as i64).into())),
			}
		}

		Ok(None)
	}

	fn get(&self, index: u8) -> &Object {
		#[cfg(debug_assertions)]
		unsafe {
			self.regs.get()
				.as_ref()
				.expect("Failed to get registers")
				.get(self.reg_base + index as usize)
				.expect("Attempted to access an out-of-bounds register")
		}

		#[cfg(not(debug_assertions))]
		unsafe { self.regs.get().as_ref_unchecked().get_unchecked(self.reg_base + index as usize) }
	}

	fn set(&mut self, index: u8, value: Object) {
		#[cfg(debug_assertions)]
		{
			*self.regs.get_mut()
				// .as_mut()
				// .expect("Failed to get registers")
				.get_mut(self.reg_base + index as usize)
				.expect("Attempted to access an out-of-bounds register")
				= value;
		}

		#[cfg(not(debug_assertions))]
		unsafe { *self.regs.get().as_mut_unchecked().get_unchecked_mut(self.reg_base + index as usize) = value; }
	}

	fn reg_len(&self) -> u8 {
		#[cfg(debug_assertions)]
		unsafe { (self.regs.get().as_ref().expect("Failed to get registers").len() - self.reg_base) as u8 }

		#[cfg(not(debug_assertions))]
		unsafe { (self.regs.get().as_ref_unchecked().len() - self.reg_base) as u8 }
	}

	// fn set

	// #[inline(always)]
	// pub fn set(&mut self, index: u8, value: Object) {
	// 	self.regs.get_mut().
	// }

	// // #[inline(always)]
	// pub fn get(&self, index: u8) -> &Object {
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
