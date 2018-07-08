extern crate byteorder;
extern crate im;

use im::Vector;
use im::HashMap;
use std::char;
use std::fs::File;
use std::io::Cursor;
use std::io::Read;
use byteorder::{LittleEndian, ReadBytesExt};

const MODULO: u16 = 32768;

type Registers = HashMap<u8, Value>;
type Program = Vector<Op>;
type RawProgram = Vector<RawValue>;
type Stack = Vector<Value>;

#[derive(Copy,Clone,Debug)]
struct RawValue(u16);

#[derive(Copy,Clone,Debug)]
struct RegisterPos(u8);

#[derive(Copy,Clone,Debug)]
struct Value(u16);

#[derive(Debug)]
enum Out {
	Continue(usize, Registers, Stack),
	FailedToParse,
	Halted,
	InvalidRegister,
	Success,
}

#[derive(Copy,Clone,Debug)]
enum Op {
	Stop,
	Set(RawValue, RawValue),
	Push(RawValue),
	Pop(RawValue),
	Eq(RawValue, RawValue, RawValue),
	Gt(RawValue, RawValue, RawValue),
	Jmp(RawValue),
	Jt(RawValue, RawValue),
	Jf(RawValue, RawValue),
	Add(RawValue, RawValue, RawValue),
	Mult(RawValue, RawValue, RawValue),
	Mod(RawValue, RawValue, RawValue),
	And(RawValue, RawValue, RawValue),
	Or(RawValue, RawValue, RawValue),
	Not(RawValue, RawValue),
	Rmem(RawValue, RawValue),
	Wmem(RawValue, RawValue),
	Call(RawValue),
	Ret,
	Out(RawValue),
	In(RawValue),
	NoOp,
}

fn get_1(vec: RawProgram) -> Option<(RawValue, RawProgram)> {
	vec.pop_front()
		.map(|(a, arest)| {
			(*a, arest)
		})
}

fn get_2(vec: RawProgram) -> Option<(RawValue, RawValue, RawProgram)> {
	vec.pop_front()
		.and_then(|(a, arest)| {
			arest.pop_front()
				.map(|(b, brest)| {
					(*a, *b, brest)
				})
		})
}

fn get_3(vec: RawProgram) -> Option<(RawValue, RawValue, RawValue, RawProgram)> {
	vec.pop_front()
		.and_then(|(a, arest)| {
			arest.pop_front()
				.and_then(|(b, brest)| {
					brest.pop_front()
						.map(|(c, crest)| {
							(*a, *b, *c, crest)
						})
				})
		})
}

fn main() {
	let raw_program = read_bin()
		.iter()
		.map(|v| RawValue(*v) )
		.collect();

	// println!("{:?}", raw_program);

	let result = parse_program(raw_program)
		.map(|program| {
			println!("{:?}", program);
			run_program(program)
		}).unwrap_or(Out::FailedToParse);

	println!("{:?}", result);
}

fn read_bin() -> Vec<u16> {
	let mut res = vec![0u16;0];

	File::open("../challenge.bin")
		.map(|mut file| 
			while let Ok(v) = file.read_u16::<LittleEndian>() {
				res.push(v);
			}
		);
	
	res
}

fn parse_input(input: &str) -> Vec<RawValue> {
	input
		.split(",")
		.map(|c| c.parse::<u16>() )
		.flat_map(|e| e)
		.map(|v| RawValue(v) )
		.collect::<Vec<RawValue>>()
}

fn parse_program(ins: Vec<RawValue>) -> Option<Program> {
	let rest = Vector::from(ins);
	let program = Vector::new();
	
	parse_instructions(program, rest)
}

fn parse_instructions(program: Program, ins: RawProgram) -> Option<Program> {
	parse_next_instruction(program, ins)
		.and_then(|(next_program, next_ins)|
			if next_ins.is_empty() {
				Some(next_program)
			} else {
				parse_instructions(next_program, next_ins)
			}
	)
}

fn parse_next_instruction(program: Program, ins: RawProgram) -> Option<(Program, RawProgram)> {
	match get_1(ins.clone()) {
		Some((RawValue(o), rest)) => {
			println!("{:?}", o);
			// println!("{:?}", rest);
			match o {
				0 => {
					Some((program.push_back(Op::Stop), rest))
				},
				1 => {
					get_2(rest).map(|(a, b, rest2)| {
						let op = Op::Set(a, b);
						(program.push_back(op), rest2)
					})
				},
				2 => {
					get_1(rest).map(|(a, rest2)| {
						let op = Op::Push(a);
						(program.push_back(op), rest2)
					})
				},
				3 => {
					get_1(rest).map(|(a, rest2)| {
						let op = Op::Pop(a);
						(program.push_back(op), rest2)
					})
				},
				4 => {
					get_3(rest).map(|(a, b, c, rest2)| {
						let op = Op::Eq(a, b, c);
						(program.push_back(op), rest2)
					})
				},
				5 => {
					get_3(rest).map(|(a, b, c, rest2)| {
						let op = Op::Gt(a, b, c);
						(program.push_back(op), rest2)
					})
				},
				6 => {
					get_1(rest).map(|(a, rest2)| {
						let op = Op::Jmp(a);
						(program.push_back(op), rest2)
					})
				},
				7 => {
					get_2(rest).map(|(a, b, rest2)| {
						let op = Op::Jt(a, b);
						(program.push_back(op), rest2)
					})
				},
				8 => {
					get_2(rest).map(|(a, b, rest2)| {
						let op = Op::Jf(a, b);
						(program.push_back(op), rest2)
					})
				},
				9 => {
					get_3(rest).map(|(a, b, c, rest2)| {
						let op = Op::Add(a, b, c);
						(program.push_back(op), rest2)
					})
				},
				10 => {
					get_3(rest).map(|(a, b, c, rest2)| {
						let op = Op::Mult(a, b, c);
						(program.push_back(op), rest2)
					})
				},
				11 => {
					get_3(rest).map(|(a, b, c, rest2)| {
						let op = Op::Mod(a, b, c);
						(program.push_back(op), rest2)
					})
				},
				12 => {
					get_3(rest).map(|(a, b, c, rest2)| {
						let op = Op::And(a, b, c);
						(program.push_back(op), rest2)
					})
				},
				13 => {
					get_3(rest).map(|(a, b, c, rest2)| {
						let op = Op::Or(a, b, c);
						(program.push_back(op), rest2)
					})
				},
				14 => {
					get_2(rest).map(|(a, b, rest2)| {
						let op = Op::Not(a, b);
						(program.push_back(op), rest2)
					})
				},
				15 => {
					get_2(rest).map(|(a, b, rest2)| {
						let op = Op::Rmem(a, b);
						(program.push_back(op), rest2)
					})
				},
				16 => {
					get_2(rest).map(|(a, b, rest2)| {
						let op = Op::Wmem(a, b);
						(program.push_back(op), rest2)
					})
				},
				17 => {
					get_1(rest).map(|(a, rest2)| {
						let op = Op::Call(a);
						(program.push_back(op), rest2)
					})
				},
				18 => {
					let op = Op::Ret;
					Some((program.push_back(op), rest))
				},
				19 => {
					get_1(rest).map(|(a, rest2)| {
						let op = Op::Out(a);
						(program.push_back(op), rest2)
					})
				},
				20 => {
					get_1(rest).map(|(a, rest2)| {
						let op = Op::In(a);
						(program.push_back(op), rest2)
					})
				},
				21 => {
					Some((program.push_back(Op::NoOp), rest))
				},
				_ => None,
			}
		},
		None => Some((program, ins)),
	}
}

fn run_program(program: Program) -> Out {
	let offset = 0;
	let regs: Registers = HashMap::new();
	let stack: Stack = Vector::new();

	run_next_operation(offset, program, regs, stack)
}

fn run_next_operation(offset: usize, program: Program, regs: Registers, stack: Stack) -> Out {
	match program.get(offset) {
		Some(op) => {
			let out = run_operation(*op, offset, regs, stack);
			match out {
				Out::Continue(new_offset, new_regs, new_stack) =>
					run_next_operation(new_offset, program, new_regs, new_stack),
				_ =>
					out,
			}
		},
		None => Out::Success,
	}
}

fn run_operation(op: Op, offset: usize, regs: Registers, stack: Stack) -> Out {
	match op {
		Op::Stop => Out::Halted,
		Op::Set(a, b) => run_set(offset, regs, stack, a, b),
		Op::Push(a) => run_push(offset, regs, stack, a),
		Op::Pop(a) => run_pop(offset, regs, stack, a),
		Op::Eq(a, b, c) => run_eq(offset, regs, stack, a, b, c),
		Op::Gt(a, b, c) => run_gt(offset, regs, stack, a, b, c),
		Op::Jmp(a) => run_jmp(offset, regs, stack, a),
		Op::Jt(a, b) => run_jt(offset, regs, stack, a, b),
		Op::Jf(a, b) => run_jf(offset, regs, stack, a, b),
		Op::Add(a, b, c) => run_add(offset, regs, stack, a, b, c),
		Op::Mult(a, b, c) => run_mult(offset, regs, stack, a, b, c),
		Op::Mod(a, b, c) => run_mod(offset, regs, stack, a, b, c),
		Op::And(a, b, c) => run_and(offset, regs, stack, a, b, c),
		Op::Or(a, b, c) => run_or(offset, regs, stack, a, b, c),
		Op::Not(a, b) => run_not(offset, regs, stack, a, b),
		Op::Rmem(a, b) => run_rmem(offset, regs, stack, a, b),
		Op::Wmem(a, b) => run_wmem(offset, regs, stack, a, b),
		Op::Call(a) => run_call(offset, regs, stack, a),
		Op::Ret => run_ret(offset, regs, stack),
		Op::Out(a) => run_out(offset, regs, stack, a),
		Op::In(a) => run_in(offset, regs, stack, a),
		Op::NoOp => Out::Continue(offset + 1, regs, stack),
	}
}

fn get_value_from_register(RegisterPos(pos): RegisterPos, regs: Registers) -> Option<Value> {
	regs
		.get(&pos)
		.map(|arc| *arc)
}

fn set_value_in_register_and_continue(offset: usize, regs: Registers, stack: Stack, reg: RawValue, val: Value) -> Out  {
	register_pos(reg)
		.map(|RegisterPos(pos)| {
			let new_registers = regs.insert(pos, val);
			Out::Continue(offset + 1, new_registers, stack)
		})
		.unwrap_or(Out::InvalidRegister)
}

fn register_pos(RawValue(value): RawValue) -> Option<RegisterPos> {
    let first = 32768;
    let last = 32775;

    if value >= first && value <= last {
        let pos = value - first;
        Some(RegisterPos(pos as u8))
    } else {
        None
    }
}

fn raw_to_value(RawValue(val): RawValue, regs: Registers) -> Value {
	match register_pos(RawValue(val)) {
		Some(pos) => get_value_from_register(pos, regs).unwrap_or(Value(0)),
		None => Value(val),
	}
}

// set: 1 a b
//   set register <a> to the value of <b>
fn run_set(offset: usize, regs: Registers, stack: Stack, a: RawValue, b: RawValue) -> Out {
	let val = raw_to_value(b, regs.clone());
	set_value_in_register_and_continue(offset, regs, stack, a, val)
}

// push: 2 a
//   push <a> onto the stack
fn run_push(offset: usize, regs: Registers, stack: Stack, a: RawValue) -> Out {
	let val = raw_to_value(a, regs.clone());
	Out::Continue(offset + 1, regs, stack.push_front(val))
}

// pop: 3 a
//   remove the top element from the stack and write it into <a>; empty stack = error
fn run_pop(offset: usize, regs: Registers, stack: Stack, a: RawValue) -> Out {
	match stack.pop_front() {
		Some((val, new_stack)) => {
			set_value_in_register_and_continue(offset, regs, new_stack, a, *val)
		},
		None => Out::Halted,
	}
}

// eq: 4 a b c
//   set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
fn run_eq(offset: usize, regs: Registers, stack: Stack, a: RawValue, b: RawValue, c: RawValue) -> Out {
	let Value(bval) = raw_to_value(b, regs.clone());
	let Value(cval) = raw_to_value(c, regs.clone());
	let val = if bval == cval { 1 } else { 0 };
	set_value_in_register_and_continue(offset, regs, stack, a, Value(val))
}

// gt: 5 a b c
//   set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
fn run_gt(offset: usize, regs: Registers, stack: Stack, a: RawValue, b: RawValue, c: RawValue) -> Out {
	let Value(bval) = raw_to_value(b, regs.clone());
	let Value(cval) = raw_to_value(c, regs.clone());
	let val = if bval > cval { 1 } else { 0 };
	set_value_in_register_and_continue(offset, regs, stack, a, Value(val))
}

// jmp: 6 a
//   jump to <a>
fn run_jmp(offset: usize, regs: Registers, stack: Stack, a: RawValue) -> Out {
	let Value(aval) = raw_to_value(a, regs.clone());
	Out::Continue(aval as usize, regs, stack)
}

// jt: 7 a b
//   if <a> is nonzero, jump to <b>
fn run_jt(offset: usize, regs: Registers, stack: Stack, a: RawValue, b: RawValue) -> Out {
	let Value(aval) = raw_to_value(a, regs.clone());
	let Value(bval) = raw_to_value(b, regs.clone());
	let new_offset = if aval == 0 { offset + 1 } else { bval as usize };
	Out::Continue(new_offset, regs, stack)
}

// jf: 8 a b
//   if <a> is zero, jump to <b>
fn run_jf(offset: usize, regs: Registers, stack: Stack, a: RawValue, b: RawValue) -> Out {
	let Value(aval) = raw_to_value(a, regs.clone());
	let Value(bval) = raw_to_value(b, regs.clone());
	let new_offset = if aval == 0 { bval as usize } else { offset + 1 };
	Out::Continue(new_offset, regs, stack)
}

// add: 9 a b c
//   assign into <a> the sum of <b> and <c> (modulo 32768)
fn run_add(offset: usize, regs: Registers, stack: Stack, a: RawValue, b: RawValue, c: RawValue) -> Out {
	let Value(b_val) = raw_to_value(b, regs.clone());
	let Value(c_val) = raw_to_value(c, regs.clone());
	let sum = (b_val + c_val) % MODULO;
	set_value_in_register_and_continue(offset, regs, stack, a, Value(sum))
}

// mult: 10 a b c
//   store into <a> the product of <b> and <c> (modulo 32768)
fn run_mult(offset: usize, regs: Registers, stack: Stack, a: RawValue, b: RawValue, c: RawValue) -> Out {
	let Value(bval) = raw_to_value(b, regs.clone());
	let Value(cval) = raw_to_value(c, regs.clone());
	let val = (bval * cval) % MODULO;
	set_value_in_register_and_continue(offset, regs, stack, a, Value(val))
}

// mod: 11 a b c
//   store into <a> the remainder of <b> divided by <c>
fn run_mod(offset: usize, regs: Registers, stack: Stack, a: RawValue, b: RawValue, c: RawValue) -> Out {
	let Value(bval) = raw_to_value(b, regs.clone());
	let Value(cval) = raw_to_value(c, regs.clone());
	let val = bval % cval;
	set_value_in_register_and_continue(offset, regs, stack, a, Value(val))
}

// and: 12 a b c
//   stores into <a> the bitwise and of <b> and <c>
fn run_and(offset: usize, regs: Registers, stack: Stack, a: RawValue, b: RawValue, c: RawValue) -> Out {
	let Value(bval) = raw_to_value(b, regs.clone());
	let Value(cval) = raw_to_value(c, regs.clone());
	let val = bval & cval;
	set_value_in_register_and_continue(offset, regs, stack, a, Value(val))
}

// or: 13 a b c
//   stores into <a> the bitwise or of <b> and <c>
fn run_or(offset: usize, regs: Registers, stack: Stack, a: RawValue, b: RawValue, c: RawValue) -> Out {
	let Value(bval) = raw_to_value(b, regs.clone());
	let Value(cval) = raw_to_value(c, regs.clone());
	let val = bval | cval;
	set_value_in_register_and_continue(offset, regs, stack, a, Value(val))
}

// not: 14 a b
//   stores 15-bit bitwise inverse of <b> in <a>
fn run_not(offset: usize, regs: Registers, stack: Stack, a: RawValue, b: RawValue) -> Out {
	let Value(bval) = raw_to_value(b, regs.clone());
	let val = !bval;
	set_value_in_register_and_continue(offset, regs, stack, a, Value(val))
}

// rmem: 15 a b
//   read memory at address <b> and write it to <a>
fn run_rmem(offset: usize, regs: Registers, stack: Stack, a: RawValue, b: RawValue) -> Out {
	register_pos(b)
		.and_then(|reg_pos|
			get_value_from_register(reg_pos, regs.clone())
				.map(|bval|
					set_value_in_register_and_continue(offset, regs, stack, a, bval)
				)
		).unwrap_or(Out::Halted)
}

// wmem: 16 a b
//   write the value from <b> into memory at address <a>
fn run_wmem(offset: usize, regs: Registers, stack: Stack, a: RawValue, b: RawValue) -> Out {
	let bval = raw_to_value(b, regs.clone());
	set_value_in_register_and_continue(offset, regs, stack, a, bval)
}

// call: 17 a
//   write the address of the next instruction to the stack and jump to <a>
fn run_call(offset: usize, regs: Registers, stack: Stack, a: RawValue) -> Out {
	let new_stack = stack.push_front(Value((offset + 1 )as u16));
	let Value(aval) = raw_to_value(a, regs.clone());
	Out::Continue(aval as usize, regs, new_stack)
}

// ret: 18
//   remove the top element from the stack and jump to it; empty stack = halt
fn run_ret(offset: usize, regs: Registers, stack: Stack) -> Out {
	stack.pop_front()
		.map(|(val, new_stack)| {
			let Value(new_offset) = *val;
			Out::Continue(new_offset as usize, regs, new_stack)
		}).unwrap_or(Out::Halted)
}
// out: 19 a
//   write the character represented by ascii code <a> to the terminal
fn run_out(offset: usize, regs: Registers, stack: Stack, a: RawValue) -> Out {
	let Value(val) = raw_to_value(a, regs.clone());
	match char::from_u32(val as u32) {
		Some(c) => print!("{:?}", c),
		None => (),
	}
	Out::Continue(offset + 1, regs, stack)
}

// in: 20 a
//   read a character from the terminal and write its ascii code to <a>; it can be assumed that once input starts, it will continue until a newline is encountered; this means that you can safely read whole lines from the keyboard and trust that they will be fully read
fn run_in(offset: usize, regs: Registers, stack: Stack, a: RawValue) -> Out {
	set_value_in_register_and_continue(offset, regs, stack, a, Value(111))
}