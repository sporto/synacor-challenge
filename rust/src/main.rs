extern crate im;

use im::Vector;
use im::HashMap;
use std::char;

type Registers = HashMap<u8, u16>;
type Program = Vector<Op>;
type Stack = Vector<u16>;

#[derive(Debug)]
enum Out {
	Continue(Registers, Stack),
	FailedToParse,
	Halted,
	Success,
}

#[derive(Debug)]
enum Op {
	Stop,
	Set(u16, u16),
	Push(u16),
	Pop(u16),
	Eq(u16, u16, u16),
	Gt(u16, u16, u16),
	Jmp(u16),
	Jt(u16, u16),
	Jf(u16, u16),
	Add(u16, u16, u16),
	Mult(u16, u16, u16),
	Mod(u16, u16, u16),
	And(u16, u16, u16),
	Or(u16, u16, u16),
	Not(u16, u16),
	Rmem(u16, u16),
	Wmem(u16, u16),
	Call(u16),
	Ret,
	Out(u16),
	In(u16),
	NoOp,
}

fn get_1(vec: Vector<u16>) -> Option<(u16, Vector<u16>)> {
	vec.pop_front()
		.map(|(a, arest)| {
			(*a, arest)
		})
}

fn get_2(vec: Vector<u16>) -> Option<(u16, u16, Vector<u16>)> {
	vec.pop_front()
		.and_then(|(a, arest)| {
			arest.pop_front()
				.map(|(b, brest)| {
					(*a, *b, brest)
				})
		})
}

fn get_3(vec: Vector<u16>) -> Option<(u16, u16, u16, Vector<u16>)> {
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
	let input = "9,32768,32769,4,19,32768";

	let raw = parse_input(input);

	let result = parse_program(raw)
		.map(|program|
			run_program(program)
		).unwrap_or(Out::FailedToParse);

	println!("{:?}", result);
}

fn parse_input(input: &str) -> Vec<u16> {
	input
		.split(",")
		.map(|c| c.parse::<u16>() )
		.flat_map(|e| e)
		.collect::<Vec<u16>>()
}

fn parse_program(ins: Vec<u16>) -> Option<Program> {
	let rest = Vector::from(ins);
	let program = Vector::new();
	
	parse_instructions(program, rest)
}

fn parse_instructions(program: Program, ins: Vector<u16>) -> Option<Program> {
	parse_next_instruction(program, ins)
		.and_then(|(next_program, next_ins)|
			if next_ins.is_empty() {
				Some(next_program)
			} else {
				parse_instructions(next_program, next_ins)
			}
	)
}

fn parse_next_instruction(program: Program, ins: Vector<u16>) -> Option<(Program, Vector<u16>)> {
	match get_1(ins.clone()) {
		Some((o, rest)) => {
			// println!("{:?}", o);
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
	Out::Success
}

fn run_operation(op: Op, regs: Registers, stack: Stack) -> Out {
	match op {
		Op::Stop => Out::Halted,
		Op::Set(a, b) => run_set(regs, stack, a, b),
		Op::Push(a) => run_push(regs, stack, a),
		Op::Pop(a) => run_pop(regs, stack, a),
		Op::Eq(a, b, c) => run_eq(regs, stack, a, b, c),
		Op::Gt(a, b, c) => run_gt(regs, stack, a, b, c),
		Op::Jmp(a) => run_jmp(regs, stack, a),
		Op::Jt(a, b) => run_jt(regs, stack, a, b),
		Op::Jf(a, b) => run_jf(regs, stack, a, b),
		Op::Add(a, b, c) => run_add(regs, stack, a, b, c),
		Op::Mult(a, b, c) => run_mult(regs, stack, a, b, c),
		Op::Mod(a, b, c) => run_mod(regs, stack, a, b, c),
		Op::And(a, b, c) => run_and(regs, stack, a, b, c),
		Op::Or(a, b, c) => run_or(regs, stack, a, b, c),
		Op::Not(a, b) => run_not(regs, stack, a, b),
		Op::Rmem(a, b) => run_rmem(regs, stack, a, b),
		Op::Wmem(a, b) => run_wmem(regs, stack, a, b),
		Op::Call(a) => run_call(regs, stack, a),
		Op::Ret => run_ret(regs, stack),
		Op::Out(a) => run_out(regs, stack, a),
		Op::In(a) => run_in(regs, stack, a),
		Op::NoOp => Out::Continue(regs, stack),
	}
}

// set: 1 a b
//   set register <a> to the value of <b>
fn run_set(regs: Registers, stack: Stack, a: u16, b: u16) -> Out {
	Out::Continue(regs, stack)
}

// push: 2 a
//   push <a> onto the stack
fn run_push(regs: Registers, stack: Stack, a: u16) -> Out {
	Out::Continue(regs, stack)
}

// pop: 3 a
//   remove the top element from the stack and write it into <a>; empty stack = error
fn run_pop(regs: Registers, stack: Stack, a: u16) -> Out {
	Out::Continue(regs, stack)
}

// eq: 4 a b c
//   set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
fn run_eq(regs: Registers, stack: Stack, a: u16, b: u16, c: u16) -> Out {
	Out::Continue(regs, stack)
}

// gt: 5 a b c
//   set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
fn run_gt(regs: Registers, stack: Stack, a: u16, b: u16, c: u16) -> Out {
	Out::Continue(regs, stack)
}

// jmp: 6 a
//   jump to <a>
fn run_jmp(regs: Registers, stack: Stack, a: u16) -> Out {
	Out::Continue(regs, stack)
}

// jt: 7 a b
//   if <a> is nonzero, jump to <b>
fn run_jt(regs: Registers, stack: Stack, a: u16, b: u16) -> Out {
	Out::Continue(regs, stack)
}

// jf: 8 a b
//   if <a> is zero, jump to <b>
fn run_jf(regs: Registers, stack: Stack, a: u16, b: u16) -> Out {
	Out::Continue(regs, stack)
}

// add: 9 a b c
//   assign into <a> the sum of <b> and <c> (modulo 32768)
fn run_add(regs: Registers, stack: Stack, a: u16, b: u16, c: u16) -> Out {
	Out::Continue(regs, stack)
}

// mult: 10 a b c
//   store into <a> the product of <b> and <c> (modulo 32768)
fn run_mult(regs: Registers, stack: Stack, a: u16, b: u16, c: u16) -> Out {
	Out::Continue(regs, stack)
}

// mod: 11 a b c
//   store into <a> the remainder of <b> divided by <c>
fn run_mod(regs: Registers, stack: Stack, a: u16, b: u16, c: u16) -> Out {
	Out::Continue(regs, stack)
}

// and: 12 a b c
//   stores into <a> the bitwise and of <b> and <c>
fn run_and(regs: Registers, stack: Stack, a: u16, b: u16, c: u16) -> Out {
	Out::Continue(regs, stack)
}

// or: 13 a b c
//   stores into <a> the bitwise or of <b> and <c>
fn run_or(regs: Registers, stack: Stack, a: u16, b: u16, c: u16) -> Out {
	Out::Continue(regs, stack)
}

// not: 14 a b
//   stores 15-bit bitwise inverse of <b> in <a>
fn run_not(regs: Registers, stack: Stack, a: u16, b: u16) -> Out {
	Out::Continue(regs, stack)
}

// rmem: 15 a b
//   read memory at address <b> and write it to <a>
fn run_rmem(regs: Registers, stack: Stack, a: u16, b: u16) -> Out {
	Out::Continue(regs, stack)
}

// wmem: 16 a b
//   write the value from <b> into memory at address <a>
fn run_wmem(regs: Registers, stack: Stack, a: u16, b: u16) -> Out {
	Out::Continue(regs, stack)
}

// call: 17 a
//   write the address of the next instruction to the stack and jump to <a>
fn run_call(regs: Registers, stack: Stack, a: u16) -> Out {
	Out::Continue(regs, stack)
}

// ret: 18
//   remove the top element from the stack and jump to it; empty stack = halt
fn run_ret(regs: Registers, stack: Stack) -> Out {
	Out::Continue(regs, stack)
}
// out: 19 a
//   write the character represented by ascii code <a> to the terminal
fn run_out(regs: Registers, stack: Stack, a: u16) -> Out {
	Out::Continue(regs, stack)
}

// in: 20 a
//   read a character from the terminal and write its ascii code to <a>; it can be assumed that once input starts, it will continue until a newline is encountered; this means that you can safely read whole lines from the keyboard and trust that they will be fully read
fn run_in(regs: Registers, stack: Stack, a: u16) -> Out {
	Out::Continue(regs, stack)
}