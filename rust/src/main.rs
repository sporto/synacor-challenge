extern crate im;

use im::Vector;
use im::HashMap;
use std::char;

type Registers = HashMap<u8, u16>;
type Program = Vector<Op>;

#[derive(Debug)]
enum Out {
	FailedToParse,
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
	Ret(u16),
	Out(u16),
	In(u16),
	Noop,
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

	let regs: Registers = HashMap::new();

	let raw = parse_input(input);

	let result = parse_program(raw);
		// .map(|program| Out::Success ).unwrap_or(Out::FailedToParse);

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
					get_1(rest).map(|(a, rest2)| {
						let op = Op::Ret(a);
						(program.push_back(op), rest2)
					})
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
					Some((program.push_back(Op::Noop), rest))
				},
				_ => None,
			}
		},
		None => Some((program, ins)),
	}
}