extern crate im;

use im::Vector;
use im::HashMap;
use std::char;

type Registers = HashMap<u8, u16>;
type Program = Vector<Op>;

enum Out {
	FailedToParse,
	Success,
}

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
    // println!("{:?}", u16::max_value());

	let raw = parse_input(input);

	let result = parse_program(raw)
		.map(|program| Out::Success ).unwrap_or(Out::FailedToParse);
}

fn parse_input(input: &str) -> Vec<u16> {
	input
		.split(",")
		.map(|c| c.parse::<u16>() )
		.flat_map(|e| e)
		.collect::<Vec<u16>>()
}

fn parse_program(ins: Vec<u16>) -> Option<Program> {
	let offset = 0;
	let rest = Vector::from(ins);
	let program = Vector::new();
	
	parse_next_instruction(program, rest )
}

fn parse_next_instruction(program: Program, ins: Vector<u16>) -> Option<Program> {
	match get_1(ins) {
		Some((a, rest)) => {
			match a {
				0 => {
					let new_program = program.push_back(Op::Stop);
					parse_next_instruction(new_program, rest)
				},
				1 => {
					get_2(rest)
					.and_then(|(b, c, rest2)| {
						let op = Op::Set(b, c);
						let new_program = program.push_back(op);
						parse_next_instruction(new_program, rest2)
					})
				}
				_ => None,
			}
		},
		None => Some(program),
	}
}