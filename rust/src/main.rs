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
	match ins.pop_front() {
		Some((a, rest)) => {
			match *a {
				0 => {
					let new_program = program.push_back(Op::Stop);
					parse_next_instruction(new_program, rest)
				},
				_ => None,
			}
		},
		None => Some(program),
	}
}