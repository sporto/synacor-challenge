extern crate im;

use im::Vector;
use im::HashMap;
use std::sync::Arc;

struct Code(i32);

#[derive(Copy,Clone,Debug)]
struct Value(i32);
// struct Raw(i32);

// #[derive(PartialEq,Eq)]
struct RegisterPos(i32);

#[derive(Copy,Clone,Debug)]
struct RawValue(i32);

type Stack = Vector<RawValue>;
type Registers = HashMap<i32, Value>;

fn main() {
    let input = "9,32768,32769,4,19,32768";
    run(input);
}

fn run(input: &str) -> usize {
    let registers: Registers = HashMap::new();

    let vec = input
        .split(",")
        .map(|c| c.parse::<i32>() )
        .flat_map(|e| e)
        .map(|v| RawValue(v))
        .collect::<Vec<RawValue>>();

    let stack: Stack = Vector::from(vec);

    next(stack, registers)
}

fn next(stack: Stack, registers: Registers) -> usize {
    match stack.pop_front() {
        Some((codeArc, rest)) => {
            let RawValue(code) = *codeArc;
            instruction(Code(code), rest, registers)
        },
        None => 1,
    }
}

fn instruction(Code(code): Code, stack: Stack, registers: Registers) -> usize {
    // println!("{:?}", code);
    match code {
        9 => iadd(stack, registers),
        _ => 1,
    }
}

// # add: 9 a b c
// #   assign into <a> the sum of <b> and <c> (modulo 32768)
fn iadd(stack: Stack, registers: Registers) -> usize {
    match get_3(stack.clone()) {
        Some((a, b, c)) => {
            // do
            println!("{:?} {:?} {:?}", a, b, c);
            let b_val = raw_to_value(b, registers.clone());
            let c_val = raw_to_value(c, registers.clone());
            next(stack.skip(3), registers)
        },
        _ => 1,
    }
}

fn raw_to_value(RawValue(n): RawValue, registers: Registers) -> Value {
    if n <= 32767 {
        Value(n)
    } else if n <= 32775 {
        let pos = RegisterPos(n - 32768);

        get_value_from_register(registers, pos)
            .unwrap_or(Value(0))
    } else {
        Value(0)
    }
}

fn get_value_from_register(registers: Registers, RegisterPos(pos): RegisterPos) -> Option<Value> {
    registers
        .get(&pos)
        .map(|arc| *arc)
}

fn get_3(stack: Stack) -> Option<(RawValue, RawValue, RawValue)> {
    let aArc = stack.get(0);
    let bArc = stack.get(1);
    let cArc = stack.get(2);

    match (aArc, bArc, cArc) {
        (Some(a), Some(b), Some(c)) => Some((*a, *b, *c)),
        _ => None,
    }
}
