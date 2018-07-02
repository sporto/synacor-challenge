extern crate im;

use im::Vector;
use im::HashMap;
use std::char;

struct InstructionCode(i32);

#[derive(Copy,Clone,Debug)]
struct Value(i32);

struct RegisterPos(i32);

#[derive(Copy,Clone,Debug)]
struct InstructionValue(i32);

type Instructions = Vector<InstructionValue>;
type Registers = HashMap<i32, Value>;
type Stack = Vector<Value>;

enum Outcome {
    Success,
    Fail,
}

fn main() {
    let input = "9,32768,32769,4,19,32768";
    run(input);
}

fn run(input: &str) -> Outcome {
    let regs: Registers = HashMap::new();

    let vec = input
        .split(",")
        .map(|c| c.parse::<i32>() )
        .flat_map(|e| e)
        .map(|v| InstructionValue(v))
        .collect::<Vec<InstructionValue>>();

    let ins: Instructions = Vector::from(vec);
    let stack: Stack = Vector::new();

    next(ins, regs, stack)
}

fn next(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    match ins.pop_front() {
        Some((code_arc, rest)) => {
            let InstructionValue(code) = *code_arc;
            instruction(InstructionCode(code), rest, regs, stack)
        },
        None => Outcome::Success,
    }
}

fn instruction(InstructionCode(code): InstructionCode, rest: Instructions, r: Registers, s: Stack) -> Outcome {
    match code {
        0 => istop(),
        1 => iset(rest, r, s),
        2 => ipush(rest, r, s),
        9 => iadd(rest, r, s),
        19 => iout(rest, r, s),
        _ => Outcome::Fail,
    }
}

// halt: 0
//   stop execution and terminate the program
fn istop() -> Outcome {
    Outcome::Success
}

// set: 1 a b
//   set register <a> to the value of <b>
fn iset(ins: Instructions, regs: Registers, stack: Stack)  -> Outcome {
    match get_2(ins.clone()) {
        Some((rest, a, b)) => {
            match register_pos(a) {
                Some(reg_pos) => {
                    let value = raw_to_value(b, regs.clone());
                    let new_registers = set_value_in_register(regs, reg_pos, value);
                    next(rest, new_registers, stack)
                },
                None => Outcome::Fail,
            }
        },
        None => Outcome::Fail,
    }
}

// push: 2 a
//   push <a> onto the stack
fn ipush(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    match get_1(ins.clone()) {
        Some((rest, a)) => {
            let value = raw_to_value(a, regs.clone());
            let new_stack = stack.push_front(value);

            next(rest, regs, new_stack)
        },
        None => Outcome::Fail,
    }
}

// # add: 9 a b c
// #   assign into <a> the sum of <b> and <c> (modulo 32768)
fn iadd(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    match get_3(ins.clone()) {
        Some((rest, a, b, c)) => {
            match register_pos(a) {
                Some(reg_pos) => {
                    let Value(b_val) = raw_to_value(b, regs.clone());
                    let Value(c_val) = raw_to_value(c, regs.clone());
                    let sum = (b_val + c_val) % 32768;
                    // println!("sum {:?}", b_val);

                    let new_registers = set_value_in_register(regs, reg_pos, Value(sum));

                    next(rest, new_registers, stack)
                },
                None => Outcome::Fail,
            }
        },
        _ => Outcome::Fail,
    }
}

// out: 19 a
//   write the character represented by ascii code <a> to the terminal
fn iout(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    match get_1(ins.clone()) {
        Some((rest, a_raw)) => {
            let Value(val) = raw_to_value(a_raw, regs.clone());
            match char::from_u32(val as u32) {
                Some(c) => print!("{:?}", c),
                None => (),
            }
            next(rest, regs, stack)
        }
        None => Outcome::Fail,
    }
}

fn raw_to_value(InstructionValue(n): InstructionValue, regs: Registers) -> Value {
    if n <= 32767 {
        Value(n)
    } else if n <= 32775 {
        let pos = RegisterPos(n - 32768);

        get_value_from_register(regs, pos)
            .unwrap_or(Value(0))
    } else {
        Value(0)
    }
}

fn register_pos(InstructionValue(value): InstructionValue) -> Option<RegisterPos> {
    if value >= 32768 && value <= 32775 {
        Some(RegisterPos(value - 32768))
    } else {
        None
    }
}

fn get_value_from_register(regs: Registers, RegisterPos(pos): RegisterPos) -> Option<Value> {
    regs
        .get(&pos)
        .map(|arc| *arc)
}

fn set_value_in_register(regs: Registers, RegisterPos(pos): RegisterPos, val: Value) -> Registers {
    regs.insert(pos, val)
}

fn get_1(ins: Instructions) -> Option<(Instructions, InstructionValue)> {
    match ins.get(0) {
        Some(a) => Some((ins.skip(1), *a)),
        _ => None,
    }
}

fn get_2(ins: Instructions) -> Option<(Instructions, InstructionValue, InstructionValue)> {
    let a_arc = ins.get(0);
    let b_arc = ins.get(1);

    match (a_arc, b_arc) {
        (Some(a), Some(b)) => Some((ins.skip(2), *a, *b)),
        _ => None,
    }
}

fn get_3(ins: Instructions) -> Option<(Instructions, InstructionValue, InstructionValue, InstructionValue)> {
    let a_arc = ins.get(0);
    let b_arc = ins.get(1);
    let c_arc = ins.get(2);

    match (a_arc, b_arc, c_arc) {
        (Some(a), Some(b), Some(c)) => Some((ins.skip(3), *a, *b, *c)),
        _ => None,
    }
}
