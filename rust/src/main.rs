extern crate im;

use im::Vector;
use im::HashMap;
use std::char;

struct Code(i32);

#[derive(Copy,Clone,Debug)]
struct Value(i32);

struct RegisterPos(i32);

#[derive(Copy,Clone,Debug)]
struct RawValue(i32);

type Stack = Vector<RawValue>;
type Registers = HashMap<i32, Value>;

enum Outcome {
    Success,
    Fail,
}

fn main() {
    let input = "9,32768,32769,4,19,32768";
    run(input);
}

fn run(input: &str) -> Outcome {
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

fn next(stack: Stack, registers: Registers) -> Outcome {
    match stack.pop_front() {
        Some((code_arc, rest)) => {
            let RawValue(code) = *code_arc;
            instruction(Code(code), rest, registers)
        },
        None => Outcome::Success,
    }
}

fn instruction(Code(code): Code, stack: Stack, registers: Registers) -> Outcome {
    // println!("{:?}", code);
    match code {
        0 => istop(stack, registers),
        9 => iadd(stack, registers),
        19 => iout(stack, registers),
        _ => Outcome::Fail,
    }
}

// halt: 0
//   stop execution and terminate the program
fn istop(stack: Stack, registers: Registers) -> Outcome {
    Outcome::Success
}

// set: 1 a b
//   set register <a> to the value of <b>
fn iset(stack: Stack, registers: Registers)  -> Outcome {
    match get_2(stack.clone()) {
        Some((new_stack, a, b)) => {
            match register_pos(a) {
                Some(reg_pos) => {
                    let value = raw_to_value(b, registers.clone());
                    let new_registers = set_value_in_register(registers, reg_pos, value);
                    next(new_stack, new_registers)
                },
                None => Outcome::Fail,
            }
        },
        None => Outcome::Fail,
    }
}

// # add: 9 a b c
// #   assign into <a> the sum of <b> and <c> (modulo 32768)
fn iadd(stack: Stack, registers: Registers) -> Outcome {
    match get_3(stack.clone()) {
        Some((new_stack, a, b, c)) => {
            match register_pos(a) {
                Some(reg_pos) => {
                    let Value(b_val) = raw_to_value(b, registers.clone());
                    let Value(c_val) = raw_to_value(c, registers.clone());
                    let sum = (b_val + c_val) % 32768;
                    // println!("sum {:?}", b_val);

                    let new_registers = set_value_in_register(registers, reg_pos, Value(sum));

                    next(new_stack, new_registers)
                },
                None => Outcome::Fail,
            }
        },
        _ => Outcome::Fail,
    }
}

// out: 19 a
//   write the character represented by ascii code <a> to the terminal
fn iout(stack: Stack, registers: Registers) -> Outcome {
    match get_1(stack.clone()) {
        Some((new_stack, a_raw)) => {
            let Value(val) = raw_to_value(a_raw, registers.clone());
            match char::from_u32(val as u32) {
                Some(c) => print!("{:?}", c),
                None => (),
            }
            next(new_stack, registers)
        }
        None => Outcome::Fail,
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

fn register_pos(RawValue(value): RawValue) -> Option<RegisterPos> {
    if value >= 32768 && value <= 32775 {
        Some(RegisterPos(value - 32768))
    } else {
        None
    }
}

fn get_value_from_register(registers: Registers, RegisterPos(pos): RegisterPos) -> Option<Value> {
    registers
        .get(&pos)
        .map(|arc| *arc)
}

fn set_value_in_register(registers: Registers, RegisterPos(pos): RegisterPos, val: Value) -> Registers {
    registers.insert(pos, val)
}

fn get_1(stack: Stack) -> Option<(Stack, RawValue)> {
    match stack.get(0) {
        Some(a) => Some((stack.skip(1), *a)),
        _ => None,
    }
}

fn get_2(stack: Stack) -> Option<(Stack, RawValue, RawValue)> {
    let a_arc = stack.get(0);
    let b_arc = stack.get(1);

    match (a_arc, b_arc) {
        (Some(a), Some(b)) => Some((stack.skip(2), *a, *b)),
        _ => None,
    }
}

fn get_3(stack: Stack) -> Option<(Stack, RawValue, RawValue, RawValue)> {
    let a_arc = stack.get(0);
    let b_arc = stack.get(1);
    let c_arc = stack.get(2);

    match (a_arc, b_arc, c_arc) {
        (Some(a), Some(b), Some(c)) => Some((stack.skip(3), *a, *b, *c)),
        _ => None,
    }
}
