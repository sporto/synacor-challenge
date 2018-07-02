extern crate im;

use im::Vector;
use im::HashMap;
use std::char;

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
        Some((code_arc, rest)) => {
            let RawValue(code) = *code_arc;
            instruction(Code(code), rest, registers)
        },
        None => 1,
    }
}

fn instruction(Code(code): Code, stack: Stack, registers: Registers) -> usize {
    // println!("{:?}", code);
    match code {
        9 => iadd(stack, registers),
        19 => iout(stack, registers),
        _ => 1,
    }
}

// # add: 9 a b c
// #   assign into <a> the sum of <b> and <c> (modulo 32768)
fn iadd(stack: Stack, registers: Registers) -> usize {
    match get_3(stack.clone()) {
        Some((a, b, c)) => {
            match register_pos(a) {
                Some(reg_pos) => {
                    let Value(b_val) = raw_to_value(b, registers.clone());
                    let Value(c_val) = raw_to_value(c, registers.clone());
                    let sum = (b_val + c_val) % 32768;
                    // println!("sum {:?}", b_val);

                    let new_registers = set_value_in_register(registers, reg_pos, Value(sum));

                    next(stack.skip(3), new_registers)
                },
                None => 0,
            }
        },
        _ => 1,
    }
}

// out: 19 a
//   write the character represented by ascii code <a> to the terminal
fn iout(stack: Stack, registers: Registers) -> usize {
    match get_1(stack.clone()) {
        Some(a_raw) => {
            let Value(val) = raw_to_value(a_raw, registers.clone());
            match char::from_u32(val as u32) {
                Some(c) => print!("{:?}", c),
                None => (),
            }
            next(stack.skip(1), registers)
        }
        None => 1,
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

fn get_1(stack: Stack) -> Option<RawValue> {
    match stack.get(0) {
        Some(a) => Some(*a),
        _ => None,
    }
}

fn get_3(stack: Stack) -> Option<(RawValue, RawValue, RawValue)> {
    let a_arc = stack.get(0);
    let b_arc = stack.get(1);
    let c_arc = stack.get(2);

    match (a_arc, b_arc, c_arc) {
        (Some(a), Some(b), Some(c)) => Some((*a, *b, *c)),
        _ => None,
    }
}
