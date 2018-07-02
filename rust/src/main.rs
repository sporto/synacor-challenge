extern crate im;

use im::Vector;
use im::HashMap;
use std::char;

struct InstructionCode(i32);

#[derive(Copy,Clone,Debug)]
struct Value(i32);

#[derive(Copy,Clone,Debug)]
struct InstructionValue(i32);

struct RegisterPos(i32);

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
    // println!("{:?}", i32::max_value());
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
        0 => i_stop(),
        1 => i_set(rest, r, s),
        2 => i_push(rest, r, s),
        3 => i_pop(rest, r, s),
        4 => i_eq(rest, r, s),
        5 => i_gt(rest, r, s),
        6 => i_jmp(rest, r, s),
        7 => i_jt(rest, r, s),
        8 => i_jf(rest, r, s),
        9 => i_add(rest, r, s),
        10 => i_mult(rest, r, s),
        11 => i_store(rest, r, s),
        12 => i_and(rest, r, s),
        13 => i_or(rest, r, s),
        14 => i_not(rest, r, s),
        15 => i_rmem(rest, r, s),
        16 => i_wmem(rest, r, s),
        17 => i_call(rest, r, s),
        18 => i_ret(rest, r, s),
        19 => i_out(rest, r, s),
        20 => i_in(rest, r, s),
        21 => i_noop(rest, r, s),
        _ => Outcome::Fail,
    }
}

// halt: 0
//   stop execution and terminate the program
fn i_stop() -> Outcome {
    Outcome::Success
}

// set: 1 a b
//   set register <a> to the value of <b>
fn i_set(ins: Instructions, regs: Registers, stack: Stack)  -> Outcome {
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
fn i_push(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    match get_1(ins.clone()) {
        Some((rest, a)) => {
            let value = raw_to_value(a, regs.clone());
            let new_stack = stack.push_front(value);

            next(rest, regs, new_stack)
        },
        None => Outcome::Fail,
    }
}

// pop: 3 a
//   remove the top element from the stack and write it into <a>; empty stack = error
fn i_pop(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    match pop_stack(stack) {
        Some((new_stack, value)) => {
            match get_1(ins.clone()) {
                Some((rest, a)) => {
                    match register_pos(a) {
                        Some(reg_pos) => {
                            let new_registers = set_value_in_register(regs, reg_pos, value);
                            next(rest, new_registers, new_stack)
                        },
                        None => Outcome::Fail,
                    }
                },
                None => Outcome::Fail,
            }
        },
        None => Outcome::Fail,
    }
}
// eq: 4 a b c
//   set <a> to 1 if <b> is equal to <c>; set it to 0 otherwise
fn i_eq(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// gt: 5 a b c
//   set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
fn i_gt(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// jmp: 6 a
//   jump to <a>
fn i_jmp(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// jt: 7 a b
//   if <a> is nonzero, jump to <b>
fn i_jt(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// jf: 8 a b
//   if <a> is zero, jump to <b>
fn i_jf(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// # add: 9 a b c
// #   assign into <a> the sum of <b> and <c> (modulo 32768)
fn i_add(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
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

// mult: 10 a b c
//   store into <a> the product of <b> and <c> (modulo 32768)
fn i_mult(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// mod: 11 a b c
//   store into <a> the remainder of <b> divided by <c>
fn i_store(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// and: 12 a b c
//   stores into <a> the bitwise and of <b> and <c>
fn i_and(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// or: 13 a b c
//   stores into <a> the bitwise or of <b> and <c>
fn i_or(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// not: 14 a b
//   stores 15-bit bitwise inverse of <b> in <a>
fn i_not(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// rmem: 15 a b
//   read memory at address <b> and write it to <a>
fn i_rmem(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// wmem: 16 a b
//   write the value from <b> into memory at address <a>
fn i_wmem(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// call: 17 a
//   write the address of the next instruction to the stack and jump to <a>
fn i_call(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// ret: 18
//   remove the top element from the stack and jump to it; empty stack = halt
fn i_ret(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// out: 19 a
//   write the character represented by ascii code <a> to the terminal
fn i_out(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
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

// in: 20 a
//   read a character from the terminal and write its ascii code to <a>; it can be assumed that once input starts, it will continue until a newline is encountered; this means that you can safely read whole lines from the keyboard and trust that they will be fully read
fn i_in(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// noop: 21
//   no operation
fn i_noop(ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}

fn raw_to_value(InstructionValue(n): InstructionValue, regs: Registers) -> Value {
    match register_pos(InstructionValue(n)) {
        Some(reg) => {
            get_value_from_register(regs, reg)
                .unwrap_or(Value(0))
        },
        None => {
            Value(n)
        },
    }
}

fn register_pos(InstructionValue(value): InstructionValue) -> Option<RegisterPos> {
    let pos = value - 32768;
    if pos >= 0 && pos <= 7 {
        Some(RegisterPos(pos))
    } else {
        None
    }
}

fn get_value_from_register(regs: Registers, RegisterPos(pos): RegisterPos) -> Option<Value> {
    regs
        .get(&pos)
        .map(|arc| *arc)
}

fn set_value_in_register(regs: Registers, RegisterPos(reg_pos): RegisterPos, val: Value) -> Registers {
    regs.insert(reg_pos, val)
}

fn pop_stack(stack: Stack) -> Option<(Stack, Value)> {
    match stack.pop_front() {
        Some((a,new_stack)) => Some((new_stack, *a)),
        None => None,
    }
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
