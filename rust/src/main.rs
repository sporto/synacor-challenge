extern crate im;

use im::Vector;
use im::HashMap;
use std::char;

enum Ins {
    Stop,
    Set,
    Push,
    Pop,
    Eq,
    Gt,
    Jmp,
    Jt,
    Jf,
    Add,
    Mult,
    Store,
    And,
    Or,
    Not,
    Rmem,
    Wmem,
    Call,
    Ret,
    Out,
    In,
    Noop,
}

#[derive(Copy,Clone,Debug)]
struct Value(i32);

#[derive(Copy,Clone,Debug)]
struct InstructionValue(i32);

struct RegisterPos(i32);

struct Offset(usize);

type Instructions = Vector<InstructionValue>;
type Registers = HashMap<i32, Value>;
type Stack = Vector<Value>;

enum Outcome {
    Success,
    Fail,
    Continue(Offset, Registers, Stack),
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

    next(Offset(0), ins, regs, stack)
}

fn next(Offset(offset): Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    match get_next_instruction(Offset(offset), ins.clone()) {
        Some(code) => {
            let outcome =
                run_instruction(code, Offset(offset + 1), ins.clone(), regs, stack);

            match outcome {
                Outcome::Continue(new_offset, new_regs, new_stack) =>
                    next(new_offset, ins, new_regs, new_stack),

                Outcome::Success =>
                    Outcome::Success,

                Outcome::Fail =>
                    Outcome::Fail,
            }
        },
        None => Outcome::Success,
    }
}

fn get_next_instruction(Offset(offset): Offset, ins: Instructions) -> Option<Ins> {
    match ins.get(offset) {
        Some(a) => {
            let InstructionValue(code) = *a;
            match code {
                0 => Some(Ins::Stop),
                1 => Some(Ins::Set),
                2 => Some(Ins::Push),
                3 => Some(Ins::Pop),
                4 => Some(Ins::Eq),
                5 => Some(Ins::Gt),
                6 => Some(Ins::Jmp),
                7 => Some(Ins::Jt),
                8 => Some(Ins::Jf),
                9 => Some(Ins::Add),
                10 =>Some(Ins::Mult),
                11 => Some(Ins::Store),
                12 => Some(Ins::And),
                13 => Some(Ins::Or),
                14 => Some(Ins::Not),
                15 => Some(Ins::Rmem),
                16 => Some(Ins::Wmem),
                17 => Some(Ins::Call),
                18 => Some(Ins::Ret),
                19 => Some(Ins::Out),
                20 => Some(Ins::In),
                21 => Some(Ins::Noop),
                _ => None,
            }
        },
        None => None,
    }
}

fn run_instruction(code: Ins, offset: Offset, ins: Instructions, r: Registers, s: Stack) -> Outcome {
    match code {
        Ins::Stop => i_stop(),
        Ins::Set => i_set(offset, ins, r, s),
        Ins::Push => i_push(offset, ins, r, s),
        Ins::Pop => i_pop(offset, ins, r, s),
        Ins::Eq => i_eq(offset, ins, r, s),
        Ins::Gt => i_gt(offset, ins, r, s),
        Ins::Jmp => i_jmp(offset, ins, r, s),
        Ins::Jt => i_jt(offset, ins, r, s),
        Ins::Jf => i_jf(offset, ins, r, s),
        Ins::Add => i_add(offset, ins, r, s),
        Ins::Mult => i_mult(offset, ins, r, s),
        Ins::Store => i_store(offset, ins, r, s),
        Ins::And => i_and(offset, ins, r, s),
        Ins::Or => i_or(offset, ins, r, s),
        Ins::Not => i_not(offset, ins, r, s),
        Ins::Rmem => i_rmem(offset, ins, r, s),
        Ins::Wmem => i_wmem(offset, ins, r, s),
        Ins::Call => i_call(offset, ins, r, s),
        Ins::Ret => i_ret(offset, ins, r, s),
        Ins::Out => i_out(offset, ins, r, s),
        Ins::In => i_in(offset, ins, r, s),
        Ins::Noop => i_noop(offset, ins, r, s),
    }
}

// halt: 0
//   stop execution and terminate the program
fn i_stop() -> Outcome {
    Outcome::Success
}

// set: 1 a b
//   set register <a> to the value of <b>
fn i_set(offset: Offset, ins: Instructions, regs: Registers, stack: Stack)  -> Outcome {
    match get_2(offset, ins.clone()) {
        Some((new_offset, a, b)) => {
            match register_pos(a) {
                Some(reg_pos) => {
                    let value = raw_to_value(b, regs.clone());
                    let new_registers = set_value_in_register(regs, reg_pos, value);
                    Outcome::Continue(new_offset, new_registers, stack)
                },
                None => Outcome::Fail,
            }
        },
        None => Outcome::Fail,
    }
}

// push: 2 a
//   push <a> onto the stack
fn i_push(offset: Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    match get_1(offset, ins.clone()) {
        Some((new_offset, a)) => {
            let value = raw_to_value(a, regs.clone());
            let new_stack = stack.push_front(value);

            Outcome::Continue(new_offset, regs, new_stack)
        },
        None => Outcome::Fail,
    }
}

// pop: 3 a
//   remove the top element from the stack and write it into <a>; empty stack = error
fn i_pop(offset: Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    match pop_stack(stack) {
        Some((new_stack, value)) => {
            match get_1(offset, ins.clone()) {
                Some((new_offset, a)) => {
                    match register_pos(a) {
                        Some(reg_pos) => {
                            let new_registers = set_value_in_register(regs, reg_pos, value);
                            Outcome::Continue(new_offset, new_registers, new_stack)
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
fn i_eq(offset: Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
        get_3(offset, ins.clone())
        .and_then(|(new_offset, a, b, c)|
            register_pos(a).map(|reg_pos| {
                    let Value(b_val) = raw_to_value(b, regs.clone());
                    let Value(c_val) = raw_to_value(c, regs.clone());

                    let value = if b_val == c_val {
                        Value(1)
                    } else {
                        Value(0)
                    };

                    let new_registers = set_value_in_register(regs, reg_pos, value);

                    Outcome::Continue(new_offset, new_registers, stack)
                }
            )
        ).unwrap_or(Outcome::Fail)
}

// gt: 5 a b c
//   set <a> to 1 if <b> is greater than <c>; set it to 0 otherwise
fn i_gt(offset: Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    get_3(offset, ins.clone())
    .and_then(|(new_offset, a, b, c)|
        register_pos(a).map(|reg_pos| {
                let Value(b_val) = raw_to_value(b, regs.clone());
                let Value(c_val) = raw_to_value(c, regs.clone());

                let value = if b_val > c_val {
                    Value(1)
                } else {
                    Value(0)
                };

                let new_registers = set_value_in_register(regs, reg_pos, value);

                Outcome::Continue(new_offset, new_registers, stack)
            }
        )
    ).unwrap_or(Outcome::Fail)
}
// jmp: 6 a
//   jump to <a>
fn i_jmp(offset: Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// jt: 7 a b
//   if <a> is nonzero, jump to <b>
fn i_jt(offset: Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// jf: 8 a b
//   if <a> is zero, jump to <b>
fn i_jf(offset: Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// # add: 9 a b c
// #   assign into <a> the sum of <b> and <c> (modulo 32768)
fn i_add(offset: Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    match get_3(offset, ins.clone()) {
        Some((new_offset, a, b, c)) => {
            match register_pos(a) {
                Some(reg_pos) => {
                    let Value(b_val) = raw_to_value(b, regs.clone());
                    let Value(c_val) = raw_to_value(c, regs.clone());
                    let sum = (b_val + c_val) % 32768;
                    // println!("sum {:?}", b_val);

                    let new_registers = set_value_in_register(regs, reg_pos, Value(sum));

                    Outcome::Continue(new_offset, new_registers, stack)
                },
                None => Outcome::Fail,
            }
        },
        _ => Outcome::Fail,
    }
}

// mult: 10 a b c
//   store into <a> the product of <b> and <c> (modulo 32768)
fn i_mult(offset: Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// mod: 11 a b c
//   store into <a> the remainder of <b> divided by <c>
fn i_store(offset: Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// and: 12 a b c
//   stores into <a> the bitwise and of <b> and <c>
fn i_and(offset: Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// or: 13 a b c
//   stores into <a> the bitwise or of <b> and <c>
fn i_or(offset: Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// not: 14 a b
//   stores 15-bit bitwise inverse of <b> in <a>
fn i_not(offset: Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// rmem: 15 a b
//   read memory at address <b> and write it to <a>
fn i_rmem(offset: Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// wmem: 16 a b
//   write the value from <b> into memory at address <a>
fn i_wmem(offset: Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// call: 17 a
//   write the address of the next instruction to the stack and jump to <a>
fn i_call(offset: Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// ret: 18
//   remove the top element from the stack and jump to it; empty stack = halt
fn i_ret(offset: Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// out: 19 a
//   write the character represented by ascii code <a> to the terminal
fn i_out(offset: Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    match get_1(offset, ins.clone()) {
        Some((new_offset, a_raw)) => {
            let Value(val) = raw_to_value(a_raw, regs.clone());
            match char::from_u32(val as u32) {
                Some(c) => print!("{:?}", c),
                None => (),
            }
            Outcome::Continue(new_offset, regs, stack)
        }
        None => Outcome::Fail,
    }
}

// in: 20 a
//   read a character from the terminal and write its ascii code to <a>; it can be assumed that once input starts, it will continue until a newline is encountered; this means that you can safely read whole lines from the keyboard and trust that they will be fully read
fn i_in(offset: Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
    Outcome::Fail
}
// noop: 21
//   no operation
fn i_noop(offset: Offset, ins: Instructions, regs: Registers, stack: Stack) -> Outcome {
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

fn get_1(Offset(offset): Offset, ins: Instructions) -> Option<(Offset, InstructionValue)> {
    match ins.get(offset) {
        Some(a) => Some(
            (
                Offset(offset + 1),
                *a,
            )
        ),
        _ => None,
    }
}

fn get_2(Offset(offset): Offset, ins: Instructions) -> Option<(Offset, InstructionValue, InstructionValue)> {
    let a_arc = ins.get(offset);
    let b_arc = ins.get(offset + 1);

    match (a_arc, b_arc) {
        (Some(a), Some(b)) => Some(
            (
                Offset(offset + 2),
                *a,
                *b,
            )
        ),
        _ => None,
    }
}

fn get_3(Offset(offset): Offset, ins: Instructions) -> Option<(Offset, InstructionValue, InstructionValue, InstructionValue)> {
    let a_arc = ins.get(offset);
    let b_arc = ins.get(offset + 1);
    let c_arc = ins.get(offset + 2);

    match (a_arc, b_arc, c_arc) {
        (Some(a), Some(b), Some(c)) => Some(
            (
                Offset(offset + 3),
                *a,
                *b,
                *c,
            )
        ),
        _ => None,
    }
}
