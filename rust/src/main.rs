extern crate im;

use im::Vector;
use im::HashMap;
use std::sync::Arc;

type Stack = Vector<i32>;
type Registers = HashMap<i32, i32>;

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
        .collect::<Vec<i32>>();

    let stack: Stack = Vector::from(vec);

    next(stack, registers)
}

fn next(stack: Stack, registers: Registers) -> usize {
    match stack.pop_front() {
        Some((code, rest)) =>
            instruction(*code, rest, registers),
        None => 1,
    }
}

fn instruction(code: i32, stack: Stack, registers: Registers) -> usize {
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
            let b_val = get_value(b, registers);
            let c_val = get_value(c, registers);
            next(stack.skip(3), registers)
        },
        _ => 1,
    }
}

fn get_value(n: Arc<i32>, registers: Registers) -> Arc<i32> {
    if n <= 32767 {
        Arc::new(n)
    } else if n <= 32775 {
        let pos = n - 32768;
        registers.get(&pos).unwrap_or(Arc::new(0))
    } else {
        Arc::new(0)
    }
}

fn get_3(stack: Stack) -> Option<(Arc<i32>, Arc<i32>, Arc<i32>)> {
    let aa = stack.get(0);
    let bb = stack.get(1);
    let cc = stack.get(2);

    match (aa, bb, cc) {
        (Some(a), Some(b), Some(c)) => Some((a, b, c)),
        _ => None,
    }
}
