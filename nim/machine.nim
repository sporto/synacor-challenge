import strutils, fp/list, fp/option

type
    Code = distinct int

proc next(stack: List[int]): int

# add: 9 a b c
#   assign into <a> the sum of <b> and <c> (modulo 32768)
proc do_add(stack: List[int]): int =
    1

# out: 19 a
#   write the character represented by ascii code <a> to the terminal
proc do_out(stack: List[int]): int =
    let a = stack.head
    echo a

    next(stack.drop(1))

proc process_instruction(code: Code, stack: List[int]): int =
    case code.int:
        of 9:
            do_add(stack)
        of 19:
            do_out(stack)
        of 21:
            do_out(stack)
        else:
            1

proc next(stack: List[int]): int =
    let first = stack.headOption()

    if first.isDefined:
        let code = first.get
        let rest = stack.drop(1)

        process_instruction(code.Code, rest)
    else:
        1

proc run*(input: string): int =

    let stack: List[int] = input
        .split(",")
        .asList
        .map(proc (c: string): int =
            parseInt(c)
        )

    next(stack)

let input = "9,32768,32769,4,19,32768"

echo run(input)