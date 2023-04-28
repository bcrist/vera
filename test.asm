fib: .entry etext
    .org 0x2000
    c   1 -> r0
    dup r0 -> x0
_loop:
    c   r0      -> r2
    add r0, r1  -> r0
    c   r2      -> r1
    c 4 * 300 -> r0
    b   (_loop) - 1

.code text
    .keep
x: c 12345 -> x0
    b x

.entry etext
    c r0 -> r15
    .db "text"

.entry etext32
    c r15 -> r0
    .db "asdf"