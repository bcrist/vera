fib: .entry etext
    .org 0x2000
    c   1 -> r0
    dup r0 -> x0
_loop:
    c   r0      -> r2
    add r0, r1  -> r0
    c   r2      -> r1
    b   _loop
    c ("asdf" ++"A"**16) .trunc (16*8) -> r0
