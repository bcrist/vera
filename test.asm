fib: .entry text
    .org 1000
    c   1 -> r0
    dup r0 -> x0
_loop:
    c   r0      -> r2
    add r0, r1  -> r0
    c   r2      -> r1
    b   _loop
