.db "asdf"

fib: .entry text
    .org 1000
    c   0x10001 -> x0
_loop:
    c   r0      -> r2
    add r0, r1  -> r0
    c   r2      -> r1
    b   _loop
    ld  something
_loop2:
    .db asdf

something:
else:
    .data asdf
_also:
    .db "123456abcdef"

.section a