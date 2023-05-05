fib: .entry
    .org 0x1000
    .def n r0
    .def last r1
    .def prev r2
    .def temp r3
    c   0 -> prev
    c   1 -> last
    b.np _end
loop:
    dec n -> n
    c   last -> temp
    add last, prev -> last
    c   temp -> prev
    b.p loop
_end:
    c   last -> r0
    c .raw str -> x15
    ld .d x15 -> r0
    ret

.const
//.align 4096, (.raw @loop) .trunc 12 // essentially says 'put this at the same page offset as loop has in its page'
//str: .db 1,2,3,4,"Hellorld!" ** 9
.align 128
str: .db -1 ' 1