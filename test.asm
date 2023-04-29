fib: .entry etext
    .org 0x2000
    .def n r0
    .def last r1
    .def prev r2
    .def temp r3
    c   0 -> prev
    c   1 -> last
    b.np _end
_loop:
    dec n -> n
    c   last -> temp
    add last, prev -> last
    c   temp -> prev
    b.p _loop
_end:
    c   last -> r0
    ret
