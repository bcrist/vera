.boot
    .org 0x10
    .def n r0
    .def last r1
    .def prev r2
    .def temp r3
    c 0 -> prev
    c 1 -> last
loop:
    c last -> temp
    add last, prev -> last
    c temp -> prev
    b   loop
