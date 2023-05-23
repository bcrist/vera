fib: .entry
    .org 0x10000
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
    c asdf -> r0.unsigned
    ret
something:
    ret
//    axe

.const x
.keep
.org 0x4000
.align 4096, (.raw @loop) .trunc 12 // essentially says 'put this at the same page offset as loop has in its page'
.db 0x12+0x65, (256 - 1).trunc 8, 0xFF + 0
.dd 0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF + 1
str: .db 1,2,3,4
    .dw 1,2,3,4,"Hellorld!" ** 9, \
    4, \
    5, \
    1111, \
    123
    .dd 1,2,3,4,"Hellorld!", 0xF .signed'40, 255 .signed'40, -1 .unsigned'40
    .db "\(U+12345)"
    .db "\(=AA++128)"
    .db "\()\(   )"
    .db "\(  1   0d11  0b1100 0q1122 0o1122 0x1122)"
    .db "\(  235 )"
    .db "\(13 10)\r\n"
    .db \\asdf
        \\     
        \\
        , 456789
    .db \\

    .local asdf 1234