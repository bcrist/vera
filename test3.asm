loop: .entry
    .push asdf
    ld xyz -> r0
    ld end -> r0
    .push more
    ld xyz -> r0
    .pop more, asdf
    b loop


.stack asdf
abc: .dd 123
xyz: .dw 0
a1bc: .dw 1
    .db .idx .r 1
end: .nil

.stack more
xyz:    .db 1, 2, 3, 4, 5
