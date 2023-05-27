loop: .entry
    .push asdf, fjlagsd
    ld xyz -> r0
    .push more
    ld xyz -> r0
    b loop


.stack asdf
abc: .dd 123
xyz: .dw 0

.stack more
    .db 1, 2, 3, 4, 5
