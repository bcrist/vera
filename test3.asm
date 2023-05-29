loop: .entry
    .push asdf
    ld abc -> r0
    ld xyz -> r0
    ld a1bc -> r0
    ld end -> r0
    .push more
    ld xyz -> r0
    .pop more, asdf
    b loop


.stack asdf
abc: .zd
xyz: .zw 0
a1bc: .zw 1
    .zb .idx .r 1
end: .nil

.stack more
xyz2:    .db 5, 1, 3, 4, 5
.zb 4
