    .boot
    .org 0
_double_fault:                  .dw (.raw @halt)'16
_page_fault:                    .dw (.raw @halt)'16
_access_fault:                  .dw (.raw @halt)'16
_page_align_fault:              .dw (.raw @halt)'16
_instruction_protection_fault:  .dw (.raw @halt)'16
_invalid_instruction:           .dw (.raw @halt)'16
_pipe_0_reset:                  .dw (.raw @reset)'16

reset:
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

halt:
    park
    b halt
