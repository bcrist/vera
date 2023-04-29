## Assembler
.db/dw/dd directives

.local defs

Usage of a label that's fixed by an .org directive should not cause the expression to be marked .constant_depends_on_layout

Warn when .org, .align, or an implied .align causes a hole in code that is reachable through control flow

How do we deal with stack labels since Sections arent set up until encode phase?

Multi-line string literals

Warn on multiple files with same name

Consider having stack sections use a different namespace from other section types, and make them local to the file they're defined in.

Use assembler for test_instruction_behavior
Use assembler and disassembler for test_instruction_encoding

## Microcode decompiler

## Validate that the same instruction encoding doesn't map to multiple opcodes/aliases

## Add gui to simulator
Single step control
Register views for pipe 0/1/2
    Include disassembly view of memory around IP

Memory view - raw (may open multiple for different locations)

Colorization
    orange=will be updated in the next clock cycle (transact stage)
    yellow=will be updated at the end of the current microcode cycle (i.e. it is setup or compute stage)
    cyan=will be read at the next clock cycle (setup stage for registers, transact stage for memory)
    blue=was or will be read during this micocode cycle

Load/save state from/to file

Interrupt controller

Virtual console

History of instructions executed, microcode cycles
    Allow undo during single stepping
    Don't track history when running in real time?

Error console
    Access to invalid memory
    Faults

## SX library - automatic (de)serialization of structs

## Add bit to address translator entries? - allow pages to disallow kernel access - extra layer of protection to avoid kernel accidentally running user code with privilege.

## Consider Shifter overflow flag - if any shifted out bits differ from the last bit that wasn't shifted out
## Consider removing wait states

## block memcpy instruction that uses PSRAM?
## count zeroes with mask instruction?

## Implement interrupt controller
## Improve SST39VF802C emulation accuracy in memory.zig (timing, status polling, IDs, write protection, etc.)
## Implement serial device for console

## Refactor instruction encoding
## ParameterEncoding.default_to_param

## Revamp instruction_builder.print_cycle_path()
## HTML instruction documentation with "disassembly" of control signals
## HTML instruction opcode table

## cycle_builder.branch(): check that either base is .zero, or RSN is not being loaded this cycle
## Add more cycle validation in cycle_builder.finish()

## Separate AT enable flag for code/stack vs data?

## Consider faster loading of small constants outside imm5:
    C imma[16,...] -> R*/X*
    C imma[...,-17] -> R*/X*

## Instructions that may not be useful:
    TESTBZ
    C IP+imm4s -> X*
    C SP+immbs -> X*
