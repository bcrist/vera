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

## Consider Shifter overflow flag - if any shifted out bits differ from the last bit that wasn't shifted out
## Consider removing wait states

## Rework logic unit
Fix BB, BBN instructions in _branch.zig

## Handle alias_XXXX functions in microcode generator (see alias_0100 -> NOPE)
## Do conditional continuations by passing a function pointer, rather than the continuation number (and relying on )

## block memcpy instruction that uses PSRAM?

## Implement interrupt controller
## Improve SST39VF802C emulation accuracy in memory.zig (timing, status polling, IDs, write protection, etc.)
## Implement serial device for console

## Refactor instruction encoding
## ParameterEncoding.default_to_param

## microcode_builder.zig: use conditional slots if we run out of unconditional ones.
## Add instruction parameters to instruction_builder.print_cycle_path()
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
