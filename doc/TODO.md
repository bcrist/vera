## Add gui to simulator

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
