## Add instructions for doing block transfers to/from FLASH/PSRAM
## Fix BB, BBN instructions in _branch.zig
## Move Simulator to be a top-level struct; same with other files
## Implement interrupt controller
## Implement serial device for console
## Normalize naming conventions
Use zig standards.
Remove individual functions to set each control signal individually (`STAT_OP(.ZN_from_L);` etc.)
Instead use `assign(.stat_op, .zn_from_l);` and create helper functions for things where that's too unclear

## Rework logic unit
## Fix LH_SRC.logic, .JH naming

## Rename HALT to WFI (wait for interrupt)

## Add gui to simulator

## Separate AT enable flag for code/stack vs data?
Then you don't need separate raw read/write instructions, maybe can reuse encoding for fast "DMA" transfers from/to cold storage and ROM

## Handle alias_XXXX functions in microcode generator (see alias_0100 -> NOPE)

## Instructions that may not be useful:
    TESTBZ
    C IP+imm4s -> X*
    C SP+immbs -> X*

## HTML instruction documentation with "disassembly" of control signals
## HTML instruction opcode table

## Consider faster loading of small constants outside imm5:
    C imma[16,...] -> R*/X*
    C imma[...,-17] -> R*/X*

## Add more cycle validation in cycle_builder.finish()
## cycle_builder.branch(): check that either base is .zero, or RSN is not being loaded this cycle
## Add instruction parameters to instruction_builder.print_cycle_path()
## ParameterEncoding.default_to_param
## microcode.zig: use conditional slots if we run out of unconditional ones.
## Documentation
## Improve SST39VF802C emulation accuracy in memory.zig (timing, status polling, IDs, write protection, etc.)
