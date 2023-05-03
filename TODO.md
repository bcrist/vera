[assembler] validate number and type of parameters for directive instructions
[assembler] validate that data and code are placed in the correct sections
[assembler] output files
[assembler] .local defs
[assembler] check for shadowed symbols
[assembler] Usage of a label that's fixed by an .org directive should not cause the expression to be marked .constant_depends_on_layout
[assembler] Multi-line string literals
[assembler] Warn on multiple files with same name
[assembler] .stack sections, .push, .pop
[assembler] Consider having stack sections use a different namespace from other section types, and make them local to the file they're defined in.
[assembler] .kentry label alignment
[assembler] section allowed page .range
[assembler] initial arrow (nil first parameter)
[assembler] Create disassembler
[assembler] .str/.kstr "whatever" - creates a ptr+len data declaration - creates a new block in the `.const strings` section with the data (.kconst for .kstr) - may dedup data (enable with global assembler flag?)

[test] Use assembler for test_instruction_behavior
[test] Use assembler and disassembler for test_instruction_encoding

[compile_arch] Add more aliases
[compile_arch] Decompile/dump to HTML
[compile_arch] Validate that the same instruction encoding doesn't map to multiple opcodes/aliases
[compile_arch] Revamp instruction_builder.print_cycle_path()
[compile_arch] HTML instruction documentation with "disassembly" of control signals
[compile_arch] HTML instruction opcode table
[compile_arch] cycle_builder.branch(): check that either base is .zero, or RSN is not being loaded this cycle
[compile_arch] Add more cycle validation in cycle_builder.finish()

[microsim] Memory view - raw (may open multiple for different locations)
[microsim] disassembly view of memory around IP
[microsim] Load/save state from/to file
[microsim] Virtual console
[microsim] Interrupt controller
[microsim] History of instructions executed, microcode cycles (don't track when running in real time)
[microsim] Allow undo during single stepping
[microsim] Error console - Access to invalid memory, Faults
[microsim] Improve SST39VF802C emulation accuracy in memory.zig (timing, status polling, IDs, write protection, etc.)

[design] Add bit to address translator entries? - allow pages to disallow kernel access - extra layer of protection to avoid kernel accidentally running user code with privilege.
[design] Consider Shifter overflow flag - if any shifted out bits differ from the last bit that wasn't shifted out
[design] Consider removing wait states
[design] count zeroes with mask instruction?
[design] Implement interrupt controller
[design] Consider separate AT enable flag for code/stack vs data?

[sx] automatic (de)serialization of structs