[assembler] CLI
[assembler] Consider allowing things that expect constant expressions to also accept absolute addresses (.db/.dw/.dd, length casts, etc.)
[assembler] Don't allow branches across .push/.pop boundaries
[assembler] "Address of end of chunk" literal?  Or maybe operator that gives the end address of the chunk that contains a queried address?  the latter is hard to implement in current code.
[assembler] Create disassembler
[assembler] add a kentry output mode to write a "header" file that exports the addresses of kentry labels
[assembler] add cycle counts to listing output
[assembler] arbitrary precision shifts
[assembler] arbitrary precision multiply
[assembler] Improve error context printing
[assembler] improve vscode plugin
[assembler] improve auto-placement of chunks

[test] Use assembler for test_instruction_behavior
[test] Use assembler and disassembler for test_instruction_encoding

[compile_arch] Compute min/max cycles per instruction, add to encoding sx
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
