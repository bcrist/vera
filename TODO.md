[arch] Consider making some instructions only allow even registers
[compile_arch] Add push/pop/ld/st instructions that use a bitset (like arm)
[arch] Add Domain.bitset - encoded as (1 << value).  Encoders using it can overlap other bitsets, but not other types of domains
[arch] Finish block diagram
[arch] Finish Encoding_Database.similar_encodings
[arch] print_encoding - print placeholders with valid ranges and constraints specified
[arch/microsim] Fault if a block load/store instruction is executed on an invalid context

[compile_arch] bp-relative loads/stores
[compile_arch] rp-relative loads/stores
[compile_arch] Microcode_Builder.Slot_Location.forced_bits - don't include in hashing; just update it to add more bits if necessary
[compile_arch] Refactor opcodes.zig - just a bunch of Encoder constants instead of enums?
[compile_arch] Compute min/max cycles per instruction, add to encoding sx
[compile_arch] Decompile/dump to HTML
[compile_arch] Validate that the same instruction encoding doesn't map to multiple opcodes/aliases
[compile_arch] Revamp instruction_builder.print_cycle_path()
[compile_arch] HTML instruction documentation with "disassembly" of control signals
[compile_arch] cycle_builder.branch(): check that either base is .zero, or RSN is not being loaded this cycle
[compile_arch] ensure exec_next_insn() is only used in microcode of instructions where isa.branch_kind returns nonbranching or conditional

[assembler] Fix EDB loading
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
[assembler] better handling of degenerate/recursive cases where the layout never reaches an equilibrium
[assembler] add an acceleration structure inside Section for finding partially filled chunks with >= N bytes free
[assembler] Listing.write_instruction_line - analyze insn.encoding.encoders to figure out which chunks to pass to write_grouped_data_and_source
[assembler] Explain magic constant in output.write_sim_sx_page for boot flash

[test] Use assembler for test_instruction_behavior
[test] Use assembler and disassembler for test_instruction_encoding

[microsim] implement sleep flag based on when all contexts are executing PARK
[microsim] fix Memory device
[microsim] Fix FrameTracker device
[microsim] Memory view - raw (may open multiple for different locations)
[microsim] disassembly view of memory around IP
[microsim] Load/save state from/to file
[microsim] Virtual console
[microsim] Interrupt controller
[microsim] History of instructions executed, microcode cycles (don't track when running in real time)
[microsim] Allow undo during single stepping
[microsim] Error console - Access to invalid memory, Faults
[microsim] Improve SST39VF802C emulation accuracy in memory.zig (timing, status polling, IDs, write protection, etc.)

[design] Implement interrupt controller

[sx] automatic (de)serialization of structs
