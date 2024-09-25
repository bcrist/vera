[compile] HTML instruction documentation with "disassembly" of control signals
[compile] Add Slot_Data flag for slots that may require kernel mode
[arch/microsim] Fault if a block load/store instruction is executed on an invalid context

[arch] Finish block diagram
[arch] Finish Encoding_Database.similar_encodings

[compile] Revamp instruction_builder.print_cycle_path()

[assembler] Need more robust checking for page alignment faults since we now load 24b for instructions
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
