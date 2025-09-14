[arch] Only reload ASN6 when special_op is not .write_to_other_rsn or special is .rsn_from_l and sr2ri is .asn

[isa] Allow arbitrary mnemonics and mnemonic suffixes - maybe use hash of text for lookup?  Can still use enum literals for manually initializing mnemonics/suffixes in the few places necessary



[arch] conditional load/store instructions - equivalent to a conditional jump just before, but 1 cycle faster when the load does need to happen

[arch] Use 4x CY7C028V-15AC (64kx16 DPSRAMs) for address translation.  4 extra address lines - ASN6 instead of ASN4, increase slot to 8 bits, decrease tag to 12 bits.  Gives 2 extra bits in entry (required pipeline number?)
[arch] 64 GPRs per RSN instead of 32; 64 RSNs instead of 128
[arch] Don't reserve half of RSNs for fault handling; instead use 4 hardcoded RSNs like for interrupts (except 4-7 instead of 0-3) - need a way for FRET to know which RSN to return to though.  Remove Special_Op.toggle_rsn

[sdram] SDRAM for unsharable memory - add pipe ID to address translation entries & new fault when it isn't owned by the current pipe? (replaces present/update-frame-state flags - use a sentinel I/O frame to indicate not present, always update frame state for ram locations?  Or have a separate bitmap ram to determine which frames are dirty-tracked)   physical address space: 64MB total (26 bit address), 16MB SRAM, 16MB I/O, 32MB private SDRAM
[sdram] refresh timing: Minimum required refreshes is 839 per 32k ucycles (8k per 64ms).  10 bit counter counts number of refreshes seen so far this 32768 cycle period.  First SR latch is set when refresh counter reaches 896.  Second SR latch is set when the 15-bit ucycle counter reaches 31872.  When the 15-bit ucycle counter resets, both SR latches and the refresh counter are also reset.  Refresh interrupt is asserted whenever the first SR latch is not set, but the second one is.

[arch] consider allowing fault handlers to set a flag that overrides the next bus operation - to allow more reasonable implementation of unaligned load/store crossing page boundaries - encode this data in the AT entry - instead of a frame number, encode the address offset that is being overridden and the pipe number being overridden - or just a flag in the status register; since RSN is only 6 bits now, there is an extra bit; maybe have a FRETO instruction that sets that bit while returning.  Maybe reuse the "last AT op" register for containing the replacement data.  Or maybe just have the flag disable the load into the DR; use the value in the DR instead?

[isa] write instructions file as zon instead of s-exp
[isa] "select" instructions - rough equivalent of cmov; pop top two register stack values, then re-add one or the other based on condition

[design] bus request & bus available signals - allow DMA devices to take exclusive control of bus - request is clocked in setup -> compute stage and compute -> transact stage.  State during transact stage is broadcast back to the bus as "available" signal. the current cycle, causing it to be run again
[design] mass storage controller based on NAND Flash & FIFO
[design] Implement interrupt controller
[design] hardware counters (global): ucycles
[design] hardware counters (per-pipe, or maybe per-RSN, excluding interrupt/fault handlers & cycles that trigger faults or sleep): initial instruction loads, auxiliary instruction loads (e.g. immediates), non-instruction loads, stores, & ucycles where none of these occur.  Since at most one of these happens every cycle, they can all be stored in an SRAM with an external RMW incrementer.  Probably make it a dual port SRAM so that the cpu can access them without interfering.  Interrupt on overflow?

[compile/assembler] Way to mark instructions that do IP-relative 32b loads - they must be assembled with even alignment

[compile] HTML instruction documentation with "disassembly" of control signals
[compile] Add Slot_Data flag for slots that may require kernel mode
[arch/microsim] Fault if a block load/store instruction is executed on an invalid context
[microsim] Add a warning if two pipes ever use the same RSN concurrently
[microsim] Warning if an interrupt or fault ends with TI > 0

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




[frontpanel] RSN, pipe number LED displays, indicator that pipe being displayed is fixed instead of the pipe currently about to finish transact stage
[frontpanel] 4x32 LED displays - (virtual addr, physical addr, D, L), (system load by pipe), (overall system load, physical memory usage, cold memory usage, dirty memory)
[frontpanel] locking toggle switch - debug/stop/run - debug is like run, but stops any time a fault occurs or an interrupt handler is entered
[frontpanel] momentary toggle switch - single step - down for one full microcycle (or continue in debug run mode), up for one clock cycle
[frontpanel] momentary toggle switch - prev/next mode
[frontpanel] momentary toggle switch - prev/next submode
[frontpanel] momentary toggle switch - inc/dec address
[frontpanel] locking toggle switch - address granularity - 1/2/4 bytes
[frontpanel] momentary toggle switch - widen/narrow data field
[frontpanel] momentary toggle switch - move data field left/right
[frontpanel] momentary toggle switch - clear/set data field
[frontpanel] momentary toggle switch - inc/dec data field
[frontpanel] locking toggle switch - revert/write data