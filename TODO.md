[isa] conditional load/store instructions - equivalent to a conditional jump just before, but 1 cycle faster when the load does need to happen
[isa] "select" instructions - rough equivalent of cmov; pop top two register stack values, then re-add one or the other based on condition

[sdram] SDRAM for unsharable memory - physical address space: 64MB total (26 bit address), 16MB SRAM, 16MB I/O, 32MB private SDRAM
[sdram] refresh timing: Minimum required refreshes is 839 per 32k ucycles (8k per 64ms).  10 bit counter counts number of refreshes seen so far this 32768 cycle period.  First SR latch is set when refresh counter reaches 896.  Second SR latch is set when the 15-bit ucycle counter reaches 31872.  When the 15-bit ucycle counter resets, both SR latches and the refresh counter are also reset.  Refresh interrupt is asserted whenever the first SR latch is not set, but the second one is.

[frametracker] always update frame state for ram locations?  Or have a separate bitmap ram to determine which frames are dirty-tracked

[design] bus request & bus available signals - allow DMA devices to take exclusive control of bus - request is clocked in setup -> compute stage and compute -> transact stage.  State during transact stage is broadcast back to the bus as "available" signal. the current cycle, causing it to be run again
[design] mass storage controller based on NAND Flash & FIFO
[design] Implement interrupt controller
[design] hardware counters (global): ucycles
[design] hardware counters (per-pipe, or maybe per-RSN, excluding interrupt/fault handlers & cycles that trigger faults or sleep): initial instruction loads, auxiliary instruction loads (e.g. immediates), non-instruction loads, stores, & ucycles where none of these occur.  Since at most one of these happens every cycle, they can all be stored in an SRAM with an external RMW incrementer.  Probably make it a dual port SRAM so that the cpu can access them without interfering.  Interrupt on overflow?

[compile/assembler] Way to mark instructions that do IP-relative 32b loads - they must be assembled with even alignment

[compile] HTML instruction documentation with "disassembly" of control signals
[compile] Add Slot_Data flag for slots that may require kernel mode
[microsim] Add a warning if two pipes ever use the same RSN concurrently
[microsim] Warning if an interrupt or fault ends with TI > 0

[isa] Finish Encoding_Database.similar_encodings

[compile] Revamp instruction_builder.print_cycle_path()

[assembler] .dw/.dd => .dh/.dw (words are 32 bit now)
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