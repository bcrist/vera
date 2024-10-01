This directory is a zig package, used by adding a `.path = "relative/path/to/here"` dependency in `build.zig.zon`.
This package exports the `arch` module, which contains a variety of types and helpers for representing the registers, buses, etc. that exist at the hardware level.


# Microarchitecture Overview

## Buses
Name | Width | Description
--- | --- | ---
`J` | 32b | Left-hand operand for computation
`K` | 32b | Right-hand operand for computation
`L` | 32b | Result of computation and source for (most) register writes
`D` | 32b | Data from/to memory or devices
`DA`| 16b | Data bus for "even" 16b half-words (LSB half of `D` when accessing an align-4 address)
`DB`| 16b | Data bus for "odd" 16b half-words (MSB half of `D` when accessing an align-4 address)
`VA`| 32b | Virtual Address
`P` | 20b | Page Number; upper bits of `VA`
`N` | 12b | Page/Frame Offset in bytes; lower bits of `VA`
`F` | 14b | Physical frame number; high bits of `DA`, `DB` bus address
`AA`| 10b | Low bits of address for the `DA` bus
`AB`| 10b | Low bits of address for the `DB` bus

## Registers
Name | Width | Description
-- | --- | ---
`R0` - `R31` | 32b | General purpose registers
`STAT` | 32b | Status flags (some read-only)
`RSN` | 7b | Register set number
`ASN` | 4b/32b | Virtual address space number
`SP` | 32b | Stack pointer
`BP` | 32b | Stack base/frame pointer
`IP` | 32b | Instruction pointer
`RP` | 32b | Return pointer
`UXP` | 32b | User context pointer
`KXP` | 32b | Kernel context pointer
`DR` | 32b | Data register[^1]
`IR` | 16b | Instruction register[^1]
`TI` | 5b | Register stack pointer[^1]

[^1]: These registers are implementation details from the perspective of the assembly language user.

## CPU Status Flags
Name | Width | Description
-- | --- | ---
`PIPE` | 2b | Current pipe number (0-2)
`Z`  | 1b | Zero flag
`N` | 1b | Negative flag
`C` | 1b | Carry/borrow flag
`V` | 1b | Overflow flag
`K` | 1b | Kernel mode
`A` | 1b | Address translation enable
`RSN` | 7b | Register set number
`TI` | 5b | Register stack pointer
`PUCS` | 12b | Microcode slot of the previous cycle (used for fault recovery)

## Clock
All activity in the CPU is coordinated using a single clock signal. A new clock cycle begins on the rising edge of the clock. This is the point where edge-triggered registers will load a new value. Asynchronous writes (i.e. to register files or RAM) begin when the clock goes low and end at the rising edge. Therefore the addressing signals for these chips must settle in less than half a clock cycle to avoid corruption of other locations.

In order to avoid bus collisions, all buses that are sourced from one of multiple separate line drivers (e.g. J, K, L, D) should only drive the bus during the second half of the cycle, when CLK is low.

### Sleep Mode
When there is no work for a pipeline to do, it should execute the `PARK` instruction.  This allows certain parts of the pipeline to enter low-power mode for the cycle, and if all pipelines are parked simultaneously, the CPU clock can be stopped entirely, until an interrupt is ready to be serviced.

## Microcode & Pipelining
Each instruction is executed with one or more "microcode cycles".  A microcode cycle has four pipeline stages.  The microcode address is determined during the first stage and remains constant throughout the following 3 stages.  Conceptually, you can imagine all the control signals being decoded in that first stage, though in hardware, some signals are actually decoded in later stages to reduce the number of pipeline registers needed.

Even though a microcode cycle takes four clock cycles, each stage is actually occurring simultaneously on every clock cycle, so there are really four instructions executing in parallel.  This is however a bit different from pipelining in a traditional RISC processor.  Instead of executing four consecutive instructions of a single program, we execute one instruction from each of up to four independent programs.  The separate "pipeline threads" don't normally share data, so all of the hazards, branch delay slots, and precise exception issues that complicate a normal pipelined architecture, simply don't exist.

The downside of a "threaded pipeline" approach is of course that for single-threaded workloads, the CPU appears four times slower than a traditional pipelined CPU.  On the other hand, we get to pretend that we have a 4-core CPU, and if we have enough work to saturate four threads, we can get even better throughput than a traditional pipelined CPU.

You might also note that this kind of design needs to have separate state for each "thread."  I will often refer to a particular set of pipeline thread state as simply a _Pipeline_ or _pipe_.  Most of this happens automatically as data is clocked into the next pipeline stage at the end of a clock cycle, but since we're using SRAM for the main register files, we need separate "slots" for each pipe.

The inspiration for this scheme came from [Dieter Mueller's notes on register files for his TREX CPU](http://www.6502.org/users/dieter/tarch/tarch_3.htm). Apart from the [Innovasic FIDO](https://www.analog.com/media/en/technical-documentation/data-sheets/fido1100.pdf) chip mentioned there, I'm not aware of any other extant CPUs that use this technique, though I'm sure there must be at least a few others.  If you're aware of or have built one, let me know!

### Pipeline Stages

- Decode
    - `IR` is decoded
    - Register file read and write indices are computed
    - The microcode slot to execute is selected
    - Control signals needed in the Setup stage are decoded
- Setup
    - Data is looked up in the register file
    - Data is driven to the `J` and `K` buses
	- A virtual address is calculated (if needed)
	- Control signals needed in the Compute stage are decoded
- Compute
	- Compute units operate on `J` and `K` buses
	- Address translation is performed, generating `F` for the next stage
	- `AA`, `AB`, and other bus-control signals are computed for the next stage
	- Any faults must be detected by the end of the stage
- Transact
	- Data is driven to the `L` and `D`/`DA`/`DB` buses
    - Information about the most recent address translation is stored in a dedicated register
        - Note this happens even if there was a fault, because it's used to determine the context of page faults
    - If there is not a fault:
        - System bus writes are performed
        - `DR` and `IR` are loaded from `D` if necessary
        - General-purpose and special-register files are written if necessary
        - Status flags are updated
        - Guard registers are updated
        - Address translation entries are updated if necessary
        - When returning from a fault handler, the previous microcode slot to return to is prepared

Note that unlike a typical RISC pipeline, there is no dedicated instruction fetch stage.  Each instruction is responsible for updating the `IP` register for the next instruction, as well as loading the instruction data into `DR` and `IR`.  The updates to `IR` and `IP` must happen in the Transact stage of the last cycle of the previous instruction, and indeed most instructions that don't reference memory can load the next instruction in parallel with the actual computational work.  Instructions that take multiple microcode cycles can load `DR` for the next instruction during any cycle, as long as the old data isn't needed anymore.  For instance, a load or store instruction with a register + offset addressing mode needs to compute the final address in the first cycle, then do the actual load/store during the second cycle.  The memory bus is free during the first cycle to load the next instruction, instead of having to add a third cycle to load the next instruction.


## Compute Units

### ALU
- 32b addition
- 32b subtraction
- 32b bitwise AND
- 32b bitwise OR
- 32b bitwise XOR

Implemented with a pair of chained L4C381 16b ALU chips.  If sufficiently fast ones cannot be acquired, it may be possible to duplicate the high half, computing the result with both states of `Cin`, then muxing the correct result based on the actual `Cout` from the first stage, once ready.

### Shifter
- 32b left and right shifts
    - Correctly handles shift amounts > 31
    - Carry flag can be set to state of bit "just barely" shifted out
    - Overflow flag can be set if any 1 bits were shifted out
    - Bits shifted in can be either 0, copies of the MSB, or copies of the carry flag
- Allows swapping/reversing bytes
- Allows swapping 16b halves
- Allows reversing all bits in bytes/halves/full word.

### Multipler
- 16b inputs
- 32b output
- Inputs can be independently treated as either signed or unsigned 
- Inputs can be sourced from either the lower or upper halves of the `J`/`K` buses
- Output can be left shifted 16b

Implemented with a pair of IDT7217 multiplier chips.  The ones I have are not fast enough to output both halves of the result on the multiplexed 16b output pins within a single cycle, so I'm using two, feeding them with exactly the same inputs, and outputting the low half on one and the high half on the other.

### Count/Extend Unit
- Count all set or clear bits (i.e. `popcnt`)
    - Optionally with a mask to count only certain bits
- Count leading set or clear bits
- Count trailing set or clear bits
- Saturate any 1 bits to the left/right (i.e. there will be no 0's after the first 1 is seen when reading in that direction)
- Saturate any 0 bits to the left/right
- Zero- or sign-extend an arbitrary width < 32b
    - Truncates any higher bits, replacing them with 0 or the sign bit
    - Can set overflow flag if any bits were changed


## Address Translation
During the Setup stage, a 32b virtual address is generated.  The upper 20 bits is called the _Page_, and during the Compute stage, it is transformed into a 14b _Frame_ number.  The frame, combined with the original lower 12 bits of the virtual address, makes up the _Physical Address_ to be accessed in the Transact stage.

Note, the page number can be further split into a 6b _Slot_ (LSBs) and a 14b _Tag_ (MSBs).  The meaning of these fields will become apparent below.

```
Virtual Address:  PPPP PPPP PPPP PPPP PPPP OOOO OOOO OOOO
             or:  TTTT TTTT TTTT TTSS SSSS OOOO OOOO OOOO

Physical Address: ---- --FF FFFF FFFF FFFF OOOO OOOO OOOO

O = Offset bit
P = Page bit
F = Frame bit
T = Tag bit
S = Slot bit
```

The transformation of page to frame is done with a 2-way set-associative cache that lives in dedicated SRAMs.  These SRAMs are addressed using the following data, which together identify which cache "set" we'll be working with during this cycle:

```
AT Entry Address:  AAAA GGSS SSSS

S = Slot bit
G = Group bit
A = Address Space Number bit
```

The slot bits come directly from the virtual page number.  The ASN bits are the least significant bits of the pipe's ASN register (which is a full 32b register, but only the least significant bits are meaningful to the hardware; the OS can use the upper bits however it wants).  The Group bits depend on what kind of bus operation is happening:

- Instruction read
- Stack read/write
- Data read
- Data write

### Translation Entries
The data read out of the translation SRAMs consists of two Translation Entries, corresponding to the two "ways" of the cache.  Each entry contains:

```
AT Entry:  TTTT TTTT TTTT TTAA PUFF FFFF FFFF FFFF

F = Frame number bit
U = Update flag (used by frame state tracker)
P = Present flag (indicates if entry is valid)
A = Access policy bit
T = Tag bit
```

### Translating Addresses
Address translation is normally enabled only when `A` flag is set.  This is initially disabled on reset, since the contents of the translation cache are unknown.  Individual microcode instructions can also force a bus operation to skip address translation by setting `VASPACE` to `raw`.  When address translation is disabled, the least significant bits of the page number are used directly as the frame number.

Otherwise, hopefully, one or the other entry's tag matches the virtual address tag, and the Present flag is set.  If so, it is called the "matching entry" and the other one is called... the "other entry".

When there is no matching entry, a page fault is generated.  When there is a matching entry, the access policy bits are checked:

- Unprivileged pages can always be accessed.  Loading an instruction from such a page will clear the K flag.
- Kernel-private pages can only be accessed when the K flag is set
- Kernel-entry pages can be accessed when the K flag is set, or when loading an instruction from one of a set of specific offsets within the page.  This will then set the K flag, allowing the next instruction to be accessed in the same page.

If the access policy check fails, an access fault is generated.  Otherwise, the frame bits are sent on to the Transact stage for the physical bus operation.

Note the access policy makes no distinction between reads and writes, but since data reads and writes use different cache sets, it is still possible to map read-only or write-only pages.

### Address Offsets
At the same time that the page is being translated to a frame, some operations are also being performed on the address offset bits:

- Since we can only check one page per cycle, we need to make sure that reads or writes do not cross the boundaries of a page.  If not, a _page align fault_ is generated.  Note this check happens even if address translation is disabled, but it is skipped if there is no read/write operation happening on the system/memory bus.
- 32b accesses must be aligned to even byte addresses, otherwise an _align fault_ is generated.
- The `AA` and `AB` addresses are computed from the address offset.  `AB` is simply the 10 most significant bits.  `AA` is the same as `AB` when the two LSB bits are 0b00 or 0b01, but it is incremented if the LSB bits are 0b10 or 0b11.
- The `LBA`, `UBA`, `LBB`, `UBB` byte enable signals are computed, to tell memory/devices which bytes the CPU is interested in reading/writing.
 
# Adding and Updating Entries
When the kernel wants to add a new translation, it will set `ATOP` to `update`.  During the Transact stage, the data from the L bus will then be stored into the primary entry for the selected cache set.  The previous primary entry will be moved to the secondary entry, and any previous secondary entry will be dropped.

# Invalidating Entries
When the kernel wants to invalidate an address range (i.e. when releasing a committed address range), it will set `ATOP` to `invalidate`.  For each of the two entries/ways, if the entry's tag ANDed with the LSBs of the `L` bus matches the tag from the virtual address ANDed with the LSBs of the `L` bus, that entry
will be cleared.  This makes it possible to initialize a new empty address space quickly, as well as invalidating a large address range (>64 pages) without
needing to iterate through each page address individually (as long as the desired range corresponds to a bitwise mask).

