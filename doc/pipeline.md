# Pipelining
Each microcode operation occurs over three-stage pipeline:
- Setup
	- Control signals from microcode ROMs are clocked in
	- ALU input buses are driven
	- Virtual address is calculated
- Compute
	- ALU operation is performed
	- Address translation is performed
	- Even/Odd RAM frame offset addresses are calculated
	- Faults and stalls must be detected by the end of this stage
- Transact
	- L and D buses are driven
	- Registers are written
	- Address translation entries are updated
	- Next microcode address is computed and applied to microcode roms

Each stage occurs on a separate consecutive clock cycle, so a microcode operation always takes three clock cycles, but each operation is also occurring simultaneously on every clock cycle, so there are actually three operations happening in parallel. Normally, pipelining like this creates a lot of hazards, where one operation depends on the result of the previous one, and therefore can't be started until the previous one has completed. The CPU either needs to detect the dependency and insert a `nop` cycle automatically, or the responsibility is passed off to the compiler to ensure such cases are avoided or only used when the unintuitive behavior is actually desired. When the "compiler" is actually a human writing assembly, this can create a large amount of cognitive load.

This CPU takes an unconventional approach that avoids most hazard conditions. Instead of executing three consecutive instructions from one program in parallel, it executes three separate programs (or threads) in parallel, but only one instruction at a time for each one.
This means there must be three independent copies of each register in the CPU, but it effectively turns the CPU into a 3-core machine. I refer to each of these pseudo-cores as a _context_. In fact, there are eight distinct sets of most registers in the CPU. For details see [Registers](registers).
 
 The inspiration for this scheme came from reading [Dieter Mueller's notes on register files for his TREX CPU](http://www.6502.org/users/dieter/tarch/tarch_3.htm). Apart from the [Innovasic FIDO](https://www.analog.com/media/en/technical-documentation/data-sheets/fido1100.pdf) chip mentioned there, I'm not aware of any other extant CPUs that use this technique, though I'm sure there must be at least a few. If you're aware of or have built one, let me know!