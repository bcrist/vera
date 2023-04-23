# Pipelining
Each microcode operation occurs over three-stage pipeline:
- Setup
	- Control signals from microcode ROMs are clocked in at the start of the stage
	- ALU input buses are driven during the second half of the stage and stable by the end
	- Virtual address is calculated and stable by the end of the stage
- Compute
	- ALU operation is performed and result is stable by the end of the stage
	- Address translation is performed and frame number is stable by the end of the stage
	- Even/Odd RAM frame offset addresses are calculated and stable by the end of the stage
	- Faults and stalls must be detected by the end of the stage
- Transact
	- L and D buses are driven for the whole clock cycle except a short "blanking" period at the beginning to prevent contention
	- DL is only transparent during the first half of the stage
	- The next microcode address is clocked in at the half cycle point, and the next microcode signals are decoded in the second half
	- Synchronous registers are written at the end of the stage
	- Memory and async register writes occur with WE active during the second half of the stage
	- Address translation entries are updated during the second half of the stage

Each stage occurs on a separate consecutive clock cycle, so a microcode operation always takes three clock cycles, but each operation is also occurring simultaneously on every clock cycle, so there are actually three operations happening in parallel. Normally, pipelining like this creates a lot of hazards, where one operation depends on the result of the previous one, and therefore can't be started until the previous one has completed. The CPU either needs to detect the dependency and insert a `nop` cycle automatically, or the responsibility is passed off to the compiler to ensure such cases are avoided or only used when the unintuitive behavior is actually desired. When the "compiler" is actually a human writing assembly, this can create a large amount of cognitive load.  In some cases the CPU could alternatively use "forwarding" to feed a result still in the pipeline back to an earlier stage (in this CPU, it would look like the ALU results being fed into the J/K buses directly) but these schemes are complicated, can be difficult to debug, and can eat up a lot of extra board space for routing the forwarded signals.

This CPU takes an unusual approach that avoids most hazard conditions. Instead of executing three consecutive instructions from one program in parallel, it executes three separate programs (or threads) in parallel, but only one instruction at a time for each one.
This means there must be (at least) three independent copies of each register in the CPU, but it effectively turns the CPU into a 3-core machine. I refer to each of these pseudo-cores as a _pipe_.
 
 The inspiration for this scheme came from reading [Dieter Mueller's notes on register files for his TREX CPU](http://www.6502.org/users/dieter/tarch/tarch_3.htm). Apart from the [Innovasic FIDO](https://www.analog.com/media/en/technical-documentation/data-sheets/fido1100.pdf) chip mentioned there, I'm not aware of any other extant CPUs that use this technique, though I'm sure there must be at least a few others. If you're aware of or have built one, let me know!