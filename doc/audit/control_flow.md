# Branches
## Conditional Branches


# Subroutine Calls
To call a subroutine, the `CALL` instruction copies the address of the next instruction (usually `IP+4`) into the `RP` register, then loads the first word from
the address of the subroutine into `DL` and updates `IP` and `MA`.  Since there may be another return address already in `RP`, the calling code needs to make
sure that it has been pushed onto the stack at some point before calling another subroutine.  When a subroutine finishes, it calls `RET`, which jumps to the
address previously stored in `RP`

# Kernel Calls
A kernel call works similarly to a subroutine call, with a few differences:
- `KCALL` is used to make the call instead of `CALL`.
- `KRET` must be used to return instead of `RET`.
- `STAT[UA]` cleared when calling.
- `STAT[UA]` is set when `KRET` is executed.
- The address loaded into the IP is specified indirectly.  An 8b index into the kernel vector table in memory frame 0 is used to load the actual procedure address.

# Interrupts
Interrupts are managed by an Interrupt Controller separate from the CPU.  All interaction between the CPU and the interrupt controller is done through the
system bus and one extra signal: `IF` (interrupt flag).

When a new instruction is being loaded, if `STAT[I]` is set and the interrupt signal is asserted, instead of setting `MA` by decoding `DL`, it will be set to
a specific location.  The microcode program at that location will save `IP` into a fixed location in memory frame 0, copy the low 8b of `STAT` into the upper
8b, clear `STAT[UAI]`, load the interrupt vector from the Interrupt Controller, and jump to that address.

When the kernel is done handling the interrupt, it executes `IRET` to restore `IP` and `STAT` and continues execution where the interrupt occurred, or if
another interrupt is pending, start handling that interrupt.

Interrupts may be handled while executing in either user mode or kernel mode (though kernel-mode may disable interrupts in critical sections if it desires).   
Generally interrupts aren't reentrant or preemptable, so `STAT[I]` shouldn't be enabled except using `IRET`, except if the context (including saved `IP`)
has been persisted, or if the task being interrupted is being killed.

# Faults
Interrupts signal that some other code should be run as soon as possible, but they don't affect the currently executing instruction.  That minimizes the amount
of state that needs to saved and restored, but it also means that interrupts aren't useful if there's a problem that prevents the currently executing
instruction from completing normally, known as a fault.  Some architectures refer to faults as exceptions, but we will avoid that term since it has been co-opted
by many programming languages to refer to software-triggered non-local control flow.

Sometimes faults are resolvable by the kernel, which means that we need to be able to ensure that all updates to registers and latches can be inhibited when
there's a fault, so that we can go back and try that machine cycle again under the same conditions as when it first occurred, other than any intentional changes
performed by the handler.  Because of this, conditions which trigger a fault must be detected at some point after `T2` (when the microcode signals for the cycle
first begin being decoded) and before `T1` of the following cycle.  The fault detected signal is transparent between `T0` and `T1`, and is latched at `T1` by the
rising edge of `phi0`.  Therefore the fault trigger signal must remain asserted until `T1`.

We also need the ability to save and restore a lot of the CPU state (including implementation details like the RT register and AL/DL latches) so that we can
actually execute handler code without disrupting the original processor state.  Some of this state is stored in shadow registers in the CPU, and other parts
are stored in a block of addresses in the physical zeropage.

Here are the actions that happen automatically when a fault happens:
 - No CPU registers are modified for the cycle that caused the fault.
 - Any bus operation for the cycle that caused the fault is cancelled.
 - `FMA` will latch the current `MA` value. 
 - `MA` will be set to a specific location depending on the type of fault.  This will handle the following additional actions:
   - Save `RT`, `IP`, `RP`, and `SP` into `FRT`, `FIP`, `FRP`, and `FSP`, respectively.
   - Save `AL`, `OB`, `OA`, `DL`, and `STAT` at specific locations in the physical zeropage.
   - Clear `STAT[UAI]`.
   - Load an address from the fault vector table in the physical zeropage and begin executing there.
 
The `FRET` and `BOFRET` instructions do the following:
 - Restore `AL`, `OB`, `OA`, `DL`, and `STAT` from the physical zeropage.
 - For `BOFRET`, set a bus override from the `RP` register.
 - Restore `RT`, `IP`, `RP`, and `SP` from `FRT`, `FIP`, `FRP`, and `FSP`, respectively.
 - Restore `MA` from `FMA`.

If the kernel deems that the process can't recover from the fault, it can clean up any resources owned by the process and switch to another context or continue
doing whatever else it wants.

The `STFX` and `LDFX` instructions can be used to load and store the fault shadow registers, `FMA`, `FRT`, `FIP`, `FRP`, and `FSP`.  If the data in the zeropage
is also moved elsewhere, it will be possible to tolerate another fault occurring during the first fault's handler.  For example, when handling a page alignment
fault, a page fault may occur on the higher page and need to be resolved before the page alignment fault handler can set the override data.

# Boostrapping & Reset
The processor contains numerous "ROMs" that are actually volatile SRAMs, because they're much faster.  When the processor powers up, these need to be initialized.
This bootstrapping could be done from actual parallel EPROMs or flash with counters, but I opted to just use microcontrollers to do the initial configuration of
these memories.  The system is already plenty complex without considering bootstrapping at all, and a single surface mount STM32 and programming header uses much less
board space than a PLCC socket for a flash chip, not to mention the counter and other and glue logic chips needed to drive it.  As a bonus, the microcontroller
can be used as an ad-hoc logic analyzer for the signals that enter or exit the SRAM, or act as an I2C-to-JTAG bridge for any ATF1502 CPLDs on the board, which should
make debugging much easier.  But the irony that each of these microcontrollers is an order of magnitude faster than the CPU itself is not lost on me.

While boostrapping is in progress, the machine cycle counter is paused and the processor will not attempt to do anything.  Some critical control signals (eg. bus
output enables) are held in specific states to avoid conflicts on buses or with the bootstrapping processes themselves.  Once bootstrapping is complete, a
Warm Reset fault is triggered.  The address of this fault handler is not found in the vector table like other faults, since we want it to be in ROM (or maybe DIP
switches).  It's this handler's responsibility to set up the rest of the vector table and load the kernel (if necessary).