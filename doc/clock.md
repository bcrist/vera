# Clock Phases
All activity in the CPU is coordinated using a single clock signal. A new clock cycle begins on the rising edge of the clock. This is the point where edge-triggered registers will load a new value. Asynchronous writes (i.e. to register files or RAM) begin when the clock goes low and end at the rising edge. Therefore the addressing signals for these chips must settle in less than half a clock cycle to avoid corruption of other locations.

In order to avoid bus collisions, all buses that are sourced from one of multiple separate line drivers (e.g. J, K, L, D) should only drive the bus during the first half of the cycle, when CLK is high. The bus state is preserved in the second half of the cycle using a bus-hold circuit. Since output enables are normally active-low, an inverted clock signal is also distributed. The slight delay introduced by the inverter has an additional benefit of reducing the chance of clock skew between boards causing a new signal from one board to overwrite the previous cycle's data before it can be clocked into any registers that may need it.

# Cycle Stretching
When a virtual address is translated, it also generates a 2b number that indicates whether the next clock cycle should be stretched to take a multiple of the normal clock cycle length. 
This allows the CPU to access slower bus devices like ROMs in the same way as it does for the fast RAM.

During a stretched cycle, the clock stays high most of the time, and goes low for exactly one full normal clock cycle at the end (twice as long as a normal cycle drives the clock low).

When address translation is disabled, the cycle is either unstretched or stretched two cycles, depending on the MSB of the page/frame number. Effectively this means RAM is not slowed down, but all other devices are slowed down by 3x.

# Sleep Mode
When in kernel mode, a special flag may be set which disables the clock as long as there is no hardware interrupt pending or running.  This reduces dynamic power usage, and allows some chips to be placed into a low-power state.  Since this affects all three contexts, the kernel should make sure there's nothing running on any of them before going to sleep.