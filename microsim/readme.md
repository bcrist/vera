# Microsim
Microsim is a standalone program which can simulate/emulate a Vera computer.  It should behave identically to the real hardware, however it is not gate-equivalent to the hardware implementation, nor does it simulate propagation delays (such a simulation would never run anywhere close to real-time).  It does simulate all internal control signals and registers, uses the actual microcode data, and accurately models interactions between pipeline threads.

This directory is also a zig package, used by adding a `.path = "relative/path/to/here"` dependency in `build.zig.zon`.
This package exports the `microsim` module, which allows the core simulation engine to be used for e.g. instruction behavior unit tests.
