This directory is a zig package, used by adding a `.path = "relative/path/to/here"` dependency in `build.zig.zon`.
This package exports the `arch` module, which contains a variety of types and helpers for representing the registers, buses, etc. that exist at the hardware level.
