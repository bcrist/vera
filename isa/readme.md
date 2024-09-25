This directory is a zig package, used by adding a `.path = "relative/path/to/here"` dependency in `build.zig.zon`.
This package exports several modules:
* `isa`: contains data structures for representing assembly language instructions and their binary encodings.
* `iedb.sx`: an s-expression file (not a zig source file!) that enumerates all valid instruction encodings
* `iedb`: contains data structures and functions for parsing/querying the instruction encoding database in `iedb.sx`
* `insn_decode.srec`: contains the data for the instruction decoder lookup table in Motorola S-Record format.
* `insn_decode.crom`: contains the data for the instruction decoder lookup table in compressed binary format.
* `setup_decode.srec`: contains the microcode data for the setup stage in Motorola S-Record format.
* `setup_decode.crom`: contains the microcode data for the setup stage in compressed binary format.
* `compute_decode.srec`: contains the microcode data for the compute stage in Motorola S-Record format.
* `compute_decode.crom`: contains the microcode data for the compute stage in compressed binary format.
* `transact_decode.srec`: contains the microcode data for the transact and decode stages in Motorola S-Record format.
* `transact_decode.crom`: contains the microcode data for the transact and decode stages in compressed binary format.
