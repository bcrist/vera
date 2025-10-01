pub const slot = .invalid_instruction;

pub fn entry(c: *Cycle) void {
    c.invalid_instruction();
}

const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Encoder;
const Int = placeholders.Int;
const Range = placeholders.Range;
const Reg = placeholders.Reg;
const placeholders = @import("../compile/placeholders.zig");
const Flags = arch.microcode.Flags;
const isa = @import("isa");
const arch = @import("arch");
