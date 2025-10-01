pub const spec = "ret";

pub const encoding = opcodes.LSB.ret;

pub fn entry(c: *Cycle) void {
    c.zero_to_l();
    c.l_to_sr(.rp);
    c.branch(.rp, 0);
}

const opcodes = @import("opcodes.zig");
const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
