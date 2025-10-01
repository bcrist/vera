pub const spec = "park";

pub const encoding = .{
    opcodes.LSB.misc_16,
    Encoder.init(8, opcodes.Misc_16.park),
};

pub fn entry(c: *Cycle, flags: Flags) void {
    if (!flags.kernel()) return c.illegal_instruction();

    c.power_mode(.sleep);
    c.exec_ir_insn();
}

const opcodes = @import("opcodes.zig");
const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
