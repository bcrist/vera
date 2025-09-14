pub const spec = "iret";

pub const encoding = .{
    opcodes.LSB.misc_16,
    Encoder.init(8, opcodes.Misc_16.iret),
};

pub fn entry(c: *Cycle, flags: Flags) void {
    if (!flags.kernel()) return c.illegal_instruction();
    c.force_normal_execution(reload_asn);
}

pub fn reload_asn(c: *Cycle) void {
    c.reload_asn();
    c.next(branch);
}

pub fn branch(c: *Cycle) void {
    c.sr_to_l(.int_flags);
    c.l_to_flags();
    c.branch(.ip, 0);
}

const opcodes = @import("opcodes.zig");
const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
