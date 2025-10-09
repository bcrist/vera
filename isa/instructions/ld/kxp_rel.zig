pub const spec =
    \\ ld.8 .d %kxp + (imm)
    \\ ld.16 .d %kxp + (imm)
    \\ ld.24 .d %kxp + (imm)
    \\ ld.32 .d %kxp + (imm)
    ;

pub const encoding = .{
    opcodes.mnemonic_encoder(opcodes.LSB, .{ .suffix = "_kxp_rel" }),
    Encoder.init(8, Int(.imm, i16)),
};

pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 1;

pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.read_to_d(.kxp, .i16_from_dr, switch (mnemonic) {
        isa.Mnemonic.init("ld.8") => .@"8b",
        isa.Mnemonic.init("ld.16") => .@"16b",
        isa.Mnemonic.init("ld.24") => .@"24b",
        isa.Mnemonic.init("ld.32") => .@"32b",
        else => unreachable,
    }, .data);
    c.d_to_l();
    c.l_to_reg(true);
    c.next(next_insn);
}

pub fn next_insn(c: *Cycle) void {
    c.load_and_exec_next_insn();
}

const opcodes = @import("../opcodes.zig");
const Int = placeholders.Int;
const placeholders = @import("../../compile/placeholders.zig");
const Cycle = @import("../../compile/Cycle.zig");
const Encoder = isa.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
const std = @import("std");