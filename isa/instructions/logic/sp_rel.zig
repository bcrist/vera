pub const spec =
    \\ and .s %sp + (imm)
    \\ or .s %sp + (imm)
    \\ xor .s %sp + (imm)
    ;

pub const encoding = .{
    opcodes.mnemonic_encoder(opcodes.LSB, .{ .suffix = "_sp_rel" }),
    Encoder.init(8, Int(.imm, i16)),
};
pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

pub fn entry(c: *Cycle) void {
    c.read_to_d(.sp, .i16_from_dr, .@"32b", .stack);
    c.d_to_l();
    c.l_to_sr(.temp_1);
    c.next(logic);
}

pub const logic = @import("ip_rel.zig").logic;

const opcodes = @import("../opcodes.zig");
const Int = placeholders.Int;
const Int_Mult = placeholders.Int_Mult;
const Reg = placeholders.Reg;
const placeholders = @import("../../compile/placeholders.zig");
const Cycle = @import("../../compile/Cycle.zig");
const Encoder = isa.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
const std = @import("std");