pub const spec =
    \\ add .d bp + (imm)
    \\ addc .d bp + (imm)
    \\ addv .d bp + (imm)
    \\ addcv .d bp + (imm)
    ;

pub const encoding = .{
    opcodes.LSB.alu_16,
    opcodes.mnemonic_encoder(opcodes.ALU_16, .{ .suffix = "_bp_rel", .offset = 8 }),
    Encoder.init(16, Int(.imm, i8)),
};

pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

pub fn entry(c: *Cycle) void {
    c.read_to_d(.bp, .i8_from_dr, .@"32b", .data);
    c.d_to_l();
    c.l_to_sr(.temp_1);
    c.next(add);
}

pub const add = @import("ip_rel.zig").add;

const opcodes = @import("../opcodes.zig");
const Int = placeholders.Int;
const Reg = placeholders.Reg;
const placeholders = @import("../../compile/placeholders.zig");
const Cycle = @import("../../compile/Cycle.zig");
const Encoder = isa.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
const std = @import("std");