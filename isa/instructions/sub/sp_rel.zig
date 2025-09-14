pub const spec =
    \\ sub .s sp + (imm)
    \\ subc .s sp + (imm)
    \\ subv .s sp + (imm)
    \\ subcv .s sp + (imm)
    ;

pub const encoding = .{
    opcodes.LSB.alu_16,
    opcodes.mnemonic_encoder(opcodes.ALU_16, .{ .suffix = "_sp_rel", .offset = 8 }),
    Encoder.init(16, Int_Mult(.imm, i8, 4)),
};
pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

pub fn entry(c: *Cycle) void {
    c.read_to_d(.sp, .i8_x4_from_dr, .@"32b", .stack);
    c.d_to_l();
    c.l_to_sr(.temp_1);
    c.next(sub);
}

pub const sub = @import("ip_rel.zig").sub;

const opcodes = @import("../opcodes.zig");
const Int = placeholders.Int;
const Int_Mult = placeholders.Int_Mult;
const Reg = placeholders.Reg;
const placeholders = @import("../../compile/placeholders.zig");
const Cycle = @import("../../compile/Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
const std = @import("std");