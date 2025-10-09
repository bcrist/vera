pub const spec =
    \\ shl (imm)
    \\ shl.vf (imm)
    \\ shr (imm)
    \\ shr.vf (imm)
    \\ shrs (imm)
    \\ shrs.vf (imm)
    ;

pub const encoding = .{
    opcodes.LSB.shift_imm,
    opcodes.mnemonic_encoder(opcodes.Shift_Imm, .{ .offset = 8 }),
    Encoder.init(11, Int(.imm, u5)),
};
pub const krio = Int(.imm, u5);
pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.krio_to_k(.zx);
    c.j_shift_k_to_l(switch (mnemonic) {
        .shl, .@"shl.vf" => .shl,
        .shr, .@"shr.vf" => .shr,
        .shrs, .@"shrs.vf" => .shrs,
        else => unreachable,
    }, .fresh, switch (mnemonic) {
        .shl, .shr, .shrs => .flags,
        .@"shl.vf", .@"shr.vf", .@"shrs.vf" => .flags__fault_on_overflow,
        else => unreachable,
    });
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

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
