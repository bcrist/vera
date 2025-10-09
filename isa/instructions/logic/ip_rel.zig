pub const spec =
    \\ and .i %ip + (imm)
    \\ or .i %ip + (imm)
    \\ xor .i %ip + (imm)
    ;

pub const encoding = .{
    opcodes.mnemonic_encoder(opcodes.LSB, .{ .suffix = "_ip_rel" }),
    Encoder.init(8, Int(.imm, i16)),
};
pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

pub fn entry(c: *Cycle) void {
    c.ip_read_to_d(.i16_from_dr, .@"32b");
    c.d_to_l();
    c.l_to_sr(.temp_1);
    c.next(logic);
}

pub fn logic(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.sr_to_k(.temp_1);
    c.j_logic_k_to_l(switch (mnemonic) {
        .@"and" => ._and,
        .@"or" => ._or,
        .xor => .xor,
        else => unreachable,
    }, .fresh, .flags);
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