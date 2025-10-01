pub const spec =
    \\ cmp .i ip + (imm)
    \\ cmpc .i ip + (imm)
    ;

pub const encoding = .{
    opcodes.LSB.alu_16,
    opcodes.mnemonic_encoder(opcodes.ALU_16, .{ .suffix = "_ip_rel", .offset = 8 }),
    Encoder.init(16, Int(.imm, i8)),
};

pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = -1;

pub fn entry(c: *Cycle) void {
    c.ip_read_to_d(.i8_from_dr, .@"32b");
    c.d_to_l();
    c.l_to_sr(.temp_1);
    c.next(cmp);
}

pub fn cmp(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.sr_to_k(.temp_1);
    c.j_minus_k_to_l(switch (mnemonic) {
        .cmp => .fresh,
        .cmpc => .cont,
        else => unreachable,
    }, .flags);
    c.wi_to_ti();
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