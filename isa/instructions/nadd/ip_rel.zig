pub const spec =
    \\ nadd .i ip + (imm)
    \\ naddc .i ip + (imm)
    \\ naddv .i ip + (imm)
    \\ naddcv .i ip + (imm)
    ;

pub const encoding = .{
    opcodes.LSB.alu_16,
    opcodes.mnemonic_encoder(opcodes.ALU_16, .{ .suffix = "_ip_rel", .offset = 8 }),
    Encoder.init(16, Int(.imm, i8)),
};
pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

pub fn entry(c: *Cycle) void {
    c.ip_read_to_d(.i8_from_dr, .@"32b");
    c.d_to_l();
    c.l_to_sr(.temp_1);
    c.next(nadd);
}

pub fn nadd(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.sr_to_k(.temp_1);
    c.k_minus_j_to_l(switch (mnemonic) {
        .nadd, .naddv => .fresh,
        .naddc, .naddcv => .cont,
        else => unreachable,
    }, switch (mnemonic) {
        .nadd, .naddc => .flags,
        .naddv, .naddcv => .flags__fault_on_overflow,
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
const Encoder = isa.Instruction_Encoding.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
const std = @import("std");