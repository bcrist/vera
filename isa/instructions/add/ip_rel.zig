pub const spec =
    \\ add .i ip + (imm)
    \\ addc .i ip + (imm)
    \\ addv .i ip + (imm)
    \\ addcv .i ip + (imm)
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
    c.next(add);
}

pub fn add(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.sr_to_k(.temp_1);
    c.j_plus_k_to_l(switch (mnemonic) {
        .add, .addv => .fresh,
        .addc, .addcv => .cont,
        else => unreachable,
    }, switch (mnemonic) {
        .add, .addc => .flags,
        .addv, .addcv => .flags__fault_on_overflow,
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