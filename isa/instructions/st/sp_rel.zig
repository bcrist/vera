pub const spec =
    \\ st.8 .s %sp + (imm)
    \\ st.16 .s %sp + (imm)
    \\ st.32 .s %sp + (imm)
    ;

pub const encoding = .{
    opcodes.mnemonic_encoder(opcodes.LSB, .{ .suffix = "_sp_rel" }),
    Encoder.init(8, Int(.imm, i16)),
};

pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = -1;

pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j_to_l();
    c.write_from_l(.sp, .i16_from_dr, switch (mnemonic) {
        isa.Mnemonic.init("st.8") => .@"8b",
        isa.Mnemonic.init("st.16") => .@"16b",
        isa.Mnemonic.init("st.32") => .@"32b",
        else => unreachable,
    }, .stack);
    c.wi_to_ti();
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