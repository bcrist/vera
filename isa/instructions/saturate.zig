pub const spec = 
    \\ ssb.l
    \\ ssb.t
    \\ szb.l
    \\ szb.t
    ;

pub const encoding = .{
    opcodes.LSB.misc_16,
    opcodes.mnemonic_encoder(opcodes.Misc_16, .{ .offset = 8 }),
};

pub const krio: arch.bus.K.Read_Index_Offset.Raw = 0;
pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.zero_to_j();
    c.reg_to_k();
    c.saturate_k_to_l(switch (mnemonic) {
        .@"ssb.l", .@"ssb.t" => .ones,
        .@"szb.l", .@"szb.t" => .zeroes,
        else => unreachable,
    }, switch (mnemonic) {
        .@"ssb.l", .@"szb.l" => .leading,
        .@"ssb.t", .@"szb.t" => .trailing,
        else => unreachable,
    }, .flags);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

const opcodes = @import("opcodes.zig");
const Int = placeholders.Int;
const Reg = placeholders.Reg;
const placeholders = @import("../compile/placeholders.zig");
const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
const std = @import("std");
