pub const spec = 
    \\ csb
    \\ csb.l
    \\ csb.t
    \\ czb
    \\ czb.l
    \\ czb.t
    ;

pub const encoding = .{
    opcodes.LSB.misc_16,
    opcodes.mnemonic_encoder(opcodes.Misc_16, .{ .offset = 8 }),
};

pub const krio: arch.bus.K.Read_Index_Offset.Raw = 0;
pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.reg_to_k();
    c.count_j_to_l(switch (mnemonic) {
        .csb, .@"csb.l", .@"csb.t" => .ones,
        .czb, .@"czb.l", .@"czb.t" => .zeroes,
        else => unreachable,
    }, switch (mnemonic) {
        .csb, .czb => .all,
        .@"csb.l", .@"czb.l" => .leading,
        .@"csb.t", .@"czb.t" => .trailing,
        else => unreachable,
    }, .fresh, .flags);
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
