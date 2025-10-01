pub const spec =
    \\ swap.16
    \\ swap.8
    \\ rev.8.32
    \\ rev.1.32
    \\ rev.1.16
    \\ rev.1.8
    ;
    
pub const encoding = .{
    opcodes.LSB.misc_16,
    opcodes.mnemonic_encoder(opcodes.Misc_16, .{ .offset = 8 }),
};
pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.zero_to_k();
    c.swap_j_to_l(switch (mnemonic) {
        .@"swap.16" => .swap_halves,
        .@"swap.8" => .swap_bytes,
        .@"rev.8.32" => .reverse_bytes,
        .@"rev.1.32" => .reverse_bits,
        .@"rev.1.16" => .reverse_bits_in_halves,
        .@"rev.1.8" => .reverse_bits_in_bytes,
        else => unreachable,
    }, .no_flags);
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
