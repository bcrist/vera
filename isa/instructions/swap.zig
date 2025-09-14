pub const spec =
    \\ swap
    \\ swap2
    \\ byterev
    \\ bitrev
    \\ bitrev1
    \\ bitrev2
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
        .swap => .swap_halves,
        .swap2 => .swap_bytes,
        .byterev => .reverse_bytes,
        .bitrev => .reverse_bits,
        .bitrev1 => .reverse_bits_in_bytes,
        .bitrev2 => .reverse_bits_in_halves,
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
const Encoder = isa.Instruction_Encoding.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
const std = @import("std");
