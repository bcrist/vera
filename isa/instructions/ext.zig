pub const spec = 
    \\ sx (bits)
    \\ zx (bits)
    \\ sxv (bits)
    \\ zxv (bits)
    ;

pub const encoding = .{
    opcodes.LSB.misc_reg,
    opcodes.mnemonic_encoder(opcodes.Misc_Reg, .{ .offset = 8 }),
    Encoder.init(11, Offset(-1, Bit(.bits))),
};

pub const krio = Offset(-1, Bit(.bits));
pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.zero_to_j();
    c.krio_bit_to_k();
    c.j_ext_k_to_l(switch (mnemonic) {
        .sx, .sxv => .sx,
        .zx, .zxv => .zx,
        else => unreachable,
    }, switch (mnemonic) {
        .sx, .zx => .flags,
        .sxv, .zxv => .flags__fault_on_overflow,
        else => unreachable,
    });
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

const opcodes = @import("opcodes.zig");
const Offset = placeholders.Offset;
const Bit = placeholders.Bit;
const placeholders = @import("../compile/placeholders.zig");
const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
const std = @import("std");
