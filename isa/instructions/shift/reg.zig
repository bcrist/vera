pub const spec =
    \\ shl r(reg)
    \\ shlv r(reg)
    \\ shr r(reg)
    \\ shrv r(reg)
    \\ shrs r(reg)
    \\ shrsv r(reg)
    ;

pub const encoding = .{
    opcodes.LSB.shift_reg,
    opcodes.mnemonic_encoder(opcodes.Shift_Reg, .{ .offset = 8 }),
    Encoder.init(11, Reg(.reg)),
};
pub const krio = Reg(.reg);
pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.reg_to_k();
    c.j_shift_k_to_l(switch (mnemonic) {
        .shl, .shlv => .shl,
        .shr, .shrv => .shr,
        .shrs, .shrsv => .shrs,
        else => unreachable,
    }, .fresh, switch (mnemonic) {
        .shl, .shr, .shrs => .flags,
        .shlv, .shrv, .shrsv => .flags__fault_on_overflow,
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
