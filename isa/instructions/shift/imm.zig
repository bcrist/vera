pub const spec =
    \\ shl (imm)
    \\ shlv (imm)
    \\ shr (imm)
    \\ shrv (imm)
    \\ shrs (imm)
    \\ shrsv (imm)
    ;

pub const encoding = .{
    opcodes.LSB.shift_imm,
    opcodes.mnemonic_encoder(opcodes.Shift_Imm, .{ .offset = 8 }),
    Encoder.init(11, Int(.imm, u5)),
};
pub const krio = Int(.imm, u5);

pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.krio_to_k();
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
    c.l_to_reg();
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
