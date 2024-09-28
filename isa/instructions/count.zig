pub const spec = 
    \\ csb
    \\ csbl
    \\ csbt
    \\ czb
    \\ czbl
    \\ czbt
    ;

pub const encoding = .{
    opcodes.LSB.misc_16,
    opcodes.mnemonic_encoder(opcodes.Misc_16, .{ .offset = 8 }),
};

pub const krio: arch.K.Read_Index_Offset.Raw = 0;

pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.reg_to_k();
    c.count_j_to_l(switch (mnemonic) {
        .csb, .csbl, .csbt => .ones,
        .czb, .czbl, .czbt => .zeroes,
        else => unreachable,
    }, switch (mnemonic) {
        .csb, .czb => .all,
        .csbl, .czbl => .leading,
        .csbt, .czbt => .trailing,
        else => unreachable,
    }, .fresh, .flags);
    c.l_to_reg();
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
