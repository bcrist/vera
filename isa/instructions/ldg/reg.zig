pub const spec =
    \\ ldg.8 .d %(reg)
    \\ ldg.16 .d %(reg)
    \\ ldg.24 .d %(reg)
    \\ ldg.32 .d %(reg)
    ;

pub const encoding = .{
    opcodes.LSB.ldg_stg_reg,
    opcodes.mnemonic_encoder(opcodes.LDG_STG_Reg, .{ .offset = 8 }),
    Encoder.init(11, Reg(.reg)),
};

pub const krio = Reg(.reg);
pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 1;

pub fn entry(c: *Cycle) void {
    c.load_next_insn();
    c.reg_to_k_to_l();
    c.l_to_sr(.temp_1);
    c.next(load_ptr);
}

pub fn load_ptr(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.read_to_d(.temp_1, 0, switch (mnemonic) {
        isa.Mnemonic.init("ldg.8") => .@"8b",
        isa.Mnemonic.init("ldg.16") => .@"16b",
        isa.Mnemonic.init("ldg.24") => .@"24b",
        isa.Mnemonic.init("ldg.32") => .@"32b",
        else => unreachable,
    }, .data);
    c.d_to_l();
    c.l_to_reg(true);
    c.set_guard();
    c.exec_next_insn();
}

const opcodes = @import("../opcodes.zig");
const Reg = placeholders.Reg;
const placeholders = @import("../../compile/placeholders.zig");
const Cycle = @import("../../compile/Cycle.zig");
const Encoder = isa.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
const std = @import("std");