pub const spec =
    \\ stg.8 .d %(reg)
    \\ stg.16 .d %(reg)
    \\ stg.24 .d %(reg)
    \\ stg.32 .d %(reg)
    ;

pub const encoding = .{
    opcodes.LSB.ldg_stg_reg,
    opcodes.mnemonic_encoder(opcodes.LDG_STG_Reg, .{ .offset = 8 }),
    Encoder.init(11, Reg(.reg)),
};

pub const krio = Reg(.reg);

pub fn entry(c: *Cycle) void {
    c.load_next_insn();
    c.reg_to_k_to_l();
    c.l_to_sr(.temp_1);
    c.next(store);
}

pub fn store(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j_to_l();
    c.write_from_l(.temp_1, 0, switch (mnemonic) {
        isa.Mnemonic.init("stg.8") => .@"8b",
        isa.Mnemonic.init("stg.16") => .@"16b",
        isa.Mnemonic.init("stg.24") => .@"24b",
        isa.Mnemonic.init("stg.32") => .@"32b",
        else => unreachable,
    }, .data);
    c.check_guard();
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