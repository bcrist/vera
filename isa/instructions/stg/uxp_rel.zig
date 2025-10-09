pub const spec =
    \\ stg.8 .d %uxp + (imm)
    \\ stg.16 .d %uxp + (imm)
    \\ stg.32 .d %uxp + (imm)
    ;

pub const encoding = .{
    opcodes.mnemonic_encoder(opcodes.LSB, .{ .suffix = "_uxp_rel" }),
    Encoder.init(8, Int(.imm, i16)),
};

pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j_to_l();
    c.write_from_l(.uxp, .i16_from_dr, switch (mnemonic) {
        isa.Mnemonic.init("stg.8") => .@"8b",
        isa.Mnemonic.init("stg.16") => .@"16b",
        isa.Mnemonic.init("stg.32") => .@"32b",
        else => unreachable,
    }, .data);
    c.check_guard();
    c.next(next_insn);
}

pub fn next_insn(c: *Cycle) void {
    c.load_and_exec_next_insn();
}

const opcodes = @import("../opcodes.zig");
const Int = placeholders.Int;
const placeholders = @import("../../compile/placeholders.zig");
const Cycle = @import("../../compile/Cycle.zig");
const Encoder = isa.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
const std = @import("std");