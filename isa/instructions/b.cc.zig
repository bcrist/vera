pub const forms = .{
    struct {
        pub const spec =
            \\ b.z .i %ip + (imm)
            \\ b.nz .i %ip + (imm)
            \\ b.n .i %ip + (imm)
            \\ b.nn .i %ip + (imm)
            \\ b.p .i %ip + (imm)
            \\ b.np .i %ip + (imm)
            \\ b.eq .i %ip + (imm)
            \\ b.ne .i %ip + (imm)
            ;

        pub const encoding = .{
            opcodes.mnemonic_encoder(opcodes.LSB, .{ .suffix = "_ip_rel" }),
            Encoder.init(8, Int(.imm, i16)),
        };
        pub const entry = b_cz_ip_rel;
        pub const Flags = arch.microcode.Flags;
    },
    struct {
        pub const spec =
            \\ b.c .i %ip + (imm)
            \\ b.nc .i %ip + (imm)
            \\ b.lt.u .i %ip + (imm)
            \\ b.gt.u .i %ip + (imm)
            \\ b.le.u .i %ip + (imm)
            \\ b.ge.u .i %ip + (imm)
            ;

        pub const encoding = .{
            opcodes.mnemonic_encoder(opcodes.LSB, .{ .suffix = "_ip_rel" }),
            Encoder.init(8, Int(.imm, i16)),
        };
        pub const entry = b_cc_ip_rel;
        pub const Flags = arch.microcode.Flags_With_Carry;
    },
    struct {
        pub const spec =
            \\ b.v .i %ip + (imm)
            \\ b.nv .i %ip + (imm)
            \\ b.lt.s .i %ip + (imm)
            \\ b.gt.s .i %ip + (imm)
            \\ b.le.s .i %ip + (imm)
            \\ b.ge.s .i %ip + (imm)
            ;

        pub const encoding = .{
            opcodes.mnemonic_encoder(opcodes.LSB, .{ .suffix = "_ip_rel" }),
            Encoder.init(8, Int(.imm, i16)),
        };
        pub const entry = b_cv_ip_rel;
        pub const Flags = arch.microcode.Flags_With_Overflow;
    },
};


pub fn b_cz_ip_rel(c: *Cycle, flags: arch.microcode.Flags, mnemonic: Mnemonic) void {
    if (switch (mnemonic) {
        Mnemonic.init("b.z") => flags.zero(),
        Mnemonic.init("b.eq") => flags.zero(),
        Mnemonic.init("b.nz") => !flags.zero(),
        Mnemonic.init("b.ne") => !flags.zero(),
        Mnemonic.init("b.n") => flags.negative(),
        Mnemonic.init("b.nn") => !flags.negative(),
        Mnemonic.init("b.p") => flags.positive(),
        Mnemonic.init("b.np") => !flags.positive(),
        else => unreachable,
    }) c.branch(.ip, .i16_from_dr) else c.load_and_exec_next_insn();
}

pub fn b_cc_ip_rel(c: *Cycle, flags: arch.microcode.Flags_With_Carry, mnemonic: Mnemonic) void {
    if (switch (mnemonic) {
        Mnemonic.init("b.c") => flags.carry(),
        Mnemonic.init("b.nc") => !flags.carry(),
        Mnemonic.init("b.lt.u") => flags.unsigned_less_than(),
        Mnemonic.init("b.gt.u") => flags.unsigned_greater_than(),
        Mnemonic.init("b.le.u") => !flags.unsigned_greater_than(),
        Mnemonic.init("b.ge.u") => !flags.unsigned_less_than(),
        else => unreachable,
    }) c.branch(.ip, .i16_from_dr) else c.load_and_exec_next_insn();
}

pub fn b_cv_ip_rel(c: *Cycle, flags: arch.microcode.Flags_With_Overflow, mnemonic: Mnemonic) void {
    if (switch (mnemonic) {
        Mnemonic.init("b.v") => flags.overflow(),
        Mnemonic.init("b.nv") => !flags.overflow(),
        Mnemonic.init("b.lt.s") => flags.signed_less_than(),
        Mnemonic.init("b.gt.s") => flags.signed_greater_than(),
        Mnemonic.init("b.le.s") => !flags.signed_greater_than(),
        Mnemonic.init("b.ge.s") => !flags.signed_less_than(),
        else => unreachable,
    }) c.branch(.ip, .i16_from_dr) else c.load_and_exec_next_insn();
}

const opcodes = @import("opcodes.zig");
const Range = placeholders.Range;
const Any = placeholders.Any;
const Int = placeholders.Int;
const Reg = placeholders.Reg;
const placeholders = @import("../compile/placeholders.zig");
const Cycle = @import("../compile/Cycle.zig");
const Mnemonic = isa.Mnemonic;
const Encoder = isa.Encoder;
const isa = @import("isa");
const arch = @import("arch");
const std = @import("std");