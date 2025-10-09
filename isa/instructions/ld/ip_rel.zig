pub const forms = .{
    struct {
        pub const spec =
            \\ ld.8 .i %ip + (imm)
            \\ ld.16 .i %ip + (imm)
            \\ ld.24 .i %ip + (imm)
            \\ ld.32 .i %ip + (imm)
            ;

        pub const encoding = .{
            opcodes.mnemonic_encoder(opcodes.LSB, .{ .suffix = "_ip_rel" }),
            Encoder.init(8, Int(.imm, i16)),
        };
        pub const entry = ld_insn;
    },
    struct {
        pub const spec =
            \\ ld.8 .d %ip + (imm)
            \\ ld.16 .d %ip + (imm)
            \\ ld.24 .d %ip + (imm)
            \\ ld.32 .d %ip + (imm)
            ;

        pub const encoding = .{
            opcodes.mnemonic_encoder(opcodes.LSB, .{ .suffix = "_ip_rel_d" }),
            Encoder.init(8, Int(.imm, i16)),
        };
        pub const entry = ld_data;
    },
};

pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 1;

pub fn ld_data(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.read_to_d(.ip, .i16_from_dr, switch (mnemonic) {
        isa.Mnemonic.init("ld.8") => .@"8b",
        isa.Mnemonic.init("ld.16") => .@"16b",
        isa.Mnemonic.init("ld.24") => .@"24b",
        isa.Mnemonic.init("ld.32") => .@"32b",
        else => unreachable,
    }, .data);
    c.d_to_l();
    c.l_to_reg(true);
    c.next(next_insn);
}

pub fn ld_insn(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.ip_read_to_d(.i16_from_dr, switch (mnemonic) {
        isa.Mnemonic.init("ld.8") => .@"8b",
        isa.Mnemonic.init("ld.16") => .@"16b",
        isa.Mnemonic.init("ld.24") => .@"24b",
        isa.Mnemonic.init("ld.32") => .@"32b",
        else => unreachable,
    });
    c.d_to_l();
    c.l_to_reg(true);
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