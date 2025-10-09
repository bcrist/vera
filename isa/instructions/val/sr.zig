pub const forms = .{
    struct {
        pub const spec = "val %bp";
        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.val_bp),
        };
        pub const entry = val_bp;
    },
    struct {
        pub const spec = "val %sp";
        pub const encoding = .{
            opcodes.LSB.val_sp,
        };
        pub const entry = val_sp;
    },
    struct {
        pub const spec = "val %rp";
        pub const encoding = .{
            opcodes.LSB.val_rp,
        };
        pub const entry = val_rp;
    },
    struct {
        pub const spec = "val %asn";
        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.val_asn),
        };
        pub const entry = val_asn;
    },
    struct {
        pub const spec = "val %flags";
        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.val_flags),
        };
        pub const entry = val_flags;
    },
    struct {
        pub const spec = "val %status";
        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.val_status),
        };
        pub const entry = val_status;
    },
    struct {
        pub const spec = "val %uxp";
        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.val_uxp),
        };
        pub const entry = val_uxp;
    },
    struct {
        pub const spec = "val %kxp";
        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.val_kxp),
        };
        pub const entry = val_kxp;
    },
};

pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 1;

pub fn val_bp(c: *Cycle) void {
    c.sr_to_l(.bp);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn val_sp(c: *Cycle) void {
    c.sr_to_l(.sp);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn val_rp(c: *Cycle) void {
    c.sr_to_l(.rp);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn val_asn(c: *Cycle) void {
    c.sr_to_l(.asn);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn val_flags(c: *Cycle) void {
    c.flags_to_l();
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn val_status(c: *Cycle) void {
    c.status_to_l();
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn val_uxp(c: *Cycle) void {
    c.sr_to_l(.uxp);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn val_kxp(c: *Cycle) void {
    c.sr_to_l(.kxp);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

const opcodes = @import("../opcodes.zig");
const Int = placeholders.Int;
const Reg = placeholders.Reg;
const placeholders = @import("../../compile/placeholders.zig");
const Cycle = @import("../../compile/Cycle.zig");
const Encoder = isa.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
const std = @import("std");