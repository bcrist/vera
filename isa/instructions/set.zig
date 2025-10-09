pub const forms = .{
    struct {
        pub const spec = "set %bp";
        pub const encoding = .{
            opcodes.LSB.set_bp,
        };
        pub const entry = set_bp;
    },
    struct {
        pub const spec = "set %sp";
        pub const encoding = .{
            opcodes.LSB.set_sp,
        };
        pub const entry = set_sp;
    },
    struct {
        pub const spec = "set %rp";
        pub const encoding = .{
            opcodes.LSB.set_rp,
        };
        pub const entry = set_rp;
    },
    struct {
        pub const spec = "set %asn";
        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.set_asn),
        };
        pub const entry = set_asn;
    },
    struct {
        pub const spec = "set %zncv";
        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.set_zncv),
        };
        pub const entry = set_zncv;
    },
    struct {
        pub const spec = "set %uxp";
        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.set_uxp),
        };
        pub const entry = set_uxp;
    },
    struct {
        pub const spec = "set %kxp";
        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.set_kxp),
        };
        pub const entry = set_kxp;
    },
};

pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = -1;

pub fn set_bp(c: *Cycle) void {
    c.reg_to_j_to_l();
    c.l_to_sr(.bp);
    c.wi_to_ti();
    c.load_and_exec_next_insn();
}

pub fn set_sp(c: *Cycle) void {
    c.reg_to_j_to_l();
    c.l_to_sr(.sp);
    c.wi_to_ti();
    c.load_and_exec_next_insn();
}

pub fn set_rp(c: *Cycle) void {
    c.reg_to_j_to_l();
    c.l_to_sr(.rp);
    c.wi_to_ti();
    c.load_and_exec_next_insn();
}

pub fn set_asn(c: *Cycle, flags: Flags) void {
    if (!flags.kernel()) return c.illegal_instruction();
    c.reg_to_j_to_l();
    c.l_to_sr(.asn);
    c.wi_to_ti();
    // We can't load next insn this cycle for 2 reasons:
    //    1. asn and ip/next_ip are both in SR2
    //    2. changing ASN may change the physical page where the next instruction
    //       is located (but normally this would only be used with AT disabled)
    c.next(next_insn);
}

pub fn next_insn(c: *Cycle) void {
    c.load_and_exec_next_insn();
}

pub fn set_zncv(c: *Cycle) void {
    c.reg_to_j_to_l();
    c.l_to_zncv();
    c.wi_to_ti();
    c.load_and_exec_next_insn();
}

pub fn set_uxp(c: *Cycle) void {
    c.reg_to_j_to_l();
    c.l_to_sr(.uxp);
    c.wi_to_ti();
    // We can't load next insn this cycle because uxp and ip/next_ip are both in SR2
    c.next(next_insn);
}

pub fn set_kxp(c: *Cycle, flags: Flags) void {
    if (!flags.kernel()) return c.illegal_instruction();
    c.reg_to_j_to_l();
    c.l_to_sr(.kxp);
    c.wi_to_ti();
    // We can't load next insn this cycle because kxp and ip/next_ip are both in SR2
    c.next(next_insn);
}

const opcodes = @import("opcodes.zig");
const Int = placeholders.Int;
const Reg = placeholders.Reg;
const placeholders = @import("../compile/placeholders.zig");
const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
const std = @import("std");