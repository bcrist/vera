pub const forms = .{
    struct {
        pub const spec = "val %ip + %(reg)";
        pub const encoding = .{
            opcodes.LSB.ip_reg,
            Encoder.init(8, opcodes.IP_Reg.val),
            Encoder.init(11, Reg(.reg)),
        };
        pub const entry = val_ip_reg;
    },
    struct {
        pub const spec = "val %bp + %(reg)";
        pub const encoding = .{
            opcodes.LSB.bp_reg,
            Encoder.init(8, opcodes.BP_Reg.val),
            Encoder.init(11, Reg(.reg)),
        };
        pub const entry = val_bp_reg;
    },
    struct {
        pub const spec = "val %sp + %(reg)";
        pub const encoding = .{
            opcodes.LSB.sp_reg,
            Encoder.init(8, opcodes.SP_Reg.val),
            Encoder.init(11, Reg(.reg)),
        };
        pub const entry = val_sp_reg;
    },
    struct {
        pub const spec = "val %uxp + %(reg)";
        pub const encoding = .{
            opcodes.LSB.uxp_reg,
            Encoder.init(8, opcodes.UXP_Reg.val),
            Encoder.init(11, Reg(.reg)),
        };
        pub const entry = val_uxp_reg;
    },
    struct {
        pub const spec = "val %kxp + %(reg)";
        pub const encoding = .{
            opcodes.LSB.kxp_reg,
            Encoder.init(8, opcodes.KXP_Reg.val),
            Encoder.init(11, Reg(.reg)),
        };
        pub const entry = val_kxp_reg;
    },
};

pub const krio = Reg(.reg);
pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 1;

pub fn val_ip_reg(c: *Cycle) void {
    c.sr_to_j(.ip);
    c.reg_to_k();
    c.j_plus_k_to_l(.fresh, .no_flags);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn val_bp_reg(c: *Cycle) void {
    c.sr_to_j(.bp);
    c.reg_to_k();
    c.j_plus_k_to_l(.fresh, .no_flags);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn val_sp_reg(c: *Cycle) void {
    c.sr_to_j(.sp);
    c.reg_to_k();
    c.j_plus_k_to_l(.fresh, .no_flags);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn val_uxp_reg(c: *Cycle) void {
    c.sr_to_j(.uxp);
    c.reg_to_k();
    c.j_plus_k_to_l(.fresh, .no_flags);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn val_kxp_reg(c: *Cycle) void {
    c.sr_to_j(.kxp);
    c.reg_to_k();
    c.j_plus_k_to_l(.fresh, .no_flags);
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