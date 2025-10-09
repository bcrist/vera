
pub const forms = .{
    struct {
        pub const spec = "val %ip + (imm)";
        pub const encoding = .{
            opcodes.LSB.val_ip_rel,
            Encoder.init(8, Int(.imm, i16)),
        };
        pub const entry = val_ip_rel;
    },
    struct {
        pub const spec = "val %bp + (imm)";
        pub const encoding = .{
            opcodes.LSB.val_bp_rel,
            Encoder.init(8, Int(.imm, i16)),
        };
        pub const entry = val_bp_rel;
    },
    struct {
        pub const spec = "val %sp + (imm)";
        pub const encoding = .{
            opcodes.LSB.val_sp_rel,
            Encoder.init(8, Int(.imm, i16)),
        };
        pub const entry = val_sp_rel;
    },
    struct {
        pub const spec = "val %uxp + (imm)";
        pub const encoding = .{
            opcodes.LSB.val_uxp_rel,
            Encoder.init(8, Int(.imm, i16)),
        };
        pub const entry = val_uxp_rel;
    },
    struct {
        pub const spec = "val %kxp + (imm)";
        pub const encoding = .{
            opcodes.LSB.val_kxp_rel,
            Encoder.init(8, Int(.imm, i16)),
        };
        pub const entry = val_kxp_rel;
    },
};

pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 1;

pub fn val_ip_rel(c: *Cycle) void {
    c.sr_to_j(.ip);
    c.dr_i16_to_k();
    c.j_plus_k_to_l(.fresh, .no_flags);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn val_bp_rel(c: *Cycle) void {
    c.sr_to_j(.bp);
    c.dr_i16_to_k();
    c.j_plus_k_to_l(.fresh, .no_flags);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn val_sp_rel(c: *Cycle) void {
    c.sr_to_j(.sp);
    c.dr_i16_to_k();
    c.j_plus_k_to_l(.fresh, .no_flags);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn val_uxp_rel(c: *Cycle) void {
    c.sr_to_j(.uxp);
    c.dr_i16_to_k();
    c.j_plus_k_to_l(.fresh, .no_flags);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn val_kxp_rel(c: *Cycle) void {
    c.sr_to_j(.kxp);
    c.dr_i16_to_k();
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