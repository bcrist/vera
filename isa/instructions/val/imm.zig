pub const spec = "val (imm)";
pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 1;

pub const forms = .{
    struct {
        const Value = Bit(.imm);
        pub const encoding = .{
            opcodes.LSB.bit_op,
            Encoder.init(8, opcodes.Bit_Op.val_bit),
            Encoder.init(11, Value),
        };
        pub const krio = Value;
        pub const entry = reg_bit;
    },
    struct {
        // -0x8000_0000 and 0x8000_0000 are the same 32b pattern; allow the negative version to be detected in ASM
        pub const spec = "val -0x8000_0000";
        pub const encoding = .{
            opcodes.LSB.bit_op,
            Encoder.init(8, opcodes.Bit_Op.val_bit),
            Encoder.init(11, @as(u5, 31)),
        };
        pub const krio: u5 = 31;
        pub const entry = reg_bit;
    },

    struct {
        const Value = Invert(Bit(.imm), 32);
        pub const encoding = .{
            opcodes.LSB.bit_op,
            Encoder.init(8, opcodes.Bit_Op.val_not_bit),
            Encoder.init(11, Value),
        };
        pub const krio = Value;
        pub const entry = not_reg_bit;
    },
    struct {
        const Value = Offset(-1, Negate(Bit(.imm)));
        pub const constraints = .{
            .{ .imm, .not_equal, -2147483649 },
        };
        pub const encoding = .{
            opcodes.LSB.bit_op,
            Encoder.init(8, opcodes.Bit_Op.val_not_bit),
            Encoder.init(11, Value),
        };
        pub const krio = Value;
        pub const entry = not_reg_bit;
    },

    struct {
        const Value = Offset(-1, Bit(.imm));
        pub const encoding = .{
            opcodes.LSB.bit_op,
            Encoder.init(8, opcodes.Bit_Op.val_bit_minus_one),
            Encoder.init(11, Value),
        };
        pub const krio = Value;
        pub const entry = reg_bit_minus_one;
    },

    struct {
        const Value = Negate(Bit(.imm));
        pub const encoding = .{
            opcodes.LSB.bit_op,
            Encoder.init(8, opcodes.Bit_Op.val_neg_bit),
            Encoder.init(11, Value),
        };
        pub const krio = Value;
        pub const entry = neg_reg_bit;
    },

    struct {
        pub const encoding = .{
            opcodes.LSB.val,
            Encoder.init(8, Int(.imm, i16)),
        };
        pub const entry = dr_i16;
    },
};

pub fn reg_bit(c: *Cycle) void {
    c.krio_bit_to_l();
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn not_reg_bit(c: *Cycle) void {
    c.not_krio_bit_to_l();
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn reg_bit_minus_one(c: *Cycle) void {
    c.krio_bit_to_k();
    c.sr_to_j(.one);
    c.k_minus_j_to_l(.fresh, .no_flags);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn neg_reg_bit(c: *Cycle) void {
    c.krio_bit_to_k();
    c.zero_to_j();
    c.j_minus_k_to_l(.fresh, .no_flags);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn dr_i16(c: *Cycle) void {
    c.dr_i16_to_l();
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

const opcodes = @import("../opcodes.zig");
const Invert = placeholders.Invert;
const Offset = placeholders.Offset;
const Negate = placeholders.Negate;
const Bit = placeholders.Bit;
const Int = placeholders.Int;
const placeholders = @import("../../compile/placeholders.zig");
const Cycle = @import("../../compile/Cycle.zig");
const Encoder = isa.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
const std = @import("std");