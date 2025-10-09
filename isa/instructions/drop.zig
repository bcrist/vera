pub const forms = .{
    struct {
        pub const spec = "drop";
        pub const encoding = .{
            opcodes.LSB.misc_12,
            Encoder.init(8, opcodes.Misc_12.drop),
            Encoder.init(12, @as(u4, 0)),
        };
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = -1;
        pub const entry = drop;
    },
    struct {
        pub const spec = "drop (imm)";
        pub const encoding = .{
            opcodes.LSB.misc_12,
            Encoder.init(8, opcodes.Misc_12.drop),
            Encoder.init(12, Range(.imm, 1, 16)),
        };
        pub const wio = Negate(Int(.imm, i5));
        pub const entry = drop;
    },
    struct {
        pub const spec = "drop.copy";
        pub const encoding = .{
            opcodes.LSB.misc_12,
            Encoder.init(8, opcodes.Misc_12.@"drop.copy"),
            Encoder.init(12, @as(u4, 0)),
        };
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = -1;
        pub const entry = drop_copy;
    },
    struct {
        pub const spec = "drop.copy (imm)";
        pub const encoding = .{
            opcodes.LSB.misc_12,
            Encoder.init(8, opcodes.Misc_12.@"drop.copy"),
            Encoder.init(12, Range(.imm, 1, 16)),
        };
        pub const constraints = .{
            .{ .imm, .greater_than, 0 },
        };
        pub const wio = Negate(Int(.imm, i5));
        pub const entry = drop_copy;
    },
};

pub fn drop(c: *Cycle) void {
    c.wi_to_ti();
    c.load_and_exec_next_insn();
}

pub fn drop_copy(c: *Cycle) void {
    c.reg_to_j_to_l();
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

const opcodes = @import("opcodes.zig");
const Negate = placeholders.Negate;
const Range = placeholders.Range;
const Any = placeholders.Any;
const Int = placeholders.Int;
const Reg = placeholders.Reg;
const placeholders = @import("../compile/placeholders.zig");
const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
const std = @import("std");