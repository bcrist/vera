pub const forms = .{
    struct {
        pub const spec =
            \\ and
            \\ or
            \\ xor
            ;
        pub const encoding = opcodes.mnemonic_encoder(opcodes.LSB, .{});
        pub const krio: arch.bus.K.Read_Index_Offset.Raw = 1;
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = -1;
    },
    struct {
        pub const spec =
            \\ and r(reg)
            \\ or r(reg)
            \\ xor r(reg)
            ;

        pub const encoding = .{
            opcodes.LSB.misc_reg,
            opcodes.mnemonic_encoder(opcodes.Misc_Reg, .{ .offset = 8 }),
            Encoder.init(11, Reg(.reg)),
        };

        pub const krio = Reg(.reg);
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;
    },
};

pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.reg_to_k();
    c.j_logic_k_to_l(switch (mnemonic) {
        .@"and" => ._and,
        .@"or" => ._or,
        .xor => .xor,
        else => unreachable,
    }, .fresh, .flags) ;
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
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