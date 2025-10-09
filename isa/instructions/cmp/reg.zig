pub const forms = .{
    struct {
        pub const spec = 
            \\ cmp
            \\ cmpc
            ;

        pub const encoding = opcodes.mnemonic_encoder(opcodes.LSB, .{});
        pub const krio: arch.bus.K.Read_Index_Offset.Raw = 1;
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = -2;
    },
    struct {
        pub const spec = 
            \\ cmp %(reg)
            \\ cmpc %(reg)
            ;

        pub const encoding = .{
            opcodes.LSB.sub_reg,
            opcodes.mnemonic_encoder(opcodes.Sub_Reg, .{ .offset = 8 }),
            Encoder.init(11, Reg(.reg)),
        };

        pub const krio = Reg(.reg);
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = -1;
    },
};

pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.reg_to_k();
    c.j_minus_k_to_l(switch (mnemonic) {
        .cmp => .fresh,
        .cmpc => .cont,
        else => unreachable,
    }, .flags);
    c.wi_to_ti();
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
