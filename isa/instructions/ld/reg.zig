pub const forms = .{
    struct {
        pub const spec =
            \\ ld.8
            \\ ld.16
            \\ ld.24
            \\ ld.32
            ;

        pub const encoding = .{
            opcodes.LSB.misc_16,
            opcodes.mnemonic_encoder(opcodes.Misc_16, .{ .offset = 8 }),
        };

        pub const krio: arch.bus.K.Read_Index_Offset.Raw = 0;
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = -1;
    },
    struct {
        pub const spec =
            \\ ld.8 .d %(reg)
            \\ ld.16 .d %(reg)
            \\ ld.24 .d %(reg)
            \\ ld.32 .d %(reg)
            ;

        pub const encoding = .{
            opcodes.LSB.ld_st_reg,
            opcodes.mnemonic_encoder(opcodes.LD_ST_Reg, .{ .offset = 8 }),
            Encoder.init(11, Reg(.reg)),
        };

        pub const krio = Reg(.reg);
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;
    },
};

pub fn entry(c: *Cycle) void {
    c.load_next_insn();
    c.reg_to_k_to_l();
    c.l_to_sr(.temp_1);
    c.next(load_ptr);
}

pub fn load_ptr(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.read_to_d(.temp_1, 0, switch (mnemonic) {
        isa.Mnemonic.init("ld.8") => .@"8b",
        isa.Mnemonic.init("ld.16") => .@"16b",
        isa.Mnemonic.init("ld.24") => .@"24b",
        isa.Mnemonic.init("ld.32") => .@"32b",
        else => unreachable,
    }, .data);
    c.d_to_l();
    c.l_to_reg(true);
    c.exec_next_insn();
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