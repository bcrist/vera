pub const forms = .{
    struct {
        pub const spec =
            \\ b
            ;
        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.b),
        };

        pub const krio: arch.bus.K.Read_Index_Offset.Raw = 0;
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = -1;
    },
    struct {
        pub const spec =
            \\ b .i %(reg)
            ;

        pub const encoding = .{
            opcodes.LSB.sub_reg,
            Encoder.init(8, opcodes.Sub_Reg.b_reg),
            Encoder.init(11, Reg(.reg)),
        };

        pub const krio = Reg(.reg);
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;
    },
};

pub fn entry(c: *Cycle) void {
    c.sr_to_j(.ip);
    c.reg_to_k();
    c.j_plus_k_to_l(.fresh, .no_flags);
    c.l_to_sr(.next_ip);
    c.wi_to_ti();
    c.next(branch);
}

pub fn branch(c: *Cycle) void {
    c.branch(.next_ip, 0);
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