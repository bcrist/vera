pub const forms = .{
    struct {
        pub const spec =
            \\ nadd
            \\ naddc
            \\ nadd.vf
            \\ naddc.vf
            ;
        pub const encoding = opcodes.mnemonic_encoder(opcodes.LSB, .{});
        pub const krio: arch.bus.K.Read_Index_Offset.Raw = 1;
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = -1;
    },
    struct {
        pub const spec =
            \\ nadd %(reg)
            \\ naddc %(reg)
            \\ nadd.vf %(reg)
            \\ naddc.vf %(reg)
            ;

        pub const encoding = .{
            opcodes.LSB.add_reg,
            opcodes.mnemonic_encoder(opcodes.Add_Reg, .{ .offset = 8 }),
            Encoder.init(11, Reg(.reg)),
        };

        pub const krio = Reg(.reg);
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;
    },
};

pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.reg_to_k();
    c.k_minus_j_to_l(switch (mnemonic) {
        .nadd, .@"nadd.vf" => .fresh,
        .naddc, .@"naddc.vf" => .cont,
        else => unreachable,
    }, switch (mnemonic) {
        .nadd, .naddc => .flags,
        .@"nadd.vf", .@"naddc.vf" => .flags__fault_on_overflow,
        else => unreachable,
    });
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
