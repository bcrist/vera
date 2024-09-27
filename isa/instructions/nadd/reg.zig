pub const forms = .{
    struct {
        pub const spec =
            \\ nadd
            \\ naddc
            \\ naddv
            \\ naddcv
            ;
        pub const encoding = opcodes.mnemonic_encoder(opcodes.LSB, .{});
        pub const krio: arch.K.Read_Index_Offset.Raw = 1;
        pub const wio: arch.Write_Index_Offset.Raw = -1;
    },
    struct {
        pub const spec =
            \\ nadd r(reg)
            \\ naddc r(reg)
            \\ naddv r(reg)
            \\ naddcv r(reg)
            ;

        pub const encoding = .{
            opcodes.LSB.add_reg,
            opcodes.mnemonic_encoder(opcodes.Add_Reg, .{ .offset = 8 }),
            Encoder.init(11, Reg(.reg)),
        };

        pub const krio = Reg(.reg);
        pub const wio: arch.Write_Index_Offset.Raw = 0;
    },
};

pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.reg_to_k();
    c.k_minus_j_to_l(switch (mnemonic) {
        .nadd, .naddv => .fresh,
        .naddc, .naddcv => .cont,
        else => unreachable,
    }, switch (mnemonic) {
        .nadd, .naddc => .flags,
        .naddv, .naddcv => .flags__fault_on_overflow,
        else => unreachable,
    });
    c.l_to_reg();
    c.wi_to_ti();
    c.load_and_exec_next_insn();
}

const opcodes = @import("../opcodes.zig");
const Reg = placeholders.Reg;
const placeholders = @import("../../compile/placeholders.zig");
const Cycle = @import("../../compile/Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
