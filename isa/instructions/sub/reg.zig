pub const forms = .{
    struct {
        pub const spec = 
            \\ sub
            \\ subc
            \\ subv
            \\ subcv
            ;

        pub const encoding = opcodes.mnemonic_encoder(opcodes.LSB, .{});
        pub const krio: arch.bus.K.Read_Index_Offset.Raw = 1;
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = -1;
    },
    struct {
        pub const spec = 
            \\ sub r(reg)
            \\ subc r(reg)
            \\ subv r(reg)
            \\ subcv r(reg)
            ;

        pub const encoding = .{
            opcodes.LSB.sub_reg,
            opcodes.mnemonic_encoder(opcodes.Sub_Reg, .{ .offset = 8 }),
            Encoder.init(11, Reg(.reg)),
        };

        pub const krio = Reg(.reg);
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;
    },
};

pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.reg_to_k();
    c.j_minus_k_to_l(switch (mnemonic) {
        .sub, .subv => .fresh,
        .subc, .subcv => .cont,
        else => unreachable,
    }, switch (mnemonic) {
        .sub, .subc => .flags,
        .subv, .subcv => .flags__fault_on_overflow,
        else => unreachable,
    });
    c.l_to_reg(true);
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
const std = @import("std");
