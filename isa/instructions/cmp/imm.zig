pub const forms = .{
    struct {
        pub const spec = 
            \\cmp (imm)
            \\cmpc (imm)
            ;

        pub const encoding = .{
            opcodes.mnemonic_encoder(opcodes.LSB, .{ .suffix = "_i16" }),
            Encoder.init(8, Int(.imm, i16)),
        };
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = -1;

        pub const entry = cmp_i16;
    },
    struct {
        pub const spec = "cmp 0";
        pub const encoding = opcodes.LSB.cmp_0;
        pub const krio: arch.bus.K.Read_Index_Offset.Raw = 0;
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = -1;

        pub const entry = cmp_krio;
    },
    struct {
        pub const spec = "cmpc 0";
        pub const encoding = opcodes.LSB.cmpc_0;
        pub const krio: arch.bus.K.Read_Index_Offset.Raw = 0;
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = -1;

        pub const entry = cmp_krio;
    },
};

pub fn cmp_i16(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.dr_i16_to_k();
    j_minus_k_to_l(c, mnemonic);
    c.wi_to_ti();
    c.load_and_exec_next_insn();
}

pub fn cmp_krio(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.krio_to_k(.zx);
    j_minus_k_to_l(c, mnemonic);
    c.wi_to_ti();
    c.load_and_exec_next_insn();
}

fn j_minus_k_to_l(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.j_minus_k_to_l(switch (mnemonic) {
        .cmp => .fresh,
        .cmpc => .cont,
        else => unreachable,
    }, .flags);
}

const opcodes = @import("../opcodes.zig");
const Negate = placeholders.Negate;
const Reg = placeholders.Reg;
const Int = placeholders.Int;
const placeholders = @import("../../compile/placeholders.zig");
const Cycle = @import("../../compile/Cycle.zig");
const Encoder = isa.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
