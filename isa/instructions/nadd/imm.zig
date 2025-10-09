pub const forms = .{
    struct {
        pub const spec =
            \\ nadd (imm)
            \\ naddc (imm)
            \\ nadd.vf (imm)
            \\ naddc.vf (imm)
            ;

        pub const encoding = .{
            opcodes.mnemonic_encoder(opcodes.LSB , .{ .suffix = "_i16" }),
            Encoder.init(8, Int(.imm, i16)),
        };
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

        pub const entry = nadd_i16;
    },
    struct {
        pub const spec =
            \\ nadd 0
            \\ naddc 0
            \\ nadd.vf 0
            \\ naddc.vf 0
            \\ neg
            \\ negc
            \\ neg.vf
            \\ negc.vf
            ;
        
        pub fn encoding(mnemonic: isa.Mnemonic) opcodes.LSB {
            return switch (mnemonic) {
                .nadd, .neg => .neg,
                .naddc, .negc => .negc,
                .@"nadd.vf", .@"neg.vf" => .@"neg.vf",
                .@"naddc.vf", .@"negc.vf" => .@"negc.vf",
                else => unreachable,
            };
        }
        pub const krio: arch.bus.K.Read_Index_Offset.Raw_Signed = 1;
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

        pub const entry = nadd_krio;
    },
};

pub fn nadd_i16(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.dr_i16_to_k();
    k_minus_j_to_l(c, mnemonic);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn nadd_krio(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.krio_to_k(.sx);
    k_minus_j_to_l(c, mnemonic);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

fn k_minus_j_to_l(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.k_minus_j_to_l(switch (mnemonic) {
        .nadd, .neg,
        .@"nadd.vf", .@"neg.vf",
        => .fresh,

        .naddc, .negc,
        .@"naddc.vf", .@"negc.vf",
        => .cont,

        else => unreachable,
    }, switch (mnemonic) {
        .nadd, .neg,
        .naddc, .negc,
        => .flags,

        .@"nadd.vf", .@"neg.vf",
        .@"naddc.vf", .@"negc.vf",
        => .flags__fault_on_overflow,
        
        else => unreachable,
    });
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
