pub const forms = .{
    struct {
        pub const spec = 
            \\add (imm)
            \\addc (imm)
            \\add.vf (imm)
            \\addc.vf (imm)
            ;

        pub const encoding = .{
            opcodes.mnemonic_encoder(opcodes.LSB, .{ .suffix = "_i16" }),
            Encoder.init(8, Int(.imm, i16)),
        };

        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

        pub const entry = add_i16;
    },
    struct {
        pub const spec =
            \\ sub (imm)
            \\ subc (imm)
            \\ sub.vf (imm)
            \\ subc.vf (imm)
            ;

        pub const encoding = .{
            mnemonic_encoder,
            Encoder.init(8, Negate(Int(.imm, i16))),
        };

        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) opcodes.LSB {
            return switch (mnemonic) {
                .sub => .add_i16,
                .subc => .addc_i16,
                .@"sub.vf" => .@"add.vf_i16",
                .@"subc.vf" => .@"addc.vf_i16",
                else => unreachable,
            };
        }

        pub const entry = add_i16;
    },
    struct {
        pub const spec =
            \\ add 1
            \\ sub -1
            \\ inc
            ;
        
        pub const encoding = opcodes.LSB.inc;
        pub const krio: arch.bus.K.Read_Index_Offset.Raw_Signed = 1;
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

        pub const entry = add_krio;
    },
    struct {
        pub const spec =
            \\ add.vf 1
            \\ sub.vf -1
            \\ inc.vf
            ;
        
        pub const encoding = opcodes.LSB.@"inc.vf";
        pub const krio: arch.bus.K.Read_Index_Offset.Raw_Signed = 1;
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

        pub const entry = add_krio;
    },
    struct {
        pub const spec =
            \\ add -1
            \\ sub 1
            \\ dec
            ;
        
        pub const encoding = opcodes.LSB.dec;
        pub const krio: arch.bus.K.Read_Index_Offset.Raw_Signed = -1;
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

        pub const entry = add_krio;
    },
    struct {
        pub const spec =
            \\ add.vf -1
            \\ sub.vf 1
            \\ dec.vf
            ;
        
        pub const encoding = opcodes.LSB.@"dec.vf";
        pub const krio: arch.bus.K.Read_Index_Offset.Raw_Signed = -1;
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

        pub const entry = add_krio;
    },
    struct {
        pub const spec =
            \\ addc 0
            \\ subc 0
            ;
        
        pub const encoding = opcodes.LSB.addc_0;
        pub const krio: arch.bus.K.Read_Index_Offset.Raw_Signed = 0;
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

        pub const entry = add_krio;
    },
    struct {
        pub const spec =
            \\ addc.vf 0
            \\ subc.vf 0
            ;
        
        pub const encoding = opcodes.LSB.@"addc.vf_0";
        pub const krio: arch.bus.K.Read_Index_Offset.Raw_Signed = 0;
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

        pub const entry = add_krio;
    },
};

pub fn add_i16(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.dr_i16_to_k();
    j_plus_k_to_l(c, mnemonic);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

pub fn add_krio(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.krio_to_k(.sx);
    j_plus_k_to_l(c, mnemonic);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

fn j_plus_k_to_l(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.j_plus_k_to_l(switch (mnemonic) {
        .add, .@"add.vf",
        .sub, .@"sub.vf",
        .inc, .@"inc.vf",
        .dec, .@"dec.vf",
        => .fresh,

        .addc, .@"addc.vf",
        .subc, .@"subc.vf",
        => .cont,

        else => unreachable,
    }, switch (mnemonic) {
        .add, .addc,
        .sub, .subc,
        .inc, .dec,
        => .flags,

        .@"add.vf", .@"addc.vf",
        .@"sub.vf", .@"subc.vf",
        .@"inc.vf", .@"dec.vf",
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
