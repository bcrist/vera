pub const forms = .{
    struct {
        pub const spec = 
            \\add (imm)
            \\addc (imm)
            \\addv (imm)
            \\addcv (imm)
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
            \\ subv (imm)
            \\ subcv (imm)
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
                .subv => .addv_i16,
                .subcv => .addcv_i16,
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
            \\ addv 1
            \\ subv -1
            \\ incv
            ;
        
        pub const encoding = opcodes.LSB.incv;
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
            \\ addv -1
            \\ subv 1
            \\ decv
            ;
        
        pub const encoding = opcodes.LSB.decv;
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
            \\ addcv 0
            \\ subcv 0
            ;
        
        pub const encoding = opcodes.LSB.addcv_0;
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
        .add, .addv,
        .sub, .subv,
        .inc, .incv,
        .dec, .decv,
        => .fresh,

        .addc, .addcv,
        .subc, .subcv,
        => .cont,

        else => unreachable,
    }, switch (mnemonic) {
        .add, .addc,
        .sub, .subc,
        .inc, .dec,
        => .flags,

        .addv, .addcv,
        .subv, .subcv,
        .incv, .decv,
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
const Encoder = isa.Instruction_Encoding.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
