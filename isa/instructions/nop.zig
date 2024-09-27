pub const forms = .{
    struct {
        pub const spec =
            \\nop
            \\nop 1
            ;
        pub const encoding = opcodes.LSB.nop_1;
        pub const krio = arch.K.Read_Index_Offset.init(1);
    },
    struct {
        pub const spec = "nop 2";
        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.nop_2),
        };
        pub const krio = arch.K.Read_Index_Offset.init(2);
    },
    struct {
        pub const spec = "nop 3";
        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.nop_3),
        };
        pub const krio = arch.K.Read_Index_Offset.init(3);
    },
};

pub fn entry(c: *Cycle, krio: arch.K.Read_Index_Offset) void {
    c.branch(.ip, krio.raw());
}

const opcodes = @import("opcodes.zig");
const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const isa = @import("isa");
const arch = @import("arch");
