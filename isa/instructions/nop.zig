pub const forms = .{
    struct {
        pub const spec =
            \\nop
            \\nop 1
            \\b .i %ip + 1
            ;
        pub const encoding = opcodes.LSB.nop_1;
        pub const krio = arch.bus.K.Read_Index_Offset.init(1);
        pub const entry = nop;
    },
    struct {
        pub const spec = "nop 2";
        pub const encoding = .{
            opcodes.LSB.misc_10,
            Encoder.init(8, opcodes.Misc_10.b),
            Encoder.init(10, @as(i6, 2)),
        };
        pub const entry = b_ip_plus_2;
    },
    struct {
        pub const spec = "nop 3";
        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.nop_3),
        };
        pub const krio = arch.bus.K.Read_Index_Offset.init(3);
        pub const entry = nop;
    },
};

pub fn b_ip_plus_2(c: *Cycle) void {
    c.branch(.ip, 2);
}

pub fn nop(c: *Cycle, krio: arch.bus.K.Read_Index_Offset) void {
    c.branch(.ip, krio.raw());
}

const opcodes = @import("opcodes.zig");
const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Encoder;
const isa = @import("isa");
const arch = @import("arch");
