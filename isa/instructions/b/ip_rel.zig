pub const spec =
    \\ b .i %ip + (imm)
    ;

pub const forms = .{
    struct {
        pub const encoding = .{
            opcodes.LSB.misc_10,
            Encoder.init(8, opcodes.Misc_10.b),
            Encoder.init(10, Int(.imm, i6)),
        };
        pub const constraints = .{
            .{ .imm, .not_equal, 0 },
            .{ .imm, .not_equal, 1 },
        };
        pub const entry = b_ip_rel;
    },
    struct {
        pub const encoding = .{
            opcodes.LSB.b_ip_rel16,
            Encoder.init(8, Int(.imm, i16)),
        };
        pub const entry = b_ip_rel16;
    },
    struct {
        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.b_ip_rel32),
            Encoder.init(16, Int(.imm, i32)),
        };
        pub const entry = b_ip_rel32;
    },
    struct {
        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.b_ip_rel32_unaligned),
            Encoder.init_dont_care(16, @as(u8, 0)),
            Encoder.init(24, Int(.imm, i32)),
        };
        pub const entry = b_ip_rel32;
    },
};

pub fn b_ip_rel(c: *Cycle, imm: Any(.imm)) void {
    c.branch(.ip, imm.value);
}

pub fn b_ip_rel16(c: *Cycle) void {
    c.branch(.ip, .i16_from_dr);
}

pub fn b_ip_rel32(c: *Cycle) void {
    c.ip_read_to_d(2, .@"32b");
    c.d_to_l();
    c.l_to_sr(.temp_1);
    c.next(compute_next_ip);
}
pub fn b_ip_rel32_unaligned(c: *Cycle) void {
    c.ip_read_to_d(3, .@"32b");
    c.d_to_l();
    c.l_to_sr(.temp_1);
    c.next(compute_next_ip);
}

pub fn compute_next_ip(c: *Cycle) void {
    c.sr_to_j(.ip);
    c.sr_to_k(.temp_1);
    c.j_plus_k_to_l(.fresh, .no_flags);
    c.l_to_sr(.next_ip);
    c.next(branch);
}

pub fn branch(c: *Cycle) void {
    c.branch(.next_ip, 0);
}

const opcodes = @import("../opcodes.zig");
const Range = placeholders.Range;
const Any = placeholders.Any;
const Int = placeholders.Int;
const Reg = placeholders.Reg;
const placeholders = @import("../../compile/placeholders.zig");
const Cycle = @import("../../compile/Cycle.zig");
const Encoder = isa.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
const std = @import("std");