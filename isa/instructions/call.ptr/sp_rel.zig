pub const forms = .{
    struct {
        pub const spec =
            \\ call.ptr .s %sp + (imm)
            ;

        pub const encoding = .{
            opcodes.LSB.call_ptr_sp_rel,
            Encoder.init(8, Int(.imm, i16)),
        };
    },
};

pub const entry = load_ptr;

pub fn load_ptr(c: *Cycle) void {
    c.read_to_d(.sp, .i16_from_dr, .@"32b", .stack);
    c.d_to_l();
    c.l_to_sr(.next_ip);
    c.next(call);
}

pub fn call(c: *Cycle) void {
    c.call(.next_ip, 0);
}

const opcodes = @import("../opcodes.zig");
const Int = placeholders.Int;
const placeholders = @import("../../compile/placeholders.zig");
const Cycle = @import("../../compile/Cycle.zig");
const Encoder = isa.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
const std = @import("std");