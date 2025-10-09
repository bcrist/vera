pub const forms = .{
    struct {
        pub const spec =
            \\ call.ptr
            ;
        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.call_ptr),
        };

        pub const krio: arch.bus.K.Read_Index_Offset.Raw = 0;
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = -1;
    },
    struct {
        pub const spec =
            \\ call.ptr .d %(reg)
            ;

        pub const encoding = .{
            opcodes.LSB.shift_reg,
            Encoder.init(8, opcodes.Shift_Reg.call_ptr_reg),
            Encoder.init(11, Reg(.reg)),
        };

        pub const krio = Reg(.reg);
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;
    },
};

pub fn entry(c: *Cycle) void {
    c.reg_to_k_to_l();
    c.l_to_sr(.temp_1);
    c.wi_to_ti();
    c.next(load_ptr);
}

pub fn load_ptr(c: *Cycle) void {
    c.read_to_d(.temp_1, 0, .@"32b", .data);
    c.d_to_l();
    c.l_to_sr(.next_ip);
    c.next(call);
}

pub fn call(c: *Cycle) void {
    c.call(.next_ip, 0);
}

const opcodes = @import("../opcodes.zig");
const Reg = placeholders.Reg;
const placeholders = @import("../../compile/placeholders.zig");
const Cycle = @import("../../compile/Cycle.zig");
const Encoder = isa.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
const std = @import("std");