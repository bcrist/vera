pub const spec = 
    \\call .i (imm)
    ;

pub const forms = .{
    struct {
        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.call_imm),
            Encoder.init(16, Int(.imm, u32)),
        };
        pub const entry = call_imm;
    },

    // Ideally this instruction does a 32b IP-relative load from the location just after the 16 bit opcode.
    // That only works if the start of the instruction is 2-byte aligned however.
    // Instead of needing to insert a nop in some cases, we have a second form where the absolute address is offset by an extra byte:
    struct {
        pub const encoding = .{
            opcodes.LSB.misc_16,
            Encoder.init(8, opcodes.Misc_16.call_imm_unaligned),
            Encoder.init_dont_care(16, @as(u8, 0)),
            Encoder.init(24, Int(.imm, u32)),
        };
        pub const entry = call_imm_unaligned;
    },
};

pub fn call_imm(c: *Cycle) void {
    c.ip_read_to_d(2, .@"32b");
    c.d_to_l();
    c.l_to_sr(.next_ip);
    c.next(call);
}

pub fn call_imm_unaligned(c: *Cycle) void {
    c.ip_read_to_d(3, .@"32b");
    c.d_to_l();
    c.l_to_sr(.next_ip);
    c.next(call);
}

pub fn call(c: *Cycle) void {
    c.call(.next_ip, 0);
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
