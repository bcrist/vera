pub const forms = .{
    struct {
        pub const spec =
            \\ st.8
            \\ st.16
            \\ st.24
            \\ st.32
            ;

        pub const encoding = .{
            opcodes.LSB.misc_16,
            opcodes.mnemonic_encoder(opcodes.Misc_16, .{ .offset = 8 }),
        };

        pub const krio: arch.bus.K.Read_Index_Offset.Raw = 0;
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = -1;
        pub const entry = pop2_store; // both offset and data will be popped
    },
    struct {
        pub const spec =
            \\ st.8 .d %(reg)
            \\ st.16 .d %(reg)
            \\ st.24 .d %(reg)
            \\ st.32 .d %(reg)
            ;

        pub const encoding = .{
            opcodes.LSB.ld_st_reg,
            opcodes.mnemonic_encoder(opcodes.LD_ST_Reg, .{ .offset = 8 }),
            Encoder.init(11, Reg(.reg)),
        };

        pub const krio = Reg(.reg);
        pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = -1;
        pub const entry = pop1_store; // only data will be popped
    },
};

pub fn pop2_store(c: *Cycle) void {
    c.load_next_insn();
    c.reg_to_k_to_l();
    c.l_to_sr(.temp_1);
    c.wi_to_ti();
    c.next(store);
}

pub fn pop1_store(c: *Cycle) void {
    c.load_next_insn();
    c.reg_to_k_to_l();
    c.l_to_sr(.temp_1);
    c.next(store);
}

pub fn store(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j_to_l();
    c.write_from_l(.temp_1, 0, switch (mnemonic) {
        isa.Mnemonic.init("st.8") => .@"8b",
        isa.Mnemonic.init("st.16") => .@"16b",
        isa.Mnemonic.init("st.24") => .@"24b",
        isa.Mnemonic.init("st.32") => .@"32b",
        else => unreachable,
    }, .data);
    c.wi_to_ti();
    c.exec_next_insn();
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