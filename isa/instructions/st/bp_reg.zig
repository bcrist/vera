pub const spec =
    \\ st.8 .d %bp + %(reg)
    \\ st.16 .d %bp + %(reg)
    \\ st.32 .d %bp + %(reg)
    ;

pub const encoding = .{
    opcodes.LSB.bp_reg,
    opcodes.mnemonic_encoder(opcodes.BP_Reg, .{ .offset = 8 }),
    Encoder.init(11, Reg(.reg)),
};

pub const krio = Reg(.reg);
pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = -1;

pub fn entry(c: *Cycle) void {
    c.load_next_insn();
    c.sr_to_j(.bp);
    c.reg_to_k();
    c.j_plus_k_to_l(.fresh, .no_flags);
    c.l_to_sr(.temp_1);
    c.next(store);
}

pub fn store(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j_to_l();
    c.write_from_l(.temp_1, 0, switch (mnemonic) {
        isa.Mnemonic.init("st.8") => .@"8b",
        isa.Mnemonic.init("st.16") => .@"16b",
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