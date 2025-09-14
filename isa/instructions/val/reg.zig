pub const spec = "val r(reg)";
pub const encoding = .{
    opcodes.LSB.misc_reg,
    Encoder.init(8, opcodes.Misc_Reg.val_reg),
    Encoder.init(11, Reg(.reg)),
};
pub const krio = Reg(.reg);
pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 1;

pub fn entry(c: *Cycle) void {
    c.reg_to_k_to_l();
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

const opcodes = @import("../opcodes.zig");
const Reg = placeholders.Reg;
const placeholders = @import("../../compile/placeholders.zig");
const Cycle = @import("../../compile/Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
const std = @import("std");