pub const spec = "sb (imm)";
pub const encoding = .{
    opcodes.LSB.misc_reg,
    Encoder.init(8, opcodes.Misc_Reg.sb),
    Encoder.init(11, Int(.imm, u5)),
};

pub const krio = Int(.imm, u5);

pub fn entry(c: *Cycle) void {
    c.reg_to_j();
    c.krio_bit_to_k();
    c.j_logic_k_to_l(._or, .fresh, .no_flags);
    c.l_to_reg();
    c.load_and_exec_next_insn();
}

const opcodes = @import("opcodes.zig");
const Reg_Bit = placeholders.Reg_Bit;
const Int = placeholders.Int;
const Reg = placeholders.Reg;
const placeholders = @import("../compile/placeholders.zig");
const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
const std = @import("std");
