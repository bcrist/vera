pub const spec = 
    \\ and (imm)
    \\ or (imm)
    \\ xor (imm)
    ;

pub const encoding = .{
    opcodes.mnemonic_encoder(opcodes.LSB, .{ .suffix = "_i16" }),
    Encoder.init(8, Int(.imm, i16)),
};
pub const wio: arch.reg.gpr.Write_Index_Offset.Raw = 0;

pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
    c.reg_to_j();
    c.dr_i16_to_k();
    c.j_logic_k_to_l(switch (mnemonic) {
        .@"and" => ._and,
        .@"or" => ._or,
        .xor => .xor,
        else => unreachable,
    }, .fresh, .flags);
    c.l_to_reg(true);
    c.load_and_exec_next_insn();
}

const opcodes = @import("../opcodes.zig");
const Negate = placeholders.Negate;
const Reg = placeholders.Reg;
const Int = placeholders.Int;
const placeholders = @import("../../compile/placeholders.zig");
const Cycle = @import("../../compile/Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const isa = @import("isa");
const Flags = arch.microcode.Flags;
const arch = @import("arch");
