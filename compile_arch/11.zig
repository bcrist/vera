const region_encoder = Encoder.shifted(14, @as(u2, 3));

pub const instructions = .{
};

const Cycle = @import("Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Instruction_Signature = isa.Instruction_Signature;
const Parameter = isa.Parameter;
const Options = placeholders.Options;
const Range = placeholders.Range;
const Int = placeholders.Int;
const Int_Mult = placeholders.Int_Mult;
const Reg = placeholders.Reg;
const Reg_Bit = placeholders.Reg_Bit;
const Even_Reg = placeholders.Even_Reg;
const Odd_Reg = placeholders.Odd_Reg;
const conditions = @import("conditions.zig");
const placeholders = @import("placeholders.zig");
const opcodes = @import("opcodes.zig");
const Control_Signals = arch.hw.Control_Signals;
const Flags = arch.hw.microcode.Flags;
const isa = arch.isa;
const arch = @import("lib_arch");
const std = @import("std");
