mnemonic: Mnemonic,
suffix: Mnemonic_Suffix,
params: []const Parameter,

const Instruction = @This();
const Parameter = isa.Parameter;
const Mnemonic = isa.Mnemonic;
const Mnemonic_Suffix = isa.Mnemonic_Suffix;
const isa = @import("../isa.zig");
