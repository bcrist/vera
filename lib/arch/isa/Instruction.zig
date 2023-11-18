mnemonic: Mnemonic,
suffix: Mnemonic_Suffix,
params: []const Parameter,

const Instruction = @This();
const Parameter = @import("Parameter.zig");
const Mnemonic = isa.Mnemonic;
const Mnemonic_Suffix = isa.Mnemonic_Suffix;
const isa = arch.isa;
const arch = @import("lib_arch");
