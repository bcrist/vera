mnemonic: Mnemonic,
suffix: Mnemonic_Suffix,
params: []const Parameter,

pub fn eql(a: Instruction, b: Instruction) bool {
     return deep_hash_map.deepEql(a, b, .DeepRecursive);
}

const Instruction = @This();
const Parameter = isa.Parameter;
const Mnemonic = isa.Mnemonic;
const Mnemonic_Suffix = isa.Mnemonic_Suffix;
const isa = @import("../isa.zig");
const deep_hash_map = @import("deep_hash_map");
