//! Represents a single instruction in an assembly language program
//! Converting it to machine code requires matching it to a `Form` and then encoding it.

mnemonic: Mnemonic,
params: []const Parameter,

pub fn eql(a: Instruction, b: Instruction) bool {
    return deep_hash_map.deepEql(a, b, .DeepRecursive);
}

pub fn format(self: Instruction, writer: *std.io.Writer) !void {
    try fmt.print_instruction(self, null, writer);
}

pub const Encoded = @import("Instruction/Encoded.zig");
pub const Signature = @import("Instruction/Signature.zig");
pub const Form = @import("Instruction/Form.zig");

const Instruction = @This();

const Mnemonic = enums.Mnemonic;
const enums = @import("enums.zig");
const Parameter = @import("Parameter.zig");
const fmt = @import("fmt.zig");
const deep_hash_map = @import("deep_hash_map");
const std = @import("std");
