mnemonic: Mnemonic,
params: []const Parameter.Signature,

pub fn eql(self: Signature, other: Signature) bool {
    if (self.mnemonic != other.mnemonic or self.params.len != other.params.len) return false;
    for (self.params, other.params) |sp, op| {
        if (!sp.eql(op)) return false;
    }
    return true;
}

pub fn format(self: Signature, writer: *std.io.Writer) !void {
    try fmt.print_instruction_signature(self, writer);
}

const Signature = @This();

const Mnemonic = enums.Mnemonic;
const enums = @import("../enums.zig");
const Parameter = @import("../Parameter.zig");
const fmt = @import("../fmt.zig");
const std = @import("std");
