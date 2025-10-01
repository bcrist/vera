address_space: ?Address_Space,
base: Parameter.Kind,
offset: Parameter.Kind,

pub fn format(self: Signature, writer: *std.io.Writer) !void {
    try fmt.print_parameter_signature(self, .{}, writer);
}

pub fn eql(a: Signature, b: Signature) bool {
    return std.meta.eql(a, b);
}

const Signature = @This();

const Parameter = @import("../Parameter.zig");
const Address_Space = enums.Address_Space;
const enums = @import("../enums.zig");
const fmt = @import("../fmt.zig");
const std = @import("std");
