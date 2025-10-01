//! Represents a single paramter of an instruction within an assembly program

signature: Signature,
base_register: arch.bus.K.Read_Index_Offset,
offset_register: arch.bus.K.Read_Index_Offset,
constant: i64, // value is exactly as it would appear in assembly; not encoded in any way.

pub fn format(self: Parameter, writer: *std.io.Writer) !void {
    try fmt.print_parameter(self, null, writer);
}

pub const Signature = @import("Parameter/Signature.zig");

pub const Kind = union (enum) {
    none,
    constant,
    reg: ?std.builtin.Signedness,
    sr: Special_Register,

    pub fn format(self: Signature, writer: *std.io.Writer) !void {
        try fmt.print_parameter_kind(self, .{}, writer);
    }
};

pub const Index = enum (u4) {
    invalid = 0xF,
    _,
    pub fn init(raw_value: u4) Index {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: Index) u4 {
        return @intFromEnum(self);
    }
    pub const Raw = meta.Backing(Index);
    pub const count = std.math.maxInt(Raw) + 1;
};

const Parameter = @This();

const Special_Register = enums.Special_Register;
const Address_Space = enums.Address_Space;
const enums = @import("enums.zig");
const fmt = @import("fmt.zig");
const arch = @import("arch");
const meta = @import("meta");
const std = @import("std");
