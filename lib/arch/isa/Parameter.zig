signature: Signature,
base_register_index: Register_Index,
offset_register_index: Register_Index,
constant: i64,

pub const Signature = struct {
    address_space: ?isa.Address_Space,
    base: Kind,
    offset: Kind,
};

pub const Kind = union (enum) {
    none,
    arrow,
    constant,
    reg8: ?Signedness,
    reg16: ?Signedness,
    reg32: ?Signedness,
    sr: Special_Register,
};

pub const Index = enum (u4) {
    _,
    pub fn init(raw_value: u4) Index {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: Index) u4 {
        return @intFromEnum(self);
    }
    pub const Raw = std.meta.Tag(Index);
    pub const count = std.math.maxInt(Index) + 1;
};

const Parameter = @This();
const Special_Register = isa.Special_Register;
const isa = arch.isa;
const Register_Index = arch.hw.Register_Index;
const arch = @import("lib_arch");
const Signedness = std.builtin.Signedness;
const std = @import("std");
