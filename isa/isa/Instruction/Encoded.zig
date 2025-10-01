data: Data,
len: Length_Bytes,

pub const Data = u128;
pub const Offset_Bits = std.math.Log2Int(Data);
pub const Length_Bits = std.math.Log2IntCeil(Data);
pub const Length_Bytes = std.meta.Int(.unsigned, std.math.log2_int_ceil(usize, @sizeOf(Data)));

pub fn as_bytes(self: *const Encoded) []const u8 {
    return std.mem.asBytes(&self.data)[0..self.len];
}

const Encoded = @This();

const std = @import("std");
