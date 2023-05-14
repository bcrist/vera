const std = @import("std");
const lex = @import("lex.zig");

const Constant = @This();

bit_count: i64, // when negative, indicates that the number is signed
data: union {
    short: [8]u8,
    long: [*]const u8,
},

pub const InternPool = std.HashMapUnmanaged(*const Constant, void, HashContext, std.hash_map.default_max_load_percentage);
const HashContext = struct {
    pub fn hash(_: HashContext, key: *const Constant) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(std.mem.asBytes(&key.bit_count));
        hasher.update(key.asString());
        return hasher.final();
    }
    pub fn eql(_: HashContext, a: *const Constant, b: *const Constant) bool {
        return a.bit_count == b.bit_count and std.mem.eql(u8, a.asString(), b.asString());
    }
};

// It is assumed that any bits in the last byte of value, beyond the last required bit,
// are either zero or sign extended from the last valid bit (depending on signedness)
fn init(value: [*]const u8, bits: u63, signedness: std.builtin.Signedness) Constant {
    if (bits <= 64) {
        var src: []const u8 = undefined;
        src.ptr = value;
        src.len = (bits + 7) / 8;

        var data: u64 = 0;
        std.mem.copy(u8, std.mem.asBytes(&data), src);

        var encoded_bits: i64 = bits;
        if (signedness == .signed) {
            encoded_bits = -encoded_bits;
            if (bits < 64) {
                const bits_u6 = @intCast(u6, bits);
                var sign = @truncate(u1, data >> (bits_u6 - 1));
                if (sign != 0) {
                    data |= @as(u64, 0xFFFF_FFFF_FFFF_FFFF) << bits_u6;
                }
            }
        }
        return .{
            .bit_count = encoded_bits,
            .data = .{ .short = std.mem.toBytes(data) },
        };
    } else return .{
        .bit_count = switch (signedness) {
            .unsigned => bits,
            .signed => -@as(i64, bits),
        },
        .data = .{ .long = value },
    };
}

pub fn initString(value: []const u8) Constant {
    return init(value.ptr, @intCast(u63, value.len * 8), .unsigned);
}

// Returns error.Overrun if the requested number of bits is larger than what's available in value.
// Returns error.Overflow if truncation of value to the requested bits changes it's numeric value, given the signedness requested.
pub fn initStringBits(value: []const u8, bits: u63, signedness: std.builtin.Signedness) !Constant {
    if (bits > value.len * 8) return error.Overrun;
    const leftover_bits = @intCast(u3, bits & 7);
    if (leftover_bits > 0) {
        const last_byte = value[bits / 8];
        var mask: u8 = 0xFF;
        mask <<= leftover_bits;
        switch (signedness) {
            .unsigned => {
                if ((last_byte & mask) != 0) return error.Overflow;
                for (value[last_byte + 1 ..]) |b| {
                    if (b != 0) return error.Overflow;
                }
            },
            .signed => {
                const sign = @truncate(u1, last_byte >> (leftover_bits - 1));
                switch (sign) {
                    0 => {
                        if ((last_byte & mask) != 0) return error.Overflow;
                        for (value[last_byte + 1 ..]) |b| {
                            if (b != 0) return error.Overflow;
                        }
                    },
                    1 => {
                        if ((last_byte & mask) != mask) return error.Overflow;
                        for (value[last_byte + 1 ..]) |b| {
                            if (b != 0xFF) return error.Overflow;
                        }
                    },
                }
            },
        }
    }
    return init(value.ptr, bits, signedness);
}

// Store an integer using the fewest bits possible, while ensuring that the MSB is always 0 for unsigned integers.
// This ensures that cloneWithSignedness won't change the value
pub fn initInt(value: anytype) Constant {
    const T = @TypeOf(value);
    const signedness = @typeInfo(T).Int.signedness;
    const T8 = std.meta.Int(signedness, @sizeOf(T) * 8);
    const ext_value = @as(T8, value);
    const bits = @bitSizeOf(T) + @as(u63, 1) - if (value < 0) @clz(~value) else @clz(value);
    return init(std.mem.asBytes(&ext_value), bits, signedness);
}

test "initInt" {
    {
        var constant = initInt(@as(u1, 0));
        try std.testing.expectEqual(@as(i64, 1), constant.bit_count);
        try std.testing.expectEqualSlices(u8, "\x00", constant.asString());
    }
    {
        var constant = initInt(@as(u1, 1));
        try std.testing.expectEqual(@as(i64, 2), constant.bit_count);
        try std.testing.expectEqualSlices(u8, "\x01", constant.asString());
    }
    {
        var constant = initInt(@as(i1, 0));
        try std.testing.expectEqual(@as(i64, -1), constant.bit_count);
        try std.testing.expectEqualSlices(u8, "\x00", constant.asString());
    }
    {
        var constant = initInt(@as(i1, -1));
        try std.testing.expectEqual(@as(i64, -1), constant.bit_count);
        try std.testing.expectEqualSlices(u8, "\xFF", constant.asString());
    }
    {
        var constant = initInt(@as(u7, 0));
        try std.testing.expectEqual(@as(i64, 1), constant.bit_count);
        try std.testing.expectEqualSlices(u8, "\x00", constant.asString());
    }
    {
        var constant = initInt(@as(u7, 127));
        try std.testing.expectEqual(@as(i64, 8), constant.bit_count);
        try std.testing.expectEqualSlices(u8, "\x7F", constant.asString());
    }
    {
        var constant = initInt(@as(i7, 0));
        try std.testing.expectEqual(@as(i64, -1), constant.bit_count);
        try std.testing.expectEqualSlices(u8, "\x00", constant.asString());
    }
    {
        var constant = initInt(@as(i7, 63));
        try std.testing.expectEqual(@as(i64, -7), constant.bit_count);
        try std.testing.expectEqualSlices(u8, "\x3F", constant.asString());
    }
    {
        var constant = initInt(@as(i7, -64));
        try std.testing.expectEqual(@as(i64, -7), constant.bit_count);
        try std.testing.expectEqualSlices(u8, "\xC0", constant.asString());
    }
}

// Returns error.Overrun if more bits are requested than are in the value.
// Returns error.Overflow if truncation of value to the requested bits changes it's numeric value, given the signedness requested.
pub fn initIntBits(value: anytype, bits: u63) !Constant {
    const T = @TypeOf(value);
    const int_info = @typeInfo(T).Int;
    const U = std.meta.Int(.unsigned, @bitSizeOf(T));
    const B = std.math.Log2Int(T);

    if (bits > @bitSizeOf(T)) return error.Overrun;

    const unsigned = @bitCast(U, value);
    switch (int_info.signedness) {
        .unsigned => {
            if (0 != (unsigned >> @intCast(B, bits))) {
                return error.Overflow;
            }
        },
        .signed => if (bits < @bitSizeOf(T)) {
            const sign = @truncate(u1, unsigned >> @intCast(B, bits - 1));
            var mask: U = @bitCast(U, @as(T, -1));
            mask <<= @intCast(B, bits);
            switch (sign) {
                0 => if ((unsigned & mask) != 0) return error.Overflow,
                1 => if ((unsigned & mask) != mask) return error.Overflow,
            }
        },
    }
    return init(std.mem.asBytes(&value), bits, int_info.signedness);
}

test "initIntBits" {
    try std.testing.expectEqualSlices(u8, "\x03\x00\x00\x00\x00\x00\x00\x00", (try Constant.initIntBits(@as(i64, 3), 64)).asString());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF", (try Constant.initIntBits(@as(i64, -1), 64)).asString());
    try std.testing.expectEqualSlices(u8, "\xFE\xFF\xFF\xFF\xFF\xFF\xFF\xFF", (try Constant.initIntBits(@as(i64, -2), 64)).asString());
}

pub fn initLiteral(allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), token: lex.Token, source: []const u8) !Constant {
    return switch (token.kind) {
        .id => initSymbolLiteral(allocator, storage, token.location(source)),
        .int_literal => initIntLiteral(allocator, storage, token.location(source)),
        .str_literal => initStringLiteral(allocator, storage, token.location(source)),
        else => error.InvalidToken,
    };
}

pub fn initSymbolLiteral(allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), location: []const u8) Constant {
    if (std.mem.indexOfScalar(u8, location, '\\')) |_| {
        storage.clearRetainingCapacity();
        for (location) |ch| {
            switch (ch) {
                '\\', 0...9, 11...' ', 127 => {},
                else => {
                    storage.append(allocator, ch) catch @panic("OOM");
                },
            }
        }
        return initString(storage.items);
    } else {
        return initString(location);
    }
}

pub fn initStringLiteral(allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), location: []const u8) !Constant {
    std.debug.assert(location[0] == '"');

    if (location.len >= 2 and location[location.len - 1] == '"' and std.mem.indexOfScalar(u8, location, '\\') == null) {
        return initString(location[1..location.len - 1]);
    }

    storage.clearRetainingCapacity();
    var escape: usize = 0;
    for (location[1..]) |ch| {
        switch (escape) {
            0 => switch (ch) {
                '"' => break,
                '\\' => escape = 1,
                else => storage.append(allocator, ch) catch @panic("OOM"),
            },
            1 => switch (ch) {
                '(' => escape = 2,
                't' => {
                    escape = 0;
                    storage.append(allocator, '\t') catch @panic("OOM");
                },
                'n' => {
                    escape = 0;
                    storage.append(allocator, '\n') catch @panic("OOM");
                },
                'r' => {
                    escape = 0;
                    storage.append(allocator, '\r') catch @panic("OOM");
                },
                'q', '"' => {
                    escape = 0;
                    storage.append(allocator, '"') catch @panic("OOM");
                },
                'b', '\\' => {
                    escape = 0;
                    storage.append(allocator, '\\') catch @panic("OOM");
                },
                else => return error.InvalidCharacter,
            },
            else => switch (ch) {
                ')' => escape = 0,
                else => {
                    // TODO parenthesized escape sequences
                },
            },
        }
    }

    return initString(storage.items);
}

pub fn initIntLiteral(allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), location: []const u8) !Constant {
    var remaining = location;
    std.debug.assert(remaining.len > 0);

    var radix: u8 = 10;
    if (remaining.len > 2 and remaining[0] == '0') {
        switch (remaining[1]) {
            'x', 'X' => {
                radix = 16;
                remaining = remaining[2..];
            },
            'b', 'B' => {
                radix = 2;
                remaining = remaining[2..];
            },
            'n', 'N' => {
                radix = 4;
                remaining = remaining[2..];
            },
            'o', 'O' => {
                radix = 8;
                remaining = remaining[2..];
            },
            'd', 'D' => {
                radix = 10;
                remaining = remaining[2..];
            },
            else => {},
        }
    }

    const bits_per_digit: u3 = switch (radix) {
        16 => 4,
        8 => 3,
        4 => 2,
        2 => 1,
        else => 0,
    };

    var maybe_bit_count: ?u63 = null;
    if (bits_per_digit > 0) {
        var bit_count: u63 = 0;
        for (remaining) |ch| {
            switch (ch) {
                '_' => continue,
                else => bit_count += bits_per_digit,
            }
        }
        maybe_bit_count = bit_count;
    }

    //trim leading zeroes and underscores
    for (remaining, 0..) |ch, i| {
        if (ch == '0' or ch == '_') continue;
        remaining = remaining[i..];
        break;
    } else {
        remaining = "0";
    }

    var storage_bit_count: u63 = 0;
    if (bits_per_digit > 0) {
        for (remaining) |ch| {
            switch (ch) {
                '_' => continue,
                else => storage_bit_count += bits_per_digit,
            }
        }
    }

    if (storage_bit_count <= 64) {
        var value = try std.fmt.parseUnsigned(u64, remaining, radix);

        if (radix == 10) {
            storage_bit_count = 65 - @clz(value);
        }

        const bit_count = maybe_bit_count orelse storage_bit_count;
        if (bit_count <= 64) {
            return initIntBits(value, bit_count) catch unreachable;
        } else {
            storage.resize(allocator, (bit_count + 7) / 8) catch @panic("OOM");
            std.mem.set(u8, storage.items[@sizeOf(u64)..], 0);
            std.mem.copy(u8, storage.items, std.mem.asBytes(&value));
            return init(storage.items.ptr, bit_count, .unsigned);
        }
    }

    std.debug.assert(radix != 10);
    const bit_count = maybe_bit_count.?;

    storage.clearRetainingCapacity();
    storage.ensureUnusedCapacity(allocator, (bit_count + 7) / 8) catch @panic("OOM");

    var accumulator: u64 = 0;
    var accumulator_bits: u5 = 0;
    var i = remaining.len;
    while (i > 0) {
        i -= 1;
        const ch = remaining[i];
        if (ch == '_') continue;
        const digit = try std.fmt.charToDigit(ch, radix);
        accumulator |= @as(u64, digit) << accumulator_bits;
        accumulator_bits += bits_per_digit;
        if (accumulator_bits >= 8) {
            storage.appendAssumeCapacity(@truncate(u8, accumulator));
            accumulator >>= 8;
            accumulator_bits -= 8;
        }
    }

    if (accumulator_bits > 0) {
        storage.appendAssumeCapacity(@truncate(u8, accumulator));
    }

    return init(storage.items.ptr, bit_count, .unsigned);
}

test "initIntLiteral" {
    var temp = std.ArrayListUnmanaged(u8) {};
    defer temp.deinit(std.testing.allocator);

    var constant = try initIntLiteral(std.testing.allocator, &temp, "0");
    try std.testing.expectEqual(@as(i64, 1), constant.bit_count);
    try std.testing.expectEqual(@as(i64, 0), try constant.asInt(i64));

    constant = try initIntLiteral(std.testing.allocator, &temp, "13");
    try std.testing.expectEqual(@as(i64, 5), constant.bit_count);
    try std.testing.expectEqual(@as(i64, 13), try constant.asInt(i64));

    constant = try initIntLiteral(std.testing.allocator, &temp, "0xFF");
    try std.testing.expectEqual(@as(i64, 8), constant.bit_count);
    try std.testing.expectEqualSlices(u8, "\xFF", constant.asString());
    try std.testing.expectEqual(@as(i64, 255), try constant.asInt(i64));

    constant = try initIntLiteral(std.testing.allocator, &temp, "0xDEADBEEFDEADBEEFDEADBEEFDEADBEEF");
    try std.testing.expectEqual(@as(i64, 128), constant.bit_count);
    try std.testing.expectEqualSlices(u8, "\xEF\xBE\xAD\xDE\xEF\xBE\xAD\xDE\xEF\xBE\xAD\xDE\xEF\xBE\xAD\xDE", constant.asString());

    constant = try initIntLiteral(std.testing.allocator, &temp, "0b1010101");
    try std.testing.expectEqual(@as(i64, 7), constant.bit_count);
    try std.testing.expectEqualSlices(u8, "\x55", constant.asString());

    constant = try initIntLiteral(std.testing.allocator, &temp, "0b001010101");
    try std.testing.expectEqual(@as(i64, 9), constant.bit_count);
    try std.testing.expectEqualSlices(u8, "\x55\x00", constant.asString());

    constant = try initIntLiteral(std.testing.allocator, &temp, "0n_0000_0000_0000_0000_0000_3210_3210_3210_3210_3210_3210_3210_3210");
    try std.testing.expectEqual(@as(i64, 104), constant.bit_count);
    try std.testing.expectEqualSlices(u8, "\xE4\xE4\xE4\xE4\xE4\xE4\xE4\xE4\x00\x00\x00\x00\x00", constant.asString());
}

pub fn deinit(self: Constant, allocator: std.mem.Allocator) void {
    if (self.bit_count > 64) {
        allocator.free(self.asString());
    }
}

pub fn getBitCount(self: Constant) u63 {
    return @intCast(u63, std.math.absCast(self.bit_count));
}

fn getFinalBitIndexInFinalByte(self: Constant) u3 {
    return @intCast(u3, (self.getBitCount() - 1) & 0x7);
}

pub fn getSignedness(self: Constant) std.builtin.Signedness {
    return if (self.bit_count < 0) .signed else .unsigned;
}

// Computes the minimum number of bits needed to represent the constant's numeric value; truncation smaller than this results in overflow.
pub fn countRequiredBits(self: Constant) u64 {
    const bits = self.getBitCount();
    const signedness = self.getSignedness();
    if (bits <= 64) {
        var value: u64 = undefined;
        std.mem.copy(u8, std.mem.asBytes(&value), &self.data.short);
        return switch (signedness) {
            .unsigned => 64 - @clz(value),
            .signed => 65 - @clz(if (value < 0) ~value else value),
        };
    }

    const buf = self.asString();
    var byte_index = buf.len - 1;
    var bit_index = self.getFinalBitIndexInFinalByte();
    var byte = buf[byte_index];
    var useless_bits: u64 = 0;

    var sign = @truncate(u1, byte >> bit_index);

    if (signedness == .unsigned) {
        if (sign == 0) {
            useless_bits += 1;
        } else {
            return bits;
        }
    }

    while (true) {
        if (bit_index == 0) {
            if (byte_index > 0) {
                byte_index -= 1;
                byte = buf[byte_index];
                bit_index = 7;
            } else {
                break;
            }
        } else {
            bit_index -= 1;
        }

        const bit = @truncate(u1, byte >> bit_index);
        if (bit != sign) break;

        useless_bits += 1;
    }

    return bits - useless_bits;
}

pub fn getSign(self: Constant) u1 {
    if (self.bit_count < 0) {
        const data = self.asString();
        return @truncate(u1, data[data.len - 1] >> 7);
    } else {
        return 0;
    }
}

pub fn getMSB(self: Constant) u1 {
    const data = self.asString();
    const leftover_bits = @intCast(u3, self.getBitCount() & 0x7);
    return getMSB1(data, leftover_bits);
}

fn getMSB1(data: []const u8, leftover_bits: u3) u1 {
    const shift = if (leftover_bits == 0) 7 else leftover_bits - 1;
    return @truncate(u1, data[data.len - 1] >> shift);
}

pub fn clone(self: Constant, allocator: std.mem.Allocator) Constant {
    if (self.bit_count > 64) {
        const buf = allocator.dupe(u8, self.asString()) catch @panic("OOM");
        return .{
            .bit_count = self.bit_count,
            .data = .{ .long = buf.ptr },
        };
    } else return self;
}

// returns error.Overflow if the constant's integer value would change due to truncation
pub fn cloneWithLength(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), result_bit_count: u63) !Constant {
    if (result_bit_count <= self.getBitCount()) {
        if (result_bit_count < self.countRequiredBits()) {
            return error.Overflow;
        }
        return init(self.asString().ptr, result_bit_count, self.getSignedness());
    }

    var small_buf: [8]u8 = undefined;
    var result = getResultBuffer(allocator, storage, &small_buf, result_bit_count);

    var iter = self.byteIterator(null);
    for (result) |*b| {
        b.* = iter.next();
    }

    return init(result.ptr, result_bit_count, self.getSignedness());
}

pub fn cloneWithSignedness(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), signedness: std.builtin.Signedness) Constant {
    const bits = self.getBitCount();

    var small_buf: [8]u8 = undefined;
    var result = getResultBuffer(allocator, storage, &small_buf, bits);

    var iter = self.byteIterator(signedness);
    for (result) |*b| {
        b.* = iter.next();
    }

    return init(result.ptr, bits, signedness);
}

test "cloneWithSignedness" {
    var temp = std.ArrayListUnmanaged(u8) {};
    defer temp.deinit(std.testing.allocator);
    var temp2 = std.ArrayListUnmanaged(u8) {};
    defer temp2.deinit(std.testing.allocator);

    {
        var constant = try initIntLiteral(std.testing.allocator, &temp, "255");
        var clone_constant = constant.cloneWithSignedness(std.testing.allocator, &temp2, .signed);
        try std.testing.expectEqual(@as(i64, -9), clone_constant.bit_count);
        try std.testing.expectEqual(@as(i64, 255), try clone_constant.asInt(i64));
    }
    {
        var constant = try initIntLiteral(std.testing.allocator, &temp, "0xFF");
        try std.testing.expectEqual(@as(i64, 8), constant.bit_count);
        try std.testing.expectEqual(@as(i64, 0xFF), try constant.asInt(i64));
        var clone_constant = constant.cloneWithSignedness(std.testing.allocator, &temp2, .signed);
        try std.testing.expectEqual(@as(i64, -8), clone_constant.bit_count);
        try std.testing.expectEqual(@as(i64, -1), try clone_constant.asInt(i64));
    }
}

pub fn truncate(self: Constant, result_bit_count: u63) Constant {
    if (result_bit_count > self.getBitCount()) {
        return self;
    }
    return init(self.asString().ptr, result_bit_count, self.getSignedness());
}

// returns error.Truncation if the result bit count is smaller than the current bit count.
// Note the signedness of the result is the same as the signedness of `self`; regardless of `ext`
pub fn extend(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), result_bit_count: u63, ext: std.builtin.Signedness) !Constant {
    if (result_bit_count < self.getBitCount()) {
        return error.Truncation;
    }

    var small_buf: [8]u8 = undefined;
    var result = getResultBuffer(allocator, storage, &small_buf, result_bit_count);

    var iter = self.byteIterator(ext);
    for (result) |*b| {
        b.* = iter.next();
    }

    return init(result.ptr, result_bit_count, self.getSignedness());
}

pub fn byteIterator(self: *const Constant, extension: ?std.builtin.Signedness) ByteIterator {
    var str = self.asString();
    var iter = ByteIterator{
        .remaining = str,
        .final_byte = str[str.len - 1],
        .ext_byte = 0,
    };

    if (extension) |ext| {
        var mask: u8 = 0xFF;
        const leftover_bits = @intCast(u3, self.getBitCount() & 0x7);
        if (leftover_bits == 0) {
            mask = 0;
        } else {
            mask <<= leftover_bits;
        }
        switch (ext) {
            .unsigned => {
                iter.final_byte &= ~mask;
            },
            .signed => switch (getMSB1(str, leftover_bits)) {
                0 => {
                    iter.final_byte &= ~mask;
                },
                1 => {
                    iter.final_byte |= mask;
                    iter.ext_byte = 0xFF;
                },
            },
        }
    } else {
        iter.ext_byte = self.getSign() * @as(u8, 0xFF);
    }

    return iter;
}

const ByteIterator = struct {
    remaining: []const u8,
    final_byte: u8,
    ext_byte: u8,

    pub fn next(self: *ByteIterator) u8 {
        const remaining_bytes = self.remaining.len;
        if (remaining_bytes > 1) {
            const b = self.remaining[0];
            self.remaining = self.remaining[1..];
            return b;
        } else if (remaining_bytes == 1) {
            self.remaining.len = 0;
            return self.final_byte;
        } else {
            return self.ext_byte;
        }
    }

    pub fn skip(self: *ByteIterator, bytes: usize) void {
        if (bytes >= self.remaining.len) {
            self.remaining.len = 0;
        } else {
            self.remaining.len -= bytes;
        }
    }
};

pub fn asString(self: *const Constant) []const u8 {
    const bits = self.getBitCount();
    var slice: []const u8 = undefined;
    slice.ptr = if (bits <= 64) &self.data.short else self.data.long;
    slice.len = (bits + 7) / 8;
    return slice;
}

test "asString" {
    try std.testing.expectEqualSlices(u8, "\x00", Constant.initInt(@as(i64, 0)).asString());
    try std.testing.expectEqualSlices(u8, "\x00", Constant.initInt(@as(u64, 0)).asString());
    try std.testing.expectEqualSlices(u8, "\xD2\x04", Constant.initInt(@as(i64, 1234)).asString());
    try std.testing.expectEqualSlices(u8, "\x2E\xFB", Constant.initInt(@as(i64, -1234)).asString());
    try std.testing.expectEqualSlices(u8, "abcdefghijklmnopqrstuvwxyz", Constant.initString("abcdefghijklmnopqrstuvwxyz").asString());
    try std.testing.expectEqualSlices(u8, "ab", (try Constant.initStringBits("abcdefghijklmnopqrstuvwxyz", 16, .unsigned)).asString());
}

pub fn asInt(self: Constant, comptime T: type) !T {
    const buf = self.asString();
    if (buf.len == 0) return 0;

    const int_info = @typeInfo(T).Int;
    const T8 = std.meta.Int(int_info.signedness, @sizeOf(T) * 8);

    var value: T8 = undefined;
    var iter = self.byteIterator(null);
    for (std.mem.asBytes(&value)) |*b| {
        b.* = iter.next();
    }
    const ext: u8 = if (value >= 0) 0 else 0xFF;
    while (iter.remaining.len > 0) {
        if (iter.next() != ext) return error.Overflow;
    }

    return std.math.cast(T, value) orelse error.Overflow;
}
pub fn asIntNegated(self: Constant, comptime T: type) !T {
    return std.math.negateCast(try self.asInt(T));
}

test "asInt" {
    try std.testing.expectEqual(@as(i64, 0), try Constant.initInt(@as(i7, 0)).asInt(i64));
    try std.testing.expectEqual(@as(i64, 0), try (try Constant.initIntBits(@as(i64, 0), 64)).asInt(i64));
    try std.testing.expectEqual(@as(i64, 1234), try Constant.initInt(@as(u16, 1234)).asInt(i64));
    try std.testing.expectEqual(@as(i64, -1), try Constant.initInt(@as(i1, -1)).asInt(i64));
    try std.testing.expectEqual(@as(i64, -1), try (try Constant.initIntBits(@as(i64, -1), 64)).asInt(i64));
    try std.testing.expectEqual(@as(i64, -1234), try Constant.initInt(@as(i16, -1234)).asInt(i64));
    try std.testing.expectEqual(@as(anyerror!i64, error.Overflow), Constant.initString("abcdefghijklmnopqrstuvwxyz").asInt(i64));
}

pub fn concat(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), other: Constant) Constant {
    const self_bits = self.getBitCount();
    if (self_bits == 0) {
        return other;
    }

    const other_bits = other.getBitCount();
    if (other_bits == 0) {
        return self;
    }

    const combined_bits = self_bits + other_bits;

    var small_buf: [8]u8 = undefined;
    var result = getResultBuffer(allocator, storage, &small_buf, combined_bits);
    var i: usize = 0;

    const self_iter = self.byteIterator(.unsigned);
    const self_leftover_bits = @intCast(u3, self_bits & 0x7);
    i = appendIter(result, i, 0, self_leftover_bits, self_iter);

    const other_iter = other.byteIterator(null);
    const other_leftover_bits = @intCast(u3, other_bits & 0x7);
    i = appendIter(result, i, self_leftover_bits, other_leftover_bits, other_iter);

    return init(result.ptr, combined_bits, other.getSignedness());
}

test "concat" {
    var alloc = std.testing.allocator;
    var temp = std.ArrayListUnmanaged(u8) {};
    defer temp.deinit(alloc);

    const abc = Constant.initString("abc");
    const def = Constant.initString("def");

    // 01100001 00111011 01110011 01101100 01100100 01101011 01100110 01101010 00100000 01100001 00111011 01110011 01101100 01100100 01101011 01100110 01101010
    const long = Constant.initString("a;sldkfj a;sldkfj");

    const bits3 = try Constant.initIntBits(@as(i64, -1), 3);
    const hexFFF = try Constant.initIntBits(@as(i64, -1), 12);
    const hex000 = try Constant.initIntBits(@as(i64, 0), 12);
    const hexA51 = try Constant.initIntBits(@as(u64, 0xA51), 12); // 0b1010_0101_0001
    const bits7 = try Constant.initIntBits(@as(i64, -1), 7);
    const bits23 = try Constant.initIntBits(@as(i64, -1), 23);

    try std.testing.expectEqualSlices(u8, "abcdef", (abc.concat(alloc, &temp, def)).asString());
    try std.testing.expectEqualSlices(u8, "defabc", (def.concat(alloc, &temp, abc)).asString());
    try std.testing.expectEqualSlices(u8, "abca;sldkfj a;sldkfj", abc.concat(alloc, &temp, long).asString());
    try std.testing.expectEqualSlices(u8, "\x8F\x52", (bits3.concat(alloc, &temp, hexA51)).asString()); // 0b101_0010_1000_1111
    try std.testing.expectEqualSlices(u8, "\x00\xF0\xFF", (hex000.concat(alloc, &temp, hexFFF)).asString());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xFF", (bits7.concat(alloc, &temp, bits23)).asString());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xFF\xFF\xFF", (bits23.concat(alloc, &temp, bits23)).asString());

    // 01100001 00111011 01110011 01101100 01100100 01101011 01100110 01101010 00100000 01100001 00111011 01110011 01101100 01100100 01101011 01100110 01101010
    // 01101010 01100110 01101011 01100100 01101100 01110011 00111011 01100001 00100000 01101010 01100110 01101011 01100100 01101100 01110011 00111011 01100001 111
    // 011_0101_0011_0011_0011_0101_1011_0010_0011_0110_0011_1001_1001_1101_1011_0000_1001_0000_0011_0101_0011_0011_0011_0101_1011_0010_0011_0110_0011_1001_1001_1101_1011_0000_1111
    // 0x3  5    3    3    3    5    B    2    3    6    3    9    9    D    B    0    9    0    3    5    3    3    3    5    B    2    3    6    3    9    9    D    B    0    F
    // 0x3_53_33_5B_23_63_99_DB_09_03_53_33_5B_23_63_99_DB_0F
    // 011010
    try std.testing.expectEqualSlices(u8, "\x0F\xDB\x99\x63\x23\x5B\x33\x53\x03\x09\xDB\x99\x63\x23\x5B\x33\x53\x03", bits3.concat(alloc, &temp, long).asString());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xB0\x9D\x39\x36\xB2\x35\x33\x35\x90\xB0\x9D\x39\x36\xB2\x35\x33\x35", bits23.concat(alloc, &temp, long).asString());
}

pub fn repeat(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), times: i64) Constant {
    if (times <= 0) return init("", 0, .unsigned);
    if (times == 1) return self;

    const times_u63 = @intCast(u63, times);

    const self_bits = self.getBitCount();
    if (self_bits == 0) return self;

    const combined_bits = self_bits * times_u63;

    var small_buf: [8]u8 = undefined;
    var result = getResultBuffer(allocator, storage, &small_buf, combined_bits);
    var i: usize = 0;

    const unsigned_iter = self.byteIterator(.unsigned);
    const self_leftover_bits = @intCast(u3, self_bits & 7);
    var leftover_bits: u3 = 0;
    for (0 .. times_u63 - 1) |_| {
        i = appendIter(result, i, leftover_bits, self_leftover_bits, unsigned_iter);
        leftover_bits = @intCast(u3, (leftover_bits + self_bits) & 7);
    }

    var iter = self.byteIterator(null);
    i = appendIter(result, i, leftover_bits, self_leftover_bits, iter);

    return init(result.ptr, combined_bits, self.getSignedness());
}

test "repeat" {
    var alloc = std.testing.allocator;
    var temp = std.ArrayListUnmanaged(u8) {};
    defer temp.deinit(alloc);

    const abc = Constant.initString("abc");
    const de = Constant.initString("de");
    const bits1 = try Constant.initIntBits(@as(i64, -1), 1);
    const hexFFF = try Constant.initIntBits(@as(u64, 0xFFF), 12);

    try std.testing.expectEqualSlices(u8, "", abc.repeat(alloc, &temp, 0).asString());
    try std.testing.expectEqualSlices(u8, "abc", abc.repeat(alloc, &temp, 1).asString());
    try std.testing.expectEqualSlices(u8, "abcabc", abc.repeat(alloc, &temp, 2).asString());
    try std.testing.expectEqualSlices(u8, "dededede", de.repeat(alloc, &temp, 4).asString());
    try std.testing.expectEqualSlices(u8, "dededededededededede", de.repeat(alloc, &temp, 10).asString());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF", hexFFF.repeat(alloc, &temp, 2).asString());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xFF\x0F", hexFFF.repeat(alloc, &temp, 3).asString());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF", bits1.repeat(alloc, &temp, 72).asString());
}

fn getResultBuffer(allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), small_buf: *[8]u8, bits: u64) []u8 {
    var bytes = (bits + 7) / 8;
    var result: []u8 = if (bits <= 64) small_buf else s: {
        storage.clearRetainingCapacity();
        storage.ensureTotalCapacity(allocator, bytes) catch @panic("OOM");
        break :s storage.items;
    };
    result.len = bytes;
    return result;
}

fn appendIter(result: []u8, starting_byte: usize, leftover_bits: u3, self_leftover_bits: u3, self_iter: ByteIterator) usize {
    var i = starting_byte;
    var iter = self_iter;
    if (leftover_bits == 0) {
        while (iter.remaining.len > 0) {
            result[i] = iter.next();
            i += 1;
        }
    } else {
        const shift_amount = @intCast(u3, @as(u4, 8) - leftover_bits);

        while (iter.remaining.len > 1) {
            const b = iter.next();
            result[i - 1] |= encodeShiftedBytePrev(b, shift_amount);
            result[i] = encodeShiftedByteNext(b, shift_amount);
            i += 1;
        }

        const b = iter.next();
        result[i - 1] |= encodeShiftedBytePrev(b, shift_amount);
        if (self_leftover_bits == 0 or self_leftover_bits > shift_amount) {
            result[i] = encodeShiftedByteNext(b, shift_amount);
            result[i] |= encodeShiftedBytePrev(iter.next(), shift_amount);
        }
    }
    return i;
}
fn encodeShiftedBytePrev(b: u8, shift_amount: u3) u8 {
    return @truncate(u8, (@as(u16, b) << 8) >> shift_amount);
}
fn encodeShiftedByteNext(b: u8, shift_amount: u3) u8 {
    return b >> shift_amount;
}

pub const BitwiseOp = enum {
    bitwise_or,
    bitwise_xor,
    bitwise_and,
};
pub fn bitwise(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), other: Constant, op: BitwiseOp) Constant {
    const self_bits = self.getBitCount();
    const other_bits = other.getBitCount();

    var result_bits: u63 = undefined;
    var result_signedness: std.builtin.Signedness = undefined;
    if (self_bits >= other_bits) {
        result_bits = self_bits;
        result_signedness = self.getSignedness();
    } else {
        result_bits = other_bits;
        result_signedness = other.getSignedness();
    }

    var small_buf: [8]u8 = undefined;
    var result = getResultBuffer(allocator, storage, &small_buf, result_bits);

    var self_iter = self.byteIterator(null);
    var other_iter = other.byteIterator(null);
    switch (op) {
        .bitwise_or =>
            for (result) |*b| {
                b.* = self_iter.next() | other_iter.next();
            },
        .bitwise_xor =>
            for (result) |*b| {
                b.* = self_iter.next() ^ other_iter.next();
            },
        .bitwise_and =>
            for (result) |*b| {
                b.* = self_iter.next() & other_iter.next();
            },
    }

    return init(result.ptr, result_bits, result_signedness);
}

pub fn complement(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8)) Constant {
    const result_bits = self.getBitCount();

    var small_buf: [8]u8 = undefined;
    var result = getResultBuffer(allocator, storage, &small_buf, result_bits);

    var self_iter = self.byteIterator(null);
    for (result) |*b| {
        b.* = ~self_iter.next();
    }

    return init(result.ptr, result_bits, self.getSignedness());
}

pub fn intern(self: *const Constant, arena: std.mem.Allocator, gpa: std.mem.Allocator, pool: *InternPool) *const Constant {
    var result = pool.getOrPut(gpa, self) catch @panic("OOM");
    errdefer _ = pool.remove(self);
    if (result.found_existing) {
        return result.key_ptr.*;
    }

    var constant = arena.create(Constant) catch @panic("OOM");
    errdefer arena.destroy(constant);
    constant.* = self.clone(arena);

    result.key_ptr.* = constant;
    return constant;
}
