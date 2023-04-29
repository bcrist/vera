const std = @import("std");
const lex = @import("lex.zig");

const Constant = @This();

bit_count: u64,
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

fn init(value: [*]const u8, bits: u64) Constant {
    if (bits <= 64) {
        var data = [_]u8{0} ** 8;
        var i: usize = 0;
        var bits_remaining = bits;
        while (bits_remaining >= 8) {
            data[i] = value[i];
            i += 1;
            bits_remaining -= 8;
        }
        if (bits_remaining > 0) {
            const mask = @intCast(u8, (@as(u16, 1) << @intCast(u4, bits_remaining)) - 1);
            data[i] = value[i] & mask;
        }
        return .{
            .bit_count = bits,
            .data = .{ .short = data },
        };
    } else return .{
        .bit_count = bits,
        .data = .{ .long = value },
    };
}

pub fn initString(value: []const u8, maybe_bits: ?u64) Constant {
    const bits = maybe_bits orelse value.len * 8;
    std.debug.assert(bits <= value.len * 8);
    return init(value.ptr, bits);
}

pub fn initInt(value: i64, maybe_bits: ?u64) Constant {
    var unsigned = @bitCast(u64, value);
    const storage_bits = 65 - @clz(if (value < 0) ~unsigned else unsigned);
    const bits = maybe_bits orelse storage_bits;
    std.debug.assert(bits >= storage_bits);
    std.debug.assert(bits <= 64);
    return init(std.mem.asBytes(&value), bits);
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
        return initString(storage.items, null);
    } else {
        return initString(location, null);
    }
}

pub fn initStringLiteral(allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), location: []const u8) !Constant {
    std.debug.assert(location[0] == '"');

    if (location.len >= 2 and location[location.len - 1] == '"' and std.mem.indexOfScalar(u8, location, '\\') == null) {
        return initString(location[1..location.len - 1], null);
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
                    // TODO
                },
            },
        }
    }

    return initString(storage.items, null);
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

    var maybe_bit_count: ?u64 = null;
    if (bits_per_digit > 0) {
        var bit_count: u64 = 0;
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

    var storage_bit_count: u64 = 0;
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
            return init(std.mem.asBytes(&value), bit_count);
        } else {
            storage.resize(allocator, (bit_count + 7) / 8) catch @panic("OOM");
            std.mem.set(u8, storage.items[@sizeOf(u64)..], 0);
            std.mem.copy(u8, storage.items, std.mem.asBytes(&value));
            return init(storage.items.ptr, bit_count);
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

    return init(storage.items.ptr, bit_count);
}

test "initIntLiteral" {
    var temp = std.ArrayListUnmanaged(u8) {};
    defer temp.deinit(std.testing.allocator);

    var constant = try initIntLiteral(std.testing.allocator, &temp, "0");
    try std.testing.expectEqual(@as(u64, 1), constant.bit_count);
    try std.testing.expectEqual(@as(i64, 0), try constant.asInt());

    constant = try initIntLiteral(std.testing.allocator, &temp, "13");
    try std.testing.expectEqual(@as(u64, 5), constant.bit_count);
    try std.testing.expectEqual(@as(i64, 13), try constant.asInt());

    constant = try initIntLiteral(std.testing.allocator, &temp, "0xFF");
    try std.testing.expectEqual(@as(u64, 8), constant.bit_count);
    try std.testing.expectEqualSlices(u8, "\xFF", constant.asString());
    try std.testing.expectEqual(@as(i64, -1), try constant.asInt());

    constant = try initIntLiteral(std.testing.allocator, &temp, "0xDEADBEEFDEADBEEFDEADBEEFDEADBEEF");
    try std.testing.expectEqual(@as(u64, 128), constant.bit_count);
    try std.testing.expectEqualSlices(u8, "\xEF\xBE\xAD\xDE\xEF\xBE\xAD\xDE\xEF\xBE\xAD\xDE\xEF\xBE\xAD\xDE", constant.asString());

    constant = try initIntLiteral(std.testing.allocator, &temp, "0b1010101");
    try std.testing.expectEqual(@as(u64, 7), constant.bit_count);
    try std.testing.expectEqualSlices(u8, "\x55", constant.asString());

    constant = try initIntLiteral(std.testing.allocator, &temp, "0b001010101");
    try std.testing.expectEqual(@as(u64, 9), constant.bit_count);
    try std.testing.expectEqualSlices(u8, "\x55\x00", constant.asString());

    constant = try initIntLiteral(std.testing.allocator, &temp, "0n_0000_0000_0000_0000_0000_3210_3210_3210_3210_3210_3210_3210_3210");
    try std.testing.expectEqual(@as(u64, 104), constant.bit_count);
    try std.testing.expectEqualSlices(u8, "\xE4\xE4\xE4\xE4\xE4\xE4\xE4\xE4\x00\x00\x00\x00\x00", constant.asString());
}

pub fn deinit(self: Constant, allocator: std.mem.Allocator) void {
    if (self.bit_count > 64) {
        allocator.free(self.asString());
    }
}

pub fn clone(self: Constant, allocator: std.mem.Allocator) Constant {
    if (self.bit_count > 64) {
        const buf = allocator.dupe(u8, self.asString()) catch @panic("OOM");
        return init(buf.ptr, self.bit_count);
    } else return self;
}

pub fn cloneWithLength(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), bit_count: u64, signedness: std.builtin.Signedness) Constant {
    if (bit_count <= self.bit_count) {
        return init(self.asString().ptr, bit_count);
    }

    storage.resize(allocator, (bit_count + 7) / 8) catch @panic("OOM");
    switch (signedness) {
        .signed => {
            var iter = SxIterator.init(&self);
            for (storage.items) |*b| {
                b.* = iter.next();
            }
        },
        .unsigned => {
            var iter = ZxIterator.init(&self);
            for (storage.items) |*b| {
                b.* = iter.next();
            }
        },
    }
    truncateFinalByte(storage.items, bit_count);
    return init(storage.items.ptr, bit_count);
}

const ZxIterator = struct {
    remaining: []const u8,

    pub fn init(constant: *const Constant) ZxIterator {
        return .{ .remaining = constant.asString(), };
    }

    pub fn next(self: *ZxIterator) u8 {
        const remaining_bytes = self.remaining.len;
        if (remaining_bytes > 0) {
            const b = self.remaining[0];
            self.remaining = self.remaining[1..];
            return b;
        } else {
            return 0;
        }
    }
};

const SxIterator = struct {
    remaining: []const u8,
    final_byte: u8,
    ext_byte: u8,

    pub fn init(constant: *const Constant) SxIterator {
        var str = constant.asString();
        var self = SxIterator{
            .remaining = str,
            .final_byte = str[str.len - 1],
            .ext_byte = 0,
        };
        const final_byte_bits = @intCast(u3, constant.bit_count & 0x7);
        if (final_byte_bits == 0) {
            if (0 != @truncate(u1, self.final_byte >> 7)) {
                self.ext_byte = 0xFF;
            }
        } else if (0 != @truncate(u1, self.final_byte >> (final_byte_bits - 1))) {
            var mask = ~@as(u8, 0);
            mask <<= final_byte_bits;
            self.final_byte |= mask;
            self.ext_byte = 0xFF;
        }
        return self;
    }

    pub fn next(self: *SxIterator) u8 {
        const remaining_bytes = self.remaining.len;
        if (remaining_bytes > 0) {
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
};

pub fn asString(self: *const Constant) []const u8 {
    var slice: []const u8 = undefined;
    slice.ptr = if (self.bit_count <= 64) &self.data.short else self.data.long;
    slice.len = (self.bit_count + 7) / 8;
    return slice;
}

test "asString" {
    try std.testing.expectEqualSlices(u8, "\x00", Constant.initInt(0, null).asString());
    try std.testing.expectEqualSlices(u8, "\xD2\x04", Constant.initInt(1234, null).asString());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF", Constant.initInt(-1, 64).asString());
    try std.testing.expectEqualSlices(u8, "\x2E\x0B", Constant.initInt(-1234, null).asString());
    try std.testing.expectEqualSlices(u8, "abcdefghijklmnopqrstuvwxyz", Constant.initString("abcdefghijklmnopqrstuvwxyz", null).asString());
    try std.testing.expectEqualSlices(u8, "ab", Constant.initString("abcdefghijklmnopqrstuvwxyz", 16).asString());
}

pub fn asInt(self: Constant) !i64 {
    const buf = self.asString();
    if (buf.len == 0) return 0;

    var value: i64 = @bitCast(i8, buf[buf.len - 1]);

    const buf_bits = buf.len * 8;
    const last_byte_bits = @intCast(u4, self.bit_count - (buf_bits - 8));
    if (last_byte_bits < 8) {
        const sign = @truncate(u1, @bitCast(u64, value >> (last_byte_bits - 1)));
        if (sign != 0) {
            var sx: i64 = -1;
            sx <<= last_byte_bits;
            value |= sx;
        }
    }

    var i = buf.len - 1;
    while (i > 0) {
        i -= 1;
        var b = buf[i];
        value = try std.math.mul(i64, value, 256);
        value |= b;
    }

    return value;
}

test "asInt" {
    try std.testing.expectEqual(@as(u64, 1), Constant.initInt(0, null).bit_count);
    try std.testing.expectEqual(@as(u64, 2), Constant.initInt(1, null).bit_count);
    try std.testing.expectEqual(@as(u64, 1), Constant.initInt(-1, null).bit_count);
    try std.testing.expectEqual(@as(i64, 0), try Constant.initInt(0, null).asInt());
    try std.testing.expectEqual(@as(i64, 0), try Constant.initInt(0, 64).asInt());
    try std.testing.expectEqual(@as(i64, 1234), try Constant.initInt(1234, null).asInt());
    try std.testing.expectEqual(@as(i64, -1), try Constant.initInt(-1, null).asInt());
    try std.testing.expectEqualSlices(u8, "\x01", Constant.initInt(-1, null).asString());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF", Constant.initInt(-1, 64).asString());
    try std.testing.expectEqual(@as(i64, -1), try Constant.initInt(-1, 64).asInt());
    try std.testing.expectEqual(@as(i64, -1234), try Constant.initInt(-1234, null).asInt());
    try std.testing.expectEqual(@as(anyerror!i64, error.Overflow), Constant.initString("abcdefghijklmnopqrstuvwxyz", null).asInt());
}

pub fn concat(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), other: Constant) Constant {
    const combined_bit_count = self.bit_count + other.bit_count;
    if (combined_bit_count <= 64) {
        var value: i64 = undefined;
        std.mem.copy(u8, std.mem.asBytes(&value), &self.data.short);
        value |= (other.asInt() catch unreachable) << @intCast(u6, self.bit_count);
        return init(std.mem.asBytes(&value), combined_bit_count);
    }

    storage.clearRetainingCapacity();
    storage.ensureUnusedCapacity(allocator, (combined_bit_count + 7) / 8) catch @panic("OOM");
    storage.appendSliceAssumeCapacity(self.asString());

    const leftover_bits = @intCast(u3, self.bit_count & 0x7);
    if (leftover_bits == 0) {
        storage.appendSliceAssumeCapacity(other.asString());
    } else {
        const other_data = other.asString();

        const shift_amount = @intCast(u3, @as(u4, 8) - leftover_bits);
        for (other_data[0 .. other_data.len - 1]) |b| {
            storage.items[storage.items.len - 1] |= @truncate(u8, (@as(u16, b) << 8) >> shift_amount);
            storage.appendAssumeCapacity(b >> shift_amount);
        }

        const last_byte = other_data[other_data.len - 1];
        storage.items[storage.items.len - 1] |= @truncate(u8, (@as(u16, last_byte) << 8) >> shift_amount);
        const other_leftover_bits = @intCast(u3, other.bit_count & 7);
        if (other_leftover_bits == 0 or other_leftover_bits > shift_amount) {
            storage.appendAssumeCapacity(last_byte >> shift_amount);
        }
    }

    return init(storage.items.ptr, combined_bit_count);
}

test "concat" {
    var alloc = std.testing.allocator;
    var temp = std.ArrayListUnmanaged(u8) {};
    defer temp.deinit(alloc);

    const abc = Constant.initString("abc", null);
    const def = Constant.initString("def", null);
    const long = Constant.initString("a;sldkfj a;sldkfj", null);
    const bits3 = Constant.initInt(-1, 3);
    const hexFFF = Constant.initInt(-1, 12);
    const hex000 = Constant.initInt(0, 12);
    const bits7 = Constant.initInt(-1, 7);
    const bits23 = Constant.initInt(-1, 23);

    try std.testing.expectEqualSlices(u8, "abcdef", (abc.concat(alloc, &temp, def)).asString());
    try std.testing.expectEqualSlices(u8, "defabc", (def.concat(alloc, &temp, abc)).asString());
    try std.testing.expectEqualSlices(u8, "abca;sldkfj a;sldkfj", abc.concat(alloc, &temp, long).asString());
    try std.testing.expectEqualSlices(u8, "\x0F\x13\x1B\x03", (bits3.concat(alloc, &temp, abc)).asString());
    try std.testing.expectEqualSlices(u8, "\x00\xF0\xFF", (hex000.concat(alloc, &temp, hexFFF)).asString());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\x3F", (bits7.concat(alloc, &temp, bits23)).asString());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xFF\xFF\x3F", (bits23.concat(alloc, &temp, bits23)).asString());
    try std.testing.expectEqualSlices(u8, "\x0F\xDB\x99\x63\x23\x5B\x33\x53\x03\x09\xDB\x99\x63\x23\x5B\x33\x53\x03", bits3.concat(alloc, &temp, long).asString());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xB0\x9D\x39\x36\xB2\x35\x33\x35\x90\xB0\x9D\x39\x36\xB2\x35\x33\x35", bits23.concat(alloc, &temp, long).asString());
}

pub fn repeat(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), times: i64) Constant {
    if (times <= 0) return init("", 0);
    if (times == 1) return self;

    const times_u64 = @intCast(u64, times);

    const combined_bit_count = self.bit_count * times_u64;
    if (combined_bit_count <= 64) {
        var self_value: u64 = undefined;
        std.mem.copy(u8, std.mem.asBytes(&self_value), &self.data.short);

        const shift = @intCast(u6, self.bit_count);
        var remaining = times_u64 - 1;
        var value = self_value;
        while (remaining > 0) : (remaining -= 1) {
            value <<= shift;
            value |= self_value;
        }

        return init(std.mem.asBytes(&value), combined_bit_count);
    }

    const self_data = self.asString();
    const self_last_byte = self_data[self_data.len - 1];
    const self_data_except_last_byte = self_data[0 .. self_data.len - 1];
    const self_leftover_bits = @intCast(u3, self.bit_count & 0x7);

    storage.clearRetainingCapacity();
    storage.ensureUnusedCapacity(allocator, (combined_bit_count + 7) / 8) catch @panic("OOM");
    storage.appendSliceAssumeCapacity(self_data);

    var leftover_bits = self_leftover_bits;
    if (leftover_bits == 0) {
        for (1..times_u64) |_| {
            storage.appendSliceAssumeCapacity(self_data);
        }
    } else {
        for (1 .. times_u64 - 1) |_| {
            if (leftover_bits == 0) {
                storage.appendSliceAssumeCapacity(self_data);
            } else {
                const shift_amount = @intCast(u3, @as(u4, 8) - leftover_bits);
                for (self_data_except_last_byte) |b| {
                    storage.items[storage.items.len - 1] |= @truncate(u8, (@as(u16, b) << 8) >> shift_amount);
                    storage.appendAssumeCapacity(b >> shift_amount);
                }

                storage.items[storage.items.len - 1] |= @truncate(u8, (@as(u16, self_last_byte) << 8) >> shift_amount);
                if (self_leftover_bits > shift_amount) {
                    storage.appendAssumeCapacity(self_last_byte >> shift_amount);
                }
            }
            leftover_bits = @intCast(u3, (leftover_bits + self.bit_count) & 7);
        }

        const shift_amount = @intCast(u3, @as(u4, 8) - leftover_bits);
        for (self_data_except_last_byte) |b| {
            storage.items[storage.items.len - 1] |= @truncate(u8, (@as(u16, b) << 8) >> shift_amount);
            storage.appendAssumeCapacity(b >> shift_amount);
        }

        storage.items[storage.items.len - 1] |= @truncate(u8, (@as(u16, self_last_byte) << 8) >> shift_amount);
        if (self_leftover_bits > shift_amount) {
            storage.appendAssumeCapacity(self_last_byte >> shift_amount);
        }
    }

    return init(storage.items.ptr, combined_bit_count);
}

test "repeat" {
    var alloc = std.testing.allocator;
    var temp = std.ArrayListUnmanaged(u8) {};
    defer temp.deinit(alloc);

    const abc = Constant.initString("abc", null);
    const de = Constant.initString("de", null);
    const bits1 = Constant.initInt(-1, 1);
    const hexFFF = Constant.initInt(-1, 12);

    try std.testing.expectEqualSlices(u8, "", abc.repeat(alloc, &temp, 0).asString());
    try std.testing.expectEqualSlices(u8, "abc", abc.repeat(alloc, &temp, 1).asString());
    try std.testing.expectEqualSlices(u8, "abcabc", abc.repeat(alloc, &temp, 2).asString());
    try std.testing.expectEqualSlices(u8, "dededede", de.repeat(alloc, &temp, 4).asString());
    try std.testing.expectEqualSlices(u8, "dededededededededede", de.repeat(alloc, &temp, 10).asString());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF", hexFFF.repeat(alloc, &temp, 2).asString());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xFF\x0F", hexFFF.repeat(alloc, &temp, 3).asString());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF", bits1.repeat(alloc, &temp, 72).asString());
}

fn sxShort(self: Constant) u64 {
    var result: u64 = undefined;
    std.mem.copy(u8, std.mem.asBytes(&result), &self.data.short);
    if (0 != @truncate(u1, result >> @intCast(u6, self.bit_count - 1))) {
        var mask = ~@as(u64, 0);
        mask <<= @intCast(u6, self.bit_count);
        result |= mask;
    }
    return result;
}

fn truncate(short: u64, bits: u64) u64 {
    return if (bits < 64) short & ((@as(u64, 1) << @intCast(u6, bits)) - 1) else short;
}

fn truncateFinalByte(storage: []u8, bit_count: u64) void {
    var final_byte_bits = @intCast(u3, bit_count & 0x7);
    if (final_byte_bits != 0) {
        storage[storage.len - 1] &= ~(@as(u8, 0xFF) << final_byte_bits);
    }
}

pub fn bitwiseOr(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), other: Constant) Constant {
    const result_bit_count = @max(self.bit_count, other.bit_count);

    if (result_bit_count <= 64) {
        const result = truncate(sxShort(self) | sxShort(other), result_bit_count);
        return init(std.mem.asBytes(&result), result_bit_count);
    }

    storage.resize(allocator, (result_bit_count + 7) / 8) catch @panic("OOM");

    var self_iter = SxIterator.init(&self);
    var other_iter = SxIterator.init(&other);
    for (storage.items) |*b| {
        b.* = self_iter.next() | other_iter.next();
    }

    truncateFinalByte(storage.items, result_bit_count);

    return init(storage.items.ptr, result_bit_count);
}

pub fn bitwiseXor(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), other: Constant) Constant {
    const result_bit_count = @max(self.bit_count, other.bit_count);

    if (result_bit_count <= 64) {
        const result = truncate(sxShort(self) ^ sxShort(other), result_bit_count);
        return init(std.mem.asBytes(&result), result_bit_count);
    }

    storage.resize(allocator, (result_bit_count + 7) / 8) catch @panic("OOM");

    var self_iter = SxIterator.init(&self);
    var other_iter = SxIterator.init(&other);
    for (storage.items) |*b| {
        b.* = self_iter.next() ^ other_iter.next();
    }

    truncateFinalByte(storage.items, result_bit_count);

    return init(storage.items.ptr, result_bit_count);
}

pub fn bitwiseAnd(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), other: Constant) Constant {
    const result_bit_count = @max(self.bit_count, other.bit_count);

    if (result_bit_count <= 64) {
        const result = truncate(sxShort(self) & sxShort(other), result_bit_count);
        return init(std.mem.asBytes(&result), result_bit_count);
    }

    storage.resize(allocator, (result_bit_count + 7) / 8) catch @panic("OOM");

    var self_iter = SxIterator.init(&self);
    var other_iter = SxIterator.init(&other);
    for (storage.items) |*b| {
        b.* = self_iter.next() & other_iter.next();
    }

    truncateFinalByte(storage.items, result_bit_count);

    return init(storage.items.ptr, result_bit_count);
}

pub fn complement(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8)) Constant {
    const bit_count = self.bit_count;

    if (bit_count <= 64) {
        const result = truncate(~sxShort(self), bit_count);
        return init(std.mem.asBytes(&result), bit_count);
    }

    const data = self.asString();

    storage.resize(allocator, data.len) catch @panic("OOM");

    for (storage.items, data) |*s, d| {
        s.* = ~d;
    }

    truncateFinalByte(storage.items, bit_count);

    return init(storage.items.ptr, bit_count);
}

pub fn suffix(self: Constant, bytes_to_skip: u64) Constant {
    const bits_to_skip = bytes_to_skip * 8;
    if (self.bit_count <= bits_to_skip) {
        return init("", 0);
    } else {
        return init(self.asString(), self.bit_count - bits_to_skip);
    }
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

pub const builtin = struct {
    pub const zero = init("", 0);
};

pub fn initInternPool(gpa: std.mem.Allocator, pool: *InternPool) void {
    inline for (@typeInfo(builtin).Struct.decls) |decl| {
        pool.put(gpa, &@field(builtin, decl.name), {}) catch @panic("OOM");
    }
}
