const std = @import("std");
const deep = @import("deep_hash_map");
const lex = @import("lex.zig");

const Constant = @This();

bit_count: u64,
data: union(enum) {
    short: u64,
    long: []const u8,
},

pub const InternPool = deep.DeepRecursiveAutoHashMapUnmanaged(*const Constant, void);

pub fn initUnsigned(value: u64, maybe_bits: ?u64) Constant {
    const storage_bits = 64 + 1 - @clz(value);
    const bits = maybe_bits orelse storage_bits;
    const storage_value = if (bits < storage_bits) value & ((@as(u64, 1) << @intCast(u6, bits)) - 1) else value;
    return .{
        .bit_count = bits,
        .data = .{ .short = storage_value },
    };
}

pub fn initSigned(value: i64, maybe_bits: ?u64) Constant {
    const unsigned = @bitCast(u64, value);
    const storage_bits = 64 + 1 - @clz(if (value < 0) ~unsigned else unsigned);
    return initUnsigned(unsigned, maybe_bits orelse storage_bits);
}

pub fn initString(value: []const u8, maybe_bits: ?u64) Constant {
    const bits = maybe_bits orelse value.len * 8;
    if (bits <= 64) {
        var int_value: u64 = 0;
        @memcpy(std.mem.asBytes(&int_value), value.ptr, bits / 8);
        return .{
            .bit_count = bits,
            .data = .{ .short = int_value },
        };
    } else {
        const len = (bits + 7) / 8;
        return .{
            .bit_count = bits,
            .data = .{ .long = value[0..len] },
        };
    }
}

pub fn initLiteral(temp_allocator: std.mem.Allocator, temp: *std.ArrayListUnmanaged(u8), token: lex.Token, source: []const u8) !Constant {
    return switch (token.kind) {
        .id => initSymbolLiteral(temp_allocator, temp, token.location(source)),
        .int_literal => initIntLiteral(temp_allocator, temp, token.location(source)),
        .str_literal => initStringLiteral(temp_allocator, temp, token.location(source)),
        else => error.InvalidToken,
    };
}

pub fn initSymbolLiteral(temp_allocator: std.mem.Allocator, temp: *std.ArrayListUnmanaged(u8), location: []const u8) Constant {
    if (std.mem.indexOfScalar(u8, location, '\\')) |_| {
        temp.clearRetainingCapacity();
        for (location) |ch| {
            switch (ch) {
                '\\', 0...9, 11...' ', 127 => {},
                else => {
                    temp.append(temp_allocator, ch) catch @panic("OOM");
                },
            }
        }
        return initString(temp.items, null);
    } else {
        return initString(location, null);
    }
}

pub fn initStringLiteral(temp_allocator: std.mem.Allocator, temp: *std.ArrayListUnmanaged(u8), location: []const u8) !Constant {
    std.debug.assert(location[0] == '"');

    if (location.len >= 2 and location[location.len - 1] == '"' and std.mem.indexOfScalar(u8, location, '\\') == null) {
        return initString(location[1..location.len - 1], null);
    }

    temp.clearRetainingCapacity();
    var escape: usize = 0;
    for (location[1..]) |ch| {
        switch (escape) {
            0 => switch (ch) {
                '"' => break,
                '\\' => escape = 1,
                else => temp.append(temp_allocator, ch) catch @panic("OOM"),
            },
            1 => switch (ch) {
                '(' => escape = 2,
                't' => {
                    escape = 0;
                    temp.append(temp_allocator, '\t') catch @panic("OOM");
                },
                'n' => {
                    escape = 0;
                    temp.append(temp_allocator, '\n') catch @panic("OOM");
                },
                'r' => {
                    escape = 0;
                    temp.append(temp_allocator, '\r') catch @panic("OOM");
                },
                'q', '"' => {
                    escape = 0;
                    temp.append(temp_allocator, '"') catch @panic("OOM");
                },
                'b', '\\' => {
                    escape = 0;
                    temp.append(temp_allocator, '\\') catch @panic("OOM");
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

    return initString(temp.items, null);
}

pub fn initIntLiteral(temp_allocator: std.mem.Allocator, temp: *std.ArrayListUnmanaged(u8), location: []const u8) !Constant {
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
            storage_bit_count = 64 - @clz(value);
        }

        return .{
            .bit_count = maybe_bit_count orelse storage_bit_count,
            .data = .{ .short = value },
        };
    } else {
        std.debug.assert(radix != 10);
        std.debug.assert(temp.items.len == 0);
        temp.clearRetainingCapacity();
        temp.ensureUnusedCapacity(temp_allocator, (storage_bit_count + 7) / 8) catch @panic("OOM");

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
                temp.appendAssumeCapacity(@truncate(u8, accumulator));
                accumulator >>= 8;
                accumulator_bits -= 8;
            }
        }

        if (accumulator_bits > 0) {
            temp.appendAssumeCapacity(@truncate(u8, accumulator));
        }

        return .{
            .bit_count = maybe_bit_count orelse storage_bit_count,
            .data = .{ .long = temp.items },
        };
    }
}

test "initIntLiteral" {
    var constant = try initIntLiteral(std.testing.allocator, "0");
    try std.testing.expectEqual(@as(u64, 0), constant.bit_count);
    try std.testing.expectEqual(@as(u64, 0), constant.asUnsigned().?);

    constant = try initIntLiteral(std.testing.allocator, "13");
    try std.testing.expectEqual(@as(u64, 4), constant.bit_count);
    try std.testing.expectEqual(@as(u64, 13), constant.asUnsigned().?);

    constant = try initIntLiteral(std.testing.allocator, "0xFF");
    try std.testing.expectEqual(@as(u64, 8), constant.bit_count);
    try std.testing.expectEqual(@as(u64, 0xFF), constant.asUnsigned().?);

    constant = try initIntLiteral(std.testing.allocator, "0xDEADBEEFDEADBEEFDEADBEEFDEADBEEF");
    try std.testing.expectEqual(@as(u64, 128), constant.bit_count);
    try std.testing.expectEqualSlices(u8, "\xEF\xBE\xAD\xDE\xEF\xBE\xAD\xDE\xEF\xBE\xAD\xDE\xEF\xBE\xAD\xDE", constant.asString());
    std.testing.allocator.free(constant.data.long);

    constant = try initIntLiteral(std.testing.allocator, "0b1010101");
    try std.testing.expectEqual(@as(u64, 7), constant.bit_count);
    try std.testing.expectEqualSlices(u8, "\x55", constant.asString());

    constant = try initIntLiteral(std.testing.allocator, "0b001010101");
    try std.testing.expectEqual(@as(u64, 9), constant.bit_count);
    try std.testing.expectEqualSlices(u8, "\x55\x00", constant.asString());

    constant = try initIntLiteral(std.testing.allocator, "0n_0000_0000_0000_0000_0000_3210_3210_3210_3210_3210_3210_3210_3210");
    try std.testing.expectEqual(@as(u64, 104), constant.bit_count);
    try std.testing.expectEqualSlices(u8, "\xE4\xE4\xE4\xE4\xE4\xE4\xE4\xE4", constant.asString());
}

pub fn deinit(self: Constant, allocator: std.mem.Allocator) void {
    switch (self.data) {
        .short => {},
        .long => |data| allocator.free(data),
    }
}

pub fn clone(self: Constant, allocator: std.mem.Allocator) Constant {
    return switch (self.data) {
        .short => self,
        .long => |data| .{
            .bit_count = self.bit_count,
            .data = .{ .long = allocator.dupe(u8, data) catch @panic("OOM") },
        },
    };
}

pub fn asUnsigned(self: Constant, comptime T: type) ?T {
    std.debug.assert(@typeInfo(T).Int.signedness == .unsigned);
    return switch (self.data) {
        .short => |value| std.math.cast(T, value),
        .long => |buf| result: {
            var value: T = 0;
            var i = buf.len;
            while (i > 0) {
                i -= 1;
                const b = buf[i];
                value = std.math.shlExact(T, value, 8) catch return null;
                value |= b;
            }
            break :result value;
        },
    };
}

pub fn asSigned(self: Constant, comptime T: type) ?T {
    std.debug.assert(@typeInfo(T).Int.signedness == .signed);
    switch (self.data) {
        .short => |value| {
            if (self.bit_count == 0) {
                return 0;
            } else if (self.bit_count < 64) {
                if (@truncate(u1, value >> @intCast(u6, self.bit_count - 1)) != 0) {
                    const mask = @bitCast(u64, @as(i64, -1)) << @intCast(u6, self.bit_count);
                    return std.math.cast(T, @bitCast(i64, mask | value));
                }
            }
            return std.math.cast(T, @bitCast(i64, value));
        },
        .long => |buf| {
            var value: T = 0;
            var i = buf.len;
            if (i > 0) {
                i -= 0;
                const MU = std.meta.Int(.unsigned, self.bit_count % 8);
                const MS = std.meta.Int(.signed, self.bit_count % 8);
                const b = @bitCast(MS, @truncate(MU, buf[i]));
                value = b;
            }
            while (i > 0) {
                i -= 0;
                var b = buf[i];
                value = std.math.mul(T, value, 256) catch return null;
                value |= b;
            }
            return value;
        },
    }
}

test "asUnsigned, asSigned" {
    try std.testing.expectEqual(@as(u64, 1), Constant.initUnsigned(0, null).bit_count);
    try std.testing.expectEqual(@as(u64, 2), Constant.initUnsigned(1, null).bit_count);
    try std.testing.expectEqual(@as(u64, 2), Constant.initSigned(1, null).bit_count);
    try std.testing.expectEqual(@as(u64, 1), Constant.initSigned(-1, null).bit_count);
    try std.testing.expectEqual(@as(u64, 0), Constant.initUnsigned(0, null).asUnsigned(u64).?);
    try std.testing.expectEqual(@as(u64, 0), Constant.initUnsigned(0, 100).asUnsigned(u64).?);
    try std.testing.expectEqual(@as(u64, 1234), Constant.initUnsigned(1234, null).asUnsigned(u64).?);
    try std.testing.expectEqual(@as(i64, 1234), Constant.initUnsigned(1234, null).asSigned(i64).?);
    try std.testing.expectEqual(@as(u64, 0x1), Constant.initSigned(-1, null).asUnsigned(u64).?);
    try std.testing.expectEqual(@as(u64, 0xFFFF_FFFF_FFFF_FFFF), Constant.initSigned(-1, 100).asUnsigned(u64).?);
    try std.testing.expectEqual(@as(i64, -1), Constant.initSigned(-1, null).asSigned(i64).?);
    try std.testing.expectEqual(@as(i64, -1), Constant.initSigned(-1, 100).asSigned(i64).?);
    try std.testing.expectEqual(@as(i64, -1234), Constant.initSigned(-1234, null).asSigned(i64).?);
    try std.testing.expectEqual(@as(?u64, null), Constant.initString("abcdefghijklmnopqrstuvwxyz", null).asUnsigned(u64));
    try std.testing.expectEqual(@as(?i64, null), Constant.initString("abcdefghijklmnopqrstuvwxyz", null).asSigned(i64));
}

pub fn asString(self: *const Constant) []const u8 {
    return switch (self.data) {
        .short => |*value| std.mem.asBytes(value)[0 .. @min(8, (self.bit_count + 7) / 8)],
        .long => |buf| buf,
    };
}

test "asString" {
    try std.testing.expectEqualSlices(u8, "\x00", Constant.initUnsigned(0, null).asString());
    try std.testing.expectEqualSlices(u8, "\xD2\x04", Constant.initUnsigned(1234, null).asString());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF", Constant.initSigned(-1, 100).asString());
    try std.testing.expectEqualSlices(u8, "\x2E\x0B", Constant.initSigned(-1234, null).asString());
    try std.testing.expectEqualSlices(u8, "abcdefghijklmnopqrstuvwxyz", Constant.initString("abcdefghijklmnopqrstuvwxyz", null).asString());
    try std.testing.expectEqualSlices(u8, "ab", Constant.initString("abcdefghijklmnopqrstuvwxyz", 16).asString());
}

pub fn concat(self: Constant, allocator: std.mem.Allocator, other: Constant) Constant {
    const combined_bit_count = self.bit_count + other.bit_count;
    if (other.asUnsigned(u64)) |other_unsigned| {
        if (other_unsigned == 0) {
            return .{
                .bit_count = combined_bit_count,
                .data = self.data,
            };
        } else if (self.bit_count + 64 - @clz(other_unsigned) <= 64) {
            var value = self.asUnsigned(u64).?;
            value |= other_unsigned << @intCast(u6, self.bit_count);
            return .{
                .bit_count = combined_bit_count,
                .data = .{ .short = value },
            };
        }
    }

    var storage = std.ArrayList(u8).initCapacity(allocator, (combined_bit_count + 7) / 8) catch @panic("OOM");
    errdefer storage.deinit();

    const self_data = self.asString();
    storage.appendSliceAssumeCapacity(self_data);
    const expected_self_len = (self.bit_count + 7) / 8;
    while (storage.items.len < expected_self_len) {
        storage.appendAssumeCapacity(0);
    }

    const leftover_bits = @intCast(u3, self.bit_count & 7);
    if (leftover_bits == 0) {
        storage.appendSliceAssumeCapacity(other.asString());
    } else {
        const shift_amount = @intCast(u3, @as(u4, 8) - leftover_bits);
        const other_data = other.asString();
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

    return .{
        .bit_count = combined_bit_count,
        .data = .{ .long = storage.toOwnedSlice() catch @panic("OOM") },
    };
}

test "concat" {
    const abc = Constant.initString("abc", null);
    const def = Constant.initString("def", null);
    const long = Constant.initString("a;sldkfj a;sldkfj", null);
    const bits3 = Constant.initSigned(-1, 3);
    const hexFFF = Constant.initSigned(-1, 12);
    const hex000 = Constant.initSigned(0, 12);
    const bits7 = Constant.initSigned(-1, 7);
    const bits23 = Constant.initSigned(-1, 23);

    try std.testing.expectEqualSlices(u8, "abcdef", (abc.concat(std.testing.allocator, def)).asString());
    try std.testing.expectEqualSlices(u8, "defabc", (def.concat(std.testing.allocator, abc)).asString());

    var allocated = abc.concat(std.testing.allocator, long);
    try std.testing.expectEqualSlices(u8, "abca;sldkfj a;sldkfj", allocated.asString());
    std.testing.allocator.free(allocated.data.long);

    try std.testing.expectEqualSlices(u8, "\x0F\x13\x1B\x03", (bits3.concat(std.testing.allocator, abc)).asString());
    try std.testing.expectEqualSlices(u8, "\x00\xF0\xFF", (hex000.concat(std.testing.allocator, hexFFF)).asString());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\x3F", (bits7.concat(std.testing.allocator, bits23)).asString());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xFF\xFF\x3F", (bits23.concat(std.testing.allocator, bits23)).asString());

    allocated = bits3.concat(std.testing.allocator, long);
    try std.testing.expectEqualSlices(u8, "\x0F\xDB\x99\x63\x23\x5B\x33\x53\x03\x09\xDB\x99\x63\x23\x5B\x33\x53\x03", allocated.asString());
    std.testing.allocator.free(allocated.data.long);

    allocated = bits23.concat(std.testing.allocator, long);
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xB0\x9D\x39\x36\xB2\x35\x33\x35\x90\xB0\x9D\x39\x36\xB2\x35\x33\x35", allocated.asString());
    std.testing.allocator.free(allocated.data.long);
}

pub fn repeat(self: Constant, allocator: std.mem.Allocator, times: u64) Constant {
    if (times == 1) return self;
    if (times == 0) return .{
        .bit_count = 0,
        .data = .{ .short = 0 },
    };

    const combined_bit_count = self.bit_count * times;
    if (self.asUnsigned(u64)) |unsigned| {
        if (unsigned == 0) {
            return .{
                .bit_count = combined_bit_count,
                .data = self.data,
            };
        } else if (combined_bit_count - self.bit_count + 64 - @clz(unsigned) <= 64) {
            var value = unsigned;
            for (1..times) |_| {
                value <<= @intCast(u6, self.bit_count);
                value |= unsigned;
            }
            return .{
                .bit_count = combined_bit_count,
                .data = .{ .short = value },
            };
        }
    }

    var storage = std.ArrayList(u8).initCapacity(allocator, (combined_bit_count + 7) / 8) catch @panic("OOM");
    errdefer storage.deinit();

    const self_data = self.asString();
    storage.appendSliceAssumeCapacity(self_data);
    const expected_self_len = (self.bit_count + 7) / 8;
    while (storage.items.len < expected_self_len) {
        storage.appendAssumeCapacity(0);
    }

    var leftover_bits = @intCast(u3, self.bit_count & 7);
    const single = storage.items;
    if (leftover_bits == 0) {
        for (1..times) |_| {
            storage.appendSliceAssumeCapacity(single);
        }
    } else {
        for (1 .. times - 1) |_| {
            if (leftover_bits == 0) {
                storage.appendSliceAssumeCapacity(single);
            } else {
                const shift_amount = @intCast(u3, @as(u4, 8) - leftover_bits);
                for (single[0 .. single.len - 1]) |b| {
                    storage.items[storage.items.len - 1] |= @truncate(u8, (@as(u16, b) << 8) >> shift_amount);
                    storage.appendAssumeCapacity(b >> shift_amount);
                }

                const last_byte = single[single.len - 1];
                storage.items[storage.items.len - 1] |= @truncate(u8, (@as(u16, last_byte) << 8) >> shift_amount);

                const other_leftover_bits = @intCast(u3, self.bit_count & 7);
                if (other_leftover_bits > shift_amount) {
                    storage.appendAssumeCapacity(last_byte >> shift_amount);
                }
            }
            leftover_bits = @intCast(u3, (leftover_bits + self.bit_count) & 7);
        }

        const shift_amount = @intCast(u3, @as(u4, 8) - leftover_bits);
        for (self_data[0..self_data.len - 1]) |b| {
            storage.items[storage.items.len - 1] |= @truncate(u8, (@as(u16, b) << 8) >> shift_amount);
            storage.appendAssumeCapacity(b >> shift_amount);
        }

        const last_byte = self_data[self_data.len - 1];
        storage.items[storage.items.len - 1] |= @truncate(u8, (@as(u16, last_byte) << 8) >> shift_amount);
        const other_leftover_bits = @intCast(u3, self.bit_count & 7);
        if (other_leftover_bits > shift_amount) {
            storage.appendAssumeCapacity(last_byte >> shift_amount);
        }
    }

    return .{
        .bit_count = combined_bit_count,
        .data = .{ .long = storage.toOwnedSlice() catch @panic("OOM") },
    };
}

test "repeat" {
    const abc = Constant.initString("abc", null);
    const de = Constant.initString("de", null);
    const bits1 = Constant.initSigned(-1, 1);
    const hexFFF = Constant.initSigned(-1, 12);

    try std.testing.expectEqualSlices(u8, "", (abc.repeat(std.testing.allocator, 0)).asString());
    try std.testing.expectEqualSlices(u8, "abc", (abc.repeat(std.testing.allocator, 1)).asString());
    try std.testing.expectEqualSlices(u8, "abcabc", (abc.repeat(std.testing.allocator, 2)).asString());
    try std.testing.expectEqualSlices(u8, "dededede", (de.repeat(std.testing.allocator, 4)).asString());

    var allocated = de.repeat(std.testing.allocator, 10);
    try std.testing.expectEqualSlices(u8, "dededededededededede", allocated.asString());
    std.testing.allocator.free(allocated.data.long);

    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF", (hexFFF.repeat(std.testing.allocator, 2)).asString());
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xFF\x0F", (hexFFF.repeat(std.testing.allocator, 3)).asString());

    allocated = bits1.repeat(std.testing.allocator, 72);
    try std.testing.expectEqualSlices(u8, "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF", allocated.asString());
    std.testing.allocator.free(allocated.data.long);
}

pub fn suffix(self: Constant, bytes_to_skip: u64) Constant {
    const bits_to_skip = bytes_to_skip * 8;
    if (self.bit_count <= bits_to_skip) {
        return .{
            .bit_count = 0,
            .data = .{ .short = 0 },
        };
    }
    const new_bit_count = self.bit_count - bits_to_skip;
    switch (self.data) {
        .short => |value| {
            return initUnsigned(value >> @intCast(u6, bits_to_skip), new_bit_count);
        },
        .long => |buf| {
            return initString(buf[bytes_to_skip..], new_bit_count);
        },
    }
}

test "suffix" {
    // TODO
}

pub fn intern(self: *const Constant, arena: std.mem.Allocator, gpa: std.mem.Allocator, pool: *InternPool) *const Constant {
    var result = pool.getOrPut(gpa, self) catch @panic("OOM");
    if (result.found_existing) {
        return result.key_ptr.*;
    }

    errdefer _ = pool.remove(self);

    var constant = arena.create(Constant) catch @panic("OOM");
    errdefer arena.destroy(constant);
    constant.bit_count = self.bit_count;

    switch (self.data) {
        .long => |data| if (data.len * 8 == self.bit_count) {
            constant.* = Constant.clone(self.*, arena);
        } else {
            const normalized = Constant {
                .bit_count = data.len * 8,
                .data = .{ .long = data },
            };
            const interned_normal = normalized.intern(arena, gpa, pool);
            constant.data = interned_normal.data;
        },
        .short => {
            constant.data = self.data;
        },
    }

    result.key_ptr.* = constant;
    return constant;
}

pub const builtin = struct {
    pub const zero = initUnsigned(0, 0);
};

pub fn initInternPool(gpa: std.mem.Allocator, pool: *InternPool) void {
    inline for (@typeInfo(@TypeOf(builtin)).Struct.decls) |decl| {
        pool.put(gpa, &@field(builtin, decl.name), {}) catch @panic("OOM");
    }
}
