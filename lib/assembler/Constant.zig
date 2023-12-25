bit_count: i64, // when negative, indicates that the number is signed
data: union {
    short: [8]u8,
    long: [*]const u8,
},

pub const Intern_Pool = std.HashMapUnmanaged(*const Constant, void, Hash_Context, std.hash_map.default_max_load_percentage);
const Hash_Context = struct {
    pub fn hash(_: Hash_Context, key: *const Constant) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(std.mem.asBytes(&key.bit_count));
        hasher.update(key.as_string());
        return hasher.final();
    }
    pub fn eql(_: Hash_Context, a: *const Constant, b: *const Constant) bool {
        return a.bit_count == b.bit_count and std.mem.eql(u8, a.as_string(), b.as_string());
    }
};

// It is assumed that any bits in the last byte of value, beyond the last required bit,
// are either zero or sign extended from the last valid bit (depending on signedness)
fn init(value: [*]const u8, num_bits: u63, sign: Signedness) Constant {
    if (num_bits <= 64) {
        var src: []const u8 = undefined;
        src.ptr = value;
        src.len = (num_bits + 7) / 8;

        var data: u64 = 0;
        @memcpy(std.mem.asBytes(&data).ptr, src);

        var encoded_bits: i64 = num_bits;
        if (sign == .signed) {
            encoded_bits = -encoded_bits;
            if (num_bits < 64) {
                const bits_u6: u6 = @intCast(num_bits);
                if (@as(u1, @truncate(data >> (bits_u6 - 1))) != 0) {
                    data |= @as(u64, 0xFFFF_FFFF_FFFF_FFFF) << bits_u6;
                }
            }
        }
        return .{
            .bit_count = encoded_bits,
            .data = .{ .short = std.mem.toBytes(data) },
        };
    } else return .{
        .bit_count = switch (sign) {
            .unsigned => num_bits,
            .signed => -@as(i64, num_bits),
        },
        .data = .{ .long = value },
    };
}

pub fn init_string(value: []const u8) Constant {
    return init(value.ptr, @intCast(value.len * 8), .unsigned);
}

// Returns error.Overrun if the requested number of bits is larger than what's available in value.
// Returns error.Overflow if truncation of value to the requested bits changes it's numeric value, given the signedness requested.
pub fn init_string_bits(value: []const u8, num_bits: u63, sign: Signedness) !Constant {
    if (num_bits > value.len * 8) return error.Overrun;
    const leftover_bits: u3 = @intCast(num_bits & 7);
    if (leftover_bits > 0) {
        const last_byte = value[num_bits / 8];
        var mask: u8 = 0xFF;
        mask <<= leftover_bits;
        switch (sign) {
            .unsigned => {
                if ((last_byte & mask) != 0) return error.Overflow;
                for (value[last_byte + 1 ..]) |b| {
                    if (b != 0) return error.Overflow;
                }
            },
            .signed => {
                switch (@as(u1, @truncate(last_byte >> (leftover_bits - 1)))) {
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
    return init(value.ptr, num_bits, sign);
}

// Store an integer using the fewest bits possible, while ensuring that the MSB is always 0 for unsigned integers.
// This ensures that clone_with_signedness won't change the value
pub fn init_int(value: anytype) Constant {
    const T = @TypeOf(value);
    const sign = @typeInfo(T).Int.signedness;
    const T8 = std.meta.Int(sign, @sizeOf(T) * 8);
    const ext_value = @as(T8, value);
    const num_bits = @bitSizeOf(T) + @as(u63, 1) - if (value < 0) @clz(~value) else @clz(value);
    return init(std.mem.asBytes(&ext_value), num_bits, sign);
}

// Returns error.Overrun if more bits are requested than are in the value.
// Returns error.Overflow if truncation of value to the requested bits changes it's numeric value, given the signedness requested.
pub fn init_int_bits(value: anytype, num_bits: u63) !Constant {
    const T = @TypeOf(value);
    const int_info = @typeInfo(T).Int;
    const U = std.meta.Int(.unsigned, @bitSizeOf(T));
    const B = std.math.Log2Int(T);

    if (num_bits > @bitSizeOf(T)) return error.Overrun;

    const unsigned: U = @bitCast(value);
    switch (int_info.signedness) {
        .unsigned => if (num_bits < @bitSizeOf(T)) {
            if (0 != (unsigned >> @as(B, @intCast(num_bits)))) {
                return error.Overflow;
            }
        },
        .signed => if (num_bits < @bitSizeOf(T)) {
            const sign: u1 = @truncate(unsigned >> @as(B, @intCast(num_bits - 1)));
            var mask: U = @bitCast(@as(T, -1));
            mask <<= @as(B, @intCast(num_bits));
            switch (sign) {
                0 => if ((unsigned & mask) != 0) return error.Overflow,
                1 => if ((unsigned & mask) != mask) return error.Overflow,
            }
        },
    }
    return init(std.mem.asBytes(&value), num_bits, int_info.signedness);
}

pub fn init_symbol_literal(allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), location: []const u8) Constant {
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
        return init_string(storage.items);
    } else {
        return init_string(location);
    }
}

pub fn init_string_literal(allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), location: []const u8) !Constant {
    std.debug.assert(location[0] == '"');
    if (location[location.len - 1] != '"') {
        return error.UnclosedLiteral;
    }

    if (location.len >= 2 and location[location.len - 1] == '"' and std.mem.indexOfScalar(u8, location, '\\') == null) {
        return init_string(location[1..location.len - 1]);
    }

    storage.clearRetainingCapacity();
    var paren_escape_begin: usize = 0;
    var escape: enum {
        none,
        single,
        paren,
    } = .none;
    for (location[1..], 1..) |ch, i| {
        switch (escape) {
            .none => switch (ch) {
                '"' => {
                    if (i != location.len - 1) {
                        return error.InvalidCharacter;
                    }
                    break;
                },
                '\\' => {
                    escape = .single;
                },
                ' ', '!', '#'...'[', ']'...'~' => {
                    storage.append(allocator, ch) catch @panic("OOM");
                },
                else => return error.InvalidCharacter,
            },
            .single => switch (ch) {
                '(' => {
                    escape = .paren;
                    paren_escape_begin = i + 1;
                },
                't' => {
                    escape = .none;
                    storage.append(allocator, '\t') catch @panic("OOM");
                },
                'n' => {
                    escape = .none;
                    storage.append(allocator, '\n') catch @panic("OOM");
                },
                'r' => {
                    escape = .none;
                    storage.append(allocator, '\r') catch @panic("OOM");
                },
                'q', '"' => {
                    escape = .none;
                    storage.append(allocator, '"') catch @panic("OOM");
                },
                'b', '\\' => {
                    escape = .none;
                    storage.append(allocator, '\\') catch @panic("OOM");
                },
                else => return error.InvalidCharacter,
            },
            .paren => switch (ch) {
                ')' => {
                    const paren_text = location[paren_escape_begin..i];
                    var iter = std.mem.tokenize(u8, paren_text, " ");
                    while (iter.next()) |token| switch (token[0]) {
                        '=' => {
                            var decoder = std.base64.standard_no_pad.Decoder;
                            decoder.char_to_index['-'] = 62;
                            decoder.char_to_index['_'] = 63;
                            decoder.char_to_index[','] = 63;

                            const trimmed = std.mem.trim(u8, token, "=");
                            const dest_len = decoder.calcSizeUpperBound(trimmed.len) catch return error.InvalidBase64;
                            const old_len = storage.items.len;
                            storage.ensureUnusedCapacity(allocator, dest_len) catch @panic("OOM");
                            storage.items.len = old_len + dest_len;
                            const dest = storage.items[old_len..];
                            decoder.decode(dest, trimmed) catch return error.InvalidBase64;
                        },
                        'U' => {
                            if (token.len < 3 or token[1] != '+') {
                                return error.InvalidCodepoint;
                            }
                            const codepoint = std.fmt.parseUnsigned(u21, token[2..], 16) catch return error.InvalidCodepoint;
                            var buf: [4]u8 = undefined;
                            const bytes = std.unicode.utf8Encode(codepoint, &buf) catch return error.InvalidCodepoint;
                            storage.appendSlice(allocator, buf[0..bytes]) catch @panic("OOM");
                        },
                        else => {
                            _ = try parse_int_literal(allocator, storage, token, true);
                        },
                    };
                    escape = .none;
                },
                else => {},
            },
        }
    }

    if (escape != .none) {
        return error.IncompleteEscape;
    } else {
        return init_string(storage.items);
    }
}

pub fn init_int_literal(allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), location: []const u8) !Constant {
    storage.clearRetainingCapacity();
    const bit_count = try parse_int_literal(allocator, storage, location, false);
    return init(storage.items.ptr, bit_count, .unsigned);
}

fn parse_int_literal(allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), location: []const u8, single_byte_decimal: bool) !u63 {
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
            'q', 'Q' => {
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
        else => {
            //trim leading zeroes and underscores
            for (remaining, 0..) |ch, i| {
                if (ch == '0' or ch == '_') continue;
                remaining = remaining[i..];
                break;
            } else {
                remaining = "0";
            }

            var value: u64 = undefined;
            var bit_count: u63 = undefined;
            if (single_byte_decimal) {
                value = try std.fmt.parseUnsigned(u8, remaining, radix);
                bit_count = 8;
            } else {
                value = try std.fmt.parseUnsigned(u64, remaining, radix);
                bit_count = 65 - @clz(value);
            }
            const byte_count = (bit_count + 7) / 8;

            storage.ensureUnusedCapacity(allocator, byte_count) catch @panic("OOM");
            for (0..byte_count) |_| {
                storage.appendAssumeCapacity(@truncate(value));
                value >>= 8;
            }
            return bit_count;
        },
    };

    var bit_count: u63 = 0;
    for (remaining) |ch| {
        switch (ch) {
            '_' => continue,
            else => bit_count += bits_per_digit,
        }
    }

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
            storage.appendAssumeCapacity(@truncate(accumulator));
            accumulator >>= 8;
            accumulator_bits -= 8;
        }
    }

    if (accumulator_bits > 0) {
        storage.appendAssumeCapacity(@truncate(accumulator));
    }

    return bit_count;
}

pub fn deinit(self: Constant, allocator: std.mem.Allocator) void {
    if (self.bits() > 64) {
        allocator.free(self.as_string());
    }
}

pub fn bits(self: Constant) u63 {
    return @intCast(@abs(self.bit_count));
}

pub fn signedness(self: Constant) Signedness {
    return if (self.bit_count < 0) .signed else .unsigned;
}

// Computes the minimum number of bits needed to represent the constant's numeric value; truncation smaller than this results in overflow.
pub fn count_required_bits(self: Constant) u63 {
    const self_bits = self.bits();
    const self_signedness = self.signedness();
    if (self_bits <= 64) {
        var value: u64 = undefined;
        @memcpy(std.mem.asBytes(&value), &self.data.short);
        return switch (self_signedness) {
            .unsigned => 64 - @clz(value),
            .signed => 65 - @clz(if (value < 0) ~value else value),
        };
    }
    return count_required_bits_in_buf(self.as_string(), self_bits, self_signedness);
}

fn count_required_bits_in_buf(buf: []const u8, num_bits: u63, sign: Signedness) u63 {
    var byte_index = buf.len - 1;
    var bit_index: u3 = @intCast((num_bits - 1) & 0x7);
    var byte = buf[byte_index];
    var useless_bits: u63 = 0;

    const the_sign_bit: u1 = @truncate(byte >> bit_index);

    if (sign == .unsigned) {
        if (the_sign_bit == 0) {
            useless_bits += 1;
        } else {
            return num_bits;
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

        const bit: u1 = @truncate(byte >> bit_index);
        if (bit != the_sign_bit) break;

        useless_bits += 1;
    }

    return num_bits - useless_bits;
}

pub fn sign_bit(self: Constant) u1 {
    if (self.bit_count < 0) {
        const data = self.as_string();
        return @truncate(data[data.len - 1] >> 7);
    } else {
        return 0;
    }
}

pub fn msb(self: Constant) u1 {
    const data = self.as_string();
    const leftover_bits: u3 = @intCast(self.bits() & 0x7);
    return msb1(data, leftover_bits);
}

fn msb1(data: []const u8, leftover_bits: u3) u1 {
    const shift = if (leftover_bits == 0) 7 else leftover_bits - 1;
    return @truncate(data[data.len - 1] >> shift);
}

pub fn clone(self: Constant, allocator: std.mem.Allocator) Constant {
    if (self.bits() > 64) {
        const buf = allocator.dupe(u8, self.as_string()) catch @panic("OOM");
        return .{
            .bit_count = self.bit_count,
            .data = .{ .long = buf.ptr },
        };
    } else return self;
}

// returns error.Overflow if the constant's integer value would change due to truncation
pub fn clone_with_length(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), result_bit_count: u63) !Constant {
    if (result_bit_count <= self.bits()) {
        if (result_bit_count < self.count_required_bits()) {
            return error.Overflow;
        }
        return init(self.as_string().ptr, result_bit_count, self.signedness());
    }

    var small_buf: [8]u8 = undefined;
    const result = get_result_buffer(allocator, storage, &small_buf, result_bit_count);

    var iter = self.byte_iterator(null);
    for (result) |*b| {
        b.* = iter.next();
    }

    return init(result.ptr, result_bit_count, self.signedness());
}

pub fn clone_with_signedness(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), sign: Signedness) Constant {
    const num_bits = self.bits();

    var small_buf: [8]u8 = undefined;
    const result = get_result_buffer(allocator, storage, &small_buf, num_bits);

    var iter = self.byte_iterator(sign);
    for (result) |*b| {
        b.* = iter.next();
    }

    return init(result.ptr, num_bits, sign);
}

pub fn truncate(self: Constant, result_bit_count: u63) Constant {
    if (result_bit_count > self.bits()) {
        return self;
    }
    return init(self.as_string().ptr, result_bit_count, self.signedness());
}

// returns error.Truncation if the result bit count is smaller than the current bit count.
// Note the signedness of the result is the same as the signedness of `self`; regardless of `ext`
pub fn extend(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), result_bit_count: u63, ext: Signedness) !Constant {
    if (result_bit_count < self.bits()) {
        return error.Truncation;
    }

    var small_buf: [8]u8 = undefined;
    const result = get_result_buffer(allocator, storage, &small_buf, result_bit_count);

    var iter = self.byte_iterator(ext);
    for (result) |*b| {
        b.* = iter.next();
    }

    return init(result.ptr, result_bit_count, self.signedness());
}

pub fn byte_iterator(self: *const Constant, extension: ?Signedness) Byte_Iterator {
    const str = self.as_string();
    if (str.len == 0) {
        return .{
            .remaining = &.{},
            .final_byte = 0,
            .ext_byte = 0,
        };
    }

    var iter = Byte_Iterator{
        .remaining = str,
        .final_byte = str[str.len - 1],
        .ext_byte = 0,
    };

    if (extension) |ext| {
        var mask: u8 = 0xFF;
        const leftover_bits: u3 = @intCast(self.bits() & 0x7);
        if (leftover_bits == 0) {
            mask = 0;
        } else {
            mask <<= leftover_bits;
        }
        switch (ext) {
            .unsigned => {
                iter.final_byte &= ~mask;
            },
            .signed => switch (msb1(str, leftover_bits)) {
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
        iter.ext_byte = self.sign_bit() * @as(u8, 0xFF);
    }

    return iter;
}

const Byte_Iterator = struct {
    remaining: []const u8,
    final_byte: u8,
    ext_byte: u8,

    pub fn next(self: *Byte_Iterator) u8 {
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

    pub fn skip(self: *Byte_Iterator, bytes: usize) void {
        if (bytes >= self.remaining.len) {
            self.remaining.len = 0;
        } else {
            self.remaining.len -= bytes;
        }
    }
};

pub fn as_string(self: *const Constant) []const u8 {
    const num_bits = self.bits();
    var slice: []const u8 = undefined;
    slice.ptr = if (num_bits <= 64) &self.data.short else self.data.long;
    slice.len = (num_bits + 7) / 8;
    return slice;
}


pub fn as_int(self: Constant, comptime T: type) !T {
    const buf = self.as_string();
    if (buf.len == 0) return 0;

    const int_info = @typeInfo(T).Int;
    const T8 = std.meta.Int(int_info.signedness, @sizeOf(T) * 8);

    var value: T8 = undefined;
    var iter = self.byte_iterator(null);
    for (std.mem.asBytes(&value)) |*b| {
        b.* = iter.next();
    }
    const ext: u8 = if (value >= 0 or self.signedness() == .unsigned) 0 else 0xFF;
    while (iter.remaining.len > 0) {
        if (iter.next() != ext) return error.Overflow;
    }

    return std.math.cast(T, value) orelse error.Overflow;
}

pub fn concat(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), other: Constant) Constant {
    const self_bits = self.bits();
    if (self_bits == 0) {
        return other;
    }

    const other_bits = other.bits();
    if (other_bits == 0) {
        return self;
    }

    const combined_bits = self_bits + other_bits;

    var small_buf: [8]u8 = undefined;
    const result = get_result_buffer(allocator, storage, &small_buf, combined_bits);
    var i: usize = 0;

    const self_iter = self.byte_iterator(.unsigned);
    const self_leftover_bits: u3 = @intCast(self_bits & 0x7);
    i = append_iter(result, i, 0, self_leftover_bits, self_iter);

    const other_iter = other.byte_iterator(null);
    const other_leftover_bits: u3 = @intCast(other_bits & 0x7);
    i = append_iter(result, i, self_leftover_bits, other_leftover_bits, other_iter);

    return init(result.ptr, combined_bits, other.signedness());
}

pub fn repeat(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), times: i64) Constant {
    if (times <= 0) return init("", 0, .unsigned);
    if (times == 1) return self;

    const times_u63: u63 = @intCast(times);

    const self_bits = self.bits();
    if (self_bits == 0) return self;

    const combined_bits = self_bits * times_u63;

    var small_buf: [8]u8 = undefined;
    const result = get_result_buffer(allocator, storage, &small_buf, combined_bits);
    var i: usize = 0;

    const unsigned_iter = self.byte_iterator(.unsigned);
    const self_leftover_bits: u3 = @intCast(self_bits & 7);
    var leftover_bits: u3 = 0;
    for (0 .. times_u63 - 1) |_| {
        i = append_iter(result, i, leftover_bits, self_leftover_bits, unsigned_iter);
        leftover_bits = @intCast((leftover_bits + self_bits) & 7);
    }

    const iter = self.byte_iterator(null);
    i = append_iter(result, i, leftover_bits, self_leftover_bits, iter);

    return init(result.ptr, combined_bits, self.signedness());
}

fn get_result_buffer(allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), small_buf: *[8]u8, num_bits: u64) []u8 {
    const bytes = (num_bits + 7) / 8;
    var result: []u8 = if (num_bits <= 64) small_buf else s: {
        storage.clearRetainingCapacity();
        storage.ensureTotalCapacity(allocator, bytes) catch @panic("OOM");
        break :s storage.items;
    };
    result.len = bytes;
    return result;
}

fn append_iter(result: []u8, starting_byte: usize, leftover_bits: u3, self_leftover_bits: u3, self_iter: Byte_Iterator) usize {
    var i = starting_byte;
    var iter = self_iter;
    if (leftover_bits == 0) {
        while (iter.remaining.len > 0) {
            result[i] = iter.next();
            i += 1;
        }
    } else {
        const shift_amount: u3 = @intCast(@as(u4, 8) - leftover_bits);

        while (iter.remaining.len > 1) {
            const b = iter.next();
            result[i - 1] |= encode_shifted_byte_prev(b, shift_amount);
            result[i] = encode_shifted_byte_next(b, shift_amount);
            i += 1;
        }

        const b = iter.next();
        result[i - 1] |= encode_shifted_byte_prev(b, shift_amount);
        if (self_leftover_bits == 0 or self_leftover_bits > shift_amount) {
            result[i] = encode_shifted_byte_next(b, shift_amount);
            result[i] |= encode_shifted_byte_prev(iter.next(), shift_amount);
        }
    }
    return i;
}
fn encode_shifted_byte_prev(b: u8, shift_amount: u3) u8 {
    return @truncate((@as(u16, b) << 8) >> shift_amount);
}
fn encode_shifted_byte_next(b: u8, shift_amount: u3) u8 {
    return b >> shift_amount;
}

pub const Binary_Op = enum {
    bitwise_or,
    bitwise_xor,
    bitwise_and,
    add,
    subtract,
};
pub fn binary_op(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8), other: Constant, comptime op: Binary_Op) Constant {
    const self_bits = self.bits();
    const other_bits = other.bits();

    var result_bits: u63 = undefined;
    var result_signedness: Signedness = undefined;

    switch (op) {
        .bitwise_or, .bitwise_xor, .bitwise_and => {
            if (self_bits >= other_bits) {
                result_bits = self_bits;
                result_signedness = self.signedness();
            } else {
                result_bits = other_bits;
                result_signedness = other.signedness();
            }
        },
        .add, .subtract => {
            result_signedness = .signed;
            result_bits = @max(self_bits, other_bits) + 2;
        },
    }

    var small_buf: [8]u8 = undefined;
    const result = get_result_buffer(allocator, storage, &small_buf, result_bits);

    var self_iter = self.byte_iterator(null);
    var other_iter = other.byte_iterator(null);
    switch (op) {
        .bitwise_or => {
            for (result) |*b| {
                b.* = self_iter.next() | other_iter.next();
            }
        },
        .bitwise_xor => {
            for (result) |*b| {
                b.* = self_iter.next() ^ other_iter.next();
            }
        },
        .bitwise_and => {
            for (result) |*b| {
                b.* = self_iter.next() & other_iter.next();
            }
        },
        .add => {
            var carry: u1 = 0;
            for (result) |*b| {
                const sum: u9 = @as(u9, self_iter.next()) + @as(u9, other_iter.next()) + carry;
                b.* = @truncate(sum);
                carry = @truncate(sum >> 8);
            }
            result_bits = count_required_bits_in_buf(result, result_bits, .signed);
        },
        .subtract => {
            var carry: u1 = 1;
            for (result) |*b| {
                const sum: u9 = @as(u9, self_iter.next()) + @as(u9, ~other_iter.next()) + carry;
                b.* = @truncate(sum);
                carry = @truncate(sum >> 8);
            }
            result_bits = count_required_bits_in_buf(result, result_bits, .signed);
        },
    }

    return init(result.ptr, result_bits, result_signedness);
}

pub fn complement(self: Constant, allocator: std.mem.Allocator, storage: *std.ArrayListUnmanaged(u8)) Constant {
    const result_bits = self.bits();

    var small_buf: [8]u8 = undefined;
    const result = get_result_buffer(allocator, storage, &small_buf, result_bits);

    var self_iter = self.byte_iterator(null);
    for (result) |*b| {
        b.* = ~self_iter.next();
    }

    return init(result.ptr, result_bits, self.signedness());
}

pub fn intern(self: *const Constant, arena: std.mem.Allocator, gpa: std.mem.Allocator, pool: *Intern_Pool) *const Constant {
    const result = pool.getOrPut(gpa, self) catch @panic("OOM");
    errdefer _ = pool.remove(self);
    if (result.found_existing) {
        return result.key_ptr.*;
    }

    const constant = arena.create(Constant) catch @panic("OOM");
    errdefer arena.destroy(constant);
    constant.* = self.clone(arena);

    result.key_ptr.* = constant;
    return constant;
}

const Constant = @This();
const Signedness = std.builtin.Signedness;
const std = @import("std");
