value: Value,
domain: Domain,
bit_offset: Instruction.Encoded.Offset_Bits,
bit_count: Instruction.Encoded.Length_Bits,

pub fn required_bits(self: Encoder) Instruction.Encoded.Length_Bits {
    return self.bit_offset + self.bit_count;
}

pub fn bit_mask(self: Encoder) Instruction.Encoded.Data {
    const W = std.meta.Int(.unsigned, @bitSizeOf(Instruction.Encoded.Data) + 1);
    const high_ones = (@as(W, 1) << self.bit_count) - 1;
    return @intCast(high_ones << self.bit_offset);
}

pub fn encode(self: Encoder, insn: Instruction, out: *Instruction.Encoded.Data) bool {
    switch (self.value) {
        .placeholder => |info| {
            if (std.mem.eql(u8, info.name, "__")) {
                // This is a "don't care" encoder; we don't encode it at all.
                return true;
            }
        },
        else => {},
    }
    return self.encode_raw(self.value.evaluate(insn.params), out);
}

pub fn encode_value(self: Encoder, value: i64, out: *Instruction.Encoded.Data) bool {
    return self.encode_raw(self.value.raw_from_value(value), out);
}

fn encode_raw(self: Encoder, value: i64, out: *Instruction.Encoded.Data) bool {
    const raw: Instruction.Encoded.Data = self.domain.encode(value) orelse return false;
    var data = @shlExact(raw, self.bit_offset);
    data &= self.bit_mask();
    out.* |= data;
    return true;
}

pub fn decode(self: Encoder, data: Instruction.Encoded.Data, out: []Parameter) bool {
    return self.value.assign(self.decode_raw(data) orelse return false, out);
}

pub fn decode_value(self: Encoder, data: Instruction.Encoded.Data) ?i64 {
    return self.value.value_from_raw(self.decode_raw(data) orelse return null);
}

fn decode_raw(self: Encoder, data: Instruction.Encoded.Data) ?i64 {
    const shifted_data = data >> self.bit_offset;
    const bits = self.bit_count;
    const mask = (@as(u64, 1) << @intCast(bits)) - 1;
    const raw: u64 = @intCast(shifted_data & mask);
    return self.domain.decode(raw);
}

pub fn init(bit_offset: Instruction.Encoded.Offset_Bits, what: anytype) Encoder {
    var value: Value = undefined;
    var domain: Domain = undefined;
    const T = @TypeOf(what);
    switch (@typeInfo(T)) {
        .@"type" => {
            if (@hasDecl(what, "Inner")) {
                const inner_encoder = comptime Encoder.init(0, what.Inner);
                if (comptime std.mem.eql(u8, @tagName(what.op), "negate")) {
                    value = .{ .negate = &inner_encoder.value };
                    domain = inner_encoder.domain;
                } else if (comptime std.mem.eql(u8, @tagName(what.op), "invert")) {
                    const bits = what.bits orelse inner_encoder.domain.min_bits();
                    value = .{ .xor = .{
                        .inner = &inner_encoder.value,
                        .mask = @bitCast((@as(u64, 1) << bits) - 1),
                    } };
                    domain = inner_encoder.domain;
                } else if (comptime std.mem.eql(u8, @tagName(what.op), "offset")) {
                    value = .{ .offset = .{
                        .inner = &inner_encoder.value,
                        .offset = what.offset,
                    } };
                    domain = inner_encoder.domain;
                } else @compileError("Unsupported placeholder transformation");
            } else {
                value = .{ .placeholder = .{
                    .kind = .param_constant,
                    .param = .invalid,
                    .name = what.placeholder,
                }};
                domain = what.domain;
            }
        },
        .@"enum" => {
            const Tag = std.meta.Tag(T);
            value = .{ .constant = @intFromEnum(what) };
            domain = .{ .int = .{
                .signedness = @typeInfo(Tag).int.signedness,
                .bits = @bitSizeOf(Tag),
                .multiple = 1,
            }};
        },
        .int => |info| {
            value = .{ .constant = what };
            domain = .{ .int = .{
                .signedness = info.signedness,
                .bits = info.bits,
                .multiple = 1,
            }};
        },
        else => {
            const bit_size = @bitSizeOf(T);
            const Signed = std.meta.Int(.signed, bit_size);
            value = .{ .constant = @as(Signed, @bitCast(what)) };
            domain = .{ .int = .{
                .signedness = .signed,
                .bits = bit_size,
                .multiple = 1,
            }};
        }
    }
    return .{
        .value = value,
        .domain = domain,
        .bit_offset = bit_offset,
        .bit_count = domain.min_bits(),
    };
}

pub fn init_dont_care(bit_offset: Instruction.Encoded.Offset_Bits, bits: anytype) Encoder {
    const T = @TypeOf(bits);
    const info = @typeInfo(T).int;
    const domain: Domain = .{ .int = .{
        .signedness = info.signedness,
        .bits = info.bits,
        .multiple = 1,
    }};
    return .{
        .value = .{ .constant_dont_care = bits },
        .domain = domain,
        .bit_offset = bit_offset,
        .bit_count = domain.min_bits(),
    };
}

pub fn eql(a: Encoder, b: Encoder) bool {
    return a.bit_offset == b.bit_offset and a.bit_count == b.bit_count and a.domain.eql(b.domain) and a.value.eql(b.value);
}

pub fn value_iterator(self: *const Encoder) Value_Iterator {
    return .{ 
        .encoder = self,
        .last_value = 0,
        .next_raw = 0,
        .max_raw = if (self.value == .constant) 0 else self.domain.max_encoded(),
    };
}

pub const Value_Iterator = struct {
    encoder: *const Encoder,
    last_value: i64,
    next_raw: u64,
    max_raw: u64,

    pub fn next(self: *Value_Iterator) ?i64 {
        if (self.next_raw > self.max_raw) return null;
        const raw = self.encoder.domain.decode(self.next_raw).?;
        self.last_value = self.encoder.value.value_from_raw(raw);
        self.next_raw += 1;
        return self.last_value;
    }

    pub fn reset(self: *Value_Iterator) void {
        self.next_raw = 0;
    }
};

/// Represents the transform between logical values (as appearing in assembly language programs) and raw values (extracted from set of machine code bits and mapped by a Domain)
pub const Value = union (enum) {
    constant: i64,
    constant_dont_care: i64, // same as .constant when encoding, but ignored when decoding
    placeholder: Placeholder,
    negate: *const Value,
    xor: struct {
        inner: *const Value,
        mask: i64,
    },
    offset: struct {
        inner: *const Value,
        offset: i64,
    },

    pub fn get_placeholder(self: Value) ?Placeholder {
        return switch (self) {
            .constant, .constant_dont_care => null,
            .placeholder => |info| info,
            .negate => |inner| inner.get_placeholder(),
            .xor => |info| info.inner.get_placeholder(),
            .offset => |info| info.inner.get_placeholder(),
        };
    }

    pub fn evaluate(self: Value, params: []const Parameter) i64 {
        return switch (self) {
            .constant, .constant_dont_care => |v| v,
            .placeholder => |info| info.evaluate(params),
            .negate => |inner| -inner.evaluate(params),
            .xor => |info| info.inner.evaluate(params) ^ info.mask,
            .offset => |info| info.inner.evaluate(params) - info.offset,
        };
    }

    pub fn assign(self: Value, value: i64, out: []Parameter) bool {
        return switch (self) {
            .constant => |v| v == value,
            .constant_dont_care => true,
            .placeholder => |info| info.assign(value, out),
            .negate => |inner| inner.assign(-value, out),
            .xor => |info| info.inner.assign(value ^ info.mask, out),
            .offset => |info| info.inner.assign(value + info.offset, out),
        };
    }

    /// Convert a "logical" value (as would appear in asm) to a "raw" value that can be encoded by a Domain
    pub fn raw_from_value(self: Value, logical: i64) i64 {
        return switch (self) {
            .constant, .constant_dont_care => |v| v,
            .placeholder => logical,
            .negate => |inner| inner.raw_from_value(-logical),
            .xor => |info| info.inner.raw_from_value(logical ^ info.mask),
            .offset => |info| info.inner.raw_from_value(logical + info.offset),
        };
    }

    /// Convert a "raw" value directly decoded by a Domain to a "logical" value as would appear in asm
    pub fn value_from_raw(self: Value, raw: i64) i64 {
        return switch (self) {
            .constant, .constant_dont_care => |v| v,
            .placeholder => raw,
            .negate => |inner| -inner.value_from_raw(raw),
            .xor => |info| info.inner.value_from_raw(raw) ^ info.mask,
            .offset => |info| info.inner.value_from_raw(raw) - info.offset,
        };
    }

    pub fn is_constant(self: Value) bool {
        return switch (self) {
            .constant, .constant_dont_care => true,
            .placeholder => false,
            .negate => |inner| inner.is_constant(),
            .xor => |info| info.inner.is_constant(),
            .offset => |info| info.inner.is_constant(),
        };
    }

    pub fn eql(a: Value, b: Value) bool {
        const atag: std.meta.Tag(Value) = a;
        const btag: std.meta.Tag(Value) = b;
        if (atag != btag) return false;
        return switch (a) {
            .constant => |c| c == b.constant,
            .constant_dont_care => |c| c == b.constant_dont_care,
            .placeholder => |info| info.eql(b.placeholder),
            .negate => |info| info.eql(b.negate.*),
            .xor => |info| info.mask == b.xor.mask and info.inner.eql(b.xor.inner.*),
            .offset => |info| info.offset == b.offset.offset and info.inner.eql(b.offset.inner.*),
        };
    }
};

pub const Domain = union (enum) {
    int: struct {
        signedness: std.builtin.Signedness,
        bits: u6,
        multiple: u8,
    },
    range: struct {
        first: i64,
        last: i64,
    },
    enumerated: []const i64,

    pub fn max_encoded(self: Domain) u64 {
        return switch (self) {
            .int => |info| (@as(u64, 1) << info.bits) - 1,
            .range => |range| @intCast(if (range.first < range.last) range.last - range.first else range.first - range.last),
            .enumerated => |values| values.len - 1,
        };
    }

    pub fn min_bits(self: Domain) Instruction.Encoded.Length_Bits {
        return switch (self) {
            .int => |info| info.bits,
            else => std.math.log2_int_ceil(u64, self.max_encoded()),
        };
    }

    /// Returns null if the provided value is outside the represented domain
    pub fn encode(self: Domain, value: i64) ?u64 {
        switch (self) {
            .int => |info| {
                const compressed = std.math.divExact(i64, value, info.multiple) catch return null;
                switch (info.signedness) {
                    .unsigned => {
                        if (compressed < 0) return null;
                        if (compressed >= @as(i64, 1) << info.bits) return null;
                    },
                    .signed => {
                        const limit = @as(i64, 1) << (info.bits - 1);
                        if (compressed < -limit) return null;
                        if (compressed >= limit) return null;
                    },
                }
                const mask = (@as(u64, 1) << info.bits) - 1;
                const unsigned: u64 = @bitCast(compressed);
                return unsigned & mask;
            },
            .range => |range| {
                if (range.first < range.last) {
                    return if (value >= range.first and value <= range.last) @bitCast(value - range.first) else null;
                } else {
                    return if (value >= range.last and value <= range.first) @bitCast(range.first - value) else null;
                }
            },
            .enumerated => |values| {
                for (0.., values) |i, v| {
                    if (v == value) return i;
                } else return null;
            },
        }
    }

    /// Returns null if the provided raw data would never be returned by a call to encode()
    pub fn decode(self: Domain, raw: u64) ?i64 {
        switch (self) {
            .int => |info| {
                const mask = (@as(u64, 1) << info.bits) - 1;
                if ((raw & mask) != raw) return null;
                switch (info.signedness) {
                    .unsigned => {
                        const compressed: i64 = @intCast(raw);
                        return compressed * info.multiple;
                    },
                    .signed => {
                        const sign: u1 = @truncate(raw >> (info.bits - 1));
                        if (sign == 1) {
                            const sign_extended = (~@as(u64, 0) << info.bits) | raw;
                            const compressed: i64 = @bitCast(sign_extended);
                            return compressed * info.multiple;
                        } else {
                            const compressed: i64 = @intCast(raw);
                            return compressed * info.multiple;
                        }
                    },
                }
            },
            .range => |range| {
                if (range.first < range.last) {
                    const diff: u64 = @intCast(range.last - range.first);
                    if (raw > diff) return null;
                    const int: i64 = @intCast(raw);
                    return range.first + int;
                } else {
                    const diff: u64 = @intCast(range.first - range.last);
                    if (raw > diff) return null;
                    const int: i64 = @intCast(raw);
                    return range.first - int;
                }
            },
            .enumerated => |values| {
                return if (raw < values.len) values[raw] else null;
            },
        }
    }

    pub fn eql(a: Domain, b: Domain) bool {
        const atag: std.meta.Tag(Domain) = a;
        const btag: std.meta.Tag(Domain) = b;
        if (atag != btag) return false;

        return switch (a) {
            .int => |info| std.meta.eql(info, b.int),
            .range => |info| std.meta.eql(info, b.range),
            .enumerated => |info| std.mem.eql(i64, info, b.enumerated),
        };
    }
};

const Encoder = @This();

const Placeholder = @import("Placeholder.zig");
const Parameter = @import("Parameter.zig");
const Instruction = @import("Instruction.zig");
const std = @import("std");
