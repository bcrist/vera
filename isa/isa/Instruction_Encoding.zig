signature: Instruction.Signature,
constraints: []const Constraint,
encoders: []const Encoder, // If there are multiple encoders, each must correspond to a unique subset of the bits of the instruction.

pub const Value = union (enum) {
    constant: i64,
    placeholder: Placeholder_Info,
    negate: *const Value,
    xor: struct {
        inner: *const Value,
        mask: i64,
    },
    offset: struct {
        inner: *const Value,
        offset: i64,
    },

    pub fn get_placeholder_info(self: Value) ?Placeholder_Info {
        return switch (self) {
            .constant => null,
            .placeholder => |info| info,
            .negate => |inner| inner.get_placeholder_info(),
            .xor => |info| info.inner.get_placeholder_info(),
            .offset => |info| info.inner.get_placeholder_info(),
        };
    }

    pub fn evaluate(self: Value, params: []const Parameter) i64 {
        return switch (self) {
            .constant => |v| v,
            .placeholder => |info| info.evaluate(params),
            .negate => |inner| -inner.evaluate(params),
            .xor => |info| info.inner.evaluate(params) ^ info.mask,
            .offset => |info| info.inner.evaluate(params) - info.offset,
        };
    }

    pub fn assign(self: Value, value: i64, out: []Parameter) bool {
        return switch (self) {
            .constant => |v| v == value,
            .placeholder => |info| info.assign(value, out),
            .negate => |inner| inner.assign(-value, out),
            .xor => |info| info.inner.assign(value ^ info.mask, out),
            .offset => |info| info.inner.assign(value + info.offset, out),
        };
    }

    /// Convert a "logical" value (as would appear in asm) to a "raw" value that can be encoded by a Domain
    pub fn raw_from_value(self: Value, logical: i64) i64 {
        return switch (self) {
            .constant => |v| v,
            .placeholder => logical,
            .negate => |inner| inner.raw_from_value(-logical),
            .xor => |info| info.inner.raw_from_value(logical ^ info.mask),
            .offset => |info| info.inner.raw_from_value(logical + info.offset),
        };
    }

    /// Convert a "raw" value directly decoded by a Domain to a "logical" value as would appear in asm
    pub fn value_from_raw(self: Value, raw: i64) i64 {
        return switch (self) {
            .constant => |v| v,
            .placeholder => raw,
            .negate => |inner| -inner.value_from_raw(raw),
            .xor => |info| info.inner.value_from_raw(raw) ^ info.mask,
            .offset => |info| info.inner.value_from_raw(raw) - info.offset,
        };
    }

    pub fn is_constant(self: Value) bool {
        return switch (self) {
            .constant => true,
            .placeholder => false,
            .negate => |inner| inner.is_constant(),
            .xor => |info| info.inner.is_constant(),
            .offset => |info| info.inner.is_constant(),
        };
    }
};

pub const Placeholder_Info = struct {
    index: isa.Parameter.Index,
    kind: Placeholder_Kind,
    name: []const u8,

    pub fn evaluate(self: Placeholder_Info, params: []const Parameter) i64 {
        const index = self.index.raw();
        if (params.len <= index) return 0; // this is part of a "don't care" encoder
        return switch (self.kind) {
            .param_constant => params[index].constant,
            .param_base_register => params[index].base_register_index,
            .param_offset_register => params[index].offset_register_index,
        };
    }

    pub fn assign(self: Placeholder_Info, value: i64, out: []Parameter) bool {
        const index = self.index.raw();
        if (out.len <= index) return true; // this is part of a "don't care" encoder
        switch (self.kind) {
            .param_constant => out[index].constant = value,
            .param_base_register => {
                if (value < 0 or value >= arch.register_count) return false;
                out[index].base_register_index = @intCast(value);
            },
            .param_offset_register => {
                if (value < 0 or value >= arch.register_count) return false;
                out[index].offset_register_index = @intCast(value);
            },
        }
        return true;
    }
};

pub const Placeholder_Kind = enum {
    param_base_register,
    param_offset_register,
    param_constant,
};

pub const Constraint = struct {
    left: Value,
    right: Value,
    kind: Kind,

    pub const Kind = enum {
        equal,
        not_equal,
        greater,
        greater_or_equal,
    };

    pub fn matches(self: Constraint, params: []const Parameter) bool {
        const left = self.left.evaluate(params);
        const right = self.right.evaluate(params);
        return switch (self.kind) {
            .equal => left == right,
            .not_equal => left != right,
            .greater => left > right,
            .greater_or_equal => left >= right,
        };
    }
};

pub const Domain = union (enum) {
    int: struct {
        signedness: Signedness,
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

    pub fn min_bits(self: Domain) Encoded_Instruction.Bit_Length_Type {
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
};

pub fn bits(self: Instruction_Encoding) Encoded_Instruction.Bit_Length_Type {
    var n: Encoded_Instruction.Bit_Length_Type = 0;
    for (self.encoders) |enc| {
        n = @max(n, enc.required_bits());
    }
    return n;
}

pub fn len(self: Instruction_Encoding) Encoded_Instruction.Length_Type {
    return @intCast(self.bits() / 8);
}

pub fn placeholders(self: Instruction_Encoding, allocator: std.mem.Allocator) ![]const Placeholder_Info {
    const Key = struct {
        index: isa.Parameter.Index,
        kind: Placeholder_Kind,
    };
    var map = std.AutoHashMap(Key, Placeholder_Info).init(allocator);
    defer map.deinit();

    for (self.encoders) |enc| {
        if (enc.value.get_placeholder_info()) |pi| {
            const k: Key = .{
                .index = pi.index,
                .kind = pi.kind,
            };
            const result = try map.getOrPut(k);

            if (!result.found_existing) {
                result.key_ptr.* = k;
                result.value_ptr.* = pi;
            }
        }
    }

    const results = try allocator.alloc(Placeholder_Info, map.count());
    errdefer allocator.free(results);

    var iter = map.valueIterator();
    var i: usize = 0;
    while (iter.next()) |v| {
        results[i] = v.*;
        i += 1;
    }

    const Sort_Context = struct {
        pub fn less_than(_: void, a: Placeholder_Info, b: Placeholder_Info) bool {
            if (a.index != b.index) return a.index.raw() < b.index.raw();
            return @intFromEnum(a.kind) < @intFromEnum(b.kind);
        }
    };

    std.sort.pdq(Placeholder_Info, results, {}, Sort_Context.less_than);
    return results;
}

pub fn matches(self: Instruction_Encoding, insn: Instruction) bool {
    if (self.signature.mnemonic != insn.mnemonic) return false;
    if (self.signature.suffix != insn.suffix) return false;
    if (self.signature.params.len != insn.params.len) return false;
    for (self.signature.params, insn.params) |ps, param| {
        if (!std.meta.eql(ps, param.signature)) return false;
    }
    for (self.encoders) |enc| {
        if (enc.value == .placeholder and enc.value.placeholder.index == .invalid) continue;
        const value = enc.value.evaluate(insn.params);
        if (enc.domain.encode(value) == null) return false;
    }
    for (self.constraints) |constraint| {
        if (!constraint.matches(insn.params)) return false;
    }
    return true;
}

pub fn encode(self: Instruction_Encoding, insn: Instruction, default_value: Encoded_Instruction.Data) Encoded_Instruction {
    std.debug.assert(self.signature.mnemonic == insn.mnemonic);
    std.debug.assert(self.signature.suffix == insn.suffix);

    std.debug.assert(self.signature.params.len == insn.params.len);
    for (self.signature.params, insn.params) |ps, param| {
        std.debug.assert(std.meta.eql(ps, param.signature));
    }

    for (self.constraints) |constraint| {
        std.debug.assert(constraint.matches(insn.params));
    }

    var out: Encoded_Instruction = .{
        .data = default_value,
        .len = self.len(),
    };

    for (self.encoders) |enc| {
        const success = enc.encode(insn, &out.data);
        std.debug.assert(success);
    }

    return out;
}

pub fn matches_data(self: Instruction_Encoding, data: Encoded_Instruction.Data) bool {
    var temp: [Parameter.Index.count]Parameter = undefined;
    var decoded_base_register_index = [_]bool { false } ** Parameter.Index.count;
    var decoded_offset_register_index = [_]bool { false } ** Parameter.Index.count;
    var decoded_constant = [_]bool { false } ** Parameter.Index.count;

    for (self.encoders) |enc| {
        if (enc.value.get_placeholder_info()) |info| {
            // Multiple encoders might be attached to the same constant/register index within a parameter.
            // In this case, we need to make sure that the encoders write the same value to that constant/register index.
            // Otherwise, this instruction encoding does not match this instruction data.
            const index = info.index.raw();
            const old = temp[index];
            if (!enc.decode(data, &temp)) return false;
            const new = temp[index];
            switch (info.kind) {
                .param_constant => {
                    if (decoded_constant[index]) {
                        if (old.constant != new.constant) return false;
                    } else {
                        decoded_constant[index] = true;
                    }
                },
                .param_base_register => {
                    if (decoded_base_register_index[index]) {
                        if (old.base_register_index != new.base_register_index) return false;
                    } else {
                        decoded_base_register_index[index] = true;
                    }
                },
                .param_offset_register => {
                    if (decoded_offset_register_index[index]) {
                        if (old.offset_register_index != new.offset_register_index) return false;
                    } else {
                        decoded_offset_register_index[index] = true;
                    }
                },
            }
        } else {
            if (!enc.decode(data, &temp)) return false;
        }
    }

    for (self.constraints) |constraint| {
        if (constraint.kind == .equal) {
            // .equal constraints can be assumed to match if the .left side is not populated by any encoders.
            // To check this, we keep track of which parameter data has been decoded explicitly by the encoders,
            // then for any constraints where the left side hasn't been decoded yet, copy it from the right side.
            if (constraint.left.get_placeholder_info()) |info| {
                const index = info.index.raw();
                const already_decoded = switch (info.kind) {
                    .param_constant => decoded_constant[index],
                    .param_base_register => decoded_base_register_index[index],
                    .param_offset_register => decoded_offset_register_index[index],
                };
                if (already_decoded) continue;
            }
            const ok = constraint.left.assign(constraint.right.evaluate(&temp), &temp);
            std.debug.assert(ok);
        }
    }

    for (self.constraints) |constraint| {
        if (!constraint.matches(&temp)) return false;
    }

    return true;
}

pub fn decode_params(self: Instruction_Encoding, data: Encoded_Instruction.Data, params: []Parameter) void {
    // N.B. this assumes that .base_register, .offset_register, and .constant have been set to 0 for all parameters

    std.debug.assert(self.signature.params.len == params.len);

    for (self.signature.params, params) |ps, param| {
        std.debug.assert(std.meta.eql(ps, param.signature));
    }

    for (self.encoders) |enc| {
        const ok = enc.decode(data, params);
        std.debug.assert(ok);
    }

    for (self.constraints) |constraint| {
        if (constraint.kind == .equal) {
            const ok = constraint.left.assign(constraint.right.evaluate(params), params);
            std.debug.assert(ok);
        }
    }
}

pub fn eql(self: Instruction_Encoding, other: Instruction_Encoding) bool {
    return deep_hash_map.deepEql(self, other, .DeepRecursive);
}

pub fn format(self: Instruction_Encoding, writer: *std.io.Writer) !void {
    try isa.print.print_encoding(self, writer);
}

pub const Encoder = @import("Encoder.zig");
pub const Placeholder_Restrictions_Iterator = @import("Placeholder_Restrictions_Iterator.zig");

const Instruction_Encoding = @This();
const Instruction = isa.Instruction;
const Parameter = isa.Parameter;
const Mnemonic = isa.Mnemonic;
const Encoded_Instruction = isa.Encoded_Instruction;
const isa = @import("../isa.zig");
const arch = @import("arch");
const deep_hash_map = @import("deep_hash_map");
const Signedness = std.builtin.Signedness;
const std = @import("std");
