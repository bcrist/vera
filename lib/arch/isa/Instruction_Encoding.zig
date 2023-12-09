signature: isa.Instruction_Signature,
constraints: []const Constraint,
encoders: []const Encoder, // If there are multiple encoders, each must correspond to a unique subset of the bits of the instruction.

pub const Value = union (enum) {
    constant: i64,
    placeholder: Placeholder_Info,

    pub fn evaluate(self: Value, params: []const Parameter) i64 {
        return switch (self) {
            .constant => |v| v,
            .placeholder => |info| info.evaluate(params),
        };
    }

    pub fn assign(self: Value, value: i64, out: []Parameter) bool {
        switch (self) {
            .constant => |v| return v == value,
            .placeholder => |info| return info.assign(value, out),
        }
        return true;
    }
};

pub const Placeholder_Info = struct {
    index: isa.Parameter.Index,
    kind: Placeholder_Kind,
    name: []const u8,

    pub fn evaluate(self: Placeholder_Info, params: []const Parameter) i64 {
        return switch (self.kind) {
            .param_constant => params[self.index.raw()].constant,
            .param_base_register => params[self.index.raw()].base_register_index,
            .param_offset_register => params[self.index.raw()].offset_register_index,
        };
    }

    pub fn assign(self: Placeholder_Info, value: i64, out: []Parameter) bool {
        switch (self.kind) {
            .param_constant => out[self.index.raw()].constant = value,
            .param_base_register => {
                if (value < 0 or value >= hw.register_count) return false;
                out[self.index.raw()].base_register_index = @intCast(value);
            },
            .param_offset_register => {
                if (value < 0 or value >= hw.register_count) return false;
                out[self.index.raw()].offset_register_index = @intCast(value);
            },
        }
        return true;
    }
};

pub const Placeholder_Kind = enum {
    param_constant,
    param_base_register,
    param_offset_register,
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

pub const Encoder = @import("Encoder.zig");

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

pub fn matches(self: Instruction_Encoding, insn: Instruction) bool {
    if (self.signature.mnemonic != insn.mnemonic) return false;
    if (self.signature.suffix != insn.suffix) return false;
    if (self.signature.params.len != insn.params.len) return false;
    for (self.signature.params, insn.params) |ps, param| {
        if (!std.meta.eql(ps, param.signature)) return false;
    }
    for (self.encoders) |enc| {
        const value = enc.value.evaluate(insn.params);
        if (enc.domain.encode(value) == null) return false;
    }
    for (self.constraints) |constraint| {
        if (!constraint.matches(insn.params)) return false;
    }
    return true;
}

pub fn encode(self: Instruction_Encoding, insn: Instruction) Encoded_Instruction {
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
        .data = 0,
        .len = self.len(),
    };

    for (self.encoders) |enc| {
        const success = enc.encode(insn, &out.data);
        std.debug.assert(success);
    }

    return out;
}

// .equal constraints can be assumed to match if the .left side is not populated by any encoders.
// To check this, we first decode the encoded parameters, then assign all the left sides of .equal
// constraints to their right side values.  In the case that both sides of an .equal constraint
// reference numbers that were decoded directly, this would normally mean we'd get some
// false-positive matches, since we've overwritten the decoded data on the left with the decoded
// data on the right.  To avoid that we re-decode the values from encoders again afterward.
// Finally we can check all the constraints.
//
// @Speed: It might be better to just keep track of which parameter values have been decoded,
// and then only assign constraints that deal with unencoded parameter values.
pub fn matches_data(self: Instruction_Encoding, data: Encoded_Instruction.Data) bool {
    var temp: [Parameter.Index.count]Parameter = undefined;

    for (self.encoders) |enc| {
        if (!enc.decode(data, &temp)) return false;
    }

    for (self.constraints) |constraint| {
        if (constraint.kind == .equal) {
            const ok = constraint.left.assign(constraint.right.evaluate(&temp), &temp);
            std.debug.assert(ok);
        }
    }

    for (self.encoders) |enc| {
        if (!enc.decode(data, &temp)) return false;
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

const Instruction_Encoding = @This();
const Instruction = isa.Instruction;
const Parameter = isa.Parameter;
const Mnemonic = isa.Mnemonic;
const Encoded_Instruction = isa.Encoded_Instruction;
const isa = @import("../isa.zig");
const hw = @import("../hardware.zig");
const deep_hash_map = @import("deep_hash_map");
const Signedness = std.builtin.Signedness;
const std = @import("std");
