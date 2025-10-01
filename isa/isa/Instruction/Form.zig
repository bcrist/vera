//! Represents a set of machine code permutations that represent similar operations
//! All permutations of a form have the same instruction signature.
//! All permutations of a form use the same rules to encode/decode assembly language parameters to/from machine code (`encoders`)
//! The set of permutations represented and length of the instruction is mostly determined by the set of encoders used and their domain,
//! but Constraints may be used to shrink the set of permutations.

signature: Instruction.Signature,
constraints: []const Constraint,
encoders: []const Encoder, // If there are multiple encoders, each must correspond to a unique subset of the bits of the instruction.
// TODO priority: u32, // When assembling, if multiple Forms match a particular instruction, the one with the priority closest to 0 is selected.


// maybe the rest of this should be in a separate struct?  Form.Meta?  since it's not needed most of the time
// TODO name
// TODO description
// TODO number of values popped
// TODO number of values pushed
// TODO requires super flag?

pub fn bits(self: Form) Instruction.Encoded.Length_Bits {
    var n: Instruction.Encoded.Length_Bits = 0;
    for (self.encoders) |enc| {
        n = @max(n, enc.required_bits());
    }
    return n;
}

pub fn len(self: Form) Instruction.Encoded.Length_Bytes {
    return @intCast(self.bits() / 8);
}

pub fn placeholders(self: Form, allocator: std.mem.Allocator) ![]const Placeholder {
    const Key = struct {
        param: Parameter.Index,
        kind: Placeholder.Kind,
    };
    var map = std.AutoHashMap(Key, Placeholder).init(allocator);
    defer map.deinit();

    for (self.encoders) |enc| {
        if (enc.value.get_placeholder_info()) |pi| {
            const k: Key = .{
                .param = pi.param,
                .kind = pi.kind,
            };
            const result = try map.getOrPut(k);

            if (!result.found_existing) {
                result.key_ptr.* = k;
                result.value_ptr.* = pi;
            }
        }
    }

    const results = try allocator.alloc(Placeholder, map.count());
    errdefer allocator.free(results);

    var iter = map.valueIterator();
    var i: usize = 0;
    while (iter.next()) |v| {
        results[i] = v.*;
        i += 1;
    }

    const Sort_Context = struct {
        pub fn less_than(_: void, a: Placeholder, b: Placeholder) bool {
            if (a.param != b.param) return a.param.raw() < b.param.raw();
            return @intFromEnum(a.kind) < @intFromEnum(b.kind);
        }
    };

    std.sort.pdq(Placeholder, results, {}, Sort_Context.less_than);
    return results;
}

pub fn matches(self: Form, insn: Instruction) bool {
    if (self.signature.mnemonic != insn.mnemonic) return false;
    if (self.signature.params.len != insn.params.len) return false;
    for (self.signature.params, insn.params) |ps, param| {
        if (!std.meta.eql(ps, param.signature)) return false;
    }
    for (self.encoders) |enc| {
        if (enc.value == .placeholder and enc.value.placeholder.param == .invalid) continue;
        const value = enc.value.evaluate(insn.params);
        if (enc.domain.encode(value) == null) return false;
    }
    for (self.constraints) |constraint| {
        if (!constraint.matches(insn.params)) return false;
    }
    return true;
}

pub fn encode(self: Form, insn: Instruction, default_value: Instruction.Encoded.Data) Instruction.Encoded {
    std.debug.assert(self.signature.mnemonic == insn.mnemonic);

    std.debug.assert(self.signature.params.len == insn.params.len);
    for (self.signature.params, insn.params) |ps, param| {
        std.debug.assert(std.meta.eql(ps, param.signature));
    }

    for (self.constraints) |constraint| {
        std.debug.assert(constraint.matches(insn.params));
    }

    var out: Instruction.Encoded = .{
        .data = default_value,
        .len = self.len(),
    };

    for (self.encoders) |enc| {
        const success = enc.encode(insn, &out.data);
        std.debug.assert(success);
    }

    return out;
}

pub fn matches_data(self: Form, data: Instruction.Encoded.Data) bool {
    var temp: [Parameter.Index.count]Parameter = undefined;
    var decoded_base_register = [_]bool { false } ** Parameter.Index.count;
    var decoded_offset_register = [_]bool { false } ** Parameter.Index.count;
    var decoded_constant = [_]bool { false } ** Parameter.Index.count;

    for (self.encoders) |enc| {
        if (enc.value.get_placeholder()) |info| {
            // Multiple encoders might be attached to the same constant/register index within a parameter.
            // In this case, we need to make sure that the encoders write the same value to that constant/register index.
            // Otherwise, this instruction encoding does not match this instruction data.
            const index = info.param.raw();
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
                    if (decoded_base_register[index]) {
                        if (old.base_register != new.base_register) return false;
                    } else {
                        decoded_base_register[index] = true;
                    }
                },
                .param_offset_register => {
                    if (decoded_offset_register[index]) {
                        if (old.offset_register != new.offset_register) return false;
                    } else {
                        decoded_offset_register[index] = true;
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
            if (constraint.left.get_placeholder()) |info| {
                const index = info.param.raw();
                const already_decoded = switch (info.kind) {
                    .param_constant => decoded_constant[index],
                    .param_base_register => decoded_base_register[index],
                    .param_offset_register => decoded_offset_register[index],
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

pub fn decode_params(self: Form, data: Instruction.Encoded.Data, params: []Parameter) void {
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

pub fn eql(self: Form, other: Form) bool {
    return deep_hash_map.deepEql(self, other, .DeepRecursive);
}

pub fn format(self: Form, writer: *std.io.Writer) !void {
    try fmt.print_form(self, writer);
}

const Form = @This();

const Constraint = @import("../Constraint.zig");
const Placeholder = @import("../Placeholder.zig");
const Encoder = @import("../Encoder.zig");
const Parameter = @import("../Parameter.zig");
const Instruction = @import("../Instruction.zig");
const fmt = @import("../fmt.zig");
const arch = @import("arch");
const deep_hash_map = @import("deep_hash_map");
const Signedness = std.builtin.Signedness;
const std = @import("std");
