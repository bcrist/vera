lookup: deep_hash_map.DeepAutoHashMapUnmanaged(Instruction_Signature, []const Instruction_Encoding) = .{},
transforms: deep_hash_map.DeepAutoHashMapUnmanaged(Instruction_Signature, []const Instruction_Transform) = .{},
all_encodings: []const *const Instruction_Encoding = &.{}, // used for similar_encodings

pub fn matching_encodings(self: Encoding_Database, insn: Instruction) Matching_Encoding_Iterator {
    var iter: Matching_Encoding_Iterator = .{
        .direct = .{
            .insn = insn,
            .remaining = self.lookup.getAdapted(insn, Instruction_Context{}) orelse &.{},
        },
        .transformed = .{
            .insn = insn,
            .remaining = &.{},
        },
        .transformed_params = undefined,
    };

    const transforms: []const Instruction_Transform = self.transforms.getAdapted(insn, Instruction_Context{}) orelse &.{};
    for (transforms) |transform| {
        if (transform.try_transform(insn, &iter.transformed.insn, &iter.transformed_params)) {
            iter.transformed.remaining = self.lookup.getAdapted(iter.transformed.insn, Instruction_Context{}) orelse &.{};
            break;
        }
    }

    return iter;
}

const Matching_Encoding_Iterator = struct {
    direct: Matching_Encoding_Iterator_Raw,
    transformed: Matching_Encoding_Iterator_Raw,
    transformed_params: [Parameter.Index.count]Parameter,

    pub fn next(self: *Matching_Encoding_Iterator) ?Instruction_Encoding {
        if (self.transformed.insn.params.ptr != &self.transformed_params) {
            self.transformed.insn.params.ptr = &self.transformed_params;
        }
        if (self.direct.next()) |direct| {
            if (self.transformed.next()) |transformed| {
                if (direct.len() <= transformed.len()) {
                    self.direct.consume();
                    return direct;
                } else {
                    self.transformed.consume();
                    return transformed;
                }
            } else {
                self.direct.consume();
                return direct;
            }
        } else if (self.transformed.next()) |transformed| {
            self.transformed.consume();
            return transformed;
        } else {
            return null;
        }
    }

    pub fn next_ptr(self: *Matching_Encoding_Iterator) ?*const Instruction_Encoding {
        if (self.transformed.insn.params.ptr != &self.transformed_params) {
            self.transformed.insn.params.ptr = &self.transformed_params;
        }
        if (self.direct.next()) |direct| {
            if (self.transformed.next()) |transformed| {
                if (direct.len() <= transformed.len()) {
                    const ptr = &self.direct.remaining[0];
                    self.direct.consume();
                    return ptr;
                } else {
                    const ptr = &self.transformed.remaining[0];
                    self.transformed.consume();
                    return ptr;
                }
            } else {
                const ptr = &self.direct.remaining[0];
                self.direct.consume();
                return ptr;
            }
        } else if (self.transformed.next() != null) {
            const ptr = &self.transformed.remaining[0];
            self.transformed.consume();
            return ptr;
        } else {
            return null;
        }
    }
};

const Matching_Encoding_Iterator_Raw = struct {
    insn: Instruction,
    remaining: []const Instruction_Encoding,

    pub fn next(self: *Matching_Encoding_Iterator_Raw) ?Instruction_Encoding {
        while (self.remaining.len > 0) {
            const encoding = self.remaining[0];
            if (encoding.matches(self.insn)) {
                return encoding;
            } else {
                self.consume();
            }
        }
        return null;
    }

    pub fn consume(self: *Matching_Encoding_Iterator_Raw) void {
        self.remaining = self.remaining[1..];
    }
};

pub fn similar_encodings(self: Encoding_Database, insn: Instruction, limit: usize) Similar_Encoding_Iterator {
    _ = insn;
    const all = self.all_encodings;

    // sort all based on similarity with insn

    return .{
        .remaining = all[0..@min(limit, all.len)],
    };
}

pub const Similar_Encoding_Iterator = struct {
    remaining: []const *const Instruction_Encoding,

    pub fn next(self: *Similar_Encoding_Iterator) ?Instruction_Encoding {
        while (self.remaining.len > 0) {
            const encoding = self.remaining[0];
            self.remaining = self.remaining[1..];
            return encoding.*;
        }
        return null;
    }
    pub fn next_ptr(self: *Similar_Encoding_Iterator) ?*const Instruction_Encoding {
        while (self.remaining.len > 0) {
            const encoding = &self.remaining[0];
            self.remaining = self.remaining[1..];
            return encoding;
        }
        return null;
    }
};

const Instruction_Context = struct {
    pub fn hash(_: Instruction_Context, key: Instruction) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, key.mnemonic);
        std.hash.autoHash(&hasher, key.suffix);
        for (key.params) |p| {
            std.hash.autoHash(&hasher, p.signature);
        }
        std.hash.autoHash(&hasher, key.params.len);
        return hasher.final();
    }
    pub fn eql(_: Instruction_Context, a: Instruction, b: Instruction_Signature) bool {
        if (a.mnemonic != b.mnemonic or a.suffix != b.suffix or a.params.len != b.params.len) return false;
        for (a.params, b.params) |param, ps| {
            if (!std.meta.eql(param.signature, ps)) return false;
        }
        return true;
    }
};

const Encoding_Database = @This();
const Instruction = isa.Instruction;
const Parameter = isa.Parameter;
const Instruction_Encoding = isa.Instruction_Encoding;
const Instruction_Transform = isa.Instruction_Transform;
const Instruction_Signature = isa.Instruction_Signature;
const isa = @import("../isa.zig");
const deep_hash_map = @import("deep_hash_map");
const std = @import("std");
