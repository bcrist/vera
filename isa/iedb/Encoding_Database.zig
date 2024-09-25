lookup: deep_hash_map.DeepAutoHashMapUnmanaged(Instruction.Signature, []const Instruction_Encoding) = .{},
all_encodings: []const *const Instruction_Encoding = &.{}, // used for similar_encodings

pub fn matching_encodings(self: Encoding_Database, insn: Instruction) Matching_Encoding_Iterator {
    return .{
        .insn = insn,
        .remaining = self.lookup.getAdapted(insn, Instruction_Context{}) orelse &.{},
    };
}

const Matching_Encoding_Iterator = struct {
    insn: Instruction,
    remaining: []const Instruction_Encoding,

    pub fn next(self: *Matching_Encoding_Iterator) ?Instruction_Encoding {
        while (self.remaining.len > 0) {
            const encoding = self.remaining[0];
            self.remaining = self.remaining[1..];
            if (encoding.matches(self.insn)) {
                return encoding;
            }
        }
        return null;
    }

    pub fn next_ptr(self: *Matching_Encoding_Iterator) ?*const Instruction_Encoding {
        while (self.remaining.len > 0) {
            const encoding = &self.remaining[0];
            self.remaining = self.remaining[1..];
            if (encoding.matches(self.insn)) {
                return encoding;
            }
        }
        return null;
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
    pub fn eql(_: Instruction_Context, a: Instruction, b: Instruction.Signature) bool {
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
const isa = @import("isa");
const deep_hash_map = @import("deep_hash_map");
const std = @import("std");
