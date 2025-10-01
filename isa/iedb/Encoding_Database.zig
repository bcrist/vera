lookup: deep_hash_map.DeepAutoHashMapUnmanaged(isa.Instruction.Signature, data.Compact_Range),

pub fn init(allocator: std.mem.Allocator) !Encoding_Database {
    var lookup: deep_hash_map.DeepAutoHashMapUnmanaged(isa.Instruction.Signature, data.Compact_Range) = .empty;
    errdefer lookup.deinit(allocator);

    var prev_signature: ?isa.Instruction.Signature = null;
    var range_start: u16 = 0;
    for (0.., data.mnemonics, data.param_ranges) |i, mnemonic, params_range| {
        const signature: isa.Instruction.Signature = .{
            .mnemonic = mnemonic,
            .params = data.param_signatures[params_range.offset..][0..params_range.len],
        };
        if (prev_signature) |prev| {
            if (!prev.eql(signature)) {
                try lookup.putNoClobber(allocator, prev, .{ .offset = range_start, .len = @intCast(i - range_start) });

                prev_signature = signature;
                range_start = @intCast(i);
            }
        } else {
            prev_signature = signature;
            range_start = @intCast(i);
        }
    }

    if (prev_signature) |signature| {
        try lookup.putNoClobber(allocator, signature, .{ .offset = range_start, .len = @intCast(data.mnemonics.len - range_start) });
    }

    return .{
        .lookup = lookup,
    };
}

pub fn deinit(self: *Encoding_Database, allocator: std.mem.Allocator) void {
    self.lookup.deinit(allocator);
}

pub fn find_matches(self: *const Encoding_Database, insn: isa.Instruction) Matching_Form_Iterator {
    const range: data.Compact_Range = self.lookup.getAdapted(insn, Instruction_Context{}) orelse .{ .offset = 0, .len = 0 };
    return .{
        .insn = insn,
        .n = range.offset,
        .e = range.offset + range.len,
    };
}

const Matching_Form_Iterator = struct {
    insn: isa.Instruction,
    n: u16,
    e: u16,

    pub fn next(self: *Matching_Form_Iterator) ?isa.Instruction.Form {
        for (self.n..self.e) |i| {
            const form = iedb.get(i);
            if (form.matches(self.insn)) {
                self.n = @intCast(i + 1);
                return form;
            }
        }
        self.n = self.e;
        return null;
    }
};

pub fn find_similar(self: *const Encoding_Database, buf: []isa.Instruction.Form, insn: isa.Instruction) []const isa.Instruction.Form {
    _ = self;
    _ = insn;

    // TODO sort all based on similarity with insn

    return buf[0..0];
}

const Instruction_Context = struct {
    pub fn hash(_: Instruction_Context, key: isa.Instruction) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, key.mnemonic);
        for (key.params) |p| {
            std.hash.autoHash(&hasher, p.signature);
        }
        std.hash.autoHash(&hasher, key.params.len);
        return hasher.final();
    }
    pub fn eql(_: Instruction_Context, a: isa.Instruction, b: isa.Instruction.Signature) bool {
        if (a.mnemonic != b.mnemonic or a.params.len != b.params.len) return false;
        for (a.params, b.params) |param, ps| {
            if (!std.meta.eql(param.signature, ps)) return false;
        }
        return true;
    }
};

const Encoding_Database = @This();

const iedb = @import("../iedb.zig");
const data = @import("iedb_data");
const isa = @import("isa");
const deep_hash_map = @import("deep_hash_map");
const std = @import("std");
