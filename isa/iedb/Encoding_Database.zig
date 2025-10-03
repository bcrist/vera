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

    if (buf.len == 0) return &.{};

    var results = std.ArrayList(isa.Instruction.Form).initBuffer(buf);

    const discard_threshold_adder = 1;
    const discard_threshold_multiplier = 1.1;

    var best_score: f32 = 0;
    var worst_score: f32 = 0;
    for (0..data.mnemonics.len) |id| {
        const form = iedb.get(id);
        const score = insn_difference_score(insn, form);

        if (results.items.len == 0) {
            best_score = score;
            worst_score = score;
            results.insertAssumeCapacity(0, form);
            continue;
        }

        if (score < best_score) {
            best_score = score;
            const discard_threshold = (score + discard_threshold_adder) * discard_threshold_multiplier;
            if (worst_score > discard_threshold or results.items.len == buf.len) {
                var last_score = score;
                var i: usize = 0;
                while (i < results.items.len) {
                    const item_score = insn_difference_score(insn, results.items[i]);
                    if (item_score > discard_threshold or i == buf.len - 1) {
                        results.items.len = i;
                    } else {
                        last_score = item_score;
                        i += 1;
                    }
                }
                worst_score = last_score;
            }

            results.insertAssumeCapacity(0, form);
            continue;
        }

        if (score < worst_score) {
            if (results.items.len == buf.len) {
                results.items.len -= 1;
            }

            var i = results.items.len;
            while (i > 0) : (i -= 1) {
                const item_score = insn_difference_score(insn, results.items[i - 1]);
                if (item_score <= score) {
                    results.insertAssumeCapacity(i, form);
                    break;
                }
            } else unreachable; // should have been handled as the score < best_score case.

            if (i + 1 == results.items.len) {
                worst_score = score;
            }
        }

        if (results.items.len < buf.len and score < (best_score + discard_threshold_adder) * discard_threshold_multiplier) {
            worst_score = score;
            results.appendAssumeCapacity(form);
        }
    }

    return results.items;
}

fn insn_difference_score(insn: isa.Instruction, form: isa.Instruction.Form) f32 {
    var mnemonic_score: usize = 0;

    if (insn.mnemonic != form.signature.mnemonic) {
        mnemonic_score += 20;
        mnemonic_score += levenshtein_distance(insn.mnemonic.name(), form.signature.mnemonic.name());
    }

    var params_score: usize = 0;

    const min_params = @min(insn.params.len, form.signature.params.len);
    const max_params = @max(insn.params.len, form.signature.params.len);
    if (min_params != max_params) {
        params_score += 5 * (max_params - min_params);
    }

    for (insn.params[0..min_params], form.signature.params[0..min_params]) |ip, fp| {
        if (!std.meta.eql(ip.signature.base, fp.base)) params_score += 1;
        if (!std.meta.eql(ip.signature.offset, fp.offset)) params_score += 1;
        if (!std.meta.eql(ip.signature.address_space, fp.address_space)) params_score += 1;
    }

    if (params_score > 0) {
        params_score += 10;
    }

    var constraints_score: usize = 0;
    if (params_score == 0) {
        var encoder_iter = form.encoders();
        while (encoder_iter.next()) |enc| {
            if (enc.value == .placeholder and enc.value.placeholder.param == .invalid) continue;
            const value = enc.value.evaluate(insn.params);
            if (enc.domain.encode(value) == null) {
                constraints_score += 1;
            }
        }
        for (form.constraints) |constraint| {
            if (!constraint.matches(insn.params)) {
                constraints_score += 1;
            }
        }
    } else {
        constraints_score = 10;
    }

    return @floatFromInt(mnemonic_score + params_score + @min(10, constraints_score));
}

fn levenshtein_distance(a: []const u8, b: []const u8) usize {
    std.debug.assert(a.len <= 16);
    std.debug.assert(b.len <= 16);

    if (a.len == 0) return b.len;
    if (b.len == 0) return a.len;

    const str1, const str2 = if (a.len <= b.len) .{ a, b } else .{ b, a };

    var prev_buf: [17]u8 = undefined;
    var cur_buf: [17]u8 = undefined;

    var prev = prev_buf[0 .. str1.len + 1];
    var cur = cur_buf[0 .. str1.len + 1];

    for (0.., prev) |i, *dist| {
        dist.* = @intCast(i);
    }

    for (0.., str2) |j, ch2| {
        cur[0] = @intCast(j + 1);

        for (0.., str1) |i, ch1| {
            const cost = @intFromBool(ch1 != ch2);

            cur[i + 1] = @min(
                prev[i + 1] + 1, // deletion
                cur[i] + 1, // insertion
                prev[i] + cost, // substitution
            );
        }

        std.mem.swap([]u8, &prev, &cur);
    }

    return prev[str1.len];
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
