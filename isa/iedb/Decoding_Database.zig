forms: std.ArrayList(u16),
lookup_8b: std.AutoHashMapUnmanaged(u8, data.Compact_Range),
lookup_12b: std.AutoHashMapUnmanaged(u12, data.Compact_Range),
lookup_16b: std.AutoHashMapUnmanaged(u16, data.Compact_Range),
lookup_extra: data.Compact_Range,

pub fn init(allocator: std.mem.Allocator, temp: std.mem.Allocator) !Decoding_Database {
    var forms: std.ArrayList(u16) = .empty;
    errdefer forms.deinit(allocator);

    var temp_8b: std.AutoHashMapUnmanaged(u8, std.ArrayListUnmanaged(u16)) = .empty;
    defer {
        var iter = temp_8b.valueIterator();
        while (iter.next()) |ptr| ptr.deinit(temp);
        temp_8b.deinit(temp);
    }

    var temp_12b: std.AutoHashMapUnmanaged(u12, std.ArrayListUnmanaged(u16)) = .empty;
    defer {
        var iter = temp_12b.valueIterator();
        while (iter.next()) |ptr| ptr.deinit(temp);
        temp_12b.deinit(temp);
    }

    var temp_16b: std.AutoHashMapUnmanaged(u16, std.ArrayListUnmanaged(u16)) = .empty;
    defer {
        var iter = temp_16b.valueIterator();
        while (iter.next()) |ptr| ptr.deinit(temp);
        temp_16b.deinit(temp);
    }

    for (0..data.mnemonics.len) |i| {
        const form = iedb.get(i);
        var encoder_iter = form.encoders();
        while (encoder_iter.next()) |enc| {
            if (enc.value == .constant and enc.bit_offset == 0) {
                var encoded: isa.Instruction.Encoded.Data = 0;
                const ok = enc.encode(.{ .mnemonic = .init(""), .params = &.{} }, &encoded);
                std.debug.assert(ok);
                const bits = enc.required_bits();
                if (bits >= 16) {
                    const prefix: u16 = @truncate(encoded);
                    const gop = try temp_16b.getOrPut(temp, prefix);
                    if (!gop.found_existing) {
                        gop.key_ptr.* = prefix;
                        gop.value_ptr.* = .empty;
                    }
                    try gop.value_ptr.append(temp, @intCast(i));
                    break;
                } else if (bits >= 12) {
                    const prefix: u12 = @truncate(encoded);
                    const result = try temp_12b.getOrPut(temp, prefix);
                    if (!result.found_existing) {
                        result.key_ptr.* = prefix;
                        result.value_ptr.* = .empty;
                    }
                    try result.value_ptr.append(temp, @intCast(i));
                    break;
                } else if (bits >= 8) {
                    const prefix: u8 = @truncate(encoded);
                    const result = try temp_8b.getOrPut(temp, prefix);
                    if (!result.found_existing) {
                        result.key_ptr.* = prefix;
                        result.value_ptr.* = .empty;
                    }
                    try result.value_ptr.append(temp, @intCast(i));
                    break;
                }
            }
        } else {
            try forms.append(allocator, @intCast(i));
        }
    }

    const extra: data.Compact_Range = .{
        .offset = 0,
        .len = @intCast(forms.items.len),
    };

    var lookup_8b: std.AutoHashMapUnmanaged(u8, data.Compact_Range) = .empty;
    errdefer lookup_8b.deinit(allocator);
    {
        try lookup_8b.ensureTotalCapacity(allocator, temp_8b.size);
        var iter = temp_8b.iterator();
        while (iter.next()) |entry| {
            const range: data.Compact_Range = .{
                .offset = @intCast(forms.items.len),
                .len = @intCast(entry.value_ptr.items.len),
            };
            try forms.appendSlice(allocator, entry.value_ptr.items);
            lookup_8b.putAssumeCapacityNoClobber(entry.key_ptr.*, range);
        }
        var viter = temp_8b.valueIterator();
        while (viter.next()) |ptr| ptr.deinit(temp);
        temp_8b.clearAndFree(temp);
    }

    var lookup_12b: std.AutoHashMapUnmanaged(u12, data.Compact_Range) = .empty;
    errdefer lookup_12b.deinit(allocator);
    {
        try lookup_12b.ensureTotalCapacity(allocator, temp_12b.size);
        var iter = temp_12b.iterator();
        while (iter.next()) |entry| {
            const range: data.Compact_Range = .{
                .offset = @intCast(forms.items.len),
                .len = @intCast(entry.value_ptr.items.len),
            };
            try forms.appendSlice(allocator, entry.value_ptr.items);
            lookup_12b.putAssumeCapacityNoClobber(entry.key_ptr.*, range);
        }
        var viter = temp_12b.valueIterator();
        while (viter.next()) |ptr| ptr.deinit(temp);
        temp_12b.clearAndFree(temp);
    }

    var lookup_16b: std.AutoHashMapUnmanaged(u16, data.Compact_Range) = .empty;
    errdefer lookup_16b.deinit(allocator);
    {
        try lookup_16b.ensureTotalCapacity(allocator, temp_16b.size);
        var iter = temp_16b.iterator();
        while (iter.next()) |entry| {
            const range: data.Compact_Range = .{
                .offset = @intCast(forms.items.len),
                .len = @intCast(entry.value_ptr.items.len),
            };
            try forms.appendSlice(allocator, entry.value_ptr.items);
            lookup_16b.putAssumeCapacityNoClobber(entry.key_ptr.*, range);
        }
        var viter = temp_16b.valueIterator();
        while (viter.next()) |ptr| ptr.deinit(temp);
        temp_16b.clearAndFree(temp);
    }

    return .{
        .forms = forms,
        .lookup_8b = lookup_8b,
        .lookup_12b = lookup_12b,
        .lookup_16b = lookup_16b,
        .lookup_extra = extra,
    };
}

pub fn deinit(self: *Decoding_Database, allocator: std.mem.Allocator) void {
    self.forms.deinit(allocator);
    self.lookup_8b.deinit(allocator);
    self.lookup_12b.deinit(allocator);
    self.lookup_16b.deinit(allocator);
}

pub fn find_matches(self: *const Decoding_Database, encoded: isa.Instruction.Encoded.Data) Iterator {
    // TODO is it valid to assume that there won't be forms in multiple of these lookup tables?
    if (self.lookup_8b.get(@truncate(encoded))) |range| {
        return .{
            .encoded = encoded,
            .remaining = self.forms.items[range.offset..][0..range.len],
        };
    } else if (self.lookup_12b.get(@truncate(encoded))) |range| {
        return .{
            .encoded = encoded,
            .remaining = self.forms.items[range.offset..][0..range.len],
        };
    } else if (self.lookup_16b.get(@truncate(encoded))) |range| {
        return .{
            .encoded = encoded,
            .remaining = self.forms.items[range.offset..][0..range.len],
        };
    } else {
        return .{
            .encoded = encoded,
            .remaining = self.forms.items[self.lookup_extra.offset..][0..self.lookup_extra.len],
        };
    }
}

pub const Iterator = struct {
    encoded: isa.Instruction.Encoded.Data,
    remaining: []const u16,

    pub fn next(self: *Iterator) ?isa.Instruction.Form {
        for (0.., self.remaining) |n, i| {
            const form = iedb.get(i);
            if (form.matches_data(self.encoded)) {
                self.remaining = self.remaining[n + 1 ..];
                return form;
            }
        }
        self.remaining = &.{};
        return null;
    }
};

pub fn decoder(self: *const Decoding_Database, allocator: ?std.mem.Allocator, program_memory: []const u8) Decoder {
    return .{
        .db = self,
        .remaining = program_memory,
        .last_instruction = &.{},
        .params_buffer = undefined,
        .params_allocator = allocator,
    };
}

pub const Decoder = struct {
    db: *const Decoding_Database,
    remaining: []const u8,
    last_instruction: []const u8,
    params_buffer: [isa.Parameter.Index.count]isa.Parameter,
    params_allocator: ?std.mem.Allocator,

    pub fn decode(self: *Decoder) !?isa.Instruction {
        var encoded: isa.Instruction.Encoded.Data = 0;
        const available_len = @min(@sizeOf(isa.Instruction.Encoded.Data), self.remaining.len);
        if (available_len == 0) return null;
        @memcpy(std.mem.asBytes(&encoded)[0..available_len], self.remaining.ptr);

        var iter = self.db.find_matches(encoded);
        const form = while (iter.next()) |f| {
            if (f.len() <= available_len) break f;
        } else return error.InvalidInstruction;

        const param_signatures = form.signature.params;

        const params = if (self.params_allocator) |alloc| try alloc.alloc(isa.Parameter, param_signatures.len) else self.params_buffer[0..param_signatures.len];
        errdefer if (self.params_allocator) |alloc| alloc.free(params);

        const insn: isa.Instruction = .{
            .mnemonic = form.signature.mnemonic,
            .params = params,
        };

        for (param_signatures, params) |param_signature, *param| {
            param.* = .{
                .signature = param_signature,
                .base_register = .init(0),
                .offset_register = .init(0),
                .constant = 0,
            };
        }
        form.decode_params(encoded, params);
        const len = form.len();

        self.last_instruction = self.remaining[0..len];
        self.remaining = self.remaining[len..];

        return insn;
    }

    pub fn last_encoded_instruction(self: *Decoder) isa.Instruction.Encoded {
        var encoded: isa.Instruction.Encoded.Data = 0;
        @memcpy(std.mem.asBytes(&encoded).ptr, self.last_instruction);
        return .{
            .data = encoded,
            .len = @intCast(self.last_instruction.len),
        };
    }
};

const Decoding_Database = @This();

const iedb = @import("../iedb.zig");
const data = @import("iedb_data");
const isa = @import("isa");
const std = @import("std");
