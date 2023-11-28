allocator: std.mem.Allocator,
lookup: deep_hash_map.DeepAutoHashMap(Instruction_Signature, []const Instruction_Encoding),

pub fn init(data: *Instruction_Encoding.Parser_Data, source: []const u8) !Encoding_Database {
    var stream = std.io.fixedBufferStream(source);
    var parser = Instruction_Encoding.parser(data, stream.reader());
    defer parser.deinit();
    defer data.deinit();
    errdefer data.free_data();

    var temp_lookup = deep_hash_map.DeepAutoHashMap(Instruction_Signature, std.ArrayListUnmanaged(Instruction_Encoding)).init(data.temp_allocator);
    defer {
        var iter = temp_lookup.valueIterator();
        while (iter.next()) |list| {
            list.deinit(data.temp_allocator);
        }
        temp_lookup.deinit();
    }

    while (parser.next() catch |err| {
        if (err == error.SExpressionSyntaxError) {
            var stderr = std.io.getStdErr().writer();
            const context = parser.reader.token_context() catch return err;
            stderr.writeAll("Syntax error in instruction encoding data:\n") catch return err;
            context.print_for_string(source, stderr, 150) catch return err;
        }
        return err;
    }) |encoding| {
        var result = try temp_lookup.getOrPutAdapted(encoding, Encoding_Context{});
        if (!result.found_existing) {
            result.key_ptr.* = encoding.signature;
            result.value_ptr.* = .{};
        }
        try result.value_ptr.append(data.temp_allocator, encoding);
    }

    var self: Encoding_Database = .{
        .allocator = data.data_allocator,
        .lookup = deep_hash_map.DeepAutoHashMap(Instruction_Signature, []const Instruction_Encoding).init(data.data_allocator),
    };
    errdefer self.deinit();
    try self.lookup.ensureTotalCapacity(temp_lookup.count());

    var iter = temp_lookup.iterator();
    while (iter.next()) |entry| {
        const encodings = try self.allocator.dupe(Instruction_Encoding, entry.value_ptr.items);
        errdefer self.allocator.free(encodings);
        try self.lookup.put(entry.key_ptr.*, encodings);
    }

    return self;
}

pub fn deinit(self: *Encoding_Database) void {
    var iter = self.lookup.valueIterator();
    while (iter.next()) |encodings| {
        self.allocator.free(encodings.*);
    }
    self.lookup.deinit();
}

pub fn matching_encodings(self: Encoding_Database, insn: Instruction) Iterator {
    const possible_encodings: []const Instruction_Encoding = self.lookup.getAdapted(insn, Instruction_Context{}) orelse &.{};
    return .{
        .insn = insn,
        .remaining = possible_encodings,
    };
}

pub fn similar_encodings(self: Encoding_Database, insn: Instruction) Iterator {
    const possible_encodings: []const Instruction_Encoding = self.lookup.getAdapted(insn, Instruction_Context{}) orelse &.{};
    return .{
        .insn = insn,
        .remaining = possible_encodings,
        .skip_checks = true,
    };
}

pub const Iterator = struct {
    insn: Instruction,
    remaining: []const Instruction_Encoding,
    skip_checks: bool = false,

    pub fn next(self: *Iterator) ?Instruction_Encoding {
        while (self.remaining.len > 0) {
            const encoding = self.remaining[0];
            self.remaining = self.remaining[1..];
            if (self.skip_checks or encoding.matches(self.insn)) {
                return encoding;
            }
        }
        return null;
    }
    pub fn next_ptr(self: *Iterator) ?*const Instruction_Encoding {
        while (self.remaining.len > 0) {
            const encoding = &self.remaining[0];
            self.remaining = self.remaining[1..];
            if (self.skip_checks or encoding.matches(self.insn)) {
                return encoding;
            }
        }
        return null;
    }
};

const Encoding_Context = struct {
    pub fn hash(_: Encoding_Context, key: Instruction_Encoding) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, key.signature.mnemonic);
        std.hash.autoHash(&hasher, key.signature.suffix);
        std.hash.autoHashStrat(&hasher, key.signature.params, .Deep);
        return hasher.final();
    }
    pub fn eql(_: Encoding_Context, a: Instruction_Encoding, b: Instruction_Signature) bool {
        return a.signature.eql(b);
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
const Instruction = @import("Instruction.zig");
const Parameter = @import("Parameter.zig");
const Instruction_Encoding = @import("Instruction_Encoding.zig");
const Instruction_Signature = isa.Instruction_Signature;
const isa = @import("lib_arch").isa;
const deep_hash_map = @import("deep_hash_map");
const std = @import("std");
