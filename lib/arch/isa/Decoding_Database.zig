allocator: std.mem.Allocator,
lookup_8b: std.AutoHashMapUnmanaged(u8, []const Instruction_Encoding),
lookup_12b: std.AutoHashMapUnmanaged(u12, []const Instruction_Encoding),
lookup_16b: std.AutoHashMapUnmanaged(u16, []const Instruction_Encoding),
lookup_extra: []const Instruction_Encoding,

pub fn init(data: *Instruction_Encoding.Parser_Data, source: []const u8) !Decoding_Database {
    var stream = std.io.fixedBufferStream(source);
    var parser = Instruction_Encoding.parser(data, stream.reader());
    defer data.deinit();
    defer parser.deinit();
    errdefer data.free_data();

    var temp_8b = std.AutoHashMap(u8, std.ArrayListUnmanaged(Instruction_Encoding)).init(data.temp_allocator);
    var temp_12b = std.AutoHashMap(u12, std.ArrayListUnmanaged(Instruction_Encoding)).init(data.temp_allocator);
    var temp_16b = std.AutoHashMap(u16, std.ArrayListUnmanaged(Instruction_Encoding)).init(data.temp_allocator);
    var temp_extra = std.ArrayList(Instruction_Encoding).init(data.temp_allocator);
    defer {
        var iter = temp_8b.valueIterator();
        while (iter.next()) |ptr| ptr.deinit(data.temp_allocator);
        temp_8b.deinit();
    }
    defer {
        var iter = temp_12b.valueIterator();
        while (iter.next()) |ptr| ptr.deinit(data.temp_allocator);
        temp_12b.deinit();
    }
    defer {
        var iter = temp_16b.valueIterator();
        while (iter.next()) |ptr| ptr.deinit(data.temp_allocator);
        temp_16b.deinit();
    }
    defer temp_extra.deinit();

    while (parser.next() catch |err| {
        if (err == error.SExpressionSyntaxError) {
            var stderr = std.io.getStdErr().writer();
            const context = parser.reader.token_context() catch return err;
            stderr.writeAll("Syntax error in instruction encoding data:\n") catch return err;
            context.print_for_string(source, stderr, 150) catch return err;
        }
        return err;
    }) |encoding| {
        for (encoding.encoders) |enc| {
            if (enc.value == .constant and enc.bit_offset == 0 and enc.arithmetic_offset == 0) {
                var encoded: Encoded_Instruction.Data = 0;
                enc.encode(.{ .mnemonic = ._reserved, .suffix = .none, .params = &.{} }, &encoded);
                const bits = enc.required_bits();
                if (bits >= 16) {
                    const prefix: u16 = @truncate(encoded);
                    const result = try temp_16b.getOrPut(prefix);
                    if (!result.found_existing) {
                        result.key_ptr.* = prefix;
                        result.value_ptr.* = .{};
                    }
                    try result.value_ptr.append(data.temp_allocator, encoding);
                    break;
                } else if (bits >= 12) {
                    const prefix: u12 = @truncate(encoded);
                    const result = try temp_12b.getOrPut(prefix);
                    if (!result.found_existing) {
                        result.key_ptr.* = prefix;
                        result.value_ptr.* = .{};
                    }
                    try result.value_ptr.append(data.temp_allocator, encoding);
                    break;
                } else if (bits >= 8) {
                    const prefix: u8 = @truncate(encoded);
                    const result = try temp_8b.getOrPut(prefix);
                    if (!result.found_existing) {
                        result.key_ptr.* = prefix;
                        result.value_ptr.* = .{};
                    }
                    try result.value_ptr.append(data.temp_allocator, encoding);
                    break;
                }
            }
        } else {
            try temp_extra.append(data.temp_allocator, encoding);
        }
    }

    var self: Decoding_Database = .{
        .allocator = data.data_allocator,
        .lookup_8b = .{},
        .lookup_12b = .{},
        .lookup_16b = .{},
        .lookup_extra = try data.data_allocator.dupe(Instruction_Encoding, temp_extra.items),
    };
    errdefer self.deinit();
    try self.lookup_8b.ensureTotalCapacity(temp_8b.count());
    try self.lookup_12b.ensureTotalCapacity(temp_12b.count());
    try self.lookup_16b.ensureTotalCapacity(temp_16b.count());

    var iter8 = temp_8b.iterator();
    while (iter8.next()) |entry| {
        const encodings = try self.allocator.dupe(Instruction_Encoding, entry.value.items);
        errdefer self.allocator.free(encodings);
        try self.lookup_8b.put(entry.key, encodings);
    }

    var iter12 = temp_12b.iterator();
    while (iter12.next()) |entry| {
        const encodings = try self.allocator.dupe(Instruction_Encoding, entry.value.items);
        errdefer self.allocator.free(encodings);
        try self.lookup_12b.put(entry.key, encodings);
    }

    var iter16 = temp_16b.iterator();
    while (iter16.next()) |entry| {
        const encodings = try self.allocator.dupe(Instruction_Encoding, entry.value.items);
        errdefer self.allocator.free(encodings);
        try self.lookup_16b.put(entry.key, encodings);
    }

    return self;
}

pub fn deinit(self: *Decoding_Database) void {
    self.lookup_8b.deinit();
}

pub fn matching_encodings(self: Decoding_Database, data: Encoded_Instruction.Data) Iterator {
    if (self.lookup_8b.get(@truncate(data))) |encodings| {
        return .{ .encoded = data, .remaining = encodings };
    } else if (self.lookup_12b.get(@truncate(data))) |encodings| {
        return .{ .encoded = data, .remaining = encodings };
    } else if (self.lookup_16b.get(@truncate(data))) |encodings| {
        return .{ .encoded = data, .remaining = encodings };
    } else {
        return .{ .encoded = data, .remaining = self.lookup_extra };
    }
}

pub const Iterator = struct {
    encoded: Encoded_Instruction.Data,
    remaining: []const Instruction_Encoding,

    pub fn next(self: *Iterator) ?Instruction_Encoding {
        while (self.remaining.len > 0) {
            const encoding = self.remaining[0];
            self.remaining = self.remaining[1..];
            if (encoding.matches_data(self.encoded)) {
                return encoding;
            }
        }
        return null;
    }
    pub fn next_ptr(self: *Iterator) ?*const Instruction_Encoding {
        while (self.remaining.len > 0) {
            const encoding = &self.remaining[0];
            self.remaining = self.remaining[1..];
            if (encoding.matches_data(self.encoded)) {
                return encoding;
            }
        }
        return null;
    }
};

pub fn decoder(self: *const Decoding_Database, allocator: std.mem.Allocator, program_memory: []const u8) Decoder {
    return .{
        .db = self,
        .allocator = allocator,
        .remaining = program_memory,
        .last_instruction = &.{},
    };
}

pub const Decoder = struct {
    db: *const Decoding_Database,
    allocator: std.mem.Allocator,
    remaining: []const u8,
    last_instruction: []const u8,

    pub fn decode(self: *Decoder) !Instruction {
        var data: Encoded_Instruction.Data = 0;
        const available_len = @min(@sizeOf(data), self.remaining.len);
        @memcpy(std.mem.asBytes(&data)[0..available_len], self.remaining.ptr);

        var iter = self.db.matching_encodings(data);
        if (iter.next()) |encoding| { // we just use the first matching interpretation
            var insn: Instruction = .{
                .mnemonic = encoding.mnemonic,
                .suffix = encoding.suffix,
                .params = try self.allocator.alloc(Instruction.Parameter, encoding.params.len),
            };
            errdefer self.allocator.free(insn.params);

            @memset(insn.params, .{ .expr_type = .unknown });
            encoding.decode_params(data, insn.params);
            const len = encoding.len();

            self.last_instruction = self.remaining[0..len];
            self.remaining = self.remaining[len..];

            return insn;
        } else return error.InvalidInstruction;
    }
};

const Decoding_Database = @This();
const Instruction_Encoding = @import("Instruction_Encoding.zig");
const Instruction = @import("Instruction.zig");
const Encoded_Instruction = isa.Encoded_Instruction;
const isa = arch.isa;
const arch = @import("lib_arch");
const std = @import("std");
