lookup_8b: std.AutoHashMapUnmanaged(u8, []const Instruction_Encoding),
lookup_12b: std.AutoHashMapUnmanaged(u12, []const Instruction_Encoding),
lookup_16b: std.AutoHashMapUnmanaged(u16, []const Instruction_Encoding),
lookup_extra: []const Instruction_Encoding,

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
