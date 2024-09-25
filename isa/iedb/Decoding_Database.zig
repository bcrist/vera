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

pub fn decoder(self: *const Decoding_Database, allocator: ?std.mem.Allocator, program_memory: []const u8) Decoder {
    return .{
        .db = self,
        .allocator = allocator,
        .remaining = program_memory,
        .last_instruction = &.{},
        .params_buffer = undefined,
    };
}

pub const Decoder = struct {
    db: *const Decoding_Database,
    allocator: ?std.mem.Allocator,
    remaining: []const u8,
    last_instruction: []const u8,
    params_buffer: [Parameter.Index.count]Parameter,

    pub fn decode(self: *Decoder) !?Instruction {
        var data: Encoded_Instruction.Data = 0;
        const available_len = @min(@sizeOf(Encoded_Instruction.Data), self.remaining.len);
        if (available_len == 0) return null;
        @memcpy(std.mem.asBytes(&data)[0..available_len], self.remaining.ptr);

        var iter = self.db.matching_encodings(data);
        const encoding = while (iter.next()) |enc| {
            if (enc.len() <= available_len) break enc;
        } else return error.InvalidInstruction;

        const param_signatures = encoding.signature.params;

        const params = if (self.allocator) |alloc| try alloc.alloc(Parameter, param_signatures.len) else self.params_buffer[0..param_signatures.len];
        errdefer if (self.allocator) |alloc| alloc.free(params);

        const insn: Instruction = .{
            .mnemonic = encoding.signature.mnemonic,
            .suffix = encoding.signature.suffix,
            .params = params,
        };

        for (param_signatures, params) |param_signature, *param| {
            param.* = .{
                .signature = param_signature,
                .base_register_index = 0,
                .offset_register_index = 0,
                .constant = 0,
            };
        }
        encoding.decode_params(data, params);
        const len = encoding.len();

        self.last_instruction = self.remaining[0..len];
        self.remaining = self.remaining[len..];

        return insn;
    }

    pub fn last_instruction_data(self: *Decoder) Encoded_Instruction {
        var data: Encoded_Instruction.Data = 0;
        @memcpy(std.mem.asBytes(&data).ptr, self.last_instruction);
        return .{
            .data = data,
            .len = @intCast(self.last_instruction.len),
        };
    }
};

const Decoding_Database = @This();
const Instruction_Encoding = isa.Instruction_Encoding;
const Instruction = isa.Instruction;
const Parameter = isa.Parameter;
const Encoded_Instruction = isa.Encoded_Instruction;
const isa = @import("isa");
const std = @import("std");
