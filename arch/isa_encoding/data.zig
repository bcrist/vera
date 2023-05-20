const std = @import("std");
const sx = @import("sx");
const deep_hash_map = @import("deep_hash_map");
const isa = @import("isa_types");
const misc = @import("misc");

const Instruction = @import("Instruction.zig");
const Parameter = @import("Parameter.zig");
const InstructionEncoding = @import("InstructionEncoding.zig");
const ParameterEncoding = @import("ParameterEncoding.zig");
const Opcode = isa.Opcode;
const Mnemonic = isa.Mnemonic;
const MnemonicSuffix = isa.MnemonicSuffix;
const AddressSpace = isa.AddressSpace;
const SpecialRegister = isa.SpecialRegister;
const ConstantRange = isa.ConstantRange;
const IndexedRegisterRange = isa.IndexedRegisterRange;

const sx_data = @embedFile("isa.sx");

pub fn writeInstructionEncoding(comptime W: type, writer: *sx.Writer(W), encoding: InstructionEncoding, maybe_desc: ?[]const u8, compact: bool) !void {
    try writer.open();
    try writer.printValue("{X:0>4}", .{ encoding.opcodes.min });
    try writer.printValue("{X:0>4}", .{ encoding.opcodes.max });
    try writer.tag(encoding.mnemonic);
    if (encoding.suffix != .none) {
        try writer.tag(encoding.suffix);
    }
    if (maybe_desc != null or encoding.params.len > 0) {
        writer.setCompact(compact);
    }
    if (maybe_desc) |desc| {
        try writer.expression("desc");
        try writer.string(desc);
        _ = try writer.close();
    }

    if (encoding.opcode_base != encoding.opcodes.min) {
        var param_uses_opcode = false;
        for (encoding.params) |param| {
            if (param.base_src == .opcode or param.offset_src == .opcode) {
                param_uses_opcode = true;
                break;
            }
        }
        if (param_uses_opcode) {
            try writer.expression("opcode-base");
            try writer.printValue("{X:0>4}", .{ encoding.opcode_base });
            _ = try writer.close();
        }
    }

    for (encoding.params) |param| {
        try writer.expression("param");
        if (param.arrow) {
            try writer.string("->");
        }

        if (param.address_space) |as| {
            try writer.expression("address");
            try writer.tag(as);
            _ = try writer.close();
        }

        try writer.expression("base");
        try writeBaseOffsetEncoding(writer, param.base);
        if (param.base_src != .implicit) {
            try writer.expression("src");
            try writer.tag(param.base_src);
            _ = try writer.close();
        }
        _ = try writer.close();

        if (param.offset != .none or param.offset_src != .implicit) {
            try writer.expression("offset");
            try writeBaseOffsetEncoding(writer, param.offset);
            if (param.offset_src != .implicit) {
                try writer.expression("src");
                try writer.tag(param.offset_src);
                _ = try writer.close();
            }
            _ = try writer.close();
        }
        _ = try writer.close();
    }

    _ = try writer.close();
}

fn writeBaseOffsetEncoding(writer: anytype, boe: ParameterEncoding.BaseOffsetEncoding) !void {
    try writer.tag(boe);
     switch (boe) {
        .none => {},
        .constant => |ce| {
            if (ce.reverse) {
                try writer.expression("rev");
                _ = try writer.close();
            }

            for (ce.ranges) |range| {
                try writer.expression("range");
                try writer.int(range.min, 10);
                try writer.int(range.max, 10);
                _ = try writer.close();
            }

            if (ce.granularity != 1) {
                try writer.expression("granularity");
                try writer.int(ce.granularity, 10);
                _ = try writer.close();
            }
        },
        .reg8, .reg16, .reg32 => |reg| {
            if (reg.min != 0 or reg.max != 15) {
                try writer.expression("index");
                try writer.int(reg.min, 10);
                try writer.int(reg.max, 10);
                _ = try writer.close();
            }
            if (reg.signedness) |s| {
                switch (s) {
                    .signed => try writer.expression("signed"),
                    .unsigned => try writer.expression("unsigned"),
                }
                _ = try writer.close();
            }
        },
        .sr => |reg| try writer.tag(reg),
    }
}

pub const InstructionEncodingParser = struct {
    reader: sx.Reader(std.io.FixedBufferStream([]const u8).Reader),
    temp_allocator: std.mem.Allocator,
    data_allocator: std.mem.Allocator,
    parse_descriptions: bool,
    first: bool = true,
    descriptions: std.StringHashMapUnmanaged(void) = .{},
    constant_ranges: deep_hash_map.DeepAutoHashMapUnmanaged([]const ConstantRange, void) = .{},
    parameter_encodings: deep_hash_map.DeepAutoHashMapUnmanaged([]const ParameterEncoding, void) = .{},
    temp_constant_ranges: std.ArrayListUnmanaged(ConstantRange) = .{},
    temp_parameter_encodings: std.ArrayListUnmanaged(ParameterEncoding) = .{},

    pub fn deinitData(self: *InstructionEncodingParser) void {
        var descriptions_iter = self.descriptions.keyIterator();
        while (descriptions_iter.next()) |k| {
            self.data_allocator.free(k.*);
        }

        var constant_ranges_iter = self.constant_ranges.keyIterator();
        while (constant_ranges_iter.next()) |k| {
            self.data_allocator.free(k.*);
        }

        var parameter_encodings_iter = self.parameter_encodings.keyIterator();
        while (parameter_encodings_iter.next()) |k| {
            self.data_allocator.free(k.*);
        }
    }

    pub fn deinit(self: *InstructionEncodingParser) void {
        self.descriptions.deinit(self.temp_allocator);
        self.constant_ranges.deinit(self.temp_allocator);
        self.parameter_encodings.deinit(self.temp_allocator);
        self.temp_constant_ranges.deinit(self.temp_allocator);
        self.temp_parameter_encodings.deinit(self.temp_allocator);
    }

    pub fn next(self: *InstructionEncodingParser) !?InstructionEncoding.WithDescription {
        if (self.first) {
            try self.reader.requireOpen();
            self.first = false;
        }
        if (!try self.reader.open()) {
            try self.reader.requireClose();
            try self.reader.requireDone();
            return null;
        }

        const min_opcode = try self.reader.requireAnyUnsigned(Opcode, 16);
        const max_opcode = try self.reader.requireAnyUnsigned(Opcode, 16);
        const mnemonic = try self.reader.requireAnyEnum(Mnemonic);
        const suffix = try self.reader.anyEnum(MnemonicSuffix) orelse .none;
        var opcode_base = min_opcode;
        var description: []const u8 = "";

        while (true) {
            if (try self.reader.expression("desc")) {
                const temp_desc = try self.reader.requireAnyString();
                if (self.parse_descriptions) {
                    description = try self.dedupDescription(temp_desc);
                }
            } else if (try self.reader.expression("param")) {
                const arrow = try self.reader.string("->");
                var param = ParameterEncoding{
                    .arrow = arrow,
                };

                while (true) {
                    if (try self.reader.expression("address")) {
                        param.address_space = try self.reader.requireAnyEnum(AddressSpace);
                    } else if (try self.reader.expression("base")) {
                        param.base = try self.parseBaseOffsetEncoding();
                        if (try self.reader.expression("src")) {
                            param.base_src = try self.reader.requireAnyEnum(ParameterEncoding.Source);
                            try self.reader.requireClose();
                        }
                    } else if (try self.reader.expression("offset")) {
                        param.offset = try self.parseBaseOffsetEncoding();
                        if (try self.reader.expression("src")) {
                            param.offset_src = try self.reader.requireAnyEnum(ParameterEncoding.Source);
                            try self.reader.requireClose();
                        }
                    } else break;

                    try self.reader.requireClose();
                }
                try self.temp_parameter_encodings.append(self.temp_allocator, param);
            } else if (try self.reader.expression("opcode-base")) {
                opcode_base = try self.reader.requireAnyUnsigned(Opcode, 16);
            } else break;

            try self.reader.requireClose();
        }
        try self.reader.requireClose();

        const params = try self.dedupParameterEncodings(self.temp_parameter_encodings.items);
        self.temp_parameter_encodings.clearRetainingCapacity();

        return InstructionEncoding.WithDescription{
            .encoding = .{
                .mnemonic = mnemonic,
                .suffix = suffix,
                .params = params,
                .opcode_base = opcode_base,
                .opcodes = .{
                    .min = min_opcode,
                    .max = max_opcode,
                },
            },
            .desc = description,
        };
    }

    fn parseBaseOffsetEncoding(self: *InstructionEncodingParser) !ParameterEncoding.BaseOffsetEncoding {
        const kind = try self.reader.requireAnyEnum(std.meta.Tag(ParameterEncoding.BaseOffsetEncoding));
        switch (kind) {
            .none => return .{ .none = {} },
            .sr => return .{ .sr = try self.reader.requireAnyEnum(SpecialRegister) },
            .constant => {
                var ce = ParameterEncoding.ConstantEncoding{};

                self.temp_constant_ranges.clearRetainingCapacity();

                while (true) {
                    if (try self.reader.expression("rev")) {
                        ce.reverse = true;
                    } else if (try self.reader.expression("range")) {
                        try self.temp_constant_ranges.append(self.temp_allocator, .{
                            .min = try self.reader.requireAnyInt(i64, 10),
                            .max = try self.reader.requireAnyInt(i64, 10),
                        });
                    } else if (try self.reader.expression("granularity")) {
                        ce.granularity = try self.reader.requireAnyInt(u3, 10);
                    } else break;

                    try self.reader.requireClose();
                }

                if (self.temp_constant_ranges.items.len > 0) {
                    ce.ranges = try self.dedupConstantRanges(self.temp_constant_ranges.items);
                }

                self.temp_constant_ranges.clearRetainingCapacity();

                return .{ .constant = ce };
            },
            .reg8, .reg16, .reg32 => {
                var irr = IndexedRegisterRange{};
                if (try self.reader.expression("index")) {
                    irr.min = try self.reader.requireAnyInt(misc.RegisterIndex, 10);
                    irr.max = try self.reader.requireAnyInt(misc.RegisterIndex, 10);
                    try self.reader.requireClose();
                }
                if (try self.reader.expression("signed")) {
                    irr.signedness = .signed;
                    try self.reader.requireClose();
                } else if (try self.reader.expression("unsigned")) {
                    irr.signedness = .unsigned;
                    try self.reader.requireClose();
                }
                return switch (kind) {
                    .reg8 => .{ .reg8 = irr },
                    .reg16 => .{ .reg16 = irr },
                    .reg32 => .{ .reg32 = irr },
                    else => unreachable,
                };
            },
        }
    }

    fn dedupDescription(self: *InstructionEncodingParser, desc: []const u8) ![]const u8 {
        if (self.descriptions.getKey(desc)) |deduped| return deduped;
        try self.descriptions.ensureUnusedCapacity(self.temp_allocator, 1);
        const to_insert = try self.data_allocator.dupe(u8, desc);
        self.descriptions.putAssumeCapacity(to_insert, {});
        return to_insert;
    }

    fn dedupConstantRanges(self: *InstructionEncodingParser, ranges: []const ConstantRange) ![]const ConstantRange {
        if (self.constant_ranges.getKey(ranges)) |deduped| return deduped;
        try self.constant_ranges.ensureUnusedCapacity(self.temp_allocator, 1);
        const to_insert = try self.data_allocator.dupe(ConstantRange, ranges);
        self.constant_ranges.putAssumeCapacity(to_insert, {});
        return to_insert;
    }

    fn dedupParameterEncodings(self: *InstructionEncodingParser, params: []const ParameterEncoding) ![]const ParameterEncoding {
        if (self.parameter_encodings.getKey(params)) |deduped| return deduped;
        try self.parameter_encodings.ensureUnusedCapacity(self.temp_allocator, 1);
        const to_insert = try self.data_allocator.dupe(ParameterEncoding, params);
        self.parameter_encodings.putAssumeCapacity(to_insert, {});
        return to_insert;
    }
};

pub const EncoderDatabase = struct {
    mnemonic_to_encoding: std.EnumMap(Mnemonic, []InstructionEncoding) = .{},

    pub fn init(arena: std.mem.Allocator, temp: std.mem.Allocator) !EncoderDatabase {
        var stream = std.io.fixedBufferStream(sx_data);
        var parser = InstructionEncodingParser{
            .reader = sx.reader(temp, stream.reader()),
            .temp_allocator = temp,
            .data_allocator = arena,
            .parse_descriptions = false,
        };
        defer parser.deinit();
        defer parser.reader.deinit();
        errdefer parser.deinitData();

        var temp_map = std.EnumMap(Mnemonic, std.ArrayListUnmanaged(InstructionEncoding)) {};
        defer {
            var iter = temp_map.iterator();
            while (iter.next()) |entry| {
                entry.value.deinit(temp);
            }
        }

        while (parser.next() catch |err| {
            if (err == error.SExpressionSyntaxError) {
                var stderr = std.io.getStdErr().writer();
                const context = parser.reader.getNextTokenContext() catch return err;
                stderr.writeAll("Syntax error in instruction encoding data:\n") catch return err;
                context.printForString(sx_data, stderr, 150) catch return err;
            }
            return err;
        }) |encoding_and_desc| {
            const encoding = encoding_and_desc.encoding;

            var list = temp_map.getPtr(encoding.mnemonic) orelse ptr: {
                temp_map.put(encoding.mnemonic, .{});
                break :ptr temp_map.getPtrAssertContains(encoding.mnemonic);
            };

            try list.append(temp, encoding);
        }

        var self = EncoderDatabase{};
        var iter = temp_map.iterator();
        while (iter.next()) |entry| {
            const encodings = try arena.dupe(InstructionEncoding, entry.value.items);
            self.mnemonic_to_encoding.put(entry.key, encodings);
        }

        return self;
    }

    pub fn getMatchingEncodings(self: EncoderDatabase, insn: Instruction) InstructionEncodingIterator {
        const encodings = self.mnemonic_to_encoding.get(insn.mnemonic) orelse &[_]InstructionEncoding{};
        return InstructionEncodingIterator.init(insn, encodings);
    }
};

pub const InstructionEncodingIterator = struct {
    insn: Instruction,
    remaining: []const InstructionEncoding,

    pub fn init(insn: Instruction, possible_encodings: []const InstructionEncoding) InstructionEncodingIterator {
        return .{
            .insn = insn,
            .remaining = possible_encodings,
        };
    }

    pub fn next(self: *InstructionEncodingIterator) ?InstructionEncoding {
        while (self.remaining.len > 0) {
            const n = self.remaining[0];
            self.remaining = self.remaining[1..];
            if (self.insn.matches(n)) {
                return n;
            }
        }
        return null;
    }
    pub fn nextPointer(self: *InstructionEncodingIterator) ?*const InstructionEncoding {
        while (self.remaining.len > 0) {
            const n = &self.remaining[0];
            self.remaining = self.remaining[1..];
            if (self.insn.matches(n.*)) {
                return n;
            }
        }
        return null;
    }
};

pub const DecoderDatabase = struct {
    opcode_to_desc_8b: std.AutoHashMapUnmanaged(u8, []const u8),
    opcode_to_desc_12b: std.AutoHashMapUnmanaged(u12, []const u8),
    opcode_to_desc_16b: std.AutoHashMapUnmanaged(u16, []const u8),

    opcode_to_encoding_8b: std.AutoHashMapUnmanaged(u8, InstructionEncoding),
    opcode_to_encoding_12b: std.AutoHashMapUnmanaged(u12, InstructionEncoding),
    opcode_to_encoding_16b: std.AutoHashMapUnmanaged(u16, InstructionEncoding),

    pub fn init(arena: std.mem.Allocator, temp: std.mem.Allocator) !DecoderDatabase {
        var stream = std.io.fixedBufferStream(sx_data);
        var parser = InstructionEncodingParser{
            .reader = sx.reader(temp, stream.reader()),
            .temp_allocator = temp,
            .data_allocator = arena,
            .parse_descriptions = true,
        };
        defer parser.deinit();
        defer parser.reader.deinit();
        errdefer parser.deinitData();

        var temp_desc_8b = std.AutoHashMap(u8, []const u8).init(temp);
        var temp_desc_12b = std.AutoHashMap(u12, []const u8).init(temp);
        var temp_desc_16b = std.AutoHashMap(u16, []const u8).init(temp);
        defer temp_desc_8b.deinit();
        defer temp_desc_12b.deinit();
        defer temp_desc_16b.deinit();

        var temp_enc_8b = std.AutoHashMap(u8, InstructionEncoding).init(temp);
        var temp_enc_12b = std.AutoHashMap(u12, InstructionEncoding).init(temp);
        var temp_enc_16b = std.AutoHashMap(u16, InstructionEncoding).init(temp);
        defer temp_enc_8b.deinit();
        defer temp_enc_12b.deinit();
        defer temp_enc_16b.deinit();

        while (parser.next() catch |err| {
            if (err == error.SExpressionSyntaxError) {
                var stderr = std.io.getStdErr().writer();
                const context = parser.reader.getNextTokenContext() catch return err;
                stderr.writeAll("Syntax error in instruction encoding data:\n") catch return err;
                context.printForString(sx_data, stderr, 150) catch return err;
            }
            return err;
        }) |encoding_and_desc| {
            const min_opcode = encoding_and_desc.encoding.opcodes.min;
            const max_opcode = encoding_and_desc.encoding.opcodes.max;

            // We ignore any duplicate encodings for the same opcode;
            // In other words we never decode aliases, since they always
            // appear at the end of instruction_encoding.sx

            if (encoding_and_desc.desc.len > 0) {
                const desc = encoding_and_desc.desc;
                var opcode: u32 = min_opcode;
                while (opcode <= max_opcode and (opcode & 0xF) != 0) : (opcode += 1) {
                    _ = try temp_desc_16b.getOrPutValue(@intCast(u16, opcode), desc);
                }
                while (opcode + 0xF <= max_opcode and (opcode & 0xFF) != 0) : (opcode += 0x10) {
                    _ = try temp_desc_12b.getOrPutValue(@intCast(u12, opcode >> 4), desc);
                }
                while (opcode + 0xFF <= max_opcode) : (opcode += 0x100) {
                    _ = try temp_desc_8b.getOrPutValue(@intCast(u8, opcode >> 8), desc);
                }
                while (opcode + 0xF <= max_opcode) : (opcode += 0x10) {
                    _ = try temp_desc_12b.getOrPutValue(@intCast(u12, opcode >> 4), desc);
                }
                while (opcode <= max_opcode) : (opcode += 1) {
                    _ = try temp_desc_16b.getOrPutValue(@intCast(u16, opcode), desc);
                }
            }

            var opcode: u32 = min_opcode;
            while (opcode <= max_opcode and (opcode & 0xF) != 0) : (opcode += 1) {
                _ = try temp_enc_16b.getOrPutValue(@intCast(u16, opcode), encoding_and_desc.encoding);
            }
            while (opcode + 0xF <= max_opcode and (opcode & 0xFF) != 0) : (opcode += 0x10) {
                _ = try temp_enc_12b.getOrPutValue(@intCast(u12, opcode >> 4), encoding_and_desc.encoding);
            }
            while (opcode + 0xFF <= max_opcode) : (opcode += 0x100) {
                _ = try temp_enc_8b.getOrPutValue(@intCast(u8, opcode >> 8), encoding_and_desc.encoding);
            }
            while (opcode + 0xF <= max_opcode) : (opcode += 0x10) {
                _ = try temp_enc_12b.getOrPutValue(@intCast(u12, opcode >> 4), encoding_and_desc.encoding);
            }
            while (opcode <= max_opcode) : (opcode += 1) {
                _ = try temp_enc_16b.getOrPutValue(@intCast(u16, opcode), encoding_and_desc.encoding);
            }
        }

        return DecoderDatabase{
            .opcode_to_desc_8b = try temp_desc_8b.unmanaged.clone(arena),
            .opcode_to_desc_12b = try temp_desc_12b.unmanaged.clone(arena),
            .opcode_to_desc_16b = try temp_desc_16b.unmanaged.clone(arena),
            .opcode_to_encoding_8b = try temp_enc_8b.unmanaged.clone(arena),
            .opcode_to_encoding_12b = try temp_enc_12b.unmanaged.clone(arena),
            .opcode_to_encoding_16b = try temp_enc_16b.unmanaged.clone(arena),
        };
    }

    pub fn getDescriptionForOpcode(self: DecoderDatabase, opcode: Opcode) ?[]const u8 {
        const op8 = @intCast(u8, opcode >> 8);
        if (self.opcode_to_desc_8b.get(op8)) |encoding| return encoding;

        const op12 = @intCast(u12, opcode >> 4);
        if (self.opcode_to_desc_12b.get(op12)) |encoding| return encoding;

        return self.opcode_to_desc_16b.get(opcode);
    }

    pub fn getEncodingForOpcode(self: DecoderDatabase, opcode: Opcode) ?InstructionEncoding {
        const op8 = @intCast(u8, opcode >> 8);
        if (self.opcode_to_encoding_8b.get(op8)) |encoding| return encoding;

        const op12 = @intCast(u12, opcode >> 4);
        if (self.opcode_to_encoding_12b.get(op12)) |encoding| return encoding;

        return self.opcode_to_encoding_16b.get(opcode);
    }

    pub fn extractEncoding(self: DecoderDatabase, encoded_instruction: []const u8) ?InstructionEncoding {
        return self.getEncodingForOpcode(Parameter.readOpcode(encoded_instruction));
    }

};

pub const Decoder = struct {
    db: *const DecoderDatabase,
    alloc: std.mem.Allocator,
    remaining: []const u8,
    last_instruction: []const u8,

    pub fn init(db: *const DecoderDatabase, alloc: std.mem.Allocator, program_memory: []const u8) Decoder {
        return .{
            .db = db,
            .alloc = alloc,
            .remaining = program_memory,
            .last_instruction = &.{},
        };
    }

    pub fn decode(self: *Decoder) !Instruction {
        const encoding = self.db.extractEncoding(self.remaining) orelse return error.InvalidInstruction;
        const params = try self.alloc.alloc(Parameter, encoding.params.len);
        errdefer self.alloc.free(params);

        var i: usize = 0;
        while (i < encoding.params.len) : (i += 1) {
            params[i] = encoding.params[i].read(self.remaining, encoding.opcode_base);
        }

        const insn = Instruction{
            .mnemonic = encoding.mnemonic,
            .suffix = encoding.suffix,
            .params = params,
        };

        const length = encoding.getInstructionLength();

        self.last_instruction = self.remaining[0..length];
        self.remaining = self.remaining[length..];

        return insn;
    }
};

// Decodes a single instruction; use Decoder directly to decode multiple instructions
pub fn decodeInstruction(db: *const DecoderDatabase, alloc: std.mem.Allocator, program_memory: []const u8) !Instruction {
    var decoder = Decoder.init(db, alloc, program_memory);
    return try decoder.decode();
}

pub fn testIdempotence(ddb: *const DecoderDatabase, edb: *const EncoderDatabase, expected_encodings: usize, insn: Instruction) !void {
    var encoding_count: usize = 0;
    var iter = edb.getMatchingEncodings(insn);
    while (iter.next()) |encoding| {
        if (encoding.suffix != insn.suffix) {
            continue;
        }

        var buf: [6]u8 = [_]u8{0} ** 6;
        var encoded = try insn.write(encoding, &buf);

        var temp: [10 * @sizeOf(Parameter)]u8 = undefined;
        var alloc = std.heap.FixedBufferAllocator.init(&temp);
        var decoder = Decoder.init(ddb, alloc.allocator(), encoded);
        const decoded = try decoder.decode();

        var decoded_encoding = ddb.extractEncoding(decoder.last_instruction) orelse return error.InvalidInstruction;

        if (!insn.eql(decoded)) {
            const stderr = std.io.getStdErr().writer();
            try stderr.writeAll("Roundtrip testing of instruction: ");
            try insn.print(stderr);
            try stderr.writeAll("\nWith encoding: ");
            try encoding.print(stderr);
            try stderr.writeAll("\nDecoded instruction: ");
            try decoded.print(stderr);
            try stderr.writeAll("\nFrom encoding: ");
            try decoded_encoding.print(stderr);
            try stderr.writeAll("\n");
            return error.TestExpectedError;
        }
        encoding_count += 1;
    }

    std.testing.expectEqual(expected_encodings, encoding_count) catch {
        const stderr = std.io.getStdErr().writer();
        try stderr.writeAll("Encodings for instruction: ");
        try insn.print(stderr);
        try stderr.writeAll("\n");
        iter = edb.getMatchingEncodings(insn);
        while (iter.next()) |encoding| {
            if (encoding.suffix != insn.suffix) {
                continue;
            }

            try encoding.print(stderr);
            try stderr.writeAll("\n");
        }
        return error.TestExpectedError;
    };
}
