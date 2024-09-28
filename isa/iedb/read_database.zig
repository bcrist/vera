pub fn parse_decoding_db(data: *Parser_Data, source: []const u8) !Decoding_Database {
    var stream = std.io.fixedBufferStream(source);
    var reader = stream.reader();
    var p = Parser.init(data, reader.any());
    defer p.deinit();

    var temp_8b = std.AutoHashMap(u8, std.ArrayListUnmanaged(Instruction_Encoding)).init(data.temp);
    var temp_12b = std.AutoHashMap(u12, std.ArrayListUnmanaged(Instruction_Encoding)).init(data.temp);
    var temp_16b = std.AutoHashMap(u16, std.ArrayListUnmanaged(Instruction_Encoding)).init(data.temp);
    var temp_extra = std.ArrayList(Instruction_Encoding).init(data.temp);
    defer {
        var iter = temp_8b.valueIterator();
        while (iter.next()) |ptr| ptr.deinit(data.temp);
        temp_8b.deinit();
    }
    defer {
        var iter = temp_12b.valueIterator();
        while (iter.next()) |ptr| ptr.deinit(data.temp);
        temp_12b.deinit();
    }
    defer {
        var iter = temp_16b.valueIterator();
        while (iter.next()) |ptr| ptr.deinit(data.temp);
        temp_16b.deinit();
    }
    defer temp_extra.deinit();

    while (p.next() catch |err| {
        if (err == error.SExpressionSyntaxError) {
            var stderr = std.io.getStdErr().writer();
            const context = p.reader.token_context() catch return err;
            stderr.writeAll("Syntax error in instruction encoding data:\n") catch return err;
            context.print_for_string(source, stderr, 150) catch return err;
        }
        return err;
    }) |encoding| {
        for (encoding.encoders) |enc| {
            if (enc.value == .constant and enc.bit_offset == 0) {
                var encoded: Encoded_Instruction.Data = 0;
                const ok = enc.encode(.{ .mnemonic = ._reserved, .suffix = .none, .params = &.{} }, &encoded);
                std.debug.assert(ok);
                const bits = enc.required_bits();
                if (bits >= 16) {
                    const prefix: u16 = @truncate(encoded);
                    const result = try temp_16b.getOrPut(prefix);
                    if (!result.found_existing) {
                        result.key_ptr.* = prefix;
                        result.value_ptr.* = .{};
                    }
                    try result.value_ptr.append(data.temp, encoding);
                    break;
                } else if (bits >= 12) {
                    const prefix: u12 = @truncate(encoded);
                    const result = try temp_12b.getOrPut(prefix);
                    if (!result.found_existing) {
                        result.key_ptr.* = prefix;
                        result.value_ptr.* = .{};
                    }
                    try result.value_ptr.append(data.temp, encoding);
                    break;
                } else if (bits >= 8) {
                    const prefix: u8 = @truncate(encoded);
                    const result = try temp_8b.getOrPut(prefix);
                    if (!result.found_existing) {
                        result.key_ptr.* = prefix;
                        result.value_ptr.* = .{};
                    }
                    try result.value_ptr.append(data.temp, encoding);
                    break;
                }
            }
        } else {
            try temp_extra.append(encoding);
        }
    }

    var self: Decoding_Database = .{
        .lookup_8b = .{},
        .lookup_12b = .{},
        .lookup_16b = .{},
        .lookup_extra = try data.arena.dupe(Instruction_Encoding, temp_extra.items),
    };
    try self.lookup_8b.ensureTotalCapacity(data.arena, temp_8b.count());
    try self.lookup_12b.ensureTotalCapacity(data.arena, temp_12b.count());
    try self.lookup_16b.ensureTotalCapacity(data.arena, temp_16b.count());

    var iter8 = temp_8b.iterator();
    while (iter8.next()) |entry| {
        const encodings = try data.arena.dupe(Instruction_Encoding, entry.value_ptr.items);
        self.lookup_8b.putAssumeCapacity(entry.key_ptr.*, encodings);
    }

    var iter12 = temp_12b.iterator();
    while (iter12.next()) |entry| {
        const encodings = try data.arena.dupe(Instruction_Encoding, entry.value_ptr.items);
        self.lookup_12b.putAssumeCapacity(entry.key_ptr.*, encodings);
    }

    var iter16 = temp_16b.iterator();
    while (iter16.next()) |entry| {
        const encodings = try data.arena.dupe(Instruction_Encoding, entry.value_ptr.items);
        self.lookup_16b.putAssumeCapacity(entry.key_ptr.*, encodings);
    }

    return self;
}

pub fn parse_encoding_db(data: *Parser_Data, source: []const u8) !Encoding_Database {
    var stream = std.io.fixedBufferStream(source);
    var reader = stream.reader();
    var p = Parser.init(data, reader.any());
    defer p.deinit();

    var temp_lookup = deep_hash_map.DeepAutoHashMap(Instruction.Signature, std.ArrayListUnmanaged(Instruction_Encoding)).init(data.temp);
    defer {
        var iter = temp_lookup.valueIterator();
        while (iter.next()) |list| {
            list.deinit(data.temp);
        }
        temp_lookup.deinit();
    }

    var total_encodings: usize = 0;

    while (p.next() catch |err| {
        if (err == error.SExpressionSyntaxError) {
            var stderr = std.io.getStdErr().writer();
            const context = p.reader.token_context() catch return err;
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
        try result.value_ptr.append(data.temp, encoding);
        total_encodings += 1;
    }

    var self: Encoding_Database = .{};
    try self.lookup.ensureTotalCapacity(data.arena, temp_lookup.count());

    var all_encodings = try data.arena.alloc(*const Instruction_Encoding, total_encodings);
    self.all_encodings = all_encodings;

    var encoding_index: usize = 0;
    var encoding_iter = temp_lookup.iterator();
    while (encoding_iter.next()) |entry| {
        const encodings = try data.arena.dupe(Instruction_Encoding, entry.value_ptr.items);
        const Sort_Context = struct {
            pub fn less_than(_: @This(), a: Instruction_Encoding, b: Instruction_Encoding) bool {
                return a.len() < b.len();
            }
        };
        std.sort.block(Instruction_Encoding, encodings, Sort_Context{}, Sort_Context.less_than);
        for (all_encodings[encoding_index..][0..encodings.len], encodings) |*ptr, *encoding| {
            ptr.* = encoding;
        }
        encoding_index += encodings.len;
        self.lookup.putAssumeCapacity(entry.key_ptr.*, encodings);
    }

    return self;
}

const Encoding_Context = struct {
    pub fn hash(_: @This(), key: Instruction_Encoding) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, key.signature.mnemonic);
        std.hash.autoHash(&hasher, key.signature.suffix);
        std.hash.autoHashStrat(&hasher, key.signature.params, .Deep);
        return hasher.final();
    }
    pub fn eql(_: @This(), a: Instruction_Encoding, b: Instruction.Signature) bool {
        return a.signature.eql(b);
    }
};

pub const Parser = struct {
    data: *Parser_Data,
    reader: sx.Reader,

    pub fn init(data: *Parser_Data, reader: std.io.AnyReader) Parser {
        return .{
            .data = data,
            .reader = sx.reader(data.temp, reader),
        };
    }

    pub fn deinit(self: *Parser) void {
        self.reader.deinit();
    }

    pub fn next(self: *Parser) !?Instruction_Encoding {
        if (!try self.reader.open()) {
            try self.reader.require_done();
            return null;
        }

        const signature = try self.parse_signature();
        const constraints = try self.parse_constraints();

        while (try self.reader.expression("encode")) {
            var enc: Encoder = .{
                .value = .{ .constant = 0 },
                .domain = .{ .range = .{ .first = 0, .last = 0 }},
                .bit_offset = 0,
                .bit_count = 0,
            };

            while (try self.reader.any_expression()) |expr| {
                if (std.mem.eql(u8, expr, "src")) {
                    enc.value = try self.parse_value_source();
                    try self.reader.require_close();
                } else if (std.mem.eql(u8, expr, "width")) {
                    enc.bit_count = try self.reader.require_any_int(Encoded_Instruction.Bit_Length_Type, 0);
                    try self.reader.require_close();
                } else if (std.mem.eql(u8, expr, "shift")) {
                    enc.bit_offset = try self.reader.require_any_int(Encoded_Instruction.Bit_Length_Type, 0);
                    try self.reader.require_close();
                } else if (std.mem.eql(u8, expr, "unsigned")) {
                    enc.domain = try self.parse_int_domain(.unsigned);
                } else if (std.mem.eql(u8, expr, "signed")) {
                    enc.domain = try self.parse_int_domain(.signed);
                } else if (std.mem.eql(u8, expr, "range")) {
                    enc.domain = try self.parse_range_domain();
                } else if (std.mem.eql(u8, expr, "values")) {
                    enc.domain = try self.parse_enumerated_domain();
                }
            }

            try self.reader.require_close();
            try self.data.encoders.temp.append(self.data.temp, enc);
        }
        const encoders = try self.data.encoders.dedup(self.data);

        try self.reader.require_close();

        return .{
            .signature = signature,
            .constraints = constraints,
            .encoders = encoders,
        };
    }

    fn parse_signature(self: *Parser) !Instruction.Signature {
        const mnemonic = try self.reader.require_any_enum(Mnemonic);
        const suffix = try self.reader.any_enum(Mnemonic_Suffix) orelse .none;

        var param_signatures: []const Parameter.Signature = &.{};
        if (try self.reader.expression("params")) {
            param_signatures = try self.parse_param_signatures();
        }

        return .{
            .mnemonic = mnemonic,
            .suffix = suffix,
            .params = param_signatures,
        };
    }

    fn parse_param_signatures(self: *Parser) ![]const Parameter.Signature {
        while (try self.reader.open()) {
            var param_signature: Parameter.Signature = .{
                .address_space = null,
                .base = .none,
                .offset = .none,
            };

            if (try self.reader.string(".i")) {
                param_signature.address_space = .insn;
            } else if (try self.reader.string(".d")) {
                param_signature.address_space = .data;
            } else if (try self.reader.string(".s")) {
                param_signature.address_space = .stack;
            }

            param_signature.base = try self.parse_param_kind();
            if (try self.reader.string("+")) {
                param_signature.offset = try self.parse_param_kind();
            }

            try self.reader.require_close();
            try self.data.param_signatures.temp.append(self.data.temp, param_signature);
        }

        try self.reader.require_close();
        return self.data.param_signatures.dedup(self.data);
    }

    fn parse_constraints(self: *Parser) ![]const Constraint {
        while (try self.reader.expression("constrain")) {
            const left = try self.parse_value_source();

            var kind: Constraint.Kind = undefined;
            if (try self.reader.string("==")) {
                kind = .equal;
            } else if (try self.reader.string("!=")) {
                kind = .not_equal;
            } else if (try self.reader.string(">=")) {
                kind = .greater_or_equal;
            } else if (try self.reader.string(">")) {
                kind = .greater;
            } else return error.SExpressionSyntaxError;

            const right = try self.parse_value_source();
            try self.reader.require_close();

            try self.data.constraints.temp.append(self.data.temp, .{
                .left = left,
                .right = right,
                .kind = kind,
            });
        }
        return self.data.constraints.dedup(self.data);
    }

    fn parse_param_kind(self: *Parser) !Parameter.Kind {
        if (try self.reader.string("->")) {
            return .arrow;
        }

        if (try self.reader.string("k")) {
            return .constant;
        }

        if (try self.reader.any_enum(isa.Special_Register)) |sr| {
            return .{ .sr = sr };
        }

        if (try self.reader.string("r")) {
            return .{ .reg = try self.parse_signedness() };
        }

        return error.SExpressionSyntaxError;
    }

    fn parse_value_source(self: *Parser) !Value {
        if (try self.reader.expression("negate")) {
            const inner = try self.parse_value_source();
            const array = try self.data.inner_values.intern(&.{ inner }, self.data);
            try self.reader.require_close();
            return .{ .negate = &array[0] };
        } else if (try self.reader.expression("xor")) {
            const mask = try self.reader.require_any_int(i64, 0);
            const inner = try self.parse_value_source();
            const array = try self.data.inner_values.intern(&.{ inner }, self.data);
            try self.reader.require_close();
            return .{ .xor = .{
                .inner = &array[0],
                .mask = mask,
            } };
        } else if (try self.reader.expression("offset")) {
            const offset = try self.reader.require_any_int(i64, 0);
            const inner = try self.parse_value_source();
            const array = try self.data.inner_values.intern(&.{ inner }, self.data);
            try self.reader.require_close();
            return .{ .offset = .{
                .inner = &array[0],
                .offset = offset,
            } };
        } else if (try self.parse_constant_optional()) |value| {
            return .{ .constant = value };
        } else {
            return .{ .placeholder = try self.parse_placeholder_info() };
        }
    }

    fn parse_constant_optional(self: *Parser) !?i64 {
        if (try self.reader.expression("constant")) {
            const value = try self.reader.require_any_int(i64, 0);
            try self.reader.require_close();
            return value;
        }
        return null;
    }

    fn parse_placeholder_info(self: *Parser) !Placeholder_Info {
        if (try self.reader.expression("param-constant")) {
            const index = Parameter.Index.init(try self.reader.require_any_int(Parameter.Index.Raw, 0));
            const name = try self.data.strings.intern((try self.reader.any_string()) orelse "", self.data);
            try self.reader.require_close();
            return .{
                .kind = .param_constant,
                .index = index,
                .name = name,
            };
        }

        if (try self.reader.expression("param-base-reg")) {
            const index = Parameter.Index.init(try self.reader.require_any_int(Parameter.Index.Raw, 0));
            const name = try self.data.strings.intern((try self.reader.any_string()) orelse "", self.data);
            try self.reader.require_close();
            return .{
                .kind = .param_base_register,
                .index = index,
                .name = name,
            };
        }

        if (try self.reader.expression("param-offset-reg")) {
            const index = Parameter.Index.init(try self.reader.require_any_int(Parameter.Index.Raw, 0));
            const name = try self.data.strings.intern((try self.reader.any_string()) orelse "", self.data);
            try self.reader.require_close();
            return .{
                .kind = .param_offset_register,
                .index = index,
                .name = name,
            };
        }

        return error.SExpressionSyntaxError;
    }

    fn parse_signedness(self: *Parser) !?Signedness {
        if (try self.reader.string(".unsigned")) return .unsigned;
        if (try self.reader.string(".signed")) return .signed;
        return null;
    }

    fn parse_int_domain(self: *Parser, signedness: Signedness) !Domain {
        const b = try self.reader.require_any_int(u6, 0);

        var multiple: u8 = 1;
        if (try self.reader.expression("multiple")) {
            multiple = try self.reader.require_any_int(u8, 0);
            try self.reader.require_close();
        }

        try self.reader.require_close();
        return .{ .int = .{
            .signedness = signedness,
            .bits = b,
            .multiple = multiple,
        }};
    }

    fn parse_range_domain(self: *Parser) !Domain {
        const first = try self.reader.require_any_int(i64, 0);
        const last = try self.reader.require_any_int(i64, 0);
        try self.reader.require_close();
        return .{ .range = .{
            .first = first,
            .last = last,
        }};
    }

    fn parse_enumerated_domain(self: *Parser) !Domain {
        while (try self.reader.any_int(i64, 0)) |value| {
            try self.data.enumerated_values.temp.append(self.data.temp, value);
        }
        try self.reader.require_close();
        return .{ .enumerated = try self.data.enumerated_values.dedup(self.data) };
    }
};

pub const Parser_Data = struct {
    arena: std.mem.Allocator,
    temp: std.mem.Allocator,

    param_signatures: Array_Intern_Pool(Parameter.Signature) = .{},
    constraints: Array_Intern_Pool(Constraint) = .{},
    encoders: Array_Intern_Pool(Encoder) = .{},
    enumerated_values: Array_Intern_Pool(i64) = .{},
    inner_values: Array_Intern_Pool(Value) = .{},
    strings: Array_Intern_Pool(u8) = .{},

    pub fn deinit(self: *Parser_Data) void {
        self.param_signatures.deinit(self.temp);
        self.constraints.deinit(self.temp);
        self.encoders.deinit(self.temp);
        self.enumerated_values.deinit(self.temp);
        self.inner_values.deinit(self.temp);
        self.strings.deinit(self.temp);
    }
};

fn Array_Intern_Pool(comptime T: type) type {
    return struct {
        const Pool = @This();

        pool: deep_hash_map.DeepRecursiveAutoHashMapUnmanaged([]const T, void) = .{},
        temp: std.ArrayListUnmanaged(T) = .{},

        pub fn deinit(self: *Pool, temp: std.mem.Allocator) void {
            // Note we don't free the array data; we're assuming it's in an arena.
            self.pool.deinit(temp);
            self.temp.deinit(temp);
        }

        pub fn dedup(self: *Pool, data: *Parser_Data) ![]const T {
            defer self.temp.clearRetainingCapacity();
            return self.intern(self.temp.items, data);
        }

        pub fn intern(self: *Pool, items: []const T, data: *Parser_Data) ![]const T {
            const result = try self.pool.getOrPut(data.temp, items);
            if (result.found_existing) {
                return result.key_ptr.*;
            } else {
                const interned = try data.arena.dupe(T, items);
                result.key_ptr.* = interned;
                return interned;
            }
        }
    };
}

const Encoding_Database = @import("Encoding_Database.zig");
const Decoding_Database = @import("Decoding_Database.zig");
const Placeholder_Info = Instruction_Encoding.Placeholder_Info;
const Domain = Instruction_Encoding.Domain;
const Constraint = Instruction_Encoding.Constraint;
const Encoder = Instruction_Encoding.Encoder;
const Value = Instruction_Encoding.Value;
const Instruction_Encoding = isa.Instruction_Encoding;
const Instruction = isa.Instruction;
const Parameter = isa.Parameter;
const Mnemonic = isa.Mnemonic;
const Mnemonic_Suffix = isa.Mnemonic_Suffix;
const Encoded_Instruction = isa.Encoded_Instruction;
const isa = @import("isa");
const sx = @import("sx");
const deep_hash_map = @import("deep_hash_map");
const Signedness = std.builtin.Signedness;
const std = @import("std");
