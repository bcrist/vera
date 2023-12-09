pub fn parse_decoding_db(data: *Parser_Data, source: []const u8) !Decoding_Database {
    var stream = std.io.fixedBufferStream(source);
    var p = parser(data, stream.reader());
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

    while (p.next_encoding_or_transform() catch |err| {
        if (err == error.SExpressionSyntaxError) {
            var stderr = std.io.getStdErr().writer();
            const context = p.reader.token_context() catch return err;
            stderr.writeAll("Syntax error in instruction encoding data:\n") catch return err;
            context.print_for_string(source, stderr, 150) catch return err;
        }
        return err;
    }) |what| switch (what) {
        .transform => {},
        .encoding => |encoding| {
            for (encoding.encoders) |enc| {
                if (enc.value == .constant and enc.bit_offset == 0 and enc.arithmetic_offset == 0) {
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
        },
    };

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
    var p = parser(data, stream.reader());
    defer p.deinit();

    var temp_lookup = deep_hash_map.DeepAutoHashMap(Instruction_Signature, std.ArrayListUnmanaged(Instruction_Encoding)).init(data.temp);
    defer {
        var iter = temp_lookup.valueIterator();
        while (iter.next()) |list| {
            list.deinit(data.temp);
        }
        temp_lookup.deinit();
    }
    var temp_transforms = deep_hash_map.DeepAutoHashMap(Instruction_Signature, std.ArrayListUnmanaged(Instruction_Transform)).init(data.temp);
    defer {
        var iter = temp_transforms.valueIterator();
        while (iter.next()) |list| {
            list.deinit(data.temp);
        }
        temp_transforms.deinit();
    }

    var total_encodings: usize = 0;

    while (p.next_encoding_or_transform() catch |err| {
        if (err == error.SExpressionSyntaxError) {
            var stderr = std.io.getStdErr().writer();
            const context = p.reader.token_context() catch return err;
            stderr.writeAll("Syntax error in instruction encoding data:\n") catch return err;
            context.print_for_string(source, stderr, 150) catch return err;
        }
        return err;
    }) |what| switch (what) {
        .encoding => |encoding| {
            var result = try temp_lookup.getOrPutAdapted(encoding, Encoding_Context{});
            if (!result.found_existing) {
                result.key_ptr.* = encoding.signature;
                result.value_ptr.* = .{};
            }
            try result.value_ptr.append(data.temp, encoding);
            total_encodings += 1;
        },
        .transform => |transform| {
            var result = try temp_transforms.getOrPutAdapted(transform, Transform_Context{});
            if (!result.found_existing) {
                result.key_ptr.* = transform.src_signature;
                result.value_ptr.* = .{};
            }
            try result.value_ptr.append(data.temp, transform);
        },
    };

    var self: Encoding_Database = .{};
    try self.lookup.ensureTotalCapacity(data.arena, temp_lookup.count());
    try self.transforms.ensureTotalCapacity(data.arena, temp_transforms.count());

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

    var transform_iter = temp_transforms.iterator();
    while (transform_iter.next()) |entry| {
        const transforms = try data.arena.dupe(Instruction_Transform, entry.value_ptr.items);
        self.transforms.putAssumeCapacity(entry.key_ptr.*, transforms);
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
    pub fn eql(_: @This(), a: Instruction_Encoding, b: Instruction_Signature) bool {
        return a.signature.eql(b);
    }
};

const Transform_Context = struct {
    pub fn hash(_: @This(), key: Instruction_Transform) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, key.src_signature.mnemonic);
        std.hash.autoHash(&hasher, key.src_signature.suffix);
        std.hash.autoHashStrat(&hasher, key.src_signature.params, .Deep);
        return hasher.final();
    }
    pub fn eql(_: @This(), a: Instruction_Transform, b: Instruction_Signature) bool {
        return a.src_signature.eql(b);
    }
};

pub fn parser(data: *Parser_Data, reader: anytype) Parser(@TypeOf(reader)) {
    return Parser(@TypeOf(reader)).init(data, reader);
}

pub const Encoding_Or_Transform = union (enum) {
    encoding: Instruction_Encoding,
    transform: Instruction_Transform,
};

pub fn Parser(comptime Reader: type) type {
    return struct {
        const Self = @This();

        data: *Parser_Data,
        reader: sx.Reader(Reader),

        pub fn init(data: *Parser_Data, reader: Reader) Self {
            return .{
                .data = data,
                .reader = sx.reader(data.temp, reader),
            };
        }

        pub fn deinit(self: *Self) void {
            self.reader.deinit();
        }

        pub fn next_encoding_or_transform(self: *Self) !?Encoding_Or_Transform {
            if (try self.next_transform_optional()) |transform| {
                return .{ .transform = transform };
            }
            if (try self.next_encoding()) |encoding| {
                return .{ .encoding = encoding };
            }
            return null;
        }

        pub fn next_transform(self: *Self) !?Instruction_Transform {
            if (try self.next_transform_optional()) |transform| {
                return transform;
            } else {
                try self.reader.require_done();
                return null;
            }
        }

        fn next_transform_optional(self: *Self) !?Instruction_Transform {
            if (!try self.reader.expression("transform")) return null;

            try self.reader.require_expression("match");
            const src_signature = try self.parse_signature();
            const constraints = try self.parse_constraints();
            try self.reader.require_close();


            try self.reader.require_expression("result");

            const dest_signature = try self.parse_signature();

            while (try self.reader.expression("constrain")) {
                const placeholder = try self.parse_placeholder_info();
                try self.reader.require_string("==");
                const constant = try self.parse_constant_optional() orelse return error.SExpressionSyntaxError;
                try self.reader.require_close();

                try self.data.constant_values.temp.append(self.data.temp, .{
                    .placeholder = placeholder,
                    .constant = constant,
                });
            }
            const constant_values = try self.data.constant_values.dedup(self.data);

            try self.reader.require_close(); // result

            while (try self.reader.expression("convert")) {
                var src: ?Placeholder_Info = null;
                var dest: ?Placeholder_Info = null;

                while (try self.reader.any_expression()) |expr| {
                    if (std.mem.eql(u8, expr, "src")) {
                        src = try self.parse_placeholder_info();

                    } else if (std.mem.eql(u8, expr, "dest")) {
                        dest = try self.parse_placeholder_info();

                    } else if (std.mem.eql(u8, expr, "negate")) {
                        try self.data.transform_ops.temp.append(self.data.temp, .negate);

                    } else if (std.mem.eql(u8, expr, "offset")) {
                        try self.data.transform_ops.temp.append(self.data.temp, .{
                            .offset = try self.reader.require_any_int(i64, 0),
                        });

                    } else if (std.mem.eql(u8, expr, "multiply")) {
                        try self.data.transform_ops.temp.append(self.data.temp, .{
                            .multiply = try self.reader.require_any_int(i64, 0),
                        });

                    } else if (std.mem.eql(u8, expr, "divide_trunc")) {
                        try self.data.transform_ops.temp.append(self.data.temp, .{
                            .divide_trunc = try self.reader.require_any_int(i64, 0),
                        });

                    } else {
                        return error.SExpressionSyntaxError;
                    }
                    try self.reader.require_close();
                }
                const ops = try self.data.transform_ops.dedup(self.data);

                try self.reader.require_close(); // ops
                try self.data.transforms.temp.append(self.data.temp, .{
                    .src = src orelse return error.SExpressionSyntaxError,
                    .dest = dest orelse return error.SExpressionSyntaxError,
                    .ops = ops,
                });
            }
            const transforms = try self.data.transforms.dedup(self.data);

            try self.reader.require_close(); // transform

            return .{
                .src_signature = src_signature,
                .src_constraints = constraints,
                .dest_signature = dest_signature,
                .dest_constant_values = constant_values,
                .transforms = transforms,
            };
        }

        pub fn next_encoding(self: *Self) !?Instruction_Encoding {
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
                    .arithmetic_offset = 0,
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
                    } else if (std.mem.eql(u8, expr, "base")) {
                        enc.arithmetic_offset = try self.reader.require_any_int(u64, 0);
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

        fn parse_signature(self: *Self) !Instruction_Signature {
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

        fn parse_param_signatures(self: *Self) ![]const Parameter.Signature {
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

        fn parse_constraints(self: *Self) ![]const Constraint {
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

        fn parse_param_kind(self: *Self) !Parameter.Kind {
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
                return .{ .reg16 = try self.parse_signedness() };
            }
            if (try self.reader.string("x")) {
                return .{ .reg32 = try self.parse_signedness() };
            }
            if (try self.reader.string("b")) {
                return .{ .reg8 = try self.parse_signedness() };
            }

            return error.SExpressionSyntaxError;
        }

        fn parse_value_source(self: *Self) !Value {
            if (try self.parse_constant_optional()) |value| {
                return .{ .constant = value };
            } else {
                return .{ .placeholder = try self.parse_placeholder_info() };
            }
        }

        fn parse_constant_optional(self: *Self) !?i64 {
            if (try self.reader.expression("constant")) {
                const value = try self.reader.require_any_int(i64, 0);
                try self.reader.require_close();
                return value;
            }
            return null;
        }

        fn parse_placeholder_info(self: *Self) !Placeholder_Info {
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

        fn parse_signedness(self: *Self) !?Signedness {
            if (try self.reader.string(".unsigned")) return .unsigned;
            if (try self.reader.string(".signed")) return .signed;
            return null;
        }

        fn parse_int_domain(self: *Self, signedness: Signedness) !Domain {
            var b = try self.reader.require_any_int(u6, 0);

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

        fn parse_range_domain(self: *Self) !Domain {
            var first = try self.reader.require_any_int(i64, 0);
            var last = try self.reader.require_any_int(i64, 0);
            try self.reader.require_close();
            return .{ .range = .{
                .first = first,
                .last = last,
            }};
        }

        fn parse_enumerated_domain(self: *Self) !Domain {
            while (try self.reader.any_int(i64, 0)) |value| {
                try self.data.enumerated_values.temp.append(self.data.temp, value);
            }
            try self.reader.require_close();
            return .{ .enumerated = try self.data.enumerated_values.dedup(self.data) };
        }
    };
}




pub const Parser_Data = struct {
    arena: std.mem.Allocator,
    temp: std.mem.Allocator,

    param_signatures: Array_Intern_Pool(Parameter.Signature) = .{},
    constant_values: Array_Intern_Pool(Constant_Value) = .{},
    transforms: Array_Intern_Pool(Transform) = .{},
    transform_ops: Array_Intern_Pool(Instruction_Transform.Op) = .{},
    constraints: Array_Intern_Pool(Constraint) = .{},
    encoders: Array_Intern_Pool(Encoder) = .{},
    enumerated_values: Array_Intern_Pool(i64) = .{},
    strings: Array_Intern_Pool(u8) = .{},

    pub fn deinit(self: *Parser_Data) void {
        self.param_signatures.deinit(self.temp);
        self.constant_values.deinit(self.temp);
        self.transforms.deinit(self.temp);
        self.transform_ops.deinit(self.temp);
        self.constraints.deinit(self.temp);
        self.encoders.deinit(self.temp);
        self.enumerated_values.deinit(self.temp);
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

const Encoding_Database = isa.Encoding_Database;
const Decoding_Database = isa.Decoding_Database;
const Transform = Instruction_Transform.Transform;
const Constant_Value = Instruction_Transform.Constant_Value;
const Instruction_Transform = isa.Instruction_Transform;
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
const Instruction_Signature = isa.Instruction_Signature;
const isa = @import("../isa.zig");
const sx = @import("sx");
const deep_hash_map = @import("deep_hash_map");
const Signedness = std.builtin.Signedness;
const std = @import("std");
