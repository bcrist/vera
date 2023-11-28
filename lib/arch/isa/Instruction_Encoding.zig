signature: Instruction_Signature,
constraints: []const Constraint,
encoders: []const Encoder, // If there are multiple encoders, each must correspond to a unique subset of the bits of the instruction.

pub const Value = union (enum) {
    constant: i64,
    placeholder: Placeholder_Info,

    pub fn evaluate(self: Value, params: []const Parameter) i64 {
        return switch (self) {
            .constant => |v| v,
            .placeholder => |info| switch (info.kind) {
                .param_constant => params[info.index.raw()].constant,
                .param_base_register => params[info.index.raw()].base_register_index,
                .param_offset_register => params[info.index.raw()].offset_register_index,
            },
        };
    }

    pub fn assign(self: Value, value: i64, out: []Parameter) bool {
        switch (self) {
            .constant => |v| return v == value,
            .placeholder => |info| switch (info.kind) {
                .param_constant => out[info.index.raw()].constant = value,
                .param_base_register => {
                    if (value < 0 or value >= arch.hw.register_count) return false;
                    out[info.index.raw()].base_register_index = @intCast(value);
                },
                .param_offset_register => {
                    if (value < 0 or value >= arch.hw.register_count) return false;
                    out[info.index.raw()].offset_register_index = @intCast(value);
                },
            },
        }
        return true;
    }
};

pub const Placeholder_Info = struct {
    index: isa.Parameter.Index,
    kind: Placeholder_Kind,
    name: []const u8,
};

pub const Placeholder_Kind = enum {
    param_constant,
    param_base_register,
    param_offset_register,
};

pub const Constraint = struct {
    left: Value,
    right: Value,
    kind: Kind,

    pub const Kind = enum {
        equal,
        not_equal,
        greater,
        greater_or_equal,
    };

    pub fn matches(self: Constraint, params: []const Parameter) bool {
        const left = self.left.evaluate(params);
        const right = self.left.evaluate(params);
        return switch (self.kind) {
            .equal => left == right,
            .not_equal => left != right,
            .greater => left > right,
            .greater_or_equal => left >= right,
        };
    }
};

pub const Domain = union (enum) {
    int: struct {
        signedness: Signedness,
        bits: u6,
        multiple: u8,
    },
    range: struct {
        min: i64,
        max: i64,
    },
    enumerated: []const i64,

    pub fn max_encoded(self: Domain) u64 {
        return switch (self) {
            .int => |info| (@as(u64, 1) << info.bits) - 1,
            .range => |range| @intCast(range.max - range.min),
            .enumerated => |values| values.len - 1,
        };
    }

    /// Returns null if the provided value is outside the represented domain
    pub fn encode(self: Domain, value: i64) ?u64 {
        switch (self) {
            .int => |info| {
                const compressed = std.math.divExact(i64, value, info.multiple) catch return null;
                switch (info.signedness) {
                    .unsigned => {
                        if (compressed < 0) return null;
                        if (compressed >= @as(i64, 1) << info.bits) return null;
                    },
                    .signed => {
                        const limit = @as(i64, 1) << (info.bits - 1);
                        if (compressed < -limit) return null;
                        if (compressed >= limit) return null;
                    },
                }
                const mask = (@as(u64, 1) << info.bits) - 1;
                const unsigned: u64 = @bitCast(compressed);
                return unsigned & mask;
            },
            .range => |range| {
                return if (value >= range.min and value <= range.max) @bitCast(value - range.min) else null;
            },
            .enumerated => |values| {
                for (0.., values) |i, v| {
                    if (v == value) return i;
                } else return null;
            },
        }
    }

    /// Returns null if the provided raw data would never be returned by a call to encode()
    pub fn decode(self: Domain, raw: u64) ?i64 {
        switch (self) {
            .int => |info| {
                const mask = (@as(u64, 1) << info.bits) - 1;
                if ((raw & mask) != raw) return null;
                switch (info.signedness) {
                    .unsigned => {
                        return @intCast(raw);
                    },
                    .signed => {
                        const sign: u1 = @truncate(raw >> (info.bits - 1));
                        if (sign == 1) {
                            const sign_extended = (~@as(u64, 0) << info.bits) | raw;
                            return @bitCast(sign_extended);
                        } else {
                            return @intCast(raw);
                        }
                    },
                }
            },
            .range => |range| {
                const diff: u64 = @intCast(range.max - range.min);
                if (raw > diff) return null;
                const int: i64 = @intCast(raw);
                return range.min + int;
            },
            .enumerated => |values| {
                return if (raw < values.len) values[raw] else null;
            },
        }
    }
};

pub const Encoder = @import("Encoder.zig");

pub fn bits(self: Instruction_Encoding) Encoded_Instruction.Bit_Length_Type {
    var n: Encoded_Instruction.Bit_Length_Type = 0;
    for (self.encoders) |enc| {
        n = @max(n, enc.required_bits());
    }
    return n;
}

pub fn len(self: Instruction_Encoding) Encoded_Instruction.Length_Type {
    return @intCast(self.bits() / 8);
}

pub fn matches(self: Instruction_Encoding, insn: Instruction) bool {
    if (self.signature.mnemonic != insn.mnemonic) return false;
    if (self.signature.suffix != insn.suffix) return false;
    if (self.signature.params.len != insn.params.len) return false;
    for (self.signature.params, insn.params) |ps, param| {
        if (!std.meta.eql(ps, param.signature)) return false;
    }
    for (self.encoders) |enc| {
        const value = enc.value.evaluate(insn.params);
        if (enc.domain.encode(value) == null) return false;
    }
    for (self.constraints) |constraint| {
        if (!constraint.matches(insn.params)) return false;
    }
    return true;
}

pub fn encode(self: Instruction_Encoding, insn: Instruction) Encoded_Instruction {
    std.debug.assert(self.signature.mnemonic == insn.mnemonic);
    std.debug.assert(self.signature.suffix == insn.suffix);

    std.debug.assert(self.signature.params.len == insn.params.len);
    for (self.signature.params, insn.params) |ps, param| {
        std.debug.assert(std.meta.eql(ps, param.signature));
    }

    for (self.constraints) |constraint| {
        std.debug.assert(constraint.matches(insn.params));
    }

    var out: Encoded_Instruction = .{
        .data = 0,
        .len = self.len(),
    };

    for (self.encoders) |enc| {
        const success = enc.encode(insn, &out.data);
        std.debug.assert(success);
    }

    return out;
}

pub fn matches_data(self: Instruction_Encoding, data: Encoded_Instruction.Data) bool {
    var temp = [_]Parameter { .{ .expr_type = .unknown } } ** Parameter.Index.count;

    for (self.encoders) |enc| {
        if (!enc.decode(data, &temp)) return false;
    }

    for (self.constraints) |constraint| {
        if (!constraint.matches(&temp)) return false;
    }

    return true;
}

pub fn decode_params(self: Instruction_Encoding, data: Encoded_Instruction.Data, params: []Parameter) void {
    // N.B. this assumes that .base_register, .offset_register, and .constant have been set to 0 for all parameters

    std.debug.assert(self.params.len == params.len);

    for (self.params, params) |ps, param| {
        std.debug.assert(std.meta.eql(ps, param.expr_type));
    }

    for (self.encoders) |enc| {
        const success = enc.decode(data, params);
        std.debug.assert(success);
    }
}

pub fn eql(self: Instruction_Encoding, other: Instruction_Encoding) bool {
    return deep_hash_map.deepEql(self, other, .DeepRecursive);
}

pub fn write(self: Instruction_Encoding, comptime W: type, writer: *sx.Writer(W), compact: bool) !void {
    const helpers = struct {
        pub fn write_param_kind(t: Parameter.Kind, w: *sx.Writer(W)) !void {
            switch (t) {
                .none => try w.string("none"),
                .arrow => try w.string("->"),
                .constant => try w.string("k"),
                .reg8, .reg16, .reg32 => |sign| {
                    try w.string(switch (t) {
                        .reg8 => "b",
                        .reg16 => "r",
                        .reg32 => "x",
                        else => unreachable,
                    });
                    if (sign) |s| try w.string(switch (s) {
                        .unsigned => ".unsigned",
                        .signed => ".signed",
                    });
                },
                .sr => |sr| try w.string(@tagName(sr)),
            }
        }

        pub fn write_value_source(v: Value, w: *sx.Writer(W)) !void {
            switch (v) {
                .constant => |k| {
                    try w.expression("constant");
                    try w.int(k, 10);
                    _ = try w.close();
                },
                .placeholder => |info| {
                    try w.expression(switch (info.kind) {
                        .param_constant => "param-constant",
                        .param_base_register => "param-base-reg",
                        .param_offset_register => "param-offset-reg",
                    });
                    try w.int(info.index.raw(), 10);
                    try w.string(info.name);
                    _ = try w.close();
                },
            }
        }
    };

    try writer.open();
    try writer.tag(self.signature.mnemonic);
    if (self.signature.suffix != .none) {
        try writer.tag(self.signature.suffix);
    }

    writer.set_compact(compact);

    if (self.signature.params.len > 0) {
        try writer.expression("params");

        for (self.signature.params) |signature| {
            try writer.open();

            if (signature.address_space) |as| {
                try writer.string(as.directive_name());
            }

            try helpers.write_param_kind(signature.base, writer);

            if (signature.offset != .none) {
                try writer.string("+");
                try helpers.write_param_kind(signature.offset, writer);
            }

            _ = try writer.close();
        }
        _ = try writer.close();
    }

    for (self.constraints) |constraint| {
        try writer.expression("constrain");
        try helpers.write_value_source(constraint.left, writer);
        try writer.string(switch (constraint.kind) {
            .equal => "==",
            .not_equal => "!=",
            .greater_or_equal => ">=",
            .greater => ">",
        });
        try helpers.write_value_source(constraint.right, writer);
        _ = try writer.close();
    }

    for (self.encoders) |enc| {
        try writer.expression("encode");

        try writer.expression("src");
        try helpers.write_value_source(enc.value, writer);
        _ = try writer.close();

        switch (enc.domain) {
            .int => |info| {
                try writer.expression(@tagName(info.signedness));
                try writer.int(info.bits, 0);
                if (info.multiple != 1) {
                    try writer.expression("multiple");
                    try writer.int(info.multiple, 0);
                    _ = try writer.close();
                }
                _ = try writer.close();
            },
            .range => |range| {
                try writer.expression("range");
                try writer.int(range.min, 0);
                try writer.int(range.max, 0);
                _ = try writer.close();
            },
            .enumerated => |values| {
                try writer.expression("values");
                for (values) |v| try writer.int(v, 0);
                _ = try writer.close();
            },
        }

        if (enc.arithmetic_offset > 0) {
            try writer.expression("base");
            try writer.int(enc.arithmetic_offset, 0);
            _ = try writer.close();
        }

        if (enc.bit_offset > 0) {
            try writer.expression("shift");
            try writer.int(enc.bit_offset, 0);
            _ = try writer.close();
        }

        _ = try writer.close();
    }

    _ = try writer.close();
}


pub const Parser_Data = struct {
    data_allocator: std.mem.Allocator,
    temp_allocator: std.mem.Allocator,

    param_signatures: deep_hash_map.DeepAutoHashMapUnmanaged([]const Parameter.Signature, void) = .{},
    constraints: deep_hash_map.DeepRecursiveAutoHashMapUnmanaged([]const Constraint, void) = .{},
    encoders: deep_hash_map.DeepRecursiveAutoHashMapUnmanaged([]const Encoder, void) = .{},
    enumerated_values: deep_hash_map.DeepAutoHashMapUnmanaged([]const i64, void) = .{},

    temp_param_signatures: std.ArrayListUnmanaged(Parameter.Signature) = .{},
    temp_constraints: std.ArrayListUnmanaged(Constraint) = .{},
    temp_encoders: std.ArrayListUnmanaged(Encoder) = .{},
    temp_enumerated_values: std.ArrayListUnmanaged(i64) = .{},

    pub fn deinit(self: *Parser_Data) void {
        self.temp_param_signatures.deinit(self.temp_allocator);
        self.temp_constraints.deinit(self.temp_allocator);
        self.temp_encoders.deinit(self.temp_allocator);
        self.temp_enumerated_values.deinit(self.temp_allocator);

        self.param_signatures.deinit(self.temp_allocator);
        self.constraints.deinit(self.temp_allocator);
        self.encoders.deinit(self.temp_allocator);
        self.enumerated_values.deinit(self.temp_allocator);
    }

    pub fn free_data(self: *Parser_Data) void {
        var param_signatures_iter = self.param_signatures.keyIterator();
        while (param_signatures_iter.next()) |k| {
            self.data_allocator.free(k.*);
        }

        var constraints_iter = self.constraints.keyIterator();
        while (constraints_iter.next()) |k| {
            for (k.*) |constraint| {
                switch (constraint.left) {
                    .placeholder => |info| self.data_allocator.free(info.name),
                    else => {},
                }
                switch (constraint.right) {
                    .placeholder => |info| self.data_allocator.free(info.name),
                    else => {},
                }
            }
            self.data_allocator.free(k.*);
        }

        var encoders_iter = self.encoders.keyIterator();
        while (encoders_iter.next()) |k| {
            for (k.*) |encoder| {
                switch (encoder.value) {
                    .placeholder => |info| self.data_allocator.free(info.name),
                    else => {},
                }
            }
            self.data_allocator.free(k.*);
        }

        var enumerated_values_iter = self.enumerated_values.keyIterator();
        while (enumerated_values_iter.next()) |k| {
            self.data_allocator.free(k.*);
        }
    }

    pub fn dedup_param_signatures(self: *Parser_Data) ![]const Parameter.Signature {
        const params = self.temp_param_signatures.items;
        defer self.temp_param_signatures.clearRetainingCapacity();
        const result = try self.param_signatures.getOrPut(self.temp_allocator, params);
        if (!result.found_existing) {
            result.key_ptr.* = try self.data_allocator.dupe(Parameter.Signature, params);
        }
        return result.key_ptr.*;
    }

    pub fn dedup_constraints(self: *Parser_Data) ![]const Constraint {
        const constraints = self.temp_constraints.items;
        defer self.temp_constraints.clearRetainingCapacity();
        const result = try self.constraints.getOrPut(self.temp_allocator, constraints);
        if (!result.found_existing) {
            const dupe_constraints = try self.data_allocator.dupe(Constraint, constraints);
            for (dupe_constraints) |*constraint| {
                switch (constraint.left) {
                    .placeholder => |*info| info.name = try self.data_allocator.dupe(u8, info.name), // TODO string intern pool
                    else => {},
                }
                switch (constraint.right) {
                    .placeholder => |*info| info.name = try self.data_allocator.dupe(u8, info.name), // TODO string intern pool
                    else => {},
                }
            }
            result.key_ptr.* = dupe_constraints;
        }
        return result.key_ptr.*;
    }

    pub fn dedup_encoders(self: *Parser_Data) ![]const Encoder {
        const encoders = self.temp_encoders.items;
        defer self.temp_encoders.clearRetainingCapacity();
        const result = try self.encoders.getOrPut(self.temp_allocator, encoders);
        if (!result.found_existing) {
            const dupe_encoders = try self.data_allocator.dupe(Constraint, encoders);
            for (dupe_encoders) |*encoder| {
                switch (encoder.value) {
                    .placeholder => |*info| info.name = try self.data_allocator.dupe(u8, info.name), // TODO string intern pool
                    else => {},
                }
            }
            result.key_ptr.* = dupe_encoders;
        }
        return result.key_ptr.*;
    }

    pub fn dedup_enumerated_values(self: *Parser_Data) ![]const i64 {
        const values = self.temp_enumerated_values.items;
        defer self.temp_enumerated_values.clearRetainingCapacity();
        const result = try self.enumerated_values.getOrPut(self.temp_allocator, values);
        if (!result.found_existing) {
            result.key_ptr.* = try self.data_allocator.dupe(i64, values);
        }
        return result.key_ptr.*;
    }

};

pub fn parser(data: *Parser_Data, reader: anytype) Parser(@TypeOf(reader)) {
    return Parser(@TypeOf(reader)).init(data, reader);
}

pub fn Parser(comptime Reader: type) type {
    return struct {
        const Self = @This();

        data: *Parser_Data,
        reader: sx.Reader(Reader),
        first: bool = true,

        pub fn init(data: *Parser_Data, reader: Reader) Self {
            return .{
                .data = data,
                .reader = sx.reader(data.temp_allocator, reader),
            };
        }

        pub fn deinit(self: *Self) void {
            self.reader.deinit();
        }

        pub fn next(self: *Self) !?Instruction_Encoding {
            if (self.first) {
                try self.reader.require_open();
                self.first = false;
            }
            if (!try self.reader.open()) {
                try self.reader.require_close();
                try self.reader.require_done();
                return null;
            }

            const mnemonic = try self.reader.require_any_enum(Mnemonic);
            const suffix = try self.reader.any_enum(Mnemonic_Suffix) orelse .none;
            var param_signatures: []const Parameter.Signature = &.{};

            while (true) {
                if (try self.reader.expression("params")) {
                    param_signatures = try self.parse_param_signatures();
                } else if (try self.reader.expression("encode")) {
                    try self.data.temp_encoders.append(self.data.temp_allocator, try self.parse_encoder());
                } else if (try self.reader.expression("constrain")) {
                    try self.data.temp_constraints.append(self.data.temp_allocator, try self.parse_constraint());
                }
            }
            try self.reader.require_close();

            return .{
                .mnemonic = mnemonic,
                .suffix = suffix,
                .params = param_signatures,
                .constraints = self.data.dedup_constraints(),
                .encoders = self.data.dedup_encoders(),
            };
        }

        fn parse_param_signatures(self: *Self) ![]const Parameter.Signature {
            while (try self.reader.open()) {
                var signature: Parameter.Signature = .{
                    .address_space = null,
                    .base = .none,
                    .offset = .none,
                };

                if (try self.reader.string(".i")) {
                    signature.address_space = .insn;
                } else if (try self.reader.string(".d")) {
                    signature.address_space = .data;
                } else if (try self.reader.string(".s")) {
                    signature.address_space = .stack;
                }

                signature.base = try self.parse_param_kind();
                if (try self.reader.string("+")) {
                    signature.offset = try self.parse_param_kind();
                }

                try self.reader.require_close();
                try self.data.temp_param_signatures.append(self.data.temp_allocator, signature);
            }

            try self.reader.require_close();
            return self.data.dedup_param_signatures();
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

        fn parse_signedness(self: *Self) !?Signedness {
            if (try self.reader.string(".unsigned")) return .unsigned;
            if (try self.reader.string(".signed")) return .signed;
            return null;
        }

        fn parse_encoder(self: *Self) !Encoder {
            var enc: Encoder = .{
                .value = .{ .constant = 0 },
                .domain = .{ .range = .{ .min = 0, .max = 0 }},
                .arithmetic_offset = 0,
                .bit_offset = 0,
            };

            while (try self.reader.any_expression()) |expr| {
                if (std.mem.eql(u8, expr, "src")) {
                    enc.value = try self.parse_value_source();
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

            return enc;
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
            var min = try self.reader.require_any_int(i64, 0);
            var max = try self.reader.require_any_int(i64, 0);
            try self.reader.require_close();
            return .{ .range = .{
                .min = min,
                .max = max,
            }};
        }

        fn parse_enumerated_domain(self: *Self) !Domain {
            while (try self.reader.any_int(i64, 0)) |value| {
                try self.data.temp_enumerated_values.append(self.data.temp_allocator, value);
            }
            try self.reader.require_close();
            return .{ .enumerated = try self.data.dedup_enumerated_values() };
        }

        fn parse_constraint(self: *Self) !Constraint {
            const left = try self.parse_value_source();

            var kind: Constraint.Kind = undefined;
            if (try self.reader.string("!=")) {
                kind = .not_equal;
            } else if (try self.reader.string(">=")) {
                kind = .greater_or_equal;
            } else if (try self.reader.string(">")) {
                kind = .greater;
            } else return error.SExpressionSyntaxError;

            const right = try self.parse_value_source();
            try self.reader.require_close();

            return .{
                .left = left,
                .right = right,
                .kind = kind,
            };
        }

        fn parse_value_source(self: *Self) !Value {
            if (try self.reader.expression("constant")) {
                const value = try self.reader.require_any_int(i64, 0);
                try self.reader.require_close();
                return .{ .constant = value };
            }

            if (try self.reader.expression("param-constant")) {
                const index = Parameter.Index.init(try self.reader.require_any_int(Parameter.Index.Raw, 0));
                const name = try self.data.temp_allocator.dupe(u8, try self.reader.require_any_string());
                try self.reader.require_close();
                return .{ .placeholder = .{
                    .kind = .param_constant,
                    .index = index,
                    .name = name,
                }};
            }

            if (try self.reader.expression("param-base-reg")) {
                const index = Parameter.Index.init(try self.reader.require_any_int(Parameter.Index.Raw, 0));
                const name = try self.data.temp_allocator.dupe(u8, try self.reader.require_any_string());
                try self.reader.require_close();
                return .{ .placeholder = .{
                    .kind = .param_base_register,
                    .index = index,
                    .name = name,
                }};
            }

            if (try self.reader.expression("param-offset-reg")) {
                const index = Parameter.Index.init(try self.reader.require_any_int(Parameter.Index.Raw, 0));
                const name = try self.data.temp_allocator.dupe(u8, try self.reader.require_any_string());
                try self.reader.require_close();
                return .{ .placeholder = .{
                    .kind = .param_offset_register,
                    .index = index,
                    .name = name,
                 }};
            }

            return error.SExpressionSyntaxError;
        }
    };
}

const Instruction_Encoding = @This();
const Instruction = @import("Instruction.zig");
const Parameter = @import("Parameter.zig");
const Mnemonic = isa.Mnemonic;
const Mnemonic_Suffix = isa.Mnemonic_Suffix;
const Encoded_Instruction = isa.Encoded_Instruction;
const Instruction_Signature = isa.Instruction_Signature;
const isa = arch.isa;
const arch = @import("lib_arch");
const sx = @import("sx");
const deep_hash_map = @import("deep_hash_map");
const Signedness = std.builtin.Signedness;
const std = @import("std");
