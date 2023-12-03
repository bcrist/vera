pub fn write(comptime W: type, writer: *sx.Writer(W), compact: bool, transforms: []const Instruction_Transform, encodings: []const Instruction_Encoding) !void {
    for (transforms) |xform| {
        try write_transform(W, writer, compact, xform);
    }
    for (encodings) |encoding| {
        try write_encoding(W, writer, compact, encoding);
    }
} 

pub fn write_encoding(comptime W: type, writer: *sx.Writer(W), compact: bool, encoding: Instruction_Encoding) !void {
    try writer.open();
    try write_signature(encoding.signature, writer, compact);
    try write_constraints(encoding.constraints, writer);

    for (encoding.encoders) |enc| {
        try writer.expression("encode");

        try writer.expression("src");
        try write_value_source(enc.value, writer);
        _ = try writer.close();

        switch (enc.domain) {
            .int => |info| {
                try writer.expression(@tagName(info.signedness));
                try writer.int(info.bits, 10);
                if (info.multiple != 1) {
                    try writer.expression("multiple");
                    try writer.int(info.multiple, 10);
                    _ = try writer.close();
                }
                _ = try writer.close();
            },
            .range => |range| {
                try writer.expression("range");
                try writer.int(range.first, 10);
                try writer.int(range.last, 10);
                _ = try writer.close();
            },
            .enumerated => |values| {
                try writer.expression("values");
                for (values) |v| try writer.int(v, 10);
                _ = try writer.close();
            },
        }

        if (enc.arithmetic_offset > 0) {
            try writer.expression("base");
            try writer.int(enc.arithmetic_offset, 10);
            _ = try writer.close();
        }

        if (enc.bit_offset > 0) {
            try writer.expression("shift");
            try writer.int(enc.bit_offset, 10);
            _ = try writer.close();
        }

        _ = try writer.close();
    }

    _ = try writer.close();
}

pub fn write_transform(comptime W: type, writer: *sx.Writer(W), compact: bool, transform: Instruction_Transform) !void {
    try writer.expression_expanded("transform");

    try writer.expression("match");
    try write_signature(transform.src_signature, writer, compact);
    if (transform.src_constraints.len > 0) {
        writer.set_compact(compact);
        try write_constraints(transform.src_constraints, writer);
    } else {
        writer.set_compact(true);
    }
    _ = try writer.close();

    try writer.expression("result");
    try write_signature(transform.dest_signature, writer, compact);
    if (transform.dest_constant_values.len > 0) {
        writer.set_compact(compact);
        try write_constant_values(transform.dest_constant_values, writer);
    } else {
        writer.set_compact(true);
    }
    _ = try writer.close();

    for (transform.transforms) |xform| {
        try writer.expression("convert");

        try writer.expression("src");
        try write_value_source(.{ .placeholder = xform.src }, writer);
        _ = try writer.close();
        try writer.expression("dest");
        try write_value_source(.{ .placeholder = xform.dest }, writer);
        _ = try writer.close();

        for (xform.ops) |op| {
            try writer.expression(@tagName(op));
            switch (op) {
                .negate => {},
                .offset, .multiply, .divide_trunc => |n| {
                    try writer.int(n, 10);
                },
            }
            _ = try writer.close();
        }

        _ = try writer.close();
    }

    _ = try writer.close();
}

fn write_signature(signature: Instruction_Signature, writer: anytype, compact: bool) !void {
    try writer.tag(signature.mnemonic);
    if (signature.suffix != .none) {
        try writer.tag(signature.suffix);
    }

    if (signature.params.len > 0) {
        try writer.expression("params");

        for (signature.params) |param_signature| {
            try writer.open();

            if (param_signature.address_space) |as| {
                try writer.string(as.directive_name());
            }

            try write_param_kind(param_signature.base, writer);

            if (param_signature.offset != .none) {
                try writer.string("+");
                try write_param_kind(param_signature.offset, writer);
            }

            _ = try writer.close();
        }
        _ = try writer.close();
    }

    writer.set_compact(compact);
}

fn write_constraints(constraints: []const Constraint, writer: anytype) !void {
    for (constraints) |constraint| {
        try writer.expression("constrain");
        try write_value_source(constraint.left, writer);
        try writer.string(switch (constraint.kind) {
            .equal => "==",
            .not_equal => "!=",
            .greater_or_equal => ">=",
            .greater => ">",
        });
        try write_value_source(constraint.right, writer);
        _ = try writer.close();
    }
}

fn write_constant_values(values: []const Instruction_Transform.Constant_Value, writer: anytype) !void {
    for (values) |value| {
        try writer.expression("constrain");
        try write_value_source(.{ .placeholder = value.placeholder }, writer);
        try writer.string("==");
        try write_value_source(.{ .constant = value.constant }, writer);
        _ = try writer.close();
    }
}

fn write_param_kind(t: Parameter.Kind, w: anytype) !void {
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

fn write_value_source(v: Value, w: anytype) !void {
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
            if (info.name.len > 0) {
                try w.string(info.name);
            }
            _ = try w.close();
        },
    }
}

const Domain = Instruction_Encoding.Domain;
const Constraint = Instruction_Encoding.Constraint;
const Encoder = Instruction_Encoding.Encoder;
const Value = Instruction_Encoding.Value;
const Instruction_Encoding = @import("Instruction_Encoding.zig");
const Instruction_Transform = @import("Instruction_Transform.zig");
const Instruction = @import("Instruction.zig");
const Parameter = @import("Parameter.zig");
const Mnemonic = isa.Mnemonic;
const Instruction_Signature = isa.Instruction_Signature;
const isa = arch.isa;
const arch = @import("lib_arch");
const sx = @import("sx");
const deep_hash_map = @import("deep_hash_map");
const Signedness = std.builtin.Signedness;
const std = @import("std");
