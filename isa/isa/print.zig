pub fn print_mnemonic_and_suffix(mnemonic: Mnemonic, suffix: Mnemonic_Suffix, writer: anytype) !usize {
    const mnemonic_str = @tagName(mnemonic);
    var len = mnemonic_str.len;
    try writer.writeAll(mnemonic_str);
    if (suffix != .none) {
        const suffix_str = @tagName(suffix);
        try writer.writeByte('.');
        for (suffix_str) |b| {
            try writer.writeByte(if (b == '_') '.' else b);
        }
        len += suffix_str.len + 1;
    }
    return len;
}

pub fn print_encoding(encoding: Instruction_Encoding, writer: anytype) !void {
    const len = try print_mnemonic_and_suffix(encoding.signature.mnemonic, encoding.signature.suffix, writer);
    if (len < 5) {
        try writer.writeByteNTimes(' ', 5 - len);
    }
    var skip_comma = true;
    for (encoding.signature.params, 0..) |param, raw_index| {
        if (param.base == .arrow) {
            skip_comma = true;
        } else if (skip_comma) {
            skip_comma = false;
        } else {
            try writer.writeByte(',');
        }
        try writer.writeByte(' ');

        const index = Parameter.Index.init(@intCast(raw_index));

        var base_register_index: ?Register_Index = null;
        var offset_register_index: ?Register_Index = null;
        var constant: ?i64 = null;

        for (encoding.constraints) |constraint| {
            if (constraint.kind == .equal and constraint.right == .constant) {
                if (constraint.left.get_placeholder_info()) |info| {
                    if (info.index == index) {
                        const c = constraint.right.constant;
                        switch (info.kind) {
                            .param_constant => constant = c,
                            .param_base_register => base_register_index = @intCast(c),
                            .param_offset_register => offset_register_index = @intCast(c),
                        }
                    }
                }
            }
        }

        try print_parameter_signature(param, .{
            .index = index,
            .encoders = encoding.encoders,
            .constraints = encoding.constraints,
            .base_register_index = base_register_index,
            .offset_register_index = offset_register_index,
            .constant = constant,
        }, writer);
    }
}

pub fn print_instruction(insn: Instruction, insn_address: ?u32, writer: anytype) !void {
    const len = try print_mnemonic_and_suffix(insn.mnemonic, insn.suffix, writer);
    if (len < 5) {
        try writer.writeByteNTimes(' ', 5 - len);
    }
    var skip_comma = true;
    for (insn.params) |param| {
        if (param.signature.base == .arrow) {
            skip_comma = true;
        } else if (skip_comma) {
            skip_comma = false;
        } else {
            try writer.writeByte(',');
        }
        try writer.writeByte(' ');
        try print_parameter(param, insn_address, writer);
    }
}

pub fn print_parameter(param: Parameter, insn_address: ?u32, writer: anytype) !void {
    try print_parameter_signature(param.signature, .{
        .base_register_index = param.base_register_index,
        .offset_register_index = param.offset_register_index,
        .constant = param.constant,
        .insn_address = insn_address,
    }, writer);
}

pub const Print_Parameter_Signature_Extra = struct {
    index: ?Parameter.Index = null,
    encoders: []const Encoder = &.{},
    constraints: []const Constraint = &.{},
    base_register_index: ?Register_Index = null,
    offset_register_index: ?Register_Index = null,
    constant: ?i64 = null,
    insn_address: ?u32 = null,
};
pub fn print_parameter_signature(signature: Parameter.Signature, extra: Print_Parameter_Signature_Extra, writer: anytype) !void {
    if (signature.address_space) |as| {
        try writer.writeAll(as.directive_name());
        try writer.writeByte(' ');
    }

    try print_parameter_kind(signature.base, .param_base_register, .{
        .index = extra.index,
        .encoders = extra.encoders,
        .constraints = extra.constraints,
        .register_index = extra.base_register_index,
        .constant = extra.constant,
    }, writer);

    if (signature.offset == .constant) {
        if (extra.constant) |k| {
            try print_offset(k, writer);
            if (signature.base == .sr and signature.base.sr == .ip) {
                if (extra.insn_address) |address| {
                    const target = address + k;
                    try writer.print(" // 0x{X}", .{ target });
                }
            }
            return;
        }
    }

    if (signature.offset != .none) {
        try writer.writeAll(" + ");
        try print_parameter_kind(signature.offset, .param_offset_register, .{
            .index = extra.index,
            .encoders = extra.encoders,
            .constraints = extra.constraints,
            .register_index = extra.offset_register_index,
            .constant = extra.constant,
        }, writer);
    }
}

pub const Print_Parameter_Kind_Extra = struct {
    index: ?Parameter.Index = null,
    encoders: []const Encoder = &.{},
    constraints: []const Constraint = &.{},
    register_index: ?Register_Index = null,
    constant: ?i64 = null,
};
pub fn print_parameter_kind(kind: Parameter.Kind, register_kind: Instruction_Encoding.Placeholder_Kind, extra: Print_Parameter_Kind_Extra, writer: anytype) !void {
    switch (kind) {
        .none => {},
        .arrow => try writer.writeAll("->"),
        .constant => if (extra.constant) |k| {
            try print_constant(k, writer);
        } else {
            try print_placeholder_list(.param_constant, extra.index, extra.encoders, extra.constraints, writer);
        },
        .reg => |sign| {
            try writer.writeByte('r');
            if (extra.register_index) |reg| {
                try writer.print("{}", .{ reg });
            } else {
                try print_placeholder_list(register_kind, extra.index, extra.encoders, extra.constraints, writer);
            }
            if (sign) |s| {
                try writer.writeAll(switch (s) {
                    .signed => " .signed",
                    .unsigned => " .unsigned",
                });
            }
        },
        .sr => |sr| {
            try writer.writeAll(@tagName(sr));
        }
    }
}

fn print_placeholder_list(kind: Instruction_Encoding.Placeholder_Kind, maybe_index: ?Parameter.Index, encoders: []const Encoder, constraints: []const Constraint, writer: anytype) !void {
    try writer.writeByte('(');
    var found_encoder = false;
    if (maybe_index) |index| {
        for (encoders) |enc| {
            if (enc.value.get_placeholder_info()) |info| {
                if (info.index == index and info.kind == kind and info.name.len > 0) {
                    if (found_encoder) try writer.writeByte(',');
                    try writer.writeAll(info.name);
                    found_encoder = true;
                }
            }
        }
        for (constraints) |c| {
            if (c.kind != .equal) continue;
            if (c.left.get_placeholder_info()) |left| {
                if (c.right.get_placeholder_info()) |right| {
                    if (left.index != index or left.kind != kind) continue;
                    if (found_encoder) try writer.writeByte(',');
                    try writer.writeAll(right.name);
                    found_encoder = true;
                }
            }
        }
    }
    if (!found_encoder) {
        try writer.writeByte('?');
    }
    try writer.writeByte(')');
}

pub fn print_constant(constant: i64, writer: anytype) !void {
    if (constant < -15) {
        return writer.print("-0x{X}", .{ @abs(constant) });
    } else if (constant > 15) {
        return writer.print("0x{X}", .{ constant });
    } else {
        return writer.print("{}", .{ constant });
    }
}

pub fn buf_print_constant(buf: []u8, constant: i64) ![]const u8 {
    if (constant < -15) {
        return std.fmt.bufPrint(buf, "-0x{X}", .{ @abs(constant) });
    } else if (constant > 15) {
        return std.fmt.bufPrint(buf, "0x{X}", .{ constant });
    } else {
        return std.fmt.bufPrint(buf, "{}", .{ constant });
    }
}

pub fn print_offset(offset: i64, writer: anytype) !void {
    if (offset < -15) {
        return writer.print("- 0x{X}", .{ @abs(offset) });
    } else if (offset > 15) {
        return writer.print("+ 0x{X}", .{ offset });
    } else if (offset < 0) {
        return writer.print("- {}", .{ @abs(offset) });
    } else {
        return writer.print("+ {}", .{ offset });
    }
}

pub fn buf_print_offset(buf: []u8, offset: i64) ![]const u8 {
    if (offset < -15) {
        return std.fmt.bufPrint(buf, "- 0x{X}", .{ @abs(offset) });
    } else if (offset > 15) {
        return std.fmt.bufPrint(buf, "+ 0x{X}", .{ offset });
    } else if (offset < 0) {
        return std.fmt.bufPrint(buf, "- {}", .{ @abs(offset) });
    } else {
        return std.fmt.bufPrint(buf, "+ {}", .{ offset });
    }
}

const Constraint = Instruction_Encoding.Constraint;
const Encoder = Instruction_Encoding.Encoder;
const Instruction_Encoding = isa.Instruction_Encoding;
const Instruction = isa.Instruction;
const Parameter = isa.Parameter;
const Mnemonic = isa.Mnemonic;
const Mnemonic_Suffix = isa.Mnemonic_Suffix;
const isa = @import("../isa.zig");
const Register_Index = arch.Register_Index;
const arch = @import("arch");
const Signedness = std.builtin.Signedness;
const std = @import("std");
