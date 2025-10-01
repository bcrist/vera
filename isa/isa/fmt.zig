pub fn print_form(form: Instruction.Form, writer: *std.io.Writer) !void {
    const mnemonic = form.signature.mnemonic.name();
    try writer.writeAll(mnemonic);
    if (mnemonic.len < 5) {
        try writer.splatByteAll(' ', 5 - mnemonic.len);
    }
    var skip_comma = true;
    for (form.signature.params, 0..) |param, raw_index| {
        if (skip_comma) {
            skip_comma = false;
        } else {
            try writer.writeByte(',');
        }
        try writer.writeByte(' ');

        const index = Parameter.Index.init(@intCast(raw_index));

        var base_register: ?arch.bus.K.Read_Index_Offset = null;
        var offset_register: ?arch.bus.K.Read_Index_Offset = null;
        var constant: ?i64 = null;

        for (form.constraints) |constraint| {
            if (constraint.kind == .equal and constraint.right == .constant) {
                if (constraint.left.get_placeholder()) |info| {
                    if (info.param == index) {
                        const c = constraint.right.constant;
                        switch (info.kind) {
                            .param_constant => constant = c,
                            .param_base_register => base_register = .init(@intCast(c)),
                            .param_offset_register => offset_register = .init(@intCast(c)),
                        }
                    }
                }
            }
        }

        try print_parameter_signature(param, .{
            .index = index,
            .encoders = form.encoders,
            .constraints = form.constraints,
            .base_register = base_register,
            .offset_register = offset_register,
            .constant = constant,
        }, writer);
    }
}

pub fn print_instruction(insn: Instruction, insn_address: ?u32, writer: *std.io.Writer) !void {
    const mnemonic = insn.mnemonic.name();
    try writer.writeAll(mnemonic);
    if (mnemonic.len < 5) {
        try writer.splatByteAll(' ', 5 - mnemonic.len);
    }
    var skip_comma = true;
    for (insn.params) |param| {
        if (skip_comma) {
            skip_comma = false;
        } else {
            try writer.writeByte(',');
        }
        try writer.writeByte(' ');
        try print_parameter(param, insn_address, writer);
    }
}

pub fn print_instruction_signature(signature: Instruction.Signature, writer: *std.io.Writer) !void {
    const mnemonic = signature.mnemonic.name();
    try writer.writeAll(mnemonic);
    if (mnemonic.len < 5) {
        try writer.splatByteAll(' ', 5 - mnemonic.len);
    }
    var skip_comma = true;
    for (signature.params) |param_signature| {
        if (param_signature.base == .arrow) {
            skip_comma = true;
        } else if (skip_comma) {
            skip_comma = false;
        } else {
            try writer.writeByte(',');
        }
        try writer.writeByte(' ');
        try print_parameter_signature(param_signature, .{}, writer);
    }
}

pub fn print_parameter(param: Parameter, insn_address: ?u32, writer: *std.io.Writer) !void {
    try print_parameter_signature(param.signature, .{
        .base_register = param.base_register_index,
        .offset_register = param.offset_register_index,
        .constant = param.constant,
        .insn_address = insn_address,
    }, writer);
}

pub const Print_Parameter_Signature_Extra = struct {
    index: ?Parameter.Index = null,
    encoders: []const Encoder = &.{},
    constraints: []const Constraint = &.{},
    base_register: ?arch.bus.K.Read_Index_Offset = null,
    offset_register: ?arch.bus.K.Read_Index_Offset = null,
    constant: ?i64 = null,
    insn_address: ?u32 = null,
};
pub fn print_parameter_signature(signature: Parameter.Signature, extra: Print_Parameter_Signature_Extra, writer: *std.io.Writer) !void {
    if (signature.address_space) |as| {
        try writer.writeAll(as.directive_name());
        try writer.writeByte(' ');
    }

    try print_parameter_kind(signature.base, .param_base_register, .{
        .index = extra.index,
        .encoders = extra.encoders,
        .constraints = extra.constraints,
        .register = extra.base_register,
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
            .register = extra.offset_register,
            .constant = extra.constant,
        }, writer);
    }
}

pub const Print_Parameter_Kind_Extra = struct {
    index: ?Parameter.Index = null,
    encoders: []const Encoder = &.{},
    constraints: []const Constraint = &.{},
    register: ?arch.bus.K.Read_Index_Offset = null,
    constant: ?i64 = null,
};
pub fn print_parameter_kind(kind: Parameter.Kind, register_kind: Placeholder.Kind, extra: Print_Parameter_Kind_Extra, writer: *std.io.Writer) !void {
    switch (kind) {
        .none => {},
        .constant => if (extra.constant) |k| {
            try print_constant(k, writer);
        } else {
            try print_placeholder_list(.param_constant, extra.index, extra.encoders, extra.constraints, writer);
        },
        .reg => |sign| {
            try writer.writeByte('r');
            if (extra.register) |reg| {
                try writer.print("{}", .{ reg.raw() });
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

fn print_placeholder_list(kind: Placeholder.Kind, maybe_index: ?Parameter.Index, encoders: []const Encoder, constraints: []const Constraint, writer: *std.io.Writer) !void {
    try writer.writeByte('(');
    var found_encoder = false;
    if (maybe_index) |index| {
        for (encoders) |enc| {
            if (enc.value.get_placeholder()) |info| {
                if (info.param == index and info.kind == kind and info.name.len > 0) {
                    if (found_encoder) try writer.writeByte(',');
                    try writer.writeAll(info.name);
                    found_encoder = true;
                }
            }
        }
        for (constraints) |c| {
            if (c.kind != .equal) continue;
            if (c.left.get_placeholder()) |left| {
                if (c.right.get_placeholder()) |right| {
                    if (left.param != index or left.kind != kind) continue;
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

pub fn print_constant(constant: i64, writer: *std.io.Writer) !void {
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

pub fn print_offset(offset: i64, writer: *std.io.Writer) !void {
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

const Placeholder = @import("Placeholder.zig");
const Constraint = @import("Constraint.zig");
const Encoder = @import("Encoder.zig");
const Instruction = @import("Instruction.zig");
const Parameter = @import("Parameter.zig");
const Mnemonic = enums.Mnemonic;
const enums = @import("enums.zig");
const arch = @import("arch");
const Signedness = std.builtin.Signedness;
const std = @import("std");
