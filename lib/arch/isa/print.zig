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
    for (encoding.signature.params) |param| {
        if (param.base == .arrow) {
            skip_comma = true;
        } else if (skip_comma) {
            skip_comma = false;
        } else {
            try writer.writeByte(',');
        }
        try writer.writeByte(' ');
        try print_parameter_signature(param, .{}, writer);
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

    try print_parameter_kind(signature.base, extra.base_register_index, extra.constant, writer);

    if (signature.offset == .constant) {
        if (extra.constant) |k| {
            if (signature.base == .sr and signature.base.sr == .ip) {
                if (extra.insn_address) |address| {
                    const target = address + k;
                    if (k < 0) {
                        try writer.print(" - 0x{X} // 0x{X}", .{ -k, target });
                    } else {
                        try writer.print(" + 0x{X} // 0x{X}", .{ k, target });
                    }
                    return;
                }
            }

            if (k < 0) {
                try writer.print(" - 0x{X}", .{ -k });
                return;
            }
        }
    }

    if (signature.offset != .none) {
        try writer.writeAll(" + ");
        try print_parameter_kind(signature.offset, extra.offset_register_index, extra.constant, writer);
    }
}

pub fn print_parameter_kind(kind: Parameter.Kind, register_index: ?Register_Index, constant: ?i64, writer: anytype) !void {
    switch (kind) {
        .none => {},
        .arrow => try writer.writeAll("->"),
        .constant => if (constant) |k| {
            if (k < 0) {
                try writer.print("-0x{X}", .{ -k });
            } else {
                try writer.print("0x{X}", .{ k });
            }
        } else {
            try writer.writeAll("k");
        },
        .reg8, .reg16, .reg32 => |sign| {
            try writer.writeAll(switch (kind) {
                .reg8 => "b",
                .reg16 => "r",
                .reg32 => "x",
                else => unreachable,
            });
            if (register_index) |reg| {
                try writer.print("{}", .{ reg });
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

const Instruction_Encoding = isa.Instruction_Encoding;
const Instruction = isa.Instruction;
const Parameter = isa.Parameter;
const Mnemonic = isa.Mnemonic;
const Mnemonic_Suffix = isa.Mnemonic_Suffix;
const isa = @import("../isa.zig");
const Register_Index = hw.Register_Index;
const hw = @import("../hardware.zig");
const Signedness = std.builtin.Signedness;
const std = @import("std");
