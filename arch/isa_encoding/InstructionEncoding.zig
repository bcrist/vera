const std = @import("std");
const isa = @import("isa_types");
const ParameterEncoding = @import("ParameterEncoding.zig");
const Mnemonic = isa.Mnemonic;
const MnemonicSuffix = isa.MnemonicSuffix;
const Opcode = isa.Opcode;
const OpcodeRange = isa.OpcodeRange;

const InstructionEncoding = @This();

mnemonic: Mnemonic,
suffix: MnemonicSuffix,
params: []const ParameterEncoding,
opcode_base: Opcode,
opcodes: OpcodeRange,

pub const WithDescription = struct {
    encoding: InstructionEncoding,
    desc: []const u8,   
};

pub fn eqlIncludingOpcodes(a: InstructionEncoding, b: InstructionEncoding) bool {
    return a.eqlExceptOpcodes(b)
        and a.opcodes.min == b.opcodes.min
        and a.opcodes.max == b.opcodes.max
        ;
}

pub fn eqlExceptOpcodes(a: InstructionEncoding, b: InstructionEncoding) bool {
    // Note intentionally not checking opcodes.min or opcodes.max
    return a.mnemonic == b.mnemonic
        and a.suffix == b.suffix
        and a.opcode_base == b.opcode_base
        and std.meta.eql(a.params, b.params)
        ;
}

pub fn getInstructionLength(self: InstructionEncoding) u3 {
    var len: u3 = 2;

    if (self.mnemonic == .NOPE) {
        // special case; NOPE is really a branch, that skips a byte,
        // but from the programmer's perspective it's an instruction with length 3.
        return 3;
    }

    for (self.params) |param| {
        const len_for_param = std.math.max(
            param.base_src.getMinInstructionLength(),
            param.offset_src.getMinInstructionLength()
        );

        if (len_for_param > len) {
            len = len_for_param;
        }
    }

    return len;
}

// Note this doesn't print the entire state of an InstructionEncoding object, only the parts
// that make up the "signature", i.e. what affects Instruction.matches().
// For debugging consider using isa_encoding.data.writeInstructionEncoding().
pub fn print(self: InstructionEncoding, writer: anytype) !void {
    var len = try isa.printMnemonicAndSuffix(writer, self.mnemonic, self.suffix);
    if (len < 5) {
        try writer.writeByteNTimes(' ', 5 - len);
    }

    const placeholders = "abcdefghijklmnopqrstuvwxyz";
    var placeholder_map = std.EnumMap(ParameterEncoding.Source, u8) {};

    var num_placeholders: usize = 0;
    for (self.params) |param| {
        if (param.base_src != .implicit and !placeholder_map.contains(param.base_src) and num_placeholders < placeholders.len) {
            placeholder_map.put(param.base_src, placeholders[num_placeholders]);
            num_placeholders += 1;
        }

        if (param.offset_src != .implicit and !placeholder_map.contains(param.offset_src) and num_placeholders < placeholders.len) {
            placeholder_map.put(param.offset_src, placeholders[num_placeholders]);
            num_placeholders += 1;
        }
    }

    var first = true;
    for (self.params) |param| {
        if (first) {
            first = false;
        } else if (!param.arrow) {
            try writer.writeByte(',');
        }
        try writer.writeByte(' ');
        try param.print(writer, placeholder_map);
    }

    if (num_placeholders == 0) return;

    try writer.writeAll(" \t\t;");

    first = true;
    for (0..num_placeholders) |i| {
        const placeholder = placeholders[i];
        var iter = placeholder_map.iterator();
        const source = while (iter.next()) |entry| {
            if (entry.value.* == placeholder) {
                break entry.key;
            }
        } else unreachable;

        if (first) {
            first = false;
        } else {
            try writer.writeByte(',');
        }
        try writer.writeByte(' ');
        try writer.writeByte(placeholder);
        try writer.writeAll(" in ");

        for (self.params) |param| {
            if (param.base_src == source) {
                try param.base.printRange(writer);
                break;
            }
            if (param.offset_src == source) {
                try param.offset.printRange(writer);
                break;
            }
        } else try writer.writeAll("?");
    }
}
