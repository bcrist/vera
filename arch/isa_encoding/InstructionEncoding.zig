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

pub fn print(self: InstructionEncoding, writer: anytype, indent: []const u8) !void {
    if (self.opcodes.min == self.opcodes.max) {
        try writer.print("{s}{X:0>4} ", .{ indent, self.opcodes.min });
    } else {
        try writer.print("{s}{X:0>4}-{X:0>4} ", .{ indent, self.opcodes.min, self.opcodes.max });
    }

    if (self.opcode_base != self.opcodes.min) {
        try writer.print("base {X:0>4} ", .{ self.opcode_base });
    }

    try writer.writeAll(@tagName(self.mnemonic));
    if (self.suffix != .none) {
        try writer.print(".{s}", .{ @tagName(self.suffix) });
    }
    try writer.writeAll("\n");

    var sources_used = std.EnumSet(ParameterEncoding.Source).initEmpty();
    var sources_double_used = std.EnumMap(ParameterEncoding.Source, u8);

    for (self.params) |param| {
        if (sources_used.contains(param.base_src)) {
            sources_double_used.put(param.base_src, "abcdefghijklmnopqrstuvwxyz"[sources_double_used.count()]);
        } else {
            sources_used.insert(param.base_src);
        }
        if (sources_used.contains(param.offset_src)) {
            sources_double_used.put(param.offset_src, "abcdefghijklmnopqrstuvwxyz"[sources_double_used.count()]);
        } else {
            sources_used.insert(param.offset_src);
        }
    }

    for (self.params) |param| {
        try writer.print("{s}   ", .{ indent });
        try param.print(writer, sources_double_used);
        try writer.writeAll("\n");
    }

    //var iter = sources_double_used
}