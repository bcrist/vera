const std = @import("std");
const bits = @import("bits");
const misc = @import("misc");
const isa = @import("isa_types");
const Parameter = @import("Parameter.zig");
const InstructionEncoding = @import("InstructionEncoding.zig");
const ParameterEncoding = @import("ParameterEncoding.zig");
const Opcode = isa.Opcode;
const Mnemonic = isa.Mnemonic;
const MnemonicSuffix = isa.MnemonicSuffix;
const writeOpcode = Parameter.writeOpcode;
const readOpcode = Parameter.readOpcode;

const Instruction = @This();

mnemonic: Mnemonic,
suffix: MnemonicSuffix,
params: []const Parameter,

pub fn print(self: Instruction, writer: anytype, address: u32) !void {
    const mnemonic_str = @tagName(self.mnemonic);
    var len = mnemonic_str.len;
    try writer.writeAll(mnemonic_str);
    if (self.suffix != .none) {
        const suffix_str = @tagName(self.suffix);
        try writer.writeByte('.');
        for (suffix_str) |b| {
            try writer.writeByte(if (b == '_') '.' else b);
        }
        len += suffix_str.len + 1;
    }
    if (len < 5) {
        try writer.writeByteNTimes(' ', 5 - len);
    }
    var first = true;
    for (self.params) |param| {
        if (first) {
            first = false;
        } else if (!param.arrow) {
            try writer.writeByte(',');
        }
        try writer.writeByte(' ');
        try param.print(writer, address);
    }
}


pub fn matches(self: Instruction, encoding: InstructionEncoding) bool {
    if (self.mnemonic != encoding.mnemonic) return false;
    if (self.suffix != encoding.suffix) return false;
    if (self.params.len != encoding.params.len) return false;

    var encoded: [6]u8 = [_]u8{0} ** 6;
    var encoded_sources = std.EnumSet(ParameterEncoding.Source){};

    var i: usize = 0;
    while (i < encoding.params.len) : (i += 1) {
        const param = self.params[i];
        const param_encoding = encoding.params[i];
        if (!param.matches(param_encoding)) return false;

        // The rest of this makes sure that if the encoding has multiple parameters stored in the same
        // place, it only matches if the actual parameters are compatible with each other.
        // e.g. The instruction
        //          ADD X0, R4S -> X2
        //      should not match the encoding
        //          ADD Xa, RbS -> Xa
        //      because OA can't encode X0 and X2 at the same time.
        const raw_base = param.getRawBase(param_encoding);
        const raw_offset = param.getRawOffset(param_encoding);

        switch (param_encoding.base_src) {
            .implicit, .opcode => {},
            else => |src| {
                if (encoded_sources.contains(src)) {
                    var required = Parameter.readRaw(&encoded, src, encoding.opcode_base);
                    if (required != raw_base) {
                        return false;
                    }
                } else {
                    Parameter.writeRaw(&encoded, raw_base, src, encoding.opcode_base);
                    encoded_sources.insert(src);
                }
            },
        }

        switch (param_encoding.offset_src) {
            .implicit, .opcode => {},
            else => |src| {
                if (encoded_sources.contains(src)) {
                    var required = Parameter.readRaw(&encoded, src, encoding.opcode_base);
                    if (required != raw_offset) {
                        return false;
                    }
                } else {
                    Parameter.writeRaw(&encoded, raw_offset, src, encoding.opcode_base);
                    encoded_sources.insert(src);
                }
            },
        }
    }

    return true;
}

pub fn write(self: Instruction, encoding: InstructionEncoding, buf: []u8) []u8 {
    std.debug.assert(self.mnemonic == encoding.mnemonic);
    std.debug.assert(self.suffix == encoding.suffix);
    std.debug.assert(self.params.len == encoding.params.len);

    writeOpcode(buf, encoding.opcodes.min);

    if (self.mnemonic == .NOPE) {
        std.debug.assert(buf.len >= 3);
        buf[2] = 0;
    }

    for (self.params, encoding.params) |param, param_encoding| {
        param.write(param_encoding, encoding.opcode_base, buf);
    }

    return buf[0..encoding.getInstructionLength()];
}
