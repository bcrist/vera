const std = @import("std");
const isa = @import("isa_types");
const AddressSpace = isa.AddressSpace;
const SpecialRegister = isa.SpecialRegister;
const Opcode = isa.Opcode;
const ConstantRange = isa.ConstantRange;
const IndexedRegisterRange = isa.IndexedRegisterRange;

const ParameterEncoding = @This();

arrow: bool = false,
address_space: ?AddressSpace = null,
base: BaseOffsetEncoding = .{ .none = {} },
offset: BaseOffsetEncoding = .{ .none = {} },
base_src: Source = .implicit,
offset_src: Source = .implicit,

pub const BaseOffsetEncoding = union(enum) {
    none,
    constant: ConstantEncoding,
    reg8: IndexedRegisterRange,
    reg16: IndexedRegisterRange,
    reg32: IndexedRegisterRange,
    sr: SpecialRegister,

    pub fn print(self: BaseOffsetEncoding, writer: anytype, maybe_placeholder: ?u8) !void {
        switch (self) {
            .none => {
                try writer.writeAll(".none");
            },
            .constant => {
                if (maybe_placeholder) |placeholder| {
                    try writer.writeByte(placeholder);
                } else try self.printRange(writer);
            },
            .reg8, .reg16, .reg32 => |irr| {
                const prefix = switch (self) {
                    .reg8 => "B",
                    .reg16 => "R",
                    .reg32 => "X",
                    else => unreachable,
                };

                if (irr.min == irr.max) {
                    try writer.print("{s}{}", .{ prefix, irr.min });
                } else if (maybe_placeholder) |placeholder| {
                    try writer.writeAll(prefix);
                    try writer.writeByte(placeholder);
                } else {
                    try writer.print("{s}{}-{s}{}", .{ prefix, irr.min, prefix, irr.max });
                }
                if (irr.signedness) |s| {
                    try writer.writeAll(switch (s) {
                        .unsigned => ".UNSIGNED",
                        .signed => ".SIGNED",
                    });
                }
            },
            .sr => |sr| {
                try writer.writeAll(@tagName(sr));
            },
        }
    }

    pub fn printRange(self: BaseOffsetEncoding, writer: anytype) !void {
        switch (self) {
            .none, .sr => try writer.writeAll("(0)"),
            .constant => |ce| {
                if (ce.ranges.len == 0) {
                    try writer.writeAll("(0)");
                } else {
                    if (ce.ranges.len > 1) {
                        try writer.writeAll("(");
                    }
                    var first = true;
                    for (ce.ranges) |range| {
                        if (first) {
                            first = false;
                        } else {
                            try writer.writeAll(", ");
                        }
                        if (range.min == range.max) {
                            try writer.print("{}", .{ range.min });
                        } else {
                            try writer.print("[{},{}]", .{ range.min, range.max });
                        }
                    }
                    if (ce.ranges.len > 1) {
                        try writer.writeAll(")");
                    }
                }
            },
            .reg8, .reg16, .reg32 => |irr| {
                if (irr.min == irr.max) {
                    try writer.print("{}", .{ irr.min });
                } else {
                    try writer.print("[{},{}]", .{ irr.min, irr.max });
                }
            },
        }
    }
};

pub const ConstantEncoding = struct {
    reverse: bool = false, // store constant as `N - value` instead of `value`
    granularity: u3 = 1,
    ranges: []const ConstantRange = &.{},

    // TODO allow restricting encoding to matching a subset of the full ConstantRanges that can be encoded.
    // that way e.g. in addition to C .imm16u -> RaU and C .imm16s -> RaS, we can also have C .imm15u -> Ra.
    // So the .signed/.unsigned suffix can be omitted for numbers that are the same regardless of
    // signed/unsigned interpretation

    // Transforms a constant value to get the raw number that's embedded in the instruction bytes
    // to represent it (but the exact location depends on base_src/offset_src)
    pub fn encodeConstant(self: ConstantEncoding, constant: i64) i64 {
        if (self.ranges.len == 0 and constant == 0) {
            return 0;
        }

        var total_values: i64 = 0;
        var raw: i64 = 0;
        var i: usize = 0;
        while (i < self.ranges.len) : (i += 1) {
            const range = self.ranges[i];
            if (constant >= range.min and constant <= range.max) {
                raw = total_values + @divTrunc((constant - range.min), self.granularity);
            }
            total_values += @divTrunc((range.max - range.min + self.granularity), self.granularity);
        }

        if (self.reverse) {
            raw = total_values - raw - 1;
        }

        return raw;
    }

    // The inverse of encodeConstant
    pub fn decodeConstant(self: ConstantEncoding, raw_value: i64) ?i64 {
        var aligned = raw_value * self.granularity;

        var total_values: i64 = 0;
        for (self.ranges) |range| {
            total_values += range.max - range.min + 1;
        }

        if (self.reverse) {
            aligned = total_values - aligned - 1;
        }

        for (self.ranges) |range| {
            const values = range.max - range.min + 1;
            if (aligned < values) {
                return range.min + aligned;
            }
            aligned -= values;
        }

        if (self.ranges.len == 0 and aligned == 0) {
            return 0;
        }

        return null;
    }
};

pub const Source = enum {
    implicit, // Parameter is not directly represented in the instruction
    opcode, // Like implicit, but used when there are multiple opcodes in the encoding, and the parameter value is linearly related to the opcode
    OA, // The initial operand A
    OB, // The initial operand B
    OB_OA, // The combination of initial operands A and B (OB is MSB, OA is LSB)
    IP_plus_2_OA, // operand A, after it has been loaded from IP+2
    IP_plus_2_OB, // operand B, after it has been loaded from IP+2
    IP_plus_2_8, // the byte at IP+2
    IP_plus_2_16, // the word at IP+2
    IP_plus_2_32, // the 2 words at IP+2 and IP+4
    IP_plus_4_16, // the word at IP+4

    pub fn getMinInstructionLength(self: Source) u3 {
        return switch (self) {
            .implicit, .OA, .OB, .OB_OA, .opcode => 2,
            .IP_plus_2_OA, .IP_plus_2_OB, .IP_plus_2_8 => 3,
            .IP_plus_2_16 => 4,
            .IP_plus_2_32, .IP_plus_4_16 => 6,
        };
    }
};

pub fn print(self: ParameterEncoding, writer: anytype, placeholders: std.EnumMap(Source, u8)) !void {
    if (self.arrow) {
        try writer.writeAll("-> ");
    }

    if (self.address_space) |as| {
        try writer.writeAll(as.getDirectiveName());
        try writer.writeByte(' ');
    }

    try self.base.print(writer, placeholders.get(self.base_src));

    if (self.offset != .none or self.offset_src != .implicit) {
        try writer.writeAll(" + ");
        try self.offset.print(writer, placeholders.get(self.offset_src));
    }
}
