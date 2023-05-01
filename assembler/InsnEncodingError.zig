const std = @import("std");
const ie = @import("instruction_encoding");
const lex = @import("lex.zig");
const Assembler = @import("Assembler.zig");
const SourceFile = @import("SourceFile.zig");
const Instruction = @import("Instruction.zig");
const Token = lex.Token;

const InsnEncodingError = @This();

file: SourceFile.Handle,
insn: Instruction.Handle,
flags: FlagSet,

pub const FlagSet = std.EnumSet(Flags);

pub const Flags = enum {
    remove_on_layout_reset,
};

pub fn print(self: InsnEncodingError, assembler: *Assembler, writer: anytype) !void {
    const file = &assembler.files.items[self.file];

    const address = file.instructions.items(.address)[self.insn];
    const op = file.instructions.items(.operation)[self.insn];
    const params = file.instructions.items(.params)[self.insn];

    const mn: Instruction.MnemonicAndSuffix = switch (op) {
        .insn => |info| info,
        .bound_insn => |encoding| .{
            .mnemonic = encoding.mnemonic,
            .suffix = encoding.suffix,
        },
        else => unreachable,
    };

    const insn = assembler.buildInstruction(file, self.file, address, mn.mnemonic, mn.suffix, params, false).?;

    try writer.print("\n{s}: No encodings found matching instruction signature: ", .{ file.name });
    try printInsnSignature(writer, insn);

    if (assembler.edb.mnemonic_to_encoding.get(insn.mnemonic)) |encodings| {
        try writer.print("Possible encodings for {s} are:\n", .{ @tagName(insn.mnemonic) });
        for (encodings) |enc| {
            try printInsnEncoding(writer, enc);
        }
    }

    const token_handle = file.instructions.items(.token)[self.insn];
    const token = file.tokens.get(token_handle);
    try token.printContext(file.source, writer, 160);
}

fn printInsnSignature(writer: anytype, insn: ie.Instruction) !void {
    try writer.print("{s}", .{ @tagName(insn.mnemonic) });
    if (insn.suffix != .none) try writer.print(".{s}", .{ @tagName(insn.suffix) });

    var first = true;
    for (insn.params) |param| {
        if (param.arrow) {
            try writer.writeAll(" -> ");
            first = false;
        } else if (first) {
            try writer.writeAll(" ");
            first = false;
        } else {
            try writer.writeAll(", ");
        }
        switch (param.expr_type) {
            .unknown => try writer.print(".unknown({})", .{ param.constant }),
            .poison => try writer.print(".poison({})", .{ param.constant }),
            .symbol_def => try writer.print(".symbol_def({})", .{ param.constant }),
            .constant => try writer.print("{}", .{ param.constant }),
            .reg8 => |reg| try printReg(writer, "B", reg),
            .reg16 => |reg| try printReg(writer, "R", reg),
            .reg32 => |reg| try printReg(writer, "X", reg),
            .sr => |reg| try printSR(writer, reg),
            .raw_base_offset => |bo| try printBO(writer, "", bo, param.constant),
            .data_address => |bo| try printBO(writer, ".d ", bo, param.constant),
            .insn_address => |bo| try printBO(writer, ".i ", bo, param.constant),
            .stack_address => |bo| try printBO(writer, ".s ", bo, param.constant),
        }
    }
    try writer.writeAll("\n");
}

fn printInsnEncoding(writer: anytype, encoding: ie.InstructionEncoding) !void {
     try writer.print("{s}", .{ @tagName(encoding.mnemonic) });
    if (encoding.suffix != .none) try writer.print(".{s}", .{ @tagName(encoding.suffix) });

    var first = true;
    for (encoding.params) |param| {
        if (param.arrow) {
            try writer.writeAll(" -> ");
            first = false;
        } else if (first) {
            try writer.writeAll(" ");
            first = false;
        } else {
            try writer.writeAll(", ");
        }
        if (param.address_space) |as| {
            try writer.writeAll(switch (as) {
                .data => ".d ",
                .insn => ".i ",
                .stack => ".s ",
            });
        }

        try printParamEncodingBaseType(writer, param.base, param);
        if (param.offset != .none) {
            try writer.writeAll(" + ");
            try printParamEncodingBaseType(writer, param.offset, param);
        }
    }
    try writer.writeAll("\n");
}

fn printParamEncodingBaseType(writer: anytype, t: ie.ParameterEncodingBaseType, param: ie.ParameterEncoding) !void {
    switch (t) {
        .none => try writer.writeAll(".none"),
        .constant => try printConstantRanges(writer, param),
        .reg8 => |reg| try printRegRange(writer, "B", reg),
        .reg16 => |reg| try printRegRange(writer, "R", reg),
        .reg32 => |reg| try printRegRange(writer, "X", reg),
        .sr => |reg| try printSR(writer, reg),
    }
}

fn printConstantRanges(writer: anytype, param: ie.ParameterEncoding) !void {
    if (param.constant_ranges.len == 0) {
        try writer.writeAll("0");
        return;
    }
    try writer.writeAll("(");
    var first = true;
    for (param.constant_ranges) |range| {
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
    if (param.alt_constant_ranges.len > 0) {
        try writer.writeAll(" | ");
        first = true;
        for (param.alt_constant_ranges) |range| {
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
    }
    if (param.constant_align != 1) {
        try writer.print(" .align {}", .{ param.constant_align });
    }
    try writer.writeAll(")");
}

fn printReg(writer: anytype, prefix: []const u8, reg: ie.IndexedRegister) !void {
    try writer.print("{s}{}", .{ prefix, reg.index });
    if (reg.signedness) |s| {
        try writer.print(" .{s}", .{ @tagName(s) });
    }
}
fn printRegRange(writer: anytype, prefix: []const u8, reg: ie.IndexedRegisterRange) !void {
    try writer.print("{s}{}", .{ prefix, reg.min });
    if (reg.max != reg.min) {
        try writer.print("-{s}{}", .{ prefix, reg.max });
    }
    if (reg.signedness) |s| {
        try writer.print(" .{s}", .{ @tagName(s) });
    }
}

fn printSR(writer: anytype, reg: ie.SpecialRegister) !void {
    try writer.writeAll(switch (reg) {
        .ip => "IP",
        .sp => "SP",
        .rp => "RP",
        .bp => "BP",
        .uxp => "UXP",
        .kxp => "KXP",
        .asn => "ASN",
        .stat => "STAT",
    });
}

fn printBO(writer: anytype, prefix: []const u8, bo: ie.BaseOffsetType, constant: i64) !void {
    try writer.writeAll(prefix);
    try printInnerType(writer, bo.base, constant);
    if (bo.offset != .none) {
        try writer.writeAll(" + ");
        try printInnerType(writer, bo.offset, constant);
    }
}

fn printInnerType(writer: anytype, t: ie.BaseOffsetType.InnerType, constant: i64) !void {
    switch (t) {
        .none => try writer.print(".none({})", .{ constant }),
        .constant => try writer.print("{}", .{ constant }),
        .reg8 => |reg| try printReg(writer, "B", reg),
        .reg16 => |reg| try printReg(writer, "R", reg),
        .reg32 => |reg| try printReg(writer, "X", reg),
        .sr => |reg| try printSR(writer, reg),
    }
}
