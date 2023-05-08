const std = @import("std");
const isa = @import("isa_types");
const ie = @import("isa_encoding");
const lex = @import("lex.zig");
const Assembler = @import("Assembler.zig");
const SourceFile = @import("SourceFile.zig");
const Instruction = @import("Instruction.zig");
const Token = lex.Token;
const ExpressionType = ie.Parameter.ExpressionType;

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
    const s = file.slices();

    const mn: Instruction.MnemonicAndSuffix = switch (s.insn.items(.operation)[self.insn]) {
        .insn => |info| info,
        .bound_insn => |encoding| .{
            .mnemonic = encoding.mnemonic,
            .suffix = encoding.suffix,
        },
        else => unreachable,
    };

    const address = s.insn.items(.address)[self.insn];
    const params = s.insn.items(.params)[self.insn];
    const insn = assembler.buildInstruction(s, address, mn.mnemonic, mn.suffix, params, false).?;

    try writer.print("\n{s}: No encodings found matching instruction signature: ", .{ file.name });
    try insn.print(writer, address);
    try writer.writeByte('\n');

    if (assembler.edb.mnemonic_to_encoding.get(insn.mnemonic)) |encodings| {
        try writer.print("Possible encodings for {s} are:\n", .{ @tagName(insn.mnemonic) });
        for (encodings) |enc| {
            try enc.print(writer);
            try writer.writeByte('\n');
        }
    }

    const token_handle = file.instructions.items(.token)[self.insn];
    const token = file.tokens.get(token_handle);
    try token.printContext(file.source, writer, 160);
}
