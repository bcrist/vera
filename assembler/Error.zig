const std = @import("std");
const lex = @import("lex.zig");
const Assembler = @import("Assembler.zig");
const SourceFile = @import("SourceFile.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const Token = lex.Token;

const Error = @This();

file: SourceFile.Handle,
context: union (enum) {
    token: lex.Token.Handle,
    instruction: Instruction.Handle,
    expression: Expression.Handle,
},
desc: []const u8,
flags: FlagSet,

pub const FlagSet = std.EnumSet(Flags);
pub const Flags = enum {
    remove_on_layout_reset,
    desc_is_allocated,
    is_instruction_encoding_error,
};

pub fn print(self: Error, a: *Assembler, writer: anytype) !void {
    const file = a.getSource(self.file);
    const s = file.slices();

    try writer.print("\n{s}: {s}", .{ file.name, self.desc });

    if (self.flags.contains(.is_instruction_encoding_error)) {
        const insn_handle = self.context.instruction;
        const mn: Instruction.MnemonicAndSuffix = switch (s.insn.items(.operation)[insn_handle]) {
            .insn => |info| info,
            .bound_insn => |encoding| .{
                .mnemonic = encoding.mnemonic,
                .suffix = encoding.suffix,
            },
            else => unreachable,
        };

        const address = s.insn.items(.address)[insn_handle];
        const params = s.insn.items(.params)[insn_handle];
        const insn = a.buildInstruction(s, address, mn.mnemonic, mn.suffix, params, false).?;
        try insn.print(writer, address);
        try writer.writeByte('\n');

        if (a.edb.mnemonic_to_encoding.get(insn.mnemonic)) |encodings| {
            try writer.print("Possible encodings for {s} are:\n", .{ @tagName(insn.mnemonic) });
            for (encodings) |enc| {
                try enc.print(writer);
                try writer.writeByte('\n');
            }
        }

    } else {
        try writer.writeByte('\n');
    }

    var context = switch (self.context) {
        .token => |handle| lex.Token.Range.expand(null, handle),
        .instruction => |handle| getInstructionContext(s, handle),
        .expression => |handle| getExpressionContext(handle, null, s.expr.items(.token), s.expr.items(.info)),
    };

    try lex.Token.printContextRange(file.tokens.get(context.first), file.tokens.get(context.last), file.source, writer, 160);
}

fn getInstructionContext(s: SourceFile.Slices, insn_handle: Instruction.Handle) lex.Token.Range {
    const token_kinds = s.file.tokens.items(.kind);
    var range = Instruction.getTokenRange(insn_handle, s.insn.items(.token), token_kinds);

    while (range.first < range.last and token_kinds[range.first] == .linespace) {
        range.first += 1;
    }

    while (range.first < range.last and switch (token_kinds[range.last]) { .linespace, .comment => true, else => false }) {
        range.last -= 1;
    }

    return range;
}

fn getExpressionContext(expr: Expression.Handle, range: ?lex.Token.Range, tokens: []const lex.Token.Handle, infos: []const Expression.Info) lex.Token.Range {
    var result = lex.Token.Range.expand(range, tokens[expr]);
    switch (infos[expr]) {
        .list, .arrow_list, .plus, .minus, .multiply, .shl, .shr,
        .concat, .concat_repeat, .bitwise_or, .bitwise_xor, .bitwise_and,
        .length_cast, .truncate, .sign_extend, .zero_extend,
        => |bin| {
            result = getExpressionContext(bin.left, result, tokens, infos);
            return getExpressionContext(bin.right, result, tokens, infos);
        },

        .arrow_prefix, .directive_symbol_def, .directive_symbol_ref,
        .local_label_def, .negate, .reg_to_index,
        .complement, .signed_cast, .unsigned_cast,
        .remove_signedness_cast, .absolute_address_cast,
        .data_address_cast, .insn_address_cast,
        .stack_address_cast, .remove_address_cast,
        .index_to_reg8, .index_to_reg16, .index_to_reg32,
        => |inner| return getExpressionContext(inner, result, tokens, infos),

        .literal_int, .literal_str, .literal_reg,
        .literal_current_address, .literal_symbol_def, .literal_symbol_ref,
        => return result,
    }
}
