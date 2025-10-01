file: Source_File.Handle,
context: Context,
desc: []const u8,
flags: Flag_Set,

pub const Context = union (enum) {
    token: lex.Token.Handle,
    instruction: Instruction.Handle,
    expression: Expression.Handle,
};

pub const Flag_Set = std.EnumSet(Flags);
pub const Flags = enum {
    remove_on_layout_reset,
    desc_is_allocated,
    is_instruction_encoding_error,
};

pub fn print(self: Error, a: *Assembler, writer: *std.io.Writer) !void {
    const file = a.get_source(self.file);
    const s = file.slices();

    const context = switch (self.context) {
        .token => |handle| lex.Token.Range.expand(null, handle),
        .instruction => |handle| get_instruction_context(s, handle),
        .expression => |handle| get_expression_context(handle, null, s.expr.items(.token), s.expr.items(.info)),
    };

    const first = file.tokens.get(context.first);
    const last = file.tokens.get(context.last);

    const highlight_start = first.offset;
    const highlight_end = last.offset + last.span(file.source).len;

    try writer.writeByte('\n');
    try console.print_context(file.source, &.{
        .{
            .offset = highlight_start,
            .len = highlight_end - highlight_start,
            .note = self.desc,
        },
    }, writer, 160, .{
        .filename = file.name,
    });

    if (self.flags.contains(.is_instruction_encoding_error)) {
        const insn_handle = self.context.instruction;
        const mnemonic: isa.Mnemonic = switch (s.insn.items(.operation)[insn_handle]) {
            .insn => |mnemonic| mnemonic,
            .bound_insn => |id| iedb.get_mnemonic(id),
            else => unreachable,
        };

        const address = s.insn.items(.address)[insn_handle];
        const params = s.insn.items(.params)[insn_handle];
        const insn = a.build_instruction(s, address, mnemonic, params, false).?;
        try isa.fmt.print_instruction(insn, address, writer);
        try writer.writeByte('\n');

        var similar_buf: [10]isa.Instruction.Form = undefined;
        const similar_forms = a.edb.find_similar(&similar_buf, insn);
        for (0.., similar_forms) |i, form| {
            if (i == 0) {
                try writer.writeAll("Encodings with the same signature:\n");
            }
            try isa.fmt.print_form(form, writer);
            try writer.writeByte('\n');
        }
    }
}

fn get_instruction_context(s: Source_File.Slices, insn_handle: Instruction.Handle) lex.Token.Range {
    const token_kinds = s.file.tokens.items(.kind);
    var range = Instruction.token_range(insn_handle, s.insn.items(.token), token_kinds);

    while (range.first < range.last and token_kinds[range.first] == .linespace) {
        range.first += 1;
    }

    while (range.first < range.last and switch (token_kinds[range.last]) { .linespace, .comment => true, else => false }) {
        range.last -= 1;
    }

    return range;
}

fn get_expression_context(expr: Expression.Handle, range: ?lex.Token.Range, tokens: []const lex.Token.Handle, infos: []const Expression.Info) lex.Token.Range {
    var result = lex.Token.Range.expand(range, tokens[expr]);
    switch (infos[expr]) {
        .list, .plus, .minus, .multiply, .shl, .shr,
        .concat, .concat_repeat, .bitwise_or, .bitwise_xor, .bitwise_and,
        .length_cast, .truncate, .sign_extend, .zero_extend,
        => |bin| {
            result = get_expression_context(bin.left, result, tokens, infos);
            return get_expression_context(bin.right, result, tokens, infos);
        },

        .directive_symbol_def, .directive_symbol_ref,
        .local_label_def, .negate, .reg_to_index,
        .complement, .signed_cast, .unsigned_cast,
        .remove_signedness_cast, .absolute_address_cast,
        .data_address_cast, .insn_address_cast,
        .stack_address_cast, .remove_address_cast,
        .index_to_reg,
        .crlf_cast, .lf_cast,
        => |inner| return get_expression_context(inner, result, tokens, infos),

        .literal_int, .literal_str, .literal_reg,
        .literal_current_address, .literal_symbol_def, .literal_symbol_ref,
        => return result,
    }
}

const Error = @This();
const Token = lex.Token;
const lex = isa.lex;
const Assembler = @import("Assembler.zig");
const Source_File = @import("Source_File.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const iedb = @import("iedb");
const isa = @import("isa");
const arch = @import("arch");
const console = @import("console");
const std = @import("std");
