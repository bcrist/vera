const std = @import("std");
const lex = @import("lex.zig");
const Assembler = @import("Assembler.zig");
const SourceFile = @import("SourceFile.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const Constant = @import("Constant.zig");

const SectionBlock = SourceFile.SectionBlock;

pub fn markBlocksToKeep(a: *Assembler, file: *SourceFile, file_handle: SourceFile.Handle) void {
    const operations = file.instructions.items(.operation);
    for (file.blocks.items(.first_insn), file.blocks.items(.end_insn), file.blocks.items(.keep)) |begin, end, *keep| {
        if (keep.*) continue;

        var iter = Instruction.Iterator{
            .begin = begin,
            .end = end,
        };
        while (iter.next()) |insn_handle| {
            switch (operations[insn_handle]) {
                .keep, .entry, .kentry => {
                    keep.* = true;
                    traceReferencesInBlock(a, file, file_handle, begin, end);
                    break;
                },
                else => {}
            }
        }
    }
}

fn traceReferencesInBlock(a: *Assembler, file: *SourceFile, file_handle: SourceFile.Handle, first_insn: Instruction.Handle, end_insn: Instruction.Handle) void {
    const insn_params = file.instructions.items(.params);
    const expr_infos = file.expressions.items(.info);

    var iter = Instruction.Iterator{
        .begin = first_insn,
        .end = end_insn,
    };
    while (iter.next()) |insn_handle| {
        if (insn_params[insn_handle]) |expr_handle| {
            traceReferencesInExpr(a, file_handle, expr_infos, expr_handle);
        }
    }
}

fn traceReferencesInExpr(a: *Assembler, file_handle: SourceFile.Handle, expr_infos: []const Expression.Info, expr_handle: Expression.Handle) void {
    switch (expr_infos[expr_handle]) {
        .list, .arrow_list, .plus, .minus, .multiply, .shl, .shr,
        .concat, .concat_repeat, .bitwise_or, .bitwise_xor, .bitwise_and,
        .length_cast, .truncate, .sign_extend, .zero_extend => |binary| {
            traceReferencesInExpr(a, file_handle, expr_infos, binary.left);
            traceReferencesInExpr(a, file_handle, expr_infos, binary.right);
        },

        .negate, .complement, .signed_cast, .unsigned_cast, .maybe_signed_cast => |inner_expr| {
            traceReferencesInExpr(a, file_handle, expr_infos, inner_expr);
        },

        .literal_symbol_ref => {
            const file = a.getSource(file_handle);
            const token_handle = file.expressions.items(.token)[expr_handle];
            const raw_symbol = file.tokens.get(token_handle).location(file.source);
            const symbol_constant = Constant.initSymbolLiteral(a.gpa, &a.constant_temp, raw_symbol);
            traceSymbol(a, file, file_handle, token_handle, symbol_constant.asString());
        },

        .directive_symbol_ref => |inner_expr| {
            const file = a.getSource(file_handle);
            const token_handle = file.expressions.items(.token)[inner_expr];
            const symbol_constant = file.expressions.items(.resolved_constant)[inner_expr].?;
            traceSymbol(a, file, file_handle, token_handle, symbol_constant.asString());
        },

        .literal_int,
        .literal_str,
        .literal_reg,
        .literal_symbol_def,
        .directive_symbol_def,
        => {},
    }
}

fn traceSymbol(a: *Assembler, file: *SourceFile, file_handle: SourceFile.Handle, token_handle: lex.Token.Handle, symbol: []const u8) void {
    if (a.lookupSymbol(file, file_handle, token_handle, symbol)) |target| switch (target) {
        .expression => |target_expr_handle| {
            const target_token_handle = file.expressions.items(.token)[target_expr_handle];
            const block_handle = file.findBlockByToken(target_token_handle);
            var keep_flags = file.blocks.items(.keep);
            if (!keep_flags[block_handle]) {
                keep_flags[block_handle] = true;
                const begin = file.blocks.items(.first_insn)[block_handle];
                const end = file.blocks.items(.end_insn)[block_handle];
                traceReferencesInBlock(a, file, file_handle, begin, end);
            }
        },
        .instruction => |insn_ref| {
            const sym_file = a.getSource(insn_ref.file);
            const block_handle = sym_file.findBlockByInstruction(insn_ref.instruction);
            var keep_flags = sym_file.blocks.items(.keep);
            if (!keep_flags[block_handle]) {
                keep_flags[block_handle] = true;
                const begin = sym_file.blocks.items(.first_insn)[block_handle];
                const end = sym_file.blocks.items(.end_insn)[block_handle];
                traceReferencesInBlock(a, sym_file, insn_ref.file, begin, end);
            }
        },
        .not_found => {},
    };
}
