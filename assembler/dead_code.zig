const std = @import("std");
const lex = @import("lex.zig");
const Assembler = @import("Assembler.zig");
const SourceFile = @import("SourceFile.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const Constant = @import("Constant.zig");

const SectionBlock = SourceFile.SectionBlock;

pub fn markBlocksToKeep(a: *Assembler, file: *SourceFile) void {
    const s = file.slices();
    const operations = s.insn.items(.operation);
    for (s.block.items(.first_insn), s.block.items(.end_insn), s.block.items(.keep)) |begin, end, *keep| {
        if (keep.*) continue;

        var iter = Instruction.Iterator{
            .begin = begin,
            .end = end,
        };
        while (iter.next()) |insn_handle| {
            switch (operations[insn_handle]) {
                .keep, .entry, .kentry => {
                    keep.* = true;
                    traceReferencesInBlock(a, s, begin, end);
                    break;
                },
                else => {}
            }
        }
    }
}

fn traceReferencesInBlock(a: *Assembler, s: SourceFile.Slices, first_insn: Instruction.Handle, end_insn: Instruction.Handle) void {
    const insn_params = s.insn.items(.params);

    var iter = Instruction.Iterator{
        .begin = first_insn,
        .end = end_insn,
    };
    while (iter.next()) |insn_handle| {
        if (insn_params[insn_handle]) |expr_handle| {
            traceReferencesInExpr(a, s, expr_handle);
        }
    }
}

fn traceReferencesInExpr(a: *Assembler, s: SourceFile.Slices, expr_handle: Expression.Handle) void {
    switch (s.expr.items(.info)[expr_handle]) {
        .list, .arrow_list, .plus, .minus, .multiply, .shl, .shr,
        .concat, .concat_repeat, .bitwise_or, .bitwise_xor, .bitwise_and,
        .length_cast, .truncate, .sign_extend, .zero_extend => |binary| {
            traceReferencesInExpr(a, s, binary.left);
            traceReferencesInExpr(a, s, binary.right);
        },

        .negate, .complement, .signed_cast, .unsigned_cast, .remove_signedness_cast,
        .data_address_cast, .insn_address_cast, .stack_address_cast, .remove_address_cast,
        .index_to_reg8, .index_to_reg16, .index_to_reg32, .reg_to_index, .absolute_address_cast => |inner_expr| {
            traceReferencesInExpr(a, s, inner_expr);
        },

        .literal_symbol_ref => {
            const token_handle = s.expr.items(.token)[expr_handle];
            const raw_symbol = s.file.tokens.get(token_handle).location(s.file.source);
            const symbol_constant = Constant.initSymbolLiteral(a.gpa, &a.constant_temp, raw_symbol);
            traceSymbol(a, s, token_handle, symbol_constant.asString());
        },

        .directive_symbol_ref => |inner_expr| {
            const token_handle = s.expr.items(.token)[inner_expr];
            const symbol_constant = s.expr.items(.resolved_constant)[inner_expr].?;
            traceSymbol(a, s, token_handle, symbol_constant.asString());
        },

        .literal_int,
        .literal_str,
        .literal_reg,
        .literal_symbol_def,
        .directive_symbol_def,
        .literal_current_address,
        => {},
    }
}

fn traceSymbol(a: *Assembler, s: SourceFile.Slices, token_handle: lex.Token.Handle, symbol: []const u8) void {
    if (a.lookupSymbol(s, token_handle, symbol)) |target| switch (target) {
        .expression => |target_expr_handle| {
            const target_token_handle = s.expr.items(.token)[target_expr_handle];
            const block_handle = s.file.findBlockByToken(target_token_handle);
            var keep_flags = s.block.items(.keep);
            if (!keep_flags[block_handle]) {
                keep_flags[block_handle] = true;
                const begin = s.block.items(.first_insn)[block_handle];
                const end = s.block.items(.end_insn)[block_handle];
                traceReferencesInBlock(a, s, begin, end);
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
                traceReferencesInBlock(a, sym_file.slices(), begin, end);
            }
        },
        .not_found => {},
    };
}
