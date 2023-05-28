const std = @import("std");
const lex = @import("lex.zig");
const symbols = @import("symbols.zig");
const Assembler = @import("Assembler.zig");
const SourceFile = @import("SourceFile.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const Constant = @import("Constant.zig");

const Block = SourceFile.Block;

pub fn markBlocksToKeep(a: *Assembler, s: SourceFile.Slices) void {
    const operations = s.insn.items(.operation);
    for (s.block.items(.first_insn), s.block.items(.end_insn), s.block.items(.keep)) |begin, end, *keep| {
        if (keep.*) continue;

        const instructions = Instruction.Iterator{
            .begin = begin,
            .end = end,
        };

        var iter = instructions;
        while (iter.next()) |insn_handle| {
            switch (operations[insn_handle]) {
                .keep, .entry, .kentry, .boot => {
                    keep.* = true;
                    traceReferencesInBlock(a, s, instructions);
                    break;
                },
                else => {}
            }
        }
    }
}

fn traceReferencesInBlock(a: *Assembler, s: SourceFile.Slices, instructions: Instruction.Iterator) void {
    const insn_params = s.insn.items(.params);

    var iter = instructions;
    while (iter.next()) |insn_handle| {
        if (insn_params[insn_handle]) |expr_handle| {
            traceReferencesInExpr(a, s, insn_handle, expr_handle);
        }
    }
}

fn traceReferencesInExpr(a: *Assembler, s: SourceFile.Slices, insn_handle: Instruction.Handle, expr_handle: Expression.Handle) void {
    switch (s.expr.items(.info)[expr_handle]) {
        .list, .arrow_list, .plus, .minus, .multiply, .shl, .shr,
        .concat, .concat_repeat, .bitwise_or, .bitwise_xor, .bitwise_and,
        .length_cast, .truncate, .sign_extend, .zero_extend => |binary| {
            traceReferencesInExpr(a, s, insn_handle, binary.left);
            traceReferencesInExpr(a, s, insn_handle, binary.right);
        },

        .arrow_prefix, .negate, .complement, .signed_cast, .unsigned_cast, .remove_signedness_cast,
        .data_address_cast, .insn_address_cast, .stack_address_cast, .remove_address_cast, .local_label_def,
        .index_to_reg8, .index_to_reg16, .index_to_reg32, .reg_to_index, .absolute_address_cast => |inner_expr| {
            traceReferencesInExpr(a, s, insn_handle, inner_expr);
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

        .literal_symbol_def, .directive_symbol_def => if (s.insn.items(.operation)[insn_handle] == .push) {
            const constant = symbols.parseSymbol(a, s, expr_handle);
            if (s.file.stacks.get(constant.asString())) |stack_block_handle| {
                var keep_flags = s.block.items(.keep);
                if (!keep_flags[stack_block_handle]) {
                    keep_flags[stack_block_handle] = true;
                    traceReferencesInBlock(a, s, s.blockInstructions(stack_block_handle));
                }
            }
        },

        .literal_int,
        .literal_str,
        .literal_reg,
        .literal_current_address,
        => {},
    }
}

fn traceSymbol(a: *Assembler, s: SourceFile.Slices, token_handle: lex.Token.Handle, symbol: []const u8) void {
    switch (symbols.lookupSymbol(a, s, token_handle, symbol, false)) {
        .expression => |target_expr_handle| {
            const target_token_handle = s.expr.items(.token)[target_expr_handle];
            const block_handle = s.file.findBlockByToken(target_token_handle);
            var keep_flags = s.block.items(.keep);
            if (!keep_flags[block_handle]) {
                keep_flags[block_handle] = true;
                traceReferencesInBlock(a, s, s.blockInstructions(block_handle));
            }
        },
        .instruction => |insn_ref| {
            const sym_file = a.getSource(insn_ref.file);
            const sym_s = sym_file.slices();
            const block_handle = sym_file.findBlockByInstruction(insn_ref.instruction);
            var keep_flags = sym_s.block.items(.keep);
            if (!keep_flags[block_handle]) {
                keep_flags[block_handle] = true;
                traceReferencesInBlock(a, sym_s, sym_s.blockInstructions(block_handle));
            }
        },
        .stack => |stack_ref| {
            // stack blocks never get kept, but they may (rarely) refer to symbols in other blocks
            const block_handle = s.file.findBlockByInstruction(stack_ref.instruction);
            var keep_flags = s.block.items(.keep);
            if (!keep_flags[block_handle]) {
                keep_flags[block_handle] = true;
                traceReferencesInBlock(a, s, s.blockInstructions(block_handle));
            }
        },
        .not_found => {},
    }
}
