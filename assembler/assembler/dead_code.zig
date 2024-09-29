pub fn mark_blocks_to_keep(a: *Assembler, s: Source_File.Slices) void {
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
                    trace_references_in_block(a, s, instructions);
                    break;
                },
                else => {}
            }
        }
    }
}

fn trace_references_in_block(a: *Assembler, s: Source_File.Slices, instructions: Instruction.Iterator) void {
    const insn_params = s.insn.items(.params);

    var iter = instructions;
    while (iter.next()) |insn_handle| {
        if (insn_params[insn_handle]) |expr_handle| {
            trace_references_in_expr(a, s, insn_handle, expr_handle);
        }
    }
}

fn trace_references_in_expr(a: *Assembler, s: Source_File.Slices, insn_handle: Instruction.Handle, expr_handle: Expression.Handle) void {
    switch (s.expr.items(.info)[expr_handle]) {
        .list, .plus, .minus, .multiply, .shl, .shr,
        .concat, .concat_repeat, .bitwise_or, .bitwise_xor, .bitwise_and,
        .length_cast, .truncate, .sign_extend, .zero_extend => |binary| {
            trace_references_in_expr(a, s, insn_handle, binary.left);
            trace_references_in_expr(a, s, insn_handle, binary.right);
        },

        .negate, .complement, .signed_cast, .unsigned_cast, .remove_signedness_cast,
        .data_address_cast, .insn_address_cast, .stack_address_cast, .remove_address_cast, .local_label_def,
        .index_to_reg, .reg_to_index, .absolute_address_cast, .crlf_cast, .lf_cast => |inner_expr| {
            trace_references_in_expr(a, s, insn_handle, inner_expr);
        },

        .literal_symbol_ref => {
            const token_handle = s.expr.items(.token)[expr_handle];
            const raw_symbol = s.file.tokens.get(token_handle).location(s.file.source);
            const symbol_constant = Constant.init_symbol_literal(a.gpa, &a.constant_temp, raw_symbol);
            trace_symbol(a, s, token_handle, symbol_constant.as_string());
        },

        .directive_symbol_ref => |inner_expr| {
            const token_handle = s.expr.items(.token)[inner_expr];
            const symbol_constant = s.expr.items(.resolved_constant)[inner_expr].?;
            trace_symbol(a, s, token_handle, symbol_constant.as_string());
        },

        .literal_symbol_def, .directive_symbol_def => if (s.insn.items(.operation)[insn_handle] == .push) {
            const constant = symbols.parse_symbol(a, s, expr_handle);
            if (s.file.stacks.get(constant.as_string())) |stack_block_handle| {
                var keep_flags = s.block.items(.keep);
                if (!keep_flags[stack_block_handle]) {
                    keep_flags[stack_block_handle] = true;
                    trace_references_in_block(a, s, s.block_instructions(stack_block_handle));
                }
            }
        },

        .literal_int,
        .literal_str,
        .literal_reg,
        .literal_current_address,
        .arrow,
        => {},
    }
}

fn trace_symbol(a: *Assembler, s: Source_File.Slices, token_handle: lex.Token.Handle, symbol: []const u8) void {
    switch (symbols.lookup_symbol(a, s, token_handle, symbol, false)) {
        .expression => |target_expr_handle| {
            const target_token_handle = s.expr.items(.token)[target_expr_handle];
            const block_handle = s.file.find_block_by_token(target_token_handle);
            var keep_flags = s.block.items(.keep);
            if (!keep_flags[block_handle]) {
                keep_flags[block_handle] = true;
                trace_references_in_block(a, s, s.block_instructions(block_handle));
            }
        },
        .instruction => |insn_ref| {
            const sym_file = a.get_source(insn_ref.file);
            const sym_s = sym_file.slices();
            const block_handle = sym_file.find_block_by_instruction(insn_ref.instruction);
            var keep_flags = sym_s.block.items(.keep);
            if (!keep_flags[block_handle]) {
                keep_flags[block_handle] = true;
                trace_references_in_block(a, sym_s, sym_s.block_instructions(block_handle));
            }
        },
        .stack => |stack_ref| {
            // stack blocks never get kept, but they may (rarely) refer to symbols in other blocks
            const block_handle = s.file.find_block_by_instruction(stack_ref.instruction);
            var keep_flags = s.block.items(.keep);
            if (!keep_flags[block_handle]) {
                keep_flags[block_handle] = true;
                trace_references_in_block(a, s, s.block_instructions(block_handle));
            }
        },
        .not_found => {},
    }
}

const Block = Source_File.Block;
const symbols = @import("symbols.zig");
const Assembler = @import("Assembler.zig");
const Source_File = @import("Source_File.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const Constant = @import("Constant.zig");
const lex = isa.lex;
const isa = @import("isa");
const std = @import("std");
