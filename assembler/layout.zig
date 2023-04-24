const std = @import("std");
const ie = @import("instruction_encoding");
const types = @import("types.zig");
const lex = @import("lex.zig");
const Assembler = @import("Assembler.zig");
const SourceFile = @import("SourceFile.zig");
const Section = @import("Section.zig");
const Constant = @import("Constant.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");

pub fn doFixedOrgLayout(a: *Assembler, chunks: std.ArrayListUnmanaged(SourceFile.Chunk)) bool {
    var layout_changed = false;
    for (chunks.items) |chunk| {
        var file = a.getSource(chunk.file);
        const operations = file.instructions.items(.operation);
        const params = file.instructions.items(.params);
        const insn_addresses = file.instructions.items(.address);

        // Find the .org directive (there will be exactly one, at or near the start)
        // Resolve it to find the initial address for the chunk:
        var initial_address: u32 = undefined;
        var iter = chunk.instructions;
        while (iter.next()) |insn_handle| {
            if (operations[insn_handle] == .org) {
                initial_address = resolveOrgAddress(a, file, chunk.file, params[insn_handle]);
                break;
            }
        }

        var address = initial_address;
        iter = chunk.instructions;
        while (iter.next()) |insn_handle| {
            const old_insn_address = insn_addresses[insn_handle];
            var new_insn_address = address;
            switch (operations[insn_handle]) {
                .none, .org, .keep, .def, .undef, .section, .code, .kcode, .entry, .kentry, .data, .kdata, .@"const", .kconst, .stack => {
                    // TODO need to look forwards to see if an .align, etc. is coming up that would require changing address now
                },
                .push => {
                    // TODO synthesize push instruction
                },
                .pop => {
                    // TODO synthesize pop instruction
                },
                .insn => {
                    address += resolveInstructionEncoding(a, &operations[insn_handle], params[insn_handle]);
                },
                .bound_insn => |encoding| {
                    address += ie.getInstructionLength(encoding.*);
                },
                .@"align" => {
                    // TODO handle alignment
                    
                },
                .db => {

                },
                .dw => {
                    // TODO handle alignment
                },
                .dd => {
                    // TODO handle alignment
                },
            }

            if (new_insn_address != old_insn_address) {
                insn_addresses[insn_handle] = new_insn_address;
                layout_changed = true;
            }
        }

        // TODO extract page number from address, allocate page to section and mark usage in page
    }
    return layout_changed;
}

pub fn doAutoOrgLayout(a: *Assembler, chunks: std.ArrayListUnmanaged(SourceFile.Chunk)) bool {
    _ = a;
    _ = chunks;
    var layout_changed = false;
    return layout_changed;
}

fn resolveOrgAddress(a: *Assembler, file: *SourceFile, file_handle: SourceFile.Handle, params: Expression.Handle) u32 {
    return resolveExpressionConstant(a, file, file_handle, params).asUnsigned(u32) orelse 0;
}

fn resolveExpressionConstant(a: *Assembler, file: *SourceFile, file_handle: SourceFile.Handle, expr_handle: Expression.Handle) *const Constant {
    var expr_infos = file.expressions.items(.info);
    var expr_tokens = file.expressions.items(.token);
    var expr_resolved_constants = file.expressions.items(.resolved_constant);

    if (expr_resolved_constants[expr_handle]) |constant| {
        return constant;
    }

    switch (expr_infos[expr_handle]) {
        .literal_symbol_def, .directive_symbol_def,
        .literal_int, .literal_str,
        => unreachable, // These should be guaranteed to already have resolved_constant set

        .list, .arrow_list, .literal_reg => {
            // These don't have a meaningful value
            // but in the case of errors, we might end up trying to look for it anyway,
            // so we'll set it to zero:
            expr_resolved_constants[expr_handle] = &Constant.builtin.zero;
        },

        .literal_symbol_ref => {
            const token_handle = expr_tokens[expr_handle];
            const raw_symbol = file.tokens.get(token_handle).location(file.source);
            const symbol_constant = Constant.initSymbolLiteral(a.gpa, &a.constant_temp, raw_symbol);
            resolveSymbolRefExprConstant(a, file, file_handle, expr_handle, token_handle, symbol_constant, expr_resolved_constants);
        },
        .directive_symbol_ref => |inner_expr| {
            const token_handle = expr_tokens[inner_expr];
            const symbol_constant = expr_resolved_constants[inner_expr].?;
            resolveSymbolRefExprConstant(a, file, file_handle, expr_handle, token_handle, symbol_constant, expr_resolved_constants);
        },
    }

    return expr_resolved_constants[expr_handle].?;
}

fn resolveSymbolRefExprConstant(
    a: *Assembler,
    file: *SourceFile,
    file_handle: SourceFile.Handle,
    expr_handle: Expression.Handle,
    symbol_token_handle: lex.Token.Handle,
    symbol_constant: Constant,
    expr_resolved_constants: []?*const Constant,
) void {
    if (a.lookupSymbol(file, file_handle, symbol_token_handle, symbol_constant.asString())) |target| {
        switch (target) {
            .expression => |target_expr_handle| {
                expr_resolved_constants[expr_handle] = resolveExpressionConstant(a, file, file_handle, target_expr_handle);
            },
            .instruction => |target_insn_ref| {
                const target_file = a.getSource(target_insn_ref.file);
                const address = target_file.instructions.items(.address)[target_insn_ref.instruction];
                const constant = Constant.initUnsigned(address, 32);
                expr_resolved_constants[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
            },
            .not_found => {
                expr_resolved_constants[expr_handle] = &Constant.builtin.zero;
            },
        }
    } else unreachable;
}

fn resolveInstructionEncoding(a: *Assembler, op: *Instruction.Operation, params: Expression.Handle) u7 {
    a.params_temp.clearRetainingCapacity();
    buildInstructionParameters(a, params);

    const what = op.insn;
    var encoding_iter = a.edb.getMatchingEncodings(.{
        .mnemonic = what.mnemonic,
        .suffix = what.suffix,
        .params = a.params_temp.items,
    });

    a.params_temp.clearRetainingCapacity();

    // TODO Any encoding without a suffix should automatically be better than an encoding with a suffix, if the operation does not specify a suffix.
    // TODO If multiple different suffixes match an unsuffixed operation, but no unsuffixed encoding is found, it is ambiguous and should not be selected.

    var best_length: ?u32 = null;
    var best_encoding: ?*const ie.InstructionEncoding = null;
    while (encoding_iter.nextPointer()) |enc| {
        const length = ie.getInstructionLength(enc.*);
        if (best_length) |cur_length| {
            if (length < cur_length) {
                best_encoding = enc;
                best_length = length;
            }
        } else {
            best_encoding = enc;
            best_length = length;
        }
    }
    if (best_encoding) |enc| {
        op.* = .{ .bound_insn = enc };
        return best_length.?;
    }
    return 0;
}

fn buildInstructionParameters(a: *Assembler, params: Expression.Handle, is_arrow: bool) void {

    switch (expr_infos[params]) {
        .list => |bin| {
            buildInstructionParameters(a, bin.left, is_arrow);
            buildInstructionParameters(a, bin.right, false);
            return;
        },
        .arrow_list => |bin| {
            buildInstructionParameters(a, bin.left, is_arrow);
            buildInstructionParameters(a, bin.right, true);
            return;
        },
        else => {}
    }

    var param = ie.Parameter{
        .arrow = is_arrow,
        .type = .{
            .base = .none,
            .offset = .none,
        },
        .base = 0,
        .offset = 0,
    };

    switch (expr_resolved_types[params]) {
        .absolute_address_base, .poison, .symbol_def => {},
        .constant => |info| {
            param.type.base = .constant;
            const constant = resolveExpressionConstant(a, file, file_handle, params);
            param.base = switch (info.signedness) {
                .signed => constant.asSigned(i64),
                .unsigned => constant.asUnsigned(u63),
            };
        },
        .reg8 => |reg| {
            param.base = reg.index;
            param.type.base = if (reg.signedness) |s| switch (s) {
                .signed => .reg8s,
                .unsigned => .reg8u,
            } else .reg8;
        },
        .reg16 => |reg| {
            param.base = reg.index;
            param.type.base = if (reg.signedness) |s| switch (s) {
                .signed => .reg16s,
                .unsigned => .reg16u,
            } else .reg16;
        },
        .reg32 => |reg| {
            param.base = reg.index;
            param.type.base = if (reg.signedness) |s| switch (s) {
                .signed => .reg32s,
                .unsigned => .reg32u,
            } else .reg32;
        },
        .sr => |reg| {
            param.type.base = switch (reg) {
                .ip => .IP,
                .sp => .SP,
                .rp => .RP,
                .bp => .BP,
                .uxp => .UXP,
                .kxp => .KXP,
                .asn => .ASN,
                .stat => .STAT,
            };
        },
        .data_address => |info| {
            switch (info.base.*) {
                .absolute_address_base, .poison, .symbol_def => {},

            }
        },
        .insn_address => {},
        .stack_address => {},
    }

    a.params_temp.append(a.gpa, param) catch @panic("OOM");
}