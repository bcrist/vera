const std = @import("std");
const ie = @import("instruction_encoding");
const lex = @import("lex.zig");
const Assembler = @import("Assembler.zig");
const SourceFile = @import("SourceFile.zig");
const Section = @import("Section.zig");
const Constant = @import("Constant.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");

pub fn resetLayoutDependentExpressions(a: *Assembler) void {
    for (a.files.items) |*file| {
        var expr_resolved_constants = file.expressions.items(.resolved_constant);
        for (file.expressions.items(.flags), 0..) |flags, expr_handle| {
            if (flags.contains(.constant_depends_on_layout)) {
                expr_resolved_constants[expr_handle] = null;
            }
        }
    }
}

pub fn doFixedOrgLayout(a: *Assembler, chunks: std.ArrayListUnmanaged(SourceFile.Chunk)) bool {
    var layout_changed = false;
    for (chunks.items) |chunk| {
        var file = a.getSource(chunk.file);
        const operations = file.instructions.items(.operation);
        const params = file.instructions.items(.params);
        const insn_flags = file.instructions.items(.flags);
        const insn_addresses = file.instructions.items(.address);

        // Find the .org directive (there will be exactly one, at or near the start)
        // Resolve it to find the initial address for the chunk:
        var initial_address: u32 = undefined;
        var iter = chunk.instructions;
        while (iter.next()) |insn_handle| {
            if (operations[insn_handle] == .org) {
                initial_address = resolveOrgAddress(a, file, chunk.file, insn_addresses[insn_handle], params[insn_handle]);
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
                    address += resolveInstructionEncoding(a, file, chunk.file, address, &operations[insn_handle], params[insn_handle]);
                    layout_changed = true;
                },
                .bound_insn => |encoding| {
                    if (insn_flags[insn_handle].contains(.encoding_depends_on_layout)) {
                        const old_encoding = encoding;
                        operations[insn_handle] = .{ .insn = .{
                            .mnemonic = old_encoding.mnemonic,
                            .suffix = old_encoding.suffix,
                        }};
                        address += resolveInstructionEncoding(a, file, chunk.file, address, &operations[insn_handle], params[insn_handle]);
                        switch (operations[insn_handle]) {
                            .bound_insn => |new_encoding| if (!ie.eql(old_encoding.*, new_encoding.*)) {
                                layout_changed = true;
                            },
                            else => {},
                        }
                    } else {
                        address += ie.getInstructionLength(encoding.*);
                    }
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

fn resolveOrgAddress(a: *Assembler, file: *SourceFile, file_handle: SourceFile.Handle, ip: u32, params: ?Expression.Handle) u32 {
    if (params) |expr_handle| {
        return std.math.cast(u32, resolveExpressionConstant(a, file, file_handle, ip, expr_handle).asInt() catch 0) orelse 0;
    } else return 0;
}

fn resolveExpressionConstant(a: *Assembler, file: *SourceFile, file_handle: SourceFile.Handle, ip: u32, expr_handle: Expression.Handle) *const Constant {
    var expr_infos = file.expressions.items(.info);
    var expr_tokens = file.expressions.items(.token);
    var expr_resolved_constants = file.expressions.items(.resolved_constant);
    var expr_resolved_types = file.expressions.items(.resolved_type);

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
            resolveSymbolRefExprConstant(a, file, file_handle, ip, expr_handle, token_handle, symbol_constant, expr_resolved_types, expr_resolved_constants);
        },
        .directive_symbol_ref => |inner_expr| {
            const token_handle = expr_tokens[inner_expr];
            const symbol_constant = expr_resolved_constants[inner_expr].?;
            resolveSymbolRefExprConstant(a, file, file_handle, ip, expr_handle, token_handle, symbol_constant.*, expr_resolved_types, expr_resolved_constants);
        },
    }

    return expr_resolved_constants[expr_handle].?;
}

fn resolveSymbolRefExprConstant(
    a: *Assembler,
    file: *SourceFile,
    file_handle: SourceFile.Handle,
    ip: u32,
    expr_handle: Expression.Handle,
    symbol_token_handle: lex.Token.Handle,
    symbol_constant: Constant,
    expr_resolved_types: []const ie.ExpressionType,
    expr_resolved_constants: []?*const Constant,
) void {
    if (a.lookupSymbol(file, file_handle, symbol_token_handle, symbol_constant.asString())) |target| {
        switch (target) {
            .expression => |target_expr_handle| {
                // .def expressions are evaluated based on the context of the definition, so we need to look up the address
                // of the actual .def instruction
                const expr_token = file.expressions.items(.token)[target_expr_handle];
                const expr_insn = file.findInstructionByToken(expr_token);
                const expr_ip = file.instructions.items(.address)[expr_insn];
                expr_resolved_constants[expr_handle] = resolveExpressionConstant(a, file, file_handle, expr_ip, target_expr_handle);
            },
            .instruction => |target_insn_ref| {
                const target_file = a.getSource(target_insn_ref.file);
                var value: i64 = target_file.instructions.items(.address)[target_insn_ref.instruction];
                switch (expr_resolved_types[expr_handle]) {
                    .raw_base_offset, .data_address, .insn_address, .stack_address => |info| {
                        if (std.meta.eql(info.base, .{ .sr = .ip })) {
                            value -= ip;
                        }
                    },
                    else => {},
                }
                const constant = Constant.initInt(value, null);
                expr_resolved_constants[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
            },
            .not_found => {
                expr_resolved_constants[expr_handle] = &Constant.builtin.zero;
            },
        }
    } else unreachable;
}

fn resolveInstructionEncoding(a: *Assembler, file: *SourceFile, file_handle: SourceFile.Handle, ip: u32, op: *Instruction.Operation, params: ?Expression.Handle) u7 {
    const expr_infos = file.expressions.items(.info);
    const expr_resolved_types = file.expressions.items(.resolved_type);

    a.params_temp.clearRetainingCapacity();
    if (params) |expr_handle| {
        buildInstructionParameters(a, file, file_handle, ip, expr_handle, false, expr_infos, expr_resolved_types);
    }

    const what = op.insn;
    var encoding_iter = a.edb.getMatchingEncodings(.{
        .mnemonic = what.mnemonic,
        .suffix = what.suffix,
        .params = a.params_temp.items,
    });

    a.params_temp.clearRetainingCapacity();

    var best_length: ?u7 = null;
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

fn buildInstructionParameters(
    a: *Assembler,
    file: *SourceFile,
    file_handle: SourceFile.Handle,
    ip: u32,
    params: Expression.Handle,
    is_arrow: bool,
    expr_infos: []const Expression.Info,
    expr_resolved_types: []const ie.ExpressionType,
) void {
    // TODO want a way to have is_arrow on the first parameter?
    switch (expr_infos[params]) {
        .list => |bin| {
            buildInstructionParameters(a, file, file_handle, ip, bin.left, is_arrow, expr_infos, expr_resolved_types);
            buildInstructionParameters(a, file, file_handle, ip, bin.right, false, expr_infos, expr_resolved_types);
            return;
        },
        .arrow_list => |bin| {
            buildInstructionParameters(a, file, file_handle, ip, bin.left, is_arrow, expr_infos, expr_resolved_types);
            buildInstructionParameters(a, file, file_handle, ip, bin.right, true, expr_infos, expr_resolved_types);
            return;
        },
        else => {}
    }

    const constant = resolveExpressionConstant(a, file, file_handle, ip, params);

    a.params_temp.append(a.gpa, .{
        .arrow = is_arrow,
        .expr_type = expr_resolved_types[params],
        .constant = constant.asInt() catch 0,
    }) catch @panic("OOM");
}