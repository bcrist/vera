const std = @import("std");
const bus = @import("bus_types");
const lex = @import("lex.zig");
const isa = @import("isa_types");
const ie = @import("isa_encoding");
const symbols = @import("symbols.zig");
const layout = @import("layout.zig");
const Assembler = @import("Assembler.zig");
const SourceFile = @import("SourceFile.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const Constant = @import("Constant.zig");
const Section = @import("Section.zig");
const Error = @import("Error.zig");
const Token = lex.Token;
const ExpressionType = ie.Parameter.ExpressionType;
const Block = SourceFile.Block;

// Set the resolved type for labels to .symbol_def.
// The resolved constant for these expressions is the raw symbol name.
// Any labels which do not begin with `_` and are not in a stack section will be added to the global symbol table.
// Create Sections for each named section block and set the Block.section handle to reference it.
pub fn processLabelsAndSections(a: *Assembler, file: *SourceFile) void {
    const s = file.slices();
    const insn_operations = s.insn.items(.operation);
    const insn_labels = s.insn.items(.label);
    const insn_params = s.insn.items(.params);
    const expr_infos = s.expr.items(.info);
    const block_labels = s.block.items(.labels);

    for (0.., s.block.items(.first_insn), s.block.items(.end_insn)) |block_handle_usize, first_insn, end_insn| {
        const block_handle = @intCast(SourceFile.Block.Handle, block_handle_usize);
        const private_labels = &block_labels[block_handle];

        var is_stack_block = false;

        for (first_insn.., insn_operations[first_insn..end_insn]) |insn_handle_usize, op| {
            const insn_handle = @intCast(Instruction.Handle, insn_handle_usize);
            switch (op) {
                .section, .boot, .code, .kcode, .entry, .kentry,
                .data, .kdata, .@"const", .kconst, .stack => {
                    s.block.items(.block_type)[block_handle] = op;

                    var maybe_section_name: ?[]const u8 = null;
                    if (insn_params[insn_handle]) |section_name_expr| {
                        const symbol_constant = resolveSymbolDefExpr(a, s, section_name_expr);
                        maybe_section_name = symbol_constant.asString();
                    }

                    if (op == .stack) {
                        is_stack_block = true;
                        processStackBlock(a, s, block_handle, insn_handle, maybe_section_name);
                    } else {
                        processSectionBlock(a, s, block_handle, insn_handle, Section.Kind.fromDirective(op), maybe_section_name);
                    }
                },
                .def => {
                    const params_expr = insn_params[insn_handle].?;
                    const symbol_expr = s.expr.items(.info)[params_expr].list.left;
                    _ = resolveSymbolDefExpr(a, s, symbol_expr);
                },
                .undef, .push, .pop => {
                    if (insn_params[insn_handle]) |params| {
                        _ = resolveSymbolDefExprList(a, s, params);
                    }
                },
                .local => {
                    const params_expr = insn_params[insn_handle].?;
                    const bin = expr_infos[params_expr].list;
                    const symbol_name = resolveSymbolDefExpr(a, s, bin.left).asString();
                    const result = s.file.locals.getOrPut(a.gpa, symbol_name) catch @panic("OOM");
                    if (result.found_existing) {
                        const token = s.expr.items(.token)[params_expr];
                        const target_insn_handle = result.value_ptr.getInstructionHandle(s);
                        const line_number = s.insn.items(.line_number)[target_insn_handle];
                        a.recordErrorFmt(s.file.handle, token, "Duplicate .local name (canonical symbol is at line {})", .{ line_number }, .{});
                    } else {
                        result.value_ptr.* = .{ .expression = bin.right };
                    }
                },
                .none, .nil, .org, .@"align", .keep, .insn, .bound_insn, .db, .dw, .dd, .range => {},
            }
        }

        for (first_insn.., insn_labels[first_insn..end_insn]) |insn_handle_usize, maybe_label_expr| {
            const insn_handle = @intCast(Instruction.Handle, insn_handle_usize);

            if (maybe_label_expr) |label_expr| switch (expr_infos[label_expr]) {
                .local_label_def => |inner_label_expr| {
                    const symbol_constant = resolveSymbolDefExpr(a, s, inner_label_expr);
                    const symbol_name = symbol_constant.asString();
                    const result = s.file.locals.getOrPut(a.gpa, symbol_name) catch @panic("OOM");
                    if (result.found_existing) {
                        const token = s.expr.items(.token)[label_expr];
                        const target_insn_handle = result.value_ptr.getInstructionHandle(s);
                        const line_number = s.insn.items(.line_number)[target_insn_handle];
                        a.recordErrorFmt(s.file.handle, token, "Duplicate .local name (canonical symbol is at line {})", .{ line_number }, .{});
                    } else {
                        result.value_ptr.* = .{ .instruction = .{
                            .file = file.handle,
                            .instruction = insn_handle,
                        }};
                    }
                },
                else => {
                    const symbol_constant = resolveSymbolDefExpr(a, s, label_expr);
                    const symbol_name = symbol_constant.asString();
                    if (is_stack_block or std.mem.startsWith(u8, symbol_name, "_")) {
                        const result = private_labels.getOrPut(a.gpa, symbol_name) catch @panic("OOM");
                        if (result.found_existing) {
                            const token = s.expr.items(.token)[label_expr];
                            const target_line_number = s.insn.items(.line_number)[result.value_ptr.*];
                            a.recordErrorFmt(s.file.handle, token, "Duplicate private label (canonical label is at line {})", .{ target_line_number }, .{});
                        } else {
                            result.value_ptr.* = insn_handle;
                        }
                    } else {
                        const result = a.public_labels.getOrPut(a.gpa, symbol_name) catch @panic("OOM");
                        if (result.found_existing) {
                            const token = s.expr.items(.token)[label_expr];
                            const target_file = a.getSource(result.value_ptr.file);
                            const target_line_number = target_file.instructions.items(.line_number)[result.value_ptr.instruction];
                            a.recordErrorFmt(s.file.handle, token, "Duplicate label (canonical label is at line {} in {s})", .{ target_line_number, target_file.name }, .{});
                        } else {
                            result.value_ptr.* = .{
                                .file = file.handle,
                                .instruction = insn_handle,
                            };
                        }
                    }
                },
            };
        }
    }
}

fn processStackBlock(a: *Assembler, s: SourceFile.Slices, block_handle: SourceFile.Block.Handle, insn_handle: Instruction.Handle, maybe_stack_name: ?[]const u8) void {
    const name = maybe_stack_name orelse "";
    var result = s.file.stacks.getOrPut(a.gpa, name) catch @panic("OOM");
    if (result.found_existing) {
        const canonical_insn_handle = s.block.items(.first_insn)[result.value_ptr.*];
        const canonical_line_number = s.insn.items(.line_number)[canonical_insn_handle];
        const token = s.insn.items(.token)[insn_handle];
        a.recordErrorFmt(s.file.handle, token, "Ignoring duplicate .stack block; canonical block is at line {}", .{ canonical_line_number }, .{});
    } else {
        result.value_ptr.* = block_handle;
    }
}

fn processSectionBlock(a: *Assembler, s: SourceFile.Slices, block_handle: SourceFile.Block.Handle, insn_handle: Instruction.Handle, kind: Section.Kind, maybe_section_name: ?[]const u8) void {
    const section_name = maybe_section_name orelse kind.defaultName();

    var range: ?Assembler.AddressRange = null;
    if (kind == .boot) {
        range = .{
            .first = 0,
            .len = 0x1_0000,
        };
    }

    const entry = a.sections.getOrPutValue(a.gpa, section_name, .{
        .name = section_name,
        .kind = kind,
        .has_chunks = false,
        .range = range,
    }) catch @panic("OOM");

    const found_kind = entry.value_ptr.kind;
    if (found_kind != kind) {
        const token = s.insn.items(.token)[insn_handle];
        a.recordErrorFmt(s.file.handle, token, "Section already exists; expected .{s}", .{ @tagName(found_kind) }, .{});
    }

    s.block.items(.section)[block_handle] = @intCast(Section.Handle, entry.index);
}

fn resolveSymbolDefExprList(a: *Assembler, s: SourceFile.Slices, symbol_def_expr: Expression.Handle) void {
    const infos = s.expr.items(.info);
    var expr = symbol_def_expr;
    while (infos[expr] == .list) {
        const bin = infos[expr].list;
        _ = resolveSymbolDefExpr(a, s, bin.left);
        expr = bin.right;
    }
    _ = resolveSymbolDefExpr(a, s, expr);
}

fn resolveSymbolDefExpr(a: *Assembler, s: SourceFile.Slices, symbol_def_expr: Expression.Handle) *const Constant {
    const symbol_name = symbols.parseSymbol(a, s, symbol_def_expr);
    s.expr.items(.resolved_constant)[symbol_def_expr] = symbol_name;
    s.expr.items(.resolved_type)[symbol_def_expr] = .{ .symbol_def = {} };
    return symbol_name;
}

pub fn resolveExpressionTypes(a: *Assembler) bool {
    var made_progress = true;
    var unresolved = true;
    while (made_progress) {
        made_progress = false;
        unresolved = false;

        for (a.files.items) |*file| {
            var slices = file.slices();
            for (slices.expr.items(.resolved_type), 0..) |expr_type, expr_handle| {
                if (expr_type == .unknown) {
                    if (tryResolveExpressionType(a, slices, @intCast(Expression.Handle, expr_handle))) {
                        made_progress = true;
                    } else {
                        unresolved = true;
                    }
                }
            }
        }
    }

    if (unresolved) {
        for (a.files.items) |*file| {
            const expr_slice = file.expressions.slice();
            const resolved_types = expr_slice.items(.resolved_type);
            for (resolved_types, 0..) |expr_type, expr_handle| {
                if (expr_type == .unknown) {
                    const token = expr_slice.items(.token)[expr_handle];
                    a.recordError(file.handle, token, "Could not determine type for expression", .{});
                    resolved_types[expr_handle] = .{ .poison = {} };
                }
            }
        }
        return false;
    }

    return true;
}

pub fn tryResolveExpressionType(a: *Assembler, s: SourceFile.Slices, expr_handle: Expression.Handle) bool {
    const expr_infos = s.expr.items(.info);
    var expr_resolved_types = s.expr.items(.resolved_type);

    std.debug.assert(expr_resolved_types[expr_handle] == .unknown);

    const info = expr_infos[expr_handle];
    switch (info) {
        .list, .arrow_list, .arrow_prefix => {
            expr_resolved_types[expr_handle] = .{ .poison = {} };
        },
        .literal_int => {
            const token_handle = s.expr.items(.token)[expr_handle];
            const token = s.file.tokens.get(token_handle);
            if (Constant.initIntLiteral(a.gpa, &a.constant_temp, token.location(s.file.source))) |constant| {
                expr_resolved_types[expr_handle] = .{ .constant = {} };
                s.expr.items(.resolved_constant)[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
            } else |err| {
                switch (err) {
                    error.Overflow         => a.recordError(s.file.handle, token_handle, "Integer literal too large", .{}),
                    error.InvalidCharacter => a.recordError(s.file.handle, token_handle, "Invalid character in integer literal", .{}),
                }
                expr_resolved_types[expr_handle] = .{ .poison = {} };
            }
        },
         .literal_str => {
            var token_handle = s.expr.items(.token)[expr_handle];
            var token = s.file.tokens.get(token_handle);
            if (token.kind == .str_literal_raw) {
                const token_kinds = s.file.tokens.items(.kind);
                a.constant_temp.clearRetainingCapacity();
                var raw = token.location(s.file.source);
                if (raw[raw.len - 1] == '\n') {
                    raw.len -= 1;
                }
                if (raw[raw.len - 1] == '\r') {
                    raw.len -= 1;
                }
                a.constant_temp.appendSlice(a.gpa, raw[2..]) catch @panic("OOM");
                token_handle += 1;
                while (true) {
                    switch (token_kinds[token_handle]) {
                        .str_literal_raw => {
                            token = s.file.tokens.get(token_handle);
                            raw = token.location(s.file.source);
                            if (raw[raw.len - 1] == '\n') {
                                raw.len -= 1;
                            }
                            if (raw[raw.len - 1] == '\r') {
                                raw.len -= 1;
                            }
                            a.constant_temp.append(a.gpa, '\n') catch @panic("OOM");
                            a.constant_temp.appendSlice(a.gpa, raw[2..]) catch @panic("OOM");
                        },
                        .linespace => {},
                        else => break,
                    }
                    token_handle += 1;
                }
                const constant = Constant.initString(a.constant_temp.items);
                expr_resolved_types[expr_handle] = .{ .constant = {} };
                s.expr.items(.resolved_constant)[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
            } else if (Constant.initStringLiteral(a.gpa, &a.constant_temp, token.location(s.file.source))) |constant| {
                expr_resolved_types[expr_handle] = .{ .constant = {} };
                s.expr.items(.resolved_constant)[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
            } else |err| {
                switch (err) {
                    error.Overflow         => a.recordError(s.file.handle, token_handle, "Invalid decimal byte in string literal escape sequence", .{}),
                    error.InvalidCharacter => a.recordError(s.file.handle, token_handle, "Invalid character in string literal", .{}),
                    error.InvalidCodepoint => a.recordError(s.file.handle, token_handle, "Invalid codepoint in string literal escape sequence", .{}),
                    error.InvalidBase64    => a.recordError(s.file.handle, token_handle, "Invalid base64 in string literal escape sequence", .{}),
                    error.UnclosedLiteral  => a.recordError(s.file.handle, token_handle, "String literal is missing closing '\"' character", .{}),
                    error.IncompleteEscape => a.recordError(s.file.handle, token_handle, "String literal contains an incomplete escape sequence", .{}),
                }
                expr_resolved_types[expr_handle] = .{ .poison = {} };
            }
        },
        .literal_reg => {
            // SFR types are preassigned when parsed, so we can assume we're dealing with a GPR name.
            const token_handle = s.expr.items(.token)[expr_handle];
            const token = s.file.tokens.get(token_handle);
            const name = token.location(s.file.source);
            const index = std.fmt.parseUnsigned(u4, name[1..], 10) catch unreachable;
            const reg_type: ExpressionType = switch (name[0]) {
                'r', 'R' => .{ .reg16 = .{
                    .index = index,
                    .signedness = null,
                }},
                'x', 'X' => .{ .reg32 = .{
                    .index = index,
                    .signedness = null,
                }},
                'b', 'B' => .{ .reg8 = .{
                    .index = index,
                    .signedness = null,
                }},
                else => unreachable,
            };

            expr_resolved_types[expr_handle] = reg_type;
        },
        .literal_current_address => {
            const token_handle = s.expr.items(.token)[expr_handle];
            const block_handle = s.file.findBlockByToken(token_handle);
            if (s.block.items(.block_type)[block_handle]) |op| {
                expr_resolved_types[expr_handle] = switch (op) {
                    .none, .nil, .insn, .bound_insn, .org, .@"align", .keep,
                    .def, .undef, .local, .db, .dw, .dd, .push, .pop, .range,
                    => unreachable,

                    .section => ExpressionType.absoluteAddress(.data),
                    .boot, .code, .kcode, .entry, .kentry => ExpressionType.relativeAddress(.insn, .{ .sr = .IP }),
                    .data, .kdata, .@"const", .kconst => ExpressionType.relativeAddress(.data, .{ .sr = .IP }),
                    .stack => ExpressionType.relativeAddress(.stack, .{ .sr = .SP }),
                } catch unreachable;
            } else {
                // Block does not have a section handle, so it's an .info section by default
                expr_resolved_types[expr_handle] = ExpressionType.absoluteAddress(.data);
            }
            s.expr.items(.flags)[expr_handle].insert(.constant_depends_on_layout);
            return true;
        },
        .literal_symbol_ref => {
            const token_handle = s.expr.items(.token)[expr_handle];
            const symbol_literal = s.file.tokens.get(token_handle).location(s.file.source);
            const symbol_constant = Constant.initSymbolLiteral(a.gpa, &a.constant_temp, symbol_literal);
            return tryResolveSymbolType(a, s, expr_handle, token_handle, symbol_constant.asString());
        },
        .directive_symbol_ref => |inner_expr| {
            if (s.expr.items(.resolved_constant)[inner_expr]) |symbol| {
                return tryResolveSymbolType(a, s, expr_handle, s.expr.items(.token)[expr_handle], symbol.asString());
            } else return false;
        },
        .literal_symbol_def, .directive_symbol_def => {
            _ = resolveSymbolDefExpr(a, s, expr_handle);
        },
        .local_label_def => |inner_expr_handle| {
            const symbol_name = resolveSymbolDefExpr(a, s, inner_expr_handle);
            s.expr.items(.resolved_constant)[expr_handle] = symbol_name;
            expr_resolved_types[expr_handle] = .{ .symbol_def = {} };
        },
        .plus => |bin| {
            var builder = ExpressionType.Builder{};
            builder.add(expr_resolved_types[bin.left]);
            builder.add(expr_resolved_types[bin.right]);
            const result_type: ExpressionType = builder.build() catch t: {
                const token = s.expr.items(.token)[expr_handle];
                a.recordError(s.file.handle, token, "Cannot represent result of symbolic addition", .{});
                break :t .{ .poison = {} };
            };
            if (result_type == .unknown) return false;
            expr_resolved_types[expr_handle] = result_type;
            resolveConstantDependsOnLayoutBinary(s, expr_handle, bin);
        },
        .minus => |bin| {
            var builder = ExpressionType.Builder{};
            builder.add(expr_resolved_types[bin.left]);
            builder.subtract(expr_resolved_types[bin.right]);
            const result_type: ExpressionType = builder.build() catch t: {
                const token = s.expr.items(.token)[expr_handle];
                a.recordError(s.file.handle, token, "Cannot represent result of symbolic subtraction", .{});
                break :t .{ .poison = {} };
            };
            if (result_type == .unknown) return false;
            expr_resolved_types[expr_handle] = result_type;
            resolveConstantDependsOnLayoutBinary(s, expr_handle, bin);
        },
        .negate, .complement => |inner_expr| {
            expr_resolved_types[expr_handle] = switch (expr_resolved_types[inner_expr]) {
                .unknown => return false,
                .poison => .{ .poison = {} },
                .constant => .{ .constant = {} },
                .raw_base_offset, .data_address, .insn_address, .stack_address,
                .symbol_def, .reg8, .reg16, .reg32, .sr => t: {
                    const token = s.expr.items(.token)[expr_handle];
                    a.recordError(s.file.handle, token, "Operand must be a constant expression", .{});
                    break :t .{ .poison = {} };
                },
            };
            resolveConstantDependsOnLayout(s, expr_handle, inner_expr);
        },
        .index_to_reg8, .index_to_reg16, .index_to_reg32 => |inner_expr| {
            if (s.expr.items(.flags)[inner_expr].contains(.constant_depends_on_layout)) {
                const token = s.expr.items(.token)[expr_handle];
                a.recordError(s.file.handle, token, "Operand cannot vary on memory layout", .{});
                expr_resolved_types[expr_handle] = .{ .poison = {} };
                return true;
            }
            expr_resolved_types[expr_handle] = switch (expr_resolved_types[inner_expr]) {
                .unknown => return false,
                .poison => .{ .poison = {} },
                .constant => t: {
                    const constant = layout.resolveExpressionConstant(a, s, 0, inner_expr) orelse {
                        break :t .{ .poison = {} };
                    };
                    const index = constant.asInt(u4) catch {
                        const token = s.expr.items(.token)[expr_handle];
                        a.recordError(s.file.handle, token, "Operand out of range", .{});
                        break :t .{ .poison = {} };
                    };

                    const reg = isa.IndexedRegister{
                        .index = index,
                        .signedness = null,
                    };

                    break :t switch (info) {
                        .index_to_reg8 => .{ .reg8 = reg },
                        .index_to_reg16 => .{ .reg16 = reg },
                        .index_to_reg32 => .{ .reg32 = reg },
                        else => unreachable,
                    };
                },
                .raw_base_offset, .data_address, .insn_address, .stack_address,
                .symbol_def, .reg8, .reg16, .reg32, .sr => t: {
                    const token = s.expr.items(.token)[expr_handle];
                    a.recordError(s.file.handle, token, "Operand must be a constant expression", .{});
                    break :t .{ .poison = {} };
                },
            };
        },
        .reg_to_index => |inner_expr| {
            expr_resolved_types[expr_handle] = switch (expr_resolved_types[inner_expr]) {
                .unknown => return false,
                .poison => .{ .poison = {} },
                .reg8, .reg16, .reg32 => |reg| t: {
                    const constant = Constant.initInt(reg.index);
                    s.expr.items(.resolved_constant)[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
                    break :t .{ .constant = {} };
                },
                .raw_base_offset, .data_address, .insn_address, .stack_address,
                .constant, .symbol_def, .sr => t: {
                    const token = s.expr.items(.token)[expr_handle];
                    a.recordError(s.file.handle, token, "Operand must be a GPR expression", .{});
                    break :t .{ .poison = {} };
                },
            };
        },
        .multiply, .shl, .shr, .concat, .concat_repeat, .bitwise_or, .bitwise_xor, .bitwise_and,
        .length_cast, .truncate, .sign_extend, .zero_extend => |bin| {
            const left = expr_resolved_types[bin.left];
            const right = expr_resolved_types[bin.right];
            expr_resolved_types[expr_handle] = t: {
                if (left == .poison or right == .poison) break :t .{ .poison = {} };
                if (left == .unknown or right == .unknown) return false;
                if (left == .constant and right == .constant) break :t .{ .constant = {} };
                const token = s.expr.items(.token)[expr_handle];
                a.recordError(s.file.handle, token, "Both operands must be constant expressions", .{});
                break :t .{ .poison = {} };
            };
            resolveConstantDependsOnLayoutBinary(s, expr_handle, bin);
        },
        .signed_cast, .unsigned_cast, .remove_signedness_cast => |inner_expr| {
            const inner_type = expr_resolved_types[inner_expr];
            expr_resolved_types[expr_handle] = switch (inner_type) {
                .unknown => return false,
                .poison => .{ .poison = {} },
                .constant => if (info == .remove_signedness_cast) t: {
                    const token = s.expr.items(.token)[expr_handle];
                    a.recordError(s.file.handle, token, "Operand must be a GPR expression", .{});
                    break :t .{ .poison = {} };
                } else .{ .constant = {} },
                .reg8, .reg16, .reg32 => |reg| t: {
                    var new_reg = reg;
                    new_reg.signedness = switch (info) {
                        .signed_cast => .signed,
                        .unsigned_cast => .unsigned,
                        .remove_signedness_cast => null,
                        else => unreachable,
                    };

                    break :t switch (inner_type) {
                        .reg8 => .{ .reg8 = new_reg },
                        .reg16 => .{ .reg16 = new_reg },
                        .reg32 => .{ .reg32 = new_reg },
                        else => unreachable,
                    };
                },
                .raw_base_offset, .data_address, .insn_address, .stack_address, .symbol_def, .sr => t: {
                    const token = s.expr.items(.token)[expr_handle];
                    a.recordError(s.file.handle, token, "Operand must be a constant or GPR expression", .{});
                    break :t .{ .poison = {} };
                },
            };
            resolveConstantDependsOnLayout(s, expr_handle, inner_expr);
        },
        .absolute_address_cast => |inner_expr| {
            const inner_type = expr_resolved_types[inner_expr];
            expr_resolved_types[expr_handle] = switch (inner_type) {
                .unknown => return false,
                .poison => .{ .poison = {} },
                .raw_base_offset, .data_address, .insn_address, .stack_address => |bo| t: {
                    if (!std.meta.eql(bo.base, .{ .sr = .IP })) {
                        const token = s.expr.items(.token)[expr_handle];
                        a.recordError(s.file.handle, token, "Operand must be an IP-relative expression", .{});
                        break :t .{ .poison = {} };
                    }
                    var builder = ExpressionType.Builder{};
                    builder.add(inner_type);
                    builder.subtract(.{ .sr = .IP });
                    break :t builder.build() catch unreachable;
                },
                .reg8, .reg16, .reg32, .constant, .symbol_def, .sr => t: {
                    const token = s.expr.items(.token)[expr_handle];
                    a.recordError(s.file.handle, token, "Operand must be an IP-relative expression", .{});
                    break :t .{ .poison = {} };
                },
            };
            resolveConstantDependsOnLayout(s, expr_handle, inner_expr);
        },
        .data_address_cast => |inner_expr| {
            const inner_type = expr_resolved_types[inner_expr];
            expr_resolved_types[expr_handle] = switch (inner_type) {
                .unknown => return false,
                .poison => .{ .poison = {} },
                .data_address => inner_type,
                .raw_base_offset => |bo| .{ .data_address = bo },
                .reg8, .reg16, .reg32, .constant, .sr => .{ .data_address = ExpressionType.BaseOffset.init(inner_type, null) catch unreachable },
                .insn_address, .stack_address => t: {
                    const token = s.expr.items(.token)[expr_handle];
                    a.recordError(s.file.handle, token, "Casting directly between address spaces is not allowed.  Use `.d .raw` if you really want this.", .{});
                    break :t .{ .poison = {} };
                },
                .symbol_def => unreachable,
            };
            resolveConstantDependsOnLayout(s, expr_handle, inner_expr);
        },
        .insn_address_cast => |inner_expr| {
            const inner_type = expr_resolved_types[inner_expr];
            expr_resolved_types[expr_handle] = switch (inner_type) {
                .unknown => return false,
                .poison => .{ .poison = {} },
                .insn_address => inner_type,
                .raw_base_offset => |bo| .{ .insn_address = bo },
                .reg8, .reg16, .reg32, .constant, .sr => .{ .insn_address = ExpressionType.BaseOffset.init(inner_type, null) catch unreachable },
                .data_address, .stack_address => t: {
                    const token = s.expr.items(.token)[expr_handle];
                    a.recordError(s.file.handle, token, "Casting directly between address spaces is not allowed.  Use `.i .raw` if you really want this.", .{});
                    break :t .{ .poison = {} };
                },
                .symbol_def => unreachable,
            };
            resolveConstantDependsOnLayout(s, expr_handle, inner_expr);
        },
        .stack_address_cast => |inner_expr| {
            const inner_type = expr_resolved_types[inner_expr];
            expr_resolved_types[expr_handle] = switch (inner_type) {
                .unknown => return false,
                .poison => .{ .poison = {} },
                .stack_address => inner_type,
                .raw_base_offset => |bo| .{ .stack_address = bo },
                .reg8, .reg16, .reg32, .constant, .sr => .{ .stack_address = ExpressionType.BaseOffset.init(inner_type, null) catch unreachable },
                .data_address, .insn_address => t: {
                    const token = s.expr.items(.token)[expr_handle];
                    a.recordError(s.file.handle, token, "Casting directly between address spaces is not allowed.  Use `.s .raw` if you really want this.", .{});
                    break :t .{ .poison = {} };
                },
                .symbol_def => unreachable,
            };
            resolveConstantDependsOnLayout(s, expr_handle, inner_expr);
        },
        .remove_address_cast => |inner_expr| {
            const inner_type = expr_resolved_types[inner_expr];
            expr_resolved_types[expr_handle] = switch (inner_type) {
                .unknown => return false,
                .poison => .{ .poison = {} },
                .raw_base_offset, .reg8, .reg16, .reg32, .constant, .symbol_def, .sr => inner_type,
                .data_address, .insn_address, .stack_address => |bo| t: {
                    if (bo.offset == .none) {
                        break :t bo.base.toExpressionType();
                    } else {
                        break :t .{ .raw_base_offset = bo };
                    }
                },
            };
            resolveConstantDependsOnLayout(s, expr_handle, inner_expr);
        },
    }
    return true;
}

fn resolveConstantDependsOnLayout(s: SourceFile.Slices, expr_handle: Expression.Handle, dependent_expr: Expression.Handle) void {
    var expr_flags = s.expr.items(.flags);
    if (expr_flags[dependent_expr].contains(.constant_depends_on_layout)) {
        expr_flags[expr_handle].insert(.constant_depends_on_layout);
    }
}

fn resolveConstantDependsOnLayoutBinary(s: SourceFile.Slices, expr_handle: Expression.Handle, bin: Expression.Binary) void {
    var expr_flags = s.expr.items(.flags);
    if (expr_flags[bin.left].contains(.constant_depends_on_layout) or expr_flags[bin.right].contains(.constant_depends_on_layout)) {
        expr_flags[expr_handle].insert(.constant_depends_on_layout);
    }
}

fn tryResolveSymbolType(a: *Assembler, s: SourceFile.Slices, expr_handle: Expression.Handle, token_handle: Token.Handle, symbol: []const u8) bool {
    var expr_resolved_types = s.expr.items(.resolved_type);
    var expr_flags = s.expr.items(.flags);

    switch (symbols.lookupSymbol(a, s, token_handle, symbol)) {
        .expression => |target_expr_handle| {
            const target_type = expr_resolved_types[target_expr_handle];
            if (target_type != .unknown) {
                expr_resolved_types[expr_handle] = target_type;
                if (expr_flags[target_expr_handle].contains(.constant_depends_on_layout)) {
                    expr_flags[expr_handle].insert(.constant_depends_on_layout);
                }
                return true;
            }
        },
        .instruction => |insn_ref| {
            const sym_file = a.getSource(insn_ref.file);
            const block_handle = sym_file.findBlockByInstruction(insn_ref.instruction);
            if (sym_file.blocks.items(.block_type)[block_handle]) |op| {
                expr_resolved_types[expr_handle] = switch (op) {
                    .none, .nil, .insn, .bound_insn, .org, .@"align", .keep,
                    .def, .undef, .local, .db, .dw, .dd, .push, .pop, .range,
                    => unreachable,

                    .section => ExpressionType.absoluteAddress(.data),
                    .boot, .code, .kcode, .entry, .kentry => ExpressionType.relativeAddress(.insn, .{ .sr = .IP }),
                    .data, .kdata, .@"const", .kconst => ExpressionType.relativeAddress(.data, .{ .sr = .IP }),

                    .stack => ExpressionType.relativeAddress(.stack, .{ .sr = .SP }), // maybe this should be unreachable?
                } catch unreachable;
            } else {
                // Block does not have a section handle, so it's an .info section by default
                expr_resolved_types[expr_handle] = ExpressionType.absoluteAddress(.data);
            }
            expr_flags[expr_handle].insert(.constant_depends_on_layout);
            return true;
        },
        .stack => {
            expr_resolved_types[expr_handle] = ExpressionType.relativeAddress(.stack, .{ .sr = .SP }) catch unreachable;
            expr_flags[expr_handle].insert(.constant_depends_on_layout);
        },
        .not_found => {
            a.recordError(s.file.handle, token_handle, "Reference to undefined symbol", .{});
            expr_resolved_types[expr_handle] = .{ .poison = {} };
            return true;
        },
    }

    return false;
}

pub fn checkInstructionsAndDirectives(a: *Assembler) void {
    for (a.files.items) |*file| {
        checkInstructionsAndDirectivesInFile(a, file.slices());
    }
}

fn checkInstructionsAndDirectivesInFile(a: *Assembler, s: SourceFile.Slices) void {
    var insn_operations = s.insn.items(.operation);
    var insn_params = s.insn.items(.params);
    var insn_labels = s.insn.items(.label);
    _ = insn_labels;

    var pushed_stacks = std.ArrayList([]const u8).init(a.gpa);
    defer pushed_stacks.deinit();

    for (s.block.items(.first_insn), s.block.items(.end_insn)) |first_insn, end_insn| {
        var allow_code = false;
        var allow_data = false;
        var allow_range = false;

        for (first_insn.., insn_operations[first_insn..end_insn], insn_params[first_insn..end_insn]) |insn_handle_usize, op, maybe_params| {
            const insn_handle = @intCast(Instruction.Handle, insn_handle_usize);
            switch (op) {
                .boot => {
                    allow_code = true;
                    allow_data = true;
                    allow_range = false;
                },
                .section => {
                    allow_code = true;
                    allow_data = true;
                    allow_range = true;
                },
                .code, .kcode, .entry, .kentry => {
                    allow_code = true;
                    allow_data = false;
                    allow_range = true;
                },
                .data, .kdata, .@"const", .kconst => {
                    allow_code = false;
                    allow_data = true;
                    allow_range = true;
                },
                .stack => {
                    allow_code = false;
                    allow_data = true;
                    allow_range = false;
                },

                .none, .nil => std.debug.assert(maybe_params == null),

                .org => checkOrgParams(a, s, insn_handle, maybe_params),

                .@"align" => checkAlignParams(a, s, insn_handle, maybe_params),

                .bound_insn => unreachable,
                .insn => {
                    if (!allow_code) {
                        const token = s.insn.items(.token)[insn_handle];
                        a.recordError(s.file.handle, token, "Instructions are not allowed in .data/.kdata/.const/.kconst/.stack sections", .{});
                    }
                    checkInstructionDependsOnLayout(s, insn_handle, maybe_params);
                },

                .db, .dw, .dd => {
                    if (!allow_data) {
                        const token = s.insn.items(.token)[insn_handle];
                        a.recordError(s.file.handle, token, "Data directives are not allowed in .entry/.kentry/.code/.kcode sections", .{});
                    }
                    if (maybe_params) |params_expr| {
                        checkDataDirectiveExprList(a, s, params_expr);
                    } else {
                        const token = s.insn.items(.token)[insn_handle];
                        a.recordError(s.file.handle, token, "Expected at least one data expression", .{});
                    }
                    checkInstructionDependsOnLayout(s, insn_handle, maybe_params);
                },

                .push => {
                    if (!allow_code) {
                        const token = s.insn.items(.token)[insn_handle];
                        a.recordError(s.file.handle, token, "Instructions are not allowed in .data/.kdata/.const/.kconst/.stack sections", .{});
                    }
                    checkPushedStacks(a, s, insn_handle, maybe_params, &pushed_stacks);
                },
                .pop => {
                    if (!allow_code) {
                        const token = s.insn.items(.token)[insn_handle];
                        a.recordError(s.file.handle, token, "Instructions are not allowed in .data/.kdata/.const/.kconst/.stack sections", .{});
                    }
                    checkPoppedStacks(a, s, insn_handle, maybe_params, &pushed_stacks);
                },

                .keep => if (maybe_params) |params_expr| {
                    const token = s.expr.items(.token)[params_expr];
                    a.recordError(s.file.handle, token, "Expected no parameters", .{});
                },

                .range => {
                    if (!allow_range) {
                        const token = s.insn.items(.token)[insn_handle];
                        a.recordError(s.file.handle, token, ".range directive is only allowed in non-boot sections", .{});
                    } else {
                        checkRangeParams(a, s, insn_handle, maybe_params);
                    }
                },

                .def, .local => {},
                .undef => if (maybe_params) |params_expr| {
                    checkUndefExprList(a, s, params_expr);
                } else {
                    const token = s.insn.items(.token)[insn_handle];
                    a.recordError(s.file.handle, token, "Expected at least one .def symbol name", .{});
                },
            }
        }

        pushed_stacks.clearRetainingCapacity();
    }
}

fn checkInstructionDependsOnLayout(s: SourceFile.Slices, insn_handle: Instruction.Handle, maybe_params: ?Expression.Handle) void {
    if (maybe_params) |params_handle| {
        if (instructionHasLayoutDependentParams(s, params_handle)) {
            s.insn.items(.flags)[insn_handle].insert(.depends_on_layout);
            return;
        }
    }
}

fn instructionHasLayoutDependentParams(s: SourceFile.Slices, params_handle: Expression.Handle) bool {
    if (s.expr.items(.flags)[params_handle].contains(.constant_depends_on_layout)) return true;

    switch (s.expr.items(.info)[params_handle]) {
        .list, .arrow_list => |bin| {
            return instructionHasLayoutDependentParams(s, bin.left)
                or instructionHasLayoutDependentParams(s, bin.right);
        },
        else => {},
    }
    return false;
}

fn checkPushedStacks(a: *Assembler, s: SourceFile.Slices, insn_handle: Instruction.Handle, maybe_params: ?Expression.Handle, pushed_stacks: *std.ArrayList([]const u8)) void {
    const expr_infos = s.expr.items(.info);

    var depends_on_layout = false;

    var maybe_expr_handle = maybe_params;
    while (maybe_expr_handle) |expr_handle| {
        switch (expr_infos[expr_handle]) {
            .list => |bin| {
                depends_on_layout = checkPushedStack(a, s, bin.left, pushed_stacks) or depends_on_layout;
                maybe_expr_handle = bin.right;
            },
            else => {
                depends_on_layout = checkPushedStack(a, s, expr_handle, pushed_stacks) or depends_on_layout;
                break;
            }
        }
    } else {
        const token = s.insn.items(.token)[insn_handle];
        a.recordError(s.file.handle, token, "Expected at least one stack block name", .{});
    }

    if (depends_on_layout) {
        s.insn.items(.flags)[insn_handle].insert(.depends_on_layout);
    }
}
fn checkPushedStack(a: *Assembler, s: SourceFile.Slices, expr_handle: Expression.Handle, pushed_stacks: *std.ArrayList([]const u8)) bool {
    const insn_flags = s.insn.items(.flags);

    var depends_on_layout = false;

    const stack_block_name = symbols.parseSymbol(a, s, expr_handle).asString();
    for (pushed_stacks.items) |name| {
        if (std.mem.eql(u8, name, stack_block_name)) {
            const token = s.expr.items(.token)[expr_handle];
            a.recordError(s.file.handle, token, "This stack block has already been pushed", .{});
            return false;
        }
    }
    pushed_stacks.append(stack_block_name) catch @panic("OOM");

    if (s.file.stacks.get(stack_block_name)) |stack_block_handle| {
        var iter = s.blockInstructions(stack_block_handle);
        while (iter.next()) |insn_handle| {
            depends_on_layout = depends_on_layout or insn_flags[insn_handle].contains(.depends_on_layout);
        }
    } else {
        const token = s.expr.items(.token)[expr_handle];
        a.recordError(s.file.handle, token, "No .stack block found with this name", .{});
    }

    return depends_on_layout;
}

fn checkPoppedStacks(a: *Assembler, s: SourceFile.Slices, insn_handle: Instruction.Handle, maybe_params: ?Expression.Handle, pushed_stacks: *std.ArrayList([]const u8)) void {
    const expr_infos = s.expr.items(.info);

    var num_blocks_popped: usize = 0;
    var maybe_expr_handle = maybe_params;
    while (maybe_expr_handle) |expr_handle| switch (expr_infos[expr_handle]) {
        .list => |bin| {
            num_blocks_popped += 1;
            maybe_expr_handle = bin.right;
        },
        else => {
            num_blocks_popped += 1;
            maybe_expr_handle = null;
        }
    };

    if (num_blocks_popped == 0) {
        const token = s.insn.items(.token)[insn_handle];
        a.recordError(s.file.handle, token, "Expected at least one stack block name", .{});
    }

    var depends_on_layout = false;

    maybe_expr_handle = maybe_params;
    while (maybe_expr_handle) |expr_handle| switch (expr_infos[expr_handle]) {
        .list => |bin| {
            depends_on_layout = checkPoppedStack(a, s, bin.left, pushed_stacks, &num_blocks_popped) or depends_on_layout;
            maybe_expr_handle = bin.right;
        },
        else => {
            depends_on_layout = checkPoppedStack(a, s, expr_handle, pushed_stacks, &num_blocks_popped) or depends_on_layout;
            maybe_expr_handle = null;
        }
    };

    if (depends_on_layout) {
        s.insn.items(.flags)[insn_handle].insert(.depends_on_layout);
    }
}
fn checkPoppedStack(a: *Assembler, s: SourceFile.Slices, expr_handle: Expression.Handle, pushed_stacks: *std.ArrayList([]const u8), num_blocks_to_pop: *usize) bool {
    const insn_flags = s.insn.items(.flags);

    var depends_on_layout = false;

    const stack_block_name = symbols.parseSymbol(a, s, expr_handle).asString();
    for (0.., pushed_stacks.items) |i, name| {
        if (std.mem.eql(u8, name, stack_block_name)) {
            if (i < pushed_stacks.items.len - num_blocks_to_pop.*) {
                const token = s.expr.items(.token)[expr_handle];
                a.recordError(s.file.handle, token, "Stack blocks must be popped in the reverse order they were pushed!", .{});
            } else {
                num_blocks_to_pop.* -= 1;
            }
            _ = pushed_stacks.orderedRemove(i);
            break;
        }
    } else {
        const token = s.expr.items(.token)[expr_handle];
        a.recordError(s.file.handle, token, "This stack block has already been popped or was never pushed!", .{});
        return false;
    }

    if (s.file.stacks.get(stack_block_name)) |stack_block_handle| {
        var iter = s.blockInstructions(stack_block_handle);
        while (iter.next()) |insn_handle| {
            depends_on_layout = depends_on_layout or insn_flags[insn_handle].contains(.depends_on_layout);
        }
    } else {
        const token = s.expr.items(.token)[expr_handle];
        a.recordError(s.file.handle, token, "No .stack block found with this name", .{});
    }

    return depends_on_layout;
}

fn checkUndefExprList(a: *Assembler, s: SourceFile.Slices, expr_handle: Expression.Handle) void {
    const infos = s.expr.items(.info);
    var expr = expr_handle;
    while (infos[expr] == .list) {
        const bin = infos[expr].list;
        _ = checkUndefExpr(a, s, bin.left);
        expr = bin.right;
    }
    _ = checkUndefExpr(a, s, expr);
}

fn checkUndefExpr(a: *Assembler, s: SourceFile.Slices, expr_handle: Expression.Handle) void {
    const symbol_name = symbols.parseSymbol(a, s, expr_handle);
    const token = s.expr.items(.token)[expr_handle];
    switch (symbols.lookupSymbol(a, s, token, symbol_name.asString())) {
        .expression => {},
        .not_found => {
            a.recordError(s.file.handle, token, "Symbol must be defined before it can be un-defined", .{});
        },
        .instruction => {
            a.recordError(s.file.handle, token, "Labels cannot be un-defined", .{});
        },
        .stack => {
            a.recordError(s.file.handle, token, "Stack labels cannot be un-defined", .{});
        },
    }
}

fn checkDataDirectiveExprList(a: *Assembler, s: SourceFile.Slices, expr_handle: Expression.Handle) void {
    const infos = s.expr.items(.info);
    var expr = expr_handle;
    while (true) {
        switch (infos[expr]) {
            .list => |bin| {
                checkDataDirectiveExpr(a, s, bin.left);
                expr = bin.right;
            },
            .arrow_list => |bin| {
                checkDataDirectiveExpr(a, s, bin.left);
                const token = s.expr.items(.token)[expr];
                a.recordError(s.file.handle, token, "Expected ','", .{});
                expr = bin.right;
            },
            else => break,
        }
    }
    checkDataDirectiveExpr(a, s, expr);
}

fn checkDataDirectiveExpr(a: *Assembler, s: SourceFile.Slices, expr_handle: Expression.Handle) void {
    switch (s.expr.items(.resolved_type)[expr_handle]) {
        .unknown, .poison, .constant => {},
        .symbol_def => unreachable,
        .raw_base_offset, .data_address, .insn_address, .stack_address,
        .reg8, .reg16, .reg32, .sr => {
            const token = s.expr.items(.token)[expr_handle];
            a.recordError(s.file.handle, token, "Expected constant expression", .{});
        },
    }
}

fn checkOrgParams(a: *Assembler, s: SourceFile.Slices, insn_handle: Instruction.Handle, maybe_params: ?Expression.Handle) void {
    const expr_resolved_types = s.expr.items(.resolved_type);

    var params = [_]?Expression.Handle{null};
    getParamHandles(a, s, maybe_params, &params);

    if (params[0]) |address_expr| {
        switch (expr_resolved_types[address_expr]) {
            .unknown, .symbol_def => unreachable,
            .poison, .constant => {},
            .reg8, .reg16, .reg32, .sr => {
                const token = s.expr.items(.token)[address_expr];
                a.recordError(s.file.handle, token, "Expected constant or absolute address, not register", .{});
            },
            .raw_base_offset, .data_address, .insn_address, .stack_address => |bo| {
                if (std.meta.eql(bo.base, .{ .sr = .IP }) and bo.offset == .constant) {
                    const token = s.expr.items(.token)[address_expr];
                    a.recordError(s.file.handle, token, "Expected absolute address, not relative; try using '@'", .{});
                } else if (bo.base != .constant or bo.offset != .none) {
                    const token = s.expr.items(.token)[address_expr];
                    a.recordError(s.file.handle, token, "Expected constant or absolute address", .{});
                }
            },
        }
    } else {
        const token = s.insn.items(.token)[insn_handle];
        a.recordError(s.file.handle, token, ".org directive must be followed by address expression", .{});
    }
}

fn checkAlignParams(a: *Assembler, s: SourceFile.Slices, insn_handle: Instruction.Handle, maybe_params: ?Expression.Handle) void {
    const expr_resolved_types = s.expr.items(.resolved_type);

    var params = [_]?Expression.Handle{null} ** 2;
    getParamHandles(a, s, maybe_params, &params);

    if (params[0]) |align_expr| {
        switch (expr_resolved_types[align_expr]) {
            .unknown, .symbol_def => unreachable,
            .poison, .constant => {},
            .reg8, .reg16, .reg32, .sr,
            .raw_base_offset, .data_address, .insn_address, .stack_address => {
                const token = s.expr.items(.token)[align_expr];
                a.recordError(s.file.handle, token, "Expected constant", .{});
            },
        }
    } else {
        const token = s.insn.items(.token)[insn_handle];
        a.recordError(s.file.handle, token, ".align directive must be followed by constant expression", .{});
    }

    if (params[1]) |offset_expr| {
        switch (expr_resolved_types[offset_expr]) {
            .unknown, .symbol_def => unreachable,
            .poison, .constant => {},
            .reg8, .reg16, .reg32, .sr,
            .raw_base_offset, .data_address, .insn_address, .stack_address => {
                const token = s.expr.items(.token)[offset_expr];
                a.recordError(s.file.handle, token, "Expected constant", .{});
            },
        }
    }
}

fn checkRangeParams(a: *Assembler, s: SourceFile.Slices, insn_handle: Instruction.Handle, maybe_params: ?Expression.Handle) void {
    const expr_flags = s.expr.items(.flags);
    const expr_resolved_types = s.expr.items(.resolved_type);

    var params = [_]?Expression.Handle{null} ** 2;
    getParamHandles(a, s, maybe_params, &params);

    var range = Assembler.AddressRange{
        .first = 0,
        .len = 0,
    };

    if (params[0]) |expr| {
        switch (expr_resolved_types[expr]) {
            .unknown, .symbol_def => unreachable,
            .poison, .constant => {},
            .raw_base_offset, .data_address, .insn_address, .stack_address,
            .reg8, .reg16, .reg32, .sr => {
                const token = s.expr.items(.token)[expr];
                a.recordError(s.file.handle, token, "Expected minimum address constant", .{});
            },
        }
        if (expr_flags[expr].contains(.constant_depends_on_layout)) {
            const token = s.expr.items(.token)[expr];
            a.recordError(s.file.handle, token, "Expression cannot depend on layout", .{});
        } else {
            const constant = layout.resolveExpressionConstantOrDefault(a, s, 0, expr, 0x1000);
            range.first = constant.asInt(u32) catch a: {
                const token = s.expr.items(.token)[expr];
                a.recordError(s.file.handle, token, "Expression must fit in u32", .{});
                break :a 0x1000;
            };
            if (@truncate(bus.PageOffset, range.first) != 0) {
                const token = s.expr.items(.token)[expr];
                a.recordError(s.file.handle, token, "Expected an address with a page offset of 0", .{});
            }
        }
    }

    if (params[1]) |expr| {
        switch (expr_resolved_types[expr]) {
            .unknown, .symbol_def => unreachable,
            .poison, .constant => {},
            .raw_base_offset, .data_address, .insn_address, .stack_address,
            .reg8, .reg16, .reg32, .sr => {
                const token = s.expr.items(.token)[expr];
                a.recordError(s.file.handle, token, "Expected maximum address constant", .{});
            },
        }
        if (expr_flags[expr].contains(.constant_depends_on_layout)) {
            const token = s.expr.items(.token)[expr];
            a.recordError(s.file.handle, token, "Expression cannot depend on layout", .{});
        } else {
            const constant = layout.resolveExpressionConstantOrDefault(a, s, 0, expr, 0xFFFF_FFFF);
            const last = constant.asInt(u32) catch a: {
                const token = s.expr.items(.token)[expr];
                a.recordError(s.file.handle, token, "Expression must fit in u32", .{});
                break :a 0xFFFF_FFFF;
            };
            if (@truncate(bus.PageOffset, last) != ~@as(bus.PageOffset, 0)) {
                const token = s.expr.items(.token)[expr];
                a.recordError(s.file.handle, token, "Expected an address with a page offset of 0xFFF", .{});
            }
            range.len = @as(usize, last - range.first) + 1;

            const block_handle = s.file.findBlockByInstruction(insn_handle);
            const section_handle = s.file.blocks.items(.section)[block_handle].?;
            const section = a.getSectionPtr(section_handle);
            if (section.range) |_| {
                const token = s.insn.items(.token)[insn_handle];
                a.recordError(s.file.handle, token, "Multiple .range directives found for this section; ignoring this one", .{});
            } else {
                section.range = range;
            }
        }
    } else {
        const token = s.insn.items(.token)[insn_handle];
        a.recordError(s.file.handle, token, ".range directive must be followed by <min_address>, <max_address> constant expressions", .{});
    }
}

fn getParamHandles(a: *Assembler, s: SourceFile.Slices, maybe_params_expr: ?Expression.Handle, out: []?Expression.Handle) void {
    if (out.len == 0) return;
    if (maybe_params_expr) |params_expr| {
        var expr_handle = params_expr;

        const expr_infos = s.expr.items(.info);
        for (out) |*p| {
            switch (expr_infos[expr_handle]) {
                .list => |bin| {
                    p.* = bin.left;
                    expr_handle = bin.right;
                },
                .arrow_list => {
                    const token = s.expr.items(.token)[expr_handle];
                    a.recordError(s.file.handle, token, "Expected ',' as parameter separator", .{});
                    return;
                },
                else => {
                    p.* = expr_handle;
                    return;
                }
            }
        }
        const token = s.expr.items(.token)[expr_handle];
        a.recordError(s.file.handle, token, "Too many parameters", .{});
    }
}
