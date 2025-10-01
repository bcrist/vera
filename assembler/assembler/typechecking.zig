// Set the resolved type for labels to .symbol_def.
// The resolved constant for these expressions is the raw symbol name.
// Any labels which do not begin with `_` and are not in a stack section will be added to the global symbol table.
// Create Sections for each named section block and set the Block.section handle to reference it.
pub fn process_labels_and_sections(a: *Assembler, file: *Source_File) void {
    const s = file.slices();
    const insn_operations = s.insn.items(.operation);
    const insn_labels = s.insn.items(.label);
    const insn_params = s.insn.items(.params);
    const expr_infos = s.expr.items(.info);
    const block_labels = s.block.items(.labels);

    for (0.., s.block.items(.first_insn), s.block.items(.end_insn)) |block_handle_usize, first_insn, end_insn| {
        const block_handle: Source_File.Block.Handle = @intCast(block_handle_usize);
        const private_labels = &block_labels[block_handle];

        var is_stack_block = false;

        for (first_insn.., insn_operations[first_insn..end_insn]) |insn_handle_usize, op| {
            const insn_handle: Instruction.Handle = @intCast(insn_handle_usize);
            switch (op) {
                .section, .boot, .code, .kcode, .entry, .kentry,
                .data, .kdata, .@"const", .kconst, .stack => {
                    s.block.items(.block_type)[block_handle] = op;

                    var maybe_section_name: ?[]const u8 = null;
                    if (insn_params[insn_handle]) |section_name_expr| {
                        const symbol_constant = resolve_symbol_def_expr(a, s, section_name_expr);
                        maybe_section_name = symbol_constant.as_string();
                    }

                    if (op == .stack) {
                        is_stack_block = true;
                        process_stack_block(a, s, block_handle, insn_handle, maybe_section_name);
                    } else {
                        process_section_block(a, s, block_handle, insn_handle, Section.Kind.from_directive(op), maybe_section_name);
                    }
                },
                .def => {
                    const params_expr = insn_params[insn_handle].?;
                    const symbol_expr = s.expr.items(.info)[params_expr].list.left;
                    _ = resolve_symbol_def_expr(a, s, symbol_expr);
                },
                .undef, .push, .pop => {
                    if (insn_params[insn_handle]) |params| {
                        _ = resolve_symbol_def_expr_list(a, s, params);
                    }
                },
                .local => {
                    const params_expr = insn_params[insn_handle].?;
                    const bin = expr_infos[params_expr].list;
                    const symbol_name = resolve_symbol_def_expr(a, s, bin.left).as_string();
                    const result = s.file.locals.getOrPut(a.gpa, symbol_name) catch @panic("OOM");
                    if (result.found_existing) {
                        const target_insn_handle = result.value_ptr.instruction_handle(s);
                        const line_number = s.insn.items(.line_number)[target_insn_handle];
                        a.record_expr_error_fmt(s.file.handle, params_expr, "Duplicate .local name (canonical symbol is at line {})", .{ line_number }, .{});
                    } else {
                        result.value_ptr.* = .{ .expression = bin.right };
                    }
                },
                .none, .nil, .org, .@"align", .keep, .insn, .bound_insn, .db, .dh, .dw, .zb, .zh, .zw, .range => {},
            }
        }

        for (first_insn.., insn_labels[first_insn..end_insn]) |insn_handle_usize, maybe_label_expr| {
            const insn_handle: Instruction.Handle = @intCast(insn_handle_usize);

            if (maybe_label_expr) |label_expr| switch (expr_infos[label_expr]) {
                .local_label_def => |inner_label_expr| {
                    const symbol_constant = resolve_symbol_def_expr(a, s, inner_label_expr);
                    const symbol_name = symbol_constant.as_string();
                    const result = s.file.locals.getOrPut(a.gpa, symbol_name) catch @panic("OOM");
                    if (result.found_existing) {
                        const target_insn_handle = result.value_ptr.instruction_handle(s);
                        const line_number = s.insn.items(.line_number)[target_insn_handle];
                        a.record_expr_error_fmt(s.file.handle, label_expr, "Duplicate .local name (canonical symbol is at line {})", .{ line_number }, .{});
                    } else {
                        result.value_ptr.* = .{ .instruction = .{
                            .file = file.handle,
                            .instruction = insn_handle,
                        }};
                    }
                },
                else => {
                    const symbol_constant = resolve_symbol_def_expr(a, s, label_expr);
                    const symbol_name = symbol_constant.as_string();
                    if (is_stack_block or std.mem.startsWith(u8, symbol_name, "_")) {
                        const result = private_labels.getOrPut(a.gpa, symbol_name) catch @panic("OOM");
                        if (result.found_existing) {
                            const target_line_number = s.insn.items(.line_number)[result.value_ptr.*];
                            a.record_expr_error_fmt(s.file.handle, label_expr, "Duplicate private label (canonical label is at line {})", .{ target_line_number }, .{});
                        } else {
                            result.value_ptr.* = insn_handle;
                        }
                    } else {
                        const result = a.public_labels.getOrPut(a.gpa, symbol_name) catch @panic("OOM");
                        if (result.found_existing) {
                            const target_file = a.get_source(result.value_ptr.file);
                            const target_line_number = target_file.instructions.items(.line_number)[result.value_ptr.instruction];
                            a.record_expr_error_fmt(s.file.handle, label_expr, "Duplicate label (canonical label is at line {} in {s})", .{ target_line_number, target_file.name }, .{});
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

fn process_stack_block(a: *Assembler, s: Source_File.Slices, block_handle: Source_File.Block.Handle, insn_handle: Instruction.Handle, maybe_stack_name: ?[]const u8) void {
    const name = maybe_stack_name orelse "";
    const result = s.file.stacks.getOrPut(a.gpa, name) catch @panic("OOM");
    if (result.found_existing) {
        const canonical_insn_handle = s.block.items(.first_insn)[result.value_ptr.*];
        const canonical_line_number = s.insn.items(.line_number)[canonical_insn_handle];
        a.record_insn_error_fmt(s.file.handle, insn_handle, "Ignoring duplicate .stack block; canonical block is at line {}", .{ canonical_line_number }, .{});
    } else {
        result.value_ptr.* = block_handle;
    }
}

fn process_section_block(a: *Assembler, s: Source_File.Slices, block_handle: Source_File.Block.Handle, insn_handle: Instruction.Handle, kind: Section.Kind, maybe_section_name: ?[]const u8) void {
    const section_name = maybe_section_name orelse kind.default_name();

    var range: ?Assembler.Address_Range = null;
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
        a.record_insn_error_fmt(s.file.handle, insn_handle, "Section already exists; expected .{s}", .{ @tagName(found_kind) }, .{});
    }

    s.block.items(.section)[block_handle] = @intCast(entry.index);
}

fn resolve_symbol_def_expr_list(a: *Assembler, s: Source_File.Slices, symbol_def_expr: Expression.Handle) void {
    const infos = s.expr.items(.info);
    var expr = symbol_def_expr;
    while (infos[expr] == .list) {
        const bin = infos[expr].list;
        _ = resolve_symbol_def_expr(a, s, bin.left);
        expr = bin.right;
    }
    _ = resolve_symbol_def_expr(a, s, expr);
}

fn resolve_symbol_def_expr(a: *Assembler, s: Source_File.Slices, symbol_def_expr: Expression.Handle) *const Constant {
    const symbol_name = symbols.parse_symbol(a, s, symbol_def_expr);
    s.expr.items(.resolved_constant)[symbol_def_expr] = symbol_name;
    s.expr.items(.resolved_type)[symbol_def_expr] = .symbol_def;
    return symbol_name;
}

pub fn resolve_expression_types(a: *Assembler) bool {
    var made_progress = true;
    var unresolved = true;
    while (made_progress) {
        made_progress = false;
        unresolved = false;

        for (a.files.items) |*file| {
            var slices = file.slices();
            for (slices.expr.items(.resolved_type), 0..) |expr_type, expr_handle| {
                if (expr_type == .unknown) {
                    if (try_resolve_expr_type(a, slices, @intCast(expr_handle))) {
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
                    a.record_expr_error(file.handle, @intCast(expr_handle), "Could not determine type for expression", .{});
                    resolved_types[expr_handle] = .poison;
                }
            }
        }
        return false;
    }

    return true;
}

pub fn try_resolve_expr_type(a: *Assembler, s: Source_File.Slices, expr_handle: Expression.Handle) bool {
    const expr_infos = s.expr.items(.info);
    var expr_resolved_types = s.expr.items(.resolved_type);

    std.debug.assert(expr_resolved_types[expr_handle] == .unknown);

    const info = expr_infos[expr_handle];
    switch (info) {
        .list => expr_resolved_types[expr_handle] = .poison,
        .literal_int => {
            const token_handle = s.expr.items(.token)[expr_handle];
            const token = s.file.tokens.get(token_handle);
            if (Constant.init_int_literal(a.gpa, &a.constant_temp, token.span(s.file.source))) |constant| {
                expr_resolved_types[expr_handle] = Expression.Type.constant();
                s.expr.items(.resolved_constant)[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
            } else |err| {
                switch (err) {
                    error.Overflow         => a.record_expr_error(s.file.handle, expr_handle, "Integer literal too large", .{}),
                    error.InvalidCharacter => a.record_expr_error(s.file.handle, expr_handle, "Invalid character in integer literal", .{}),
                }
                expr_resolved_types[expr_handle] = .poison;
            }
        },
        .literal_str => {
            var token_handle = s.expr.items(.token)[expr_handle];
            var token = s.file.tokens.get(token_handle);
            if (token.kind == .str_literal_raw) {
                const token_kinds = s.file.tokens.items(.kind);
                a.constant_temp.clearRetainingCapacity();
                var raw = token.span(s.file.source);
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
                            raw = token.span(s.file.source);
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
                const constant = Constant.init_string(a.constant_temp.items);
                expr_resolved_types[expr_handle] = Expression.Type.constant();
                s.expr.items(.resolved_constant)[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
            } else if (Constant.init_string_literal(a.gpa, &a.constant_temp, token.span(s.file.source))) |constant| {
                expr_resolved_types[expr_handle] = Expression.Type.constant();
                s.expr.items(.resolved_constant)[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
            } else |err| {
                switch (err) {
                    error.Overflow         => a.record_expr_error(s.file.handle, expr_handle, "Invalid decimal byte in string literal escape sequence", .{}),
                    error.InvalidCharacter => a.record_expr_error(s.file.handle, expr_handle, "Invalid character in string literal", .{}),
                    error.InvalidCodepoint => a.record_expr_error(s.file.handle, expr_handle, "Invalid codepoint in string literal escape sequence", .{}),
                    error.InvalidBase64    => a.record_expr_error(s.file.handle, expr_handle, "Invalid base64 in string literal escape sequence", .{}),
                    error.UnclosedLiteral  => a.record_expr_error(s.file.handle, expr_handle, "String literal is missing closing '\"' character", .{}),
                    error.IncompleteEscape => a.record_expr_error(s.file.handle, expr_handle, "String literal contains an incomplete escape sequence", .{}),
                }
                expr_resolved_types[expr_handle] = .poison;
            }
        },
        .literal_reg => {
            // SFR types are preassigned when parsed, so we can assume we're dealing with a GPR name.
            const token_handle = s.expr.items(.token)[expr_handle];
            const token = s.file.tokens.get(token_handle);
            const name = token.span(s.file.source);
            const offset = std.fmt.parseUnsigned(arch.bus.K.Read_Index_Offset.Raw, name[1..], 10) catch unreachable;
            const reg_type: Expression.Type = switch (name[0]) {
                'r', 'R' => Expression.Type.reg(.init(offset), null),
                else => unreachable,
            };
            expr_resolved_types[expr_handle] = reg_type;
        },
        .literal_current_address => {
            const token_handle = s.expr.items(.token)[expr_handle];
            const block_handle = s.file.find_block_by_token(token_handle);
            if (s.block.items(.block_type)[block_handle]) |op| {
                expr_resolved_types[expr_handle] = switch (op) {
                    .none, .nil, .insn, .bound_insn, .org, .@"align", .keep, .range,
                    .def, .undef, .local, .db, .dh, .dw, .zb, .zh, .zw, .push, .pop,
                    => unreachable,

                    .section => Expression.Type.absolute_address(.data),
                    .boot, .code, .kcode, .entry, .kentry => Expression.Type.relative_address(.insn, Expression.Type.sr(.ip)),
                    .data, .kdata, .@"const", .kconst => Expression.Type.relative_address(.data, Expression.Type.sr(.ip)),
                    .stack => Expression.Type.relative_address(.stack, Expression.Type.sr(.sp)),
                } catch unreachable;
            } else {
                // Block does not have a section handle, so it's an .info section by default
                expr_resolved_types[expr_handle] = Expression.Type.absolute_address(.data);
            }
            s.expr.items(.flags)[expr_handle].insert(.constant_depends_on_layout);
            return true;
        },
        .literal_symbol_ref => {
            const token_handle = s.expr.items(.token)[expr_handle];
            const symbol_literal = s.file.tokens.get(token_handle).span(s.file.source);
            const symbol_constant = Constant.init_symbol_literal(a.gpa, &a.constant_temp, symbol_literal);
            return try_resolve_symbol_type(a, s, expr_handle, token_handle, symbol_constant.as_string());
        },
        .directive_symbol_ref => |inner_expr| {
            if (s.expr.items(.resolved_constant)[inner_expr]) |symbol| {
                return try_resolve_symbol_type(a, s, expr_handle, s.expr.items(.token)[expr_handle], symbol.as_string());
            } else return false;
        },
        .literal_symbol_def, .directive_symbol_def => {
            _ = resolve_symbol_def_expr(a, s, expr_handle);
        },
        .local_label_def => |inner_expr_handle| {
            const symbol_name = resolve_symbol_def_expr(a, s, inner_expr_handle);
            s.expr.items(.resolved_constant)[expr_handle] = symbol_name;
            expr_resolved_types[expr_handle] = .symbol_def;
        },

        // unary operators:
        .negate, .complement, .crlf_cast, .lf_cast,
        .index_to_reg, .reg_to_index,
        .signed_cast, .unsigned_cast, .remove_signedness_cast,
        .absolute_address_cast, .data_address_cast, .insn_address_cast, .stack_address_cast, .remove_address_cast,
        => |inner_expr| {
            const inner_type = expr_resolved_types[inner_expr];
            if (inner_type == .unknown) return false;
            if (inner_type == .poison) {
                expr_resolved_types[expr_handle] = .poison;
                return true;
            }
            defer resolve_constant_depends_on_layout(s, expr_handle, inner_expr);
            switch (info) {
                .negate, .complement, .crlf_cast, .lf_cast => {
                    if (inner_type.is_constant()) {
                        expr_resolved_types[expr_handle] = Expression.Type.constant();
                        return true;
                    }

                    a.record_expr_error(s.file.handle, expr_handle, "Operand must be a constant expression", .{});
                    expr_resolved_types[expr_handle] = .poison;
                    return true;
                },

                .index_to_reg => {
                    if (s.expr.items(.flags)[inner_expr].contains(.constant_depends_on_layout)) {
                        a.record_expr_error(s.file.handle, expr_handle, "Operand cannot vary on memory layout", .{});
                        expr_resolved_types[expr_handle] = .poison;
                        return true;
                    }

                    if (inner_type.is_constant()) {
                        const constant = layout.resolve_expression_constant(a, s, 0, inner_expr) orelse {
                            expr_resolved_types[expr_handle] = .poison;
                            return true;
                        };
                        const offset = constant.as_int(arch.bus.K.Read_Index_Offset.Raw) catch {
                            a.record_expr_error(s.file.handle, expr_handle, "Operand out of range", .{});
                            expr_resolved_types[expr_handle] = .poison;
                            return true;
                        };

                        expr_resolved_types[expr_handle] = Expression.Type.reg(.init(offset), null);
                        return true;
                    }

                    a.record_expr_error(s.file.handle, expr_handle, "Operand must be a constant expression", .{});
                    expr_resolved_types[expr_handle] =  .poison;
                    return true;
                },

                .reg_to_index => {
                    if (inner_type.simple_base()) |base| {
                        if (base.register_offset()) |offset| {
                            const constant = Constant.init_int(offset.raw());
                            s.expr.items(.resolved_constant)[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
                            expr_resolved_types[expr_handle] = Expression.Type.constant();
                            return true;
                        }
                    }

                    a.record_expr_error(s.file.handle, expr_handle, "Operand must be a GPR expression", .{});
                    expr_resolved_types[expr_handle] = .poison;
                    return true;
                },

                .signed_cast, .unsigned_cast, .remove_signedness_cast => {
                    if (inner_type.is_constant()) {
                        if (info == .remove_signedness_cast) {
                            a.record_expr_error(s.file.handle, expr_handle, "Operand must be a GPR expression", .{});
                            expr_resolved_types[expr_handle] = .poison;
                        } else {
                            expr_resolved_types[expr_handle] = Expression.Type.constant();
                        }
                        return true;
                    } else if (inner_type.simple_base()) |base| {
                        switch (base) {
                            .reg => |reg| {
                                const signedness: ?std.builtin.Signedness = switch (info) {
                                    .signed_cast => .signed,
                                    .unsigned_cast => .unsigned,
                                    .remove_signedness_cast => null,
                                    else => unreachable,
                                };

                                expr_resolved_types[expr_handle] = Expression.Type.reg(reg.offset, signedness);
                                return true;
                            },
                            else => {},
                        }
                    }
                    a.record_expr_error(s.file.handle, expr_handle, "Operand must be a constant or GPR expression", .{});
                    expr_resolved_types[expr_handle] = .poison;
                    return true;
                },

                .absolute_address_cast => {
                    if (inner_type.base_offset_type()) |bot| {
                        if (bot.base == .sr and bot.base.sr == .ip) {
                            var builder: Expression.Type_Builder = .{};
                            builder.add(inner_type);
                            builder.subtract(Expression.Type.sr(.ip));
                            expr_resolved_types[expr_handle] = builder.build() catch unreachable;
                            return true;
                        }
                    }
                    a.record_expr_error(s.file.handle, expr_handle, "Operand must be an IP-relative expression", .{});
                    expr_resolved_types[expr_handle] = .poison;
                    return true;
                },

                .data_address_cast, .insn_address_cast, .stack_address_cast => {
                    if (inner_type.base_offset_type()) |bot| {
                        const new_space: isa.Address_Space = switch (info) {
                            .data_address_cast => .data,
                            .insn_address_cast => .insn,
                            .stack_address_cast => .stack,
                            else => unreachable,
                        };
                        if (inner_type.address_space()) |current_space| {
                            if (current_space != new_space) {
                                a.record_expr_error(s.file.handle, expr_handle, "Casting directly between address spaces is not allowed.  Use `.raw` first if you really want this.", .{});
                                expr_resolved_types[expr_handle] = .poison;
                                return true;
                            }
                        }
                        expr_resolved_types[expr_handle] = switch (new_space) {
                            inline else => |space| Expression.Type.address_space_cast(space, bot)
                        };
                        return true;
                    }
                    a.record_expr_error(s.file.handle, expr_handle, "Invalid operand for address space cast", .{});
                    expr_resolved_types[expr_handle] = .poison;
                    return true;
                },

                .remove_address_cast => {
                    if (inner_type.base_offset_type()) |bot| {
                        expr_resolved_types[expr_handle] = .{ .raw = bot };
                        return true;
                    }
                    a.record_expr_error(s.file.handle, expr_handle, "Invalid operand for address space cast", .{});
                    expr_resolved_types[expr_handle] = .poison;
                    return true;
                },

                else => unreachable,
            }
            unreachable; // no fallthrough to avoid bugs
        },

        // binary operators:
        .plus => |bin| {
            var builder: Expression.Type_Builder = .{};
            builder.add(expr_resolved_types[bin.left]);
            builder.add(expr_resolved_types[bin.right]);
            const result_type = builder.build() catch t: {
                a.record_expr_error(s.file.handle, expr_handle, "Cannot represent result of symbolic addition", .{});
                break :t .poison;
            };
            if (result_type == .unknown) return false;
            expr_resolved_types[expr_handle] = result_type;
            resolve_constant_depends_on_layout_binary(s, expr_handle, bin);
        },
        .minus => |bin| {
            var builder: Expression.Type_Builder = .{};
            builder.add(expr_resolved_types[bin.left]);
            builder.subtract(expr_resolved_types[bin.right]);
            const result_type = builder.build() catch t: {
                a.record_expr_error(s.file.handle, expr_handle, "Cannot represent result of symbolic subtraction", .{});
                break :t .poison;
            };
            if (result_type == .unknown) return false;
            expr_resolved_types[expr_handle] = result_type;
            resolve_constant_depends_on_layout_binary(s, expr_handle, bin);
        },
        .multiply, .shl, .shr, .concat, .concat_repeat, .bitwise_or, .bitwise_xor, .bitwise_and,
        .length_cast, .truncate, .sign_extend, .zero_extend => |bin| {
            const left = expr_resolved_types[bin.left];
            const right = expr_resolved_types[bin.right];
            if (left == .poison or right == .poison) {
                expr_resolved_types[expr_handle] = .poison;
                return true;
            }
            if (left == .unknown or right == .unknown) return false;
            if (left.is_constant() and right.is_constant()) {
                expr_resolved_types[expr_handle] = Expression.Type.constant();
            } else {
                a.record_expr_error(s.file.handle, expr_handle, "Both operands must be constant expressions", .{});
                expr_resolved_types[expr_handle] = .poison;
            }
            resolve_constant_depends_on_layout_binary(s, expr_handle, bin);
        },
    }
    return true;
}

fn resolve_constant_depends_on_layout(s: Source_File.Slices, expr_handle: Expression.Handle, dependent_expr: Expression.Handle) void {
    var expr_flags = s.expr.items(.flags);
    if (expr_flags[dependent_expr].contains(.constant_depends_on_layout)) {
        expr_flags[expr_handle].insert(.constant_depends_on_layout);
    }
}

fn resolve_constant_depends_on_layout_binary(s: Source_File.Slices, expr_handle: Expression.Handle, bin: Expression.Binary) void {
    var expr_flags = s.expr.items(.flags);
    if (expr_flags[bin.left].contains(.constant_depends_on_layout) or expr_flags[bin.right].contains(.constant_depends_on_layout)) {
        expr_flags[expr_handle].insert(.constant_depends_on_layout);
    }
}

fn try_resolve_symbol_type(a: *Assembler, s: Source_File.Slices, expr_handle: Expression.Handle, token_handle: isa.lex.Token.Handle, symbol: []const u8) bool {
    var expr_resolved_types = s.expr.items(.resolved_type);
    var expr_flags = s.expr.items(.flags);

    switch (symbols.lookup_symbol(a, s, token_handle, symbol, false)) {
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
            const sym_file = a.get_source(insn_ref.file);
            const block_handle = sym_file.find_block_by_instruction(insn_ref.instruction);
            if (sym_file.blocks.items(.block_type)[block_handle]) |op| {
                expr_resolved_types[expr_handle] = switch (op) {
                    .none, .nil, .insn, .bound_insn, .org, .@"align", .keep, .range,
                    .def, .undef, .local, .db, .dh, .dw, .zb, .zh, .zw, .push, .pop,
                    => unreachable,

                    .section => Expression.Type.absolute_address(.data),
                    .boot, .code, .kcode, .entry, .kentry => Expression.Type.relative_address(.insn, Expression.Type.sr(.ip)),
                    .data, .kdata, .@"const", .kconst => Expression.Type.relative_address(.data, Expression.Type.sr(.ip)),

                    .stack => Expression.Type.relative_address(.stack, Expression.Type.sr(.sp)), // maybe this should be unreachable?
                } catch unreachable;
            } else {
                // Block does not have a section handle, so it's an .info section by default
                expr_resolved_types[expr_handle] = Expression.Type.absolute_address(.data);
            }
            expr_flags[expr_handle].insert(.constant_depends_on_layout);
            return true;
        },
        .stack => {
            expr_resolved_types[expr_handle] = Expression.Type.relative_address(.stack, Expression.Type.sr(.sp)) catch unreachable;
            expr_flags[expr_handle].insert(.constant_depends_on_layout);
        },
        .not_found => {
            a.record_expr_error(s.file.handle, expr_handle, "Reference to undefined symbol", .{});
            expr_resolved_types[expr_handle] = .poison;
            return true;
        },
    }

    return false;
}

pub fn check_instructions_and_directives_in_file(a: *Assembler, s: Source_File.Slices) void {
    var insn_operations = s.insn.items(.operation);
    var insn_params = s.insn.items(.params);

    var pushed_stacks: std.ArrayList([]const u8) = .empty;
    defer pushed_stacks.deinit(a.gpa);

    for (s.block.items(.first_insn), s.block.items(.end_insn)) |first_insn, end_insn| {
        var allow_code = false;
        var allow_data = false;
        var allow_range = false;

        for (first_insn.., insn_operations[first_insn..end_insn], insn_params[first_insn..end_insn]) |insn_handle_usize, op, maybe_params| {
            const insn_handle: Instruction.Handle = @intCast(insn_handle_usize);
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

                .org => check_org_params(a, s, insn_handle, maybe_params),

                .@"align" => check_align_params(a, s, insn_handle, maybe_params),

                .bound_insn => unreachable,
                .insn => {
                    if (!allow_code) {
                        a.record_insn_error(s.file.handle, insn_handle, "Instructions are not allowed in .data/.kdata/.const/.kconst/.stack sections", .{});
                    }
                    check_instruction_depends_on_layout(s, insn_handle, maybe_params);
                },

                .db, .dh, .dw => {
                    if (!allow_data) {
                        a.record_insn_error(s.file.handle, insn_handle, "Data directives are not allowed in .entry/.kentry/.code/.kcode sections", .{});
                    }
                    if (maybe_params) |params_expr| {
                        check_data_directive_expr_list(a, s, params_expr);
                    } else {
                        a.record_insn_error(s.file.handle, insn_handle, "Expected at least one data expression", .{});
                    }
                    check_instruction_depends_on_layout(s, insn_handle, maybe_params);
                },

                .zb, .zh, .zw => {
                    if (!allow_data) {
                        a.record_insn_error(s.file.handle, insn_handle, "Data directives are not allowed in .entry/.kentry/.code/.kcode sections", .{});
                    }
                    if (maybe_params) |params_expr| {
                        if (s.expr.items(.resolved_type)[params_expr].is_non_constant()) {
                            a.record_expr_error(s.file.handle, params_expr, "Expected constant expression", .{});
                        }
                    }
                    check_instruction_depends_on_layout(s, insn_handle, maybe_params);
                },

                .push => {
                    if (!allow_code) {
                        a.record_insn_error(s.file.handle, insn_handle, "Instructions are not allowed in .data/.kdata/.const/.kconst/.stack sections", .{});
                    }
                    check_pushed_stacks(a, s, insn_handle, maybe_params, &pushed_stacks);
                },
                .pop => {
                    if (!allow_code) {
                        a.record_insn_error(s.file.handle, insn_handle, "Instructions are not allowed in .data/.kdata/.const/.kconst/.stack sections", .{});
                    }
                    check_popped_stacks(a, s, insn_handle, maybe_params, &pushed_stacks);
                },

                .none, .nil, .keep => if (maybe_params) |params_expr| {
                    a.record_expr_error(s.file.handle, params_expr, "Expected no parameters", .{});
                },

                .range => {
                    if (!allow_range) {
                        a.record_insn_error(s.file.handle, insn_handle, ".range directive is only allowed in non-boot sections", .{});
                    } else {
                        check_range_params(a, s, insn_handle, maybe_params);
                    }
                },

                .def, .local => {},
                .undef => if (maybe_params) |params_expr| {
                    check_undef_expr_list(a, s, params_expr);
                } else {
                    a.record_insn_error(s.file.handle, insn_handle, "Expected at least one .def symbol name", .{});
                },
            }
        }

        pushed_stacks.clearRetainingCapacity();
    }
}

fn check_instruction_depends_on_layout(s: Source_File.Slices, insn_handle: Instruction.Handle, maybe_params: ?Expression.Handle) void {
    if (maybe_params) |params_handle| {
        if (instruction_has_layout_dependent_params(s, params_handle)) {
            s.insn.items(.flags)[insn_handle].insert(.depends_on_layout);
            return;
        }
    }
}

fn instruction_has_layout_dependent_params(s: Source_File.Slices, params_handle: Expression.Handle) bool {
    if (s.expr.items(.flags)[params_handle].contains(.constant_depends_on_layout)) return true;

    switch (s.expr.items(.info)[params_handle]) {
        .list => |bin| {
            return instruction_has_layout_dependent_params(s, bin.left)
                or instruction_has_layout_dependent_params(s, bin.right);
        },
        else => {},
    }
    return false;
}

fn check_pushed_stacks(a: *Assembler, s: Source_File.Slices, insn_handle: Instruction.Handle, maybe_params: ?Expression.Handle, pushed_stacks: *std.ArrayList([]const u8)) void {
    const expr_infos = s.expr.items(.info);

    var depends_on_layout = false;

    var maybe_expr_handle = maybe_params;
    while (maybe_expr_handle) |expr_handle| {
        switch (expr_infos[expr_handle]) {
            .list => |bin| {
                depends_on_layout = check_pushed_stack(a, s, bin.left, pushed_stacks) or depends_on_layout;
                maybe_expr_handle = bin.right;
            },
            else => {
                depends_on_layout = check_pushed_stack(a, s, expr_handle, pushed_stacks) or depends_on_layout;
                break;
            }
        }
    } else {
        a.record_insn_error(s.file.handle, insn_handle, "Expected at least one stack block name", .{});
    }

    if (depends_on_layout) {
        s.insn.items(.flags)[insn_handle].insert(.depends_on_layout);
    }
}
fn check_pushed_stack(a: *Assembler, s: Source_File.Slices, expr_handle: Expression.Handle, pushed_stacks: *std.ArrayList([]const u8)) bool {
    const insn_flags = s.insn.items(.flags);

    var depends_on_layout = false;

    const stack_block_name = symbols.parse_symbol(a, s, expr_handle).as_string();
    for (pushed_stacks.items) |name| {
        if (std.mem.eql(u8, name, stack_block_name)) {
            a.record_expr_error(s.file.handle, expr_handle, "This stack block has already been pushed", .{});
            return false;
        }
    }
    pushed_stacks.append(a.gpa, stack_block_name) catch @panic("OOM");

    if (s.file.stacks.get(stack_block_name)) |stack_block_handle| {
        var iter = s.block_instructions(stack_block_handle);
        while (iter.next()) |insn_handle| {
            depends_on_layout = depends_on_layout or insn_flags[insn_handle].contains(.depends_on_layout);
        }
    } else {
        a.record_expr_error(s.file.handle, expr_handle, "No .stack block found with this name", .{});
    }

    return depends_on_layout;
}

fn check_popped_stacks(a: *Assembler, s: Source_File.Slices, insn_handle: Instruction.Handle, maybe_params: ?Expression.Handle, pushed_stacks: *std.ArrayList([]const u8)) void {
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
        a.record_insn_error(s.file.handle, insn_handle, "Expected at least one stack block name", .{});
    }

    var depends_on_layout = false;

    maybe_expr_handle = maybe_params;
    while (maybe_expr_handle) |expr_handle| switch (expr_infos[expr_handle]) {
        .list => |bin| {
            depends_on_layout = check_popped_stack(a, s, bin.left, pushed_stacks, &num_blocks_popped) or depends_on_layout;
            maybe_expr_handle = bin.right;
        },
        else => {
            depends_on_layout = check_popped_stack(a, s, expr_handle, pushed_stacks, &num_blocks_popped) or depends_on_layout;
            maybe_expr_handle = null;
        }
    };

    if (depends_on_layout) {
        s.insn.items(.flags)[insn_handle].insert(.depends_on_layout);
    }
}
fn check_popped_stack(a: *Assembler, s: Source_File.Slices, expr_handle: Expression.Handle, pushed_stacks: *std.ArrayList([]const u8), num_blocks_to_pop: *usize) bool {
    const insn_flags = s.insn.items(.flags);

    var depends_on_layout = false;

    const stack_block_name = symbols.parse_symbol(a, s, expr_handle).as_string();
    for (0.., pushed_stacks.items) |i, name| {
        if (std.mem.eql(u8, name, stack_block_name)) {
            if (i + num_blocks_to_pop.* < pushed_stacks.items.len) {
                a.record_expr_error(s.file.handle, expr_handle, "Stack blocks must be popped in the reverse order they were pushed!", .{});
            } else {
                num_blocks_to_pop.* -= 1;
            }
            _ = pushed_stacks.orderedRemove(i);
            break;
        }
    } else {
        a.record_expr_error(s.file.handle, expr_handle, "This stack block has already been popped or was never pushed!", .{});
        return false;
    }

    if (s.file.stacks.get(stack_block_name)) |stack_block_handle| {
        var iter = s.block_instructions(stack_block_handle);
        while (iter.next()) |insn_handle| {
            depends_on_layout = depends_on_layout or insn_flags[insn_handle].contains(.depends_on_layout);
        }
    } else {
        a.record_expr_error(s.file.handle, expr_handle, "No .stack block found with this name", .{});
    }

    return depends_on_layout;
}

fn check_undef_expr_list(a: *Assembler, s: Source_File.Slices, expr_handle: Expression.Handle) void {
    const infos = s.expr.items(.info);
    var expr = expr_handle;
    while (infos[expr] == .list) {
        const bin = infos[expr].list;
        _ = check_undef_expr(a, s, bin.left);
        expr = bin.right;
    }
    _ = check_undef_expr(a, s, expr);
}

fn check_undef_expr(a: *Assembler, s: Source_File.Slices, expr_handle: Expression.Handle) void {
    const symbol_name = symbols.parse_symbol(a, s, expr_handle);
    const token = s.expr.items(.token)[expr_handle];
    switch (symbols.lookup_symbol(a, s, token, symbol_name.as_string(), false)) {
        .expression => {},
        .not_found => {
            a.record_expr_error(s.file.handle, expr_handle, "Symbol must be defined before it can be un-defined", .{});
        },
        .instruction => {
            a.record_expr_error(s.file.handle, expr_handle, "Labels cannot be un-defined", .{});
        },
        .stack => {
            a.record_expr_error(s.file.handle, expr_handle, "Stack labels cannot be un-defined", .{});
        },
    }
}

fn check_data_directive_expr_list(a: *Assembler, s: Source_File.Slices, expr_handle: Expression.Handle) void {
    const infos = s.expr.items(.info);
    var expr = expr_handle;
    while (true) {
        switch (infos[expr]) {
            .list => |bin| {
                check_data_directive_expr(a, s, bin.left);
                expr = bin.right;
            },
            else => break,
        }
    }
    check_data_directive_expr(a, s, expr);
}

fn check_data_directive_expr(a: *Assembler, s: Source_File.Slices, expr_handle: Expression.Handle) void {
    const expr_type = s.expr.items(.resolved_type)[expr_handle];
    if (expr_type.base_offset_type() != null and !expr_type.is_constant()) {
        a.record_expr_error(s.file.handle, expr_handle, "Expected constant expression", .{});
    }
}

fn check_org_params(a: *Assembler, s: Source_File.Slices, insn_handle: Instruction.Handle, maybe_params: ?Expression.Handle) void {
    const expr_resolved_types = s.expr.items(.resolved_type);

    var params = [_]?Expression.Handle{null};
    get_param_handles(a, s, maybe_params, &params);

    if (params[0]) |address_expr| {
        const expr_type = expr_resolved_types[address_expr];
        if (expr_type.base_offset_type()) |bot| {
            if (bot.base == .sr and bot.base.sr == .ip and bot.offset == .constant) {
                a.record_expr_error(s.file.handle, address_expr, "Expected absolute address, not relative; try using '@'", .{});
            } else if (bot.base != .constant) {
                a.record_expr_error(s.file.handle, address_expr, "Expected constant or absolute address", .{});
            }
        }
    } else {
        a.record_insn_error(s.file.handle, insn_handle, ".org directive must be followed by address expression", .{});
    }
}

fn check_align_params(a: *Assembler, s: Source_File.Slices, insn_handle: Instruction.Handle, maybe_params: ?Expression.Handle) void {
    const expr_resolved_types = s.expr.items(.resolved_type);

    var params = [_]?Expression.Handle{null} ** 2;
    get_param_handles(a, s, maybe_params, &params);

    if (params[0]) |align_expr| {
        if (expr_resolved_types[align_expr].is_non_constant()) {
            a.record_expr_error(s.file.handle, align_expr, "Expected constant", .{});
        }
    } else {
        a.record_insn_error(s.file.handle, insn_handle, ".align directive must be followed by constant expression", .{});
    }

    if (params[1]) |offset_expr| {
        if (expr_resolved_types[offset_expr].is_non_constant()) {
            a.record_expr_error(s.file.handle, offset_expr, "Expected constant", .{});
        }
    }
}

fn check_range_params(a: *Assembler, s: Source_File.Slices, insn_handle: Instruction.Handle, maybe_params: ?Expression.Handle) void {
    const expr_flags = s.expr.items(.flags);
    const expr_resolved_types = s.expr.items(.resolved_type);

    var params = [_]?Expression.Handle{null} ** 2;
    get_param_handles(a, s, maybe_params, &params);

    var range = Assembler.Address_Range{
        .first = 0,
        .len = 0,
    };

    if (params[0]) |expr| {
        if (expr_resolved_types[expr].is_non_constant()) {
            a.record_expr_error(s.file.handle, expr, "Expected minimum address constant", .{});
        }
        if (expr_flags[expr].contains(.constant_depends_on_layout)) {
            a.record_expr_error(s.file.handle, expr, "Expression cannot depend on layout", .{});
        } else {
            const constant = layout.resolve_expression_constant_or_default(a, s, 0, expr, 0x1000);
            range.first = constant.as_int(u32) catch a: {
                a.record_expr_error(s.file.handle, expr, "Expression must fit in u32", .{});
                break :a 0x1000;
            };
            if (arch.addr.Page.Offset.init(@truncate(range.first)).raw() != 0) {
                a.record_expr_error(s.file.handle, expr, "Expected an address with a page offset of 0", .{});
            }
        }
    }

    if (params[1]) |expr| {
        if (expr_resolved_types[expr].is_non_constant()) {
            a.record_expr_error(s.file.handle, expr, "Expected maximum address constant", .{});
        }
        if (expr_flags[expr].contains(.constant_depends_on_layout)) {
            a.record_expr_error(s.file.handle, expr, "Expression cannot depend on layout", .{});
        } else {
            const constant = layout.resolve_expression_constant_or_default(a, s, 0, expr, 0xFFFF_FFFF);
            const last = constant.as_int(u32) catch a: {
                a.record_expr_error(s.file.handle, expr, "Expression must fit in u32", .{});
                break :a 0xFFFF_FFFF;
            };
            if (arch.addr.Page.Offset.init(@truncate(last)) != arch.addr.Page.Offset.max) {
                a.record_expr_error(s.file.handle, expr, "Expected an address with a page offset of 0xFFF", .{});
            }
            range.len = @as(usize, last - range.first) + 1;

            const block_handle = s.file.find_block_by_instruction(insn_handle);
            const section_handle = s.file.blocks.items(.section)[block_handle].?;
            const section = a.get_section_ptr(section_handle);
            if (section.range) |_| {
                a.record_insn_error(s.file.handle, insn_handle, "Multiple .range directives found for this section; ignoring this one", .{});
            } else {
                section.range = range;
            }
        }
    } else {
        a.record_insn_error(s.file.handle, insn_handle, ".range directive must be followed by <min_address>, <max_address> constant expressions", .{});
    }
}

fn get_param_handles(a: *Assembler, s: Source_File.Slices, maybe_params_expr: ?Expression.Handle, out: []?Expression.Handle) void {
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
                else => {
                    p.* = expr_handle;
                    return;
                }
            }
        }
        a.record_expr_error(s.file.handle, expr_handle, "Too many parameters", .{});
    }
}

pub fn check_symbol_ambiguity_in_file(a: *Assembler, s: Source_File.Slices) void {
    const expr_tokens = s.expr.items(.token);
    for (0.., s.expr.items(.info)) |expr_handle, info| switch (info) {
        .literal_symbol_ref, .directive_symbol_ref => {
            const constant = symbols.parse_symbol(a, s, @intCast(expr_handle));
            _ = symbols.lookup_symbol(a, s, expr_tokens[expr_handle], constant.as_string(), true);
        },
        else => {},
    };
}

const symbols = @import("symbols.zig");
const layout = @import("layout.zig");
const Assembler = @import("Assembler.zig");
const Block = Source_File.Block;
const Source_File = @import("Source_File.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const Constant = @import("Constant.zig");
const Section = @import("Section.zig");
const Error = @import("Error.zig");
const isa = @import("isa");
const arch = @import("arch");
const std = @import("std");
