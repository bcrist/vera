const std = @import("std");
const lex = @import("lex.zig");
const ie = @import("instruction_encoding");
const layout = @import("layout.zig");
const Assembler = @import("Assembler.zig");
const SourceFile = @import("SourceFile.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const Constant = @import("Constant.zig");
const Section = @import("Section.zig");
const Error = @import("Error.zig");
const Token = lex.Token;
const ExpressionType = ie.ExpressionType;
const SectionBlock = SourceFile.SectionBlock;

// Set the resolved type for labels to .symbol_def.
// The resolved constant for these expressions is the raw symbol name.
// Any labels which do not begin with `_` and are not in a stack section will be added to the global symbol table.
// Create Sections for each named section block and set the SectionBlock.section handle to reference it.
pub fn processLabelsAndSections(a: *Assembler, file: *SourceFile) void {
    const s = file.slices();
    const insn_params = s.insn.items(.params);

    var in_stack_section = false;
    for (0.., s.insn.items(.operation), s.insn.items(.label)) |insn_handle, op, maybe_label_expr| {
        if (Instruction.isSectionDirective(op)) {
            in_stack_section = op == .stack;

            if (insn_params[insn_handle]) |section_name_expr| {
                const kind: Section.Kind = switch (op) {
                    .section => .info,
                    .code => .code_user,
                    .kcode => .code_kernel,
                    .entry => .entry_user,
                    .kentry => .entry_kernel,
                    .data => .data_user,
                    .kdata => .data_kernel,
                    .@"const" => .constant_user,
                    .kconst => .constant_kernel,
                    .stack => .stack,

                    .none, .insn, .bound_insn, .org, .@"align", .keep, .def, .undef, .db, .dw, .dd, .push, .pop,
                    => unreachable,
                };

                const symbol_constant = resolveSymbolDefExpr(a, s, section_name_expr);
                const section_name = symbol_constant.asString();
                const entry = a.sections.getOrPutValue(a.gpa, section_name, .{
                    .name = section_name,
                    .kind = kind,
                }) catch @panic("OOM");
                const found_kind = entry.value_ptr.kind;
                if (found_kind != kind) switch (found_kind) {
                    inline else => |k| {
                        const token = s.insn.items(.token)[insn_handle];
                        a.recordError(file.handle, token, "Section already exists; expected ." ++ @tagName(k), .{});
                    },
                };
                const block_handle = file.findBlockByInstruction(@intCast(Instruction.Handle, insn_handle));
                s.block.items(.section)[block_handle] = @intCast(Section.Handle, entry.index);
            } else {
                const token = s.insn.items(.token)[insn_handle];
                a.recordError(file.handle, token, "Unnamed sections are not currently supported", .{});
            }
        } else if (op == .def) {
            const params_expr = insn_params[insn_handle].?;
            const symbol_expr = s.expr.items(.info)[params_expr].list.left;
            _ = resolveSymbolDefExpr(a, s, symbol_expr);
        } else if (op == .undef) {
            if (insn_params[insn_handle]) |params| {
                _ = resolveSymbolDefExpr(a, s, params);
            }
        }

        if (maybe_label_expr) |label_expr| {
            const symbol_constant = resolveSymbolDefExpr(a, s, label_expr);
            const symbol = symbol_constant.asString();
            if (!in_stack_section and !std.mem.startsWith(u8, symbol, "_")) {
                a.symbols.put(a.gpa, symbol, .{
                    .file = file.handle,
                    .instruction = @intCast(Instruction.Handle, insn_handle),
                }) catch @panic("OOM");
            }
        }
    }
}

fn resolveSymbolDefExpr(a: *Assembler, s: SourceFile.Slices, symbol_def_expr: Expression.Handle) *const Constant {
    var token_handle = s.expr.items(.token)[symbol_def_expr];
    switch (s.expr.items(.info)[symbol_def_expr]) {
        .list => |bin| {
            _ = resolveSymbolDefExpr(a, s, bin.left);
            return resolveSymbolDefExpr(a, s, bin.right);
        },
        .literal_symbol_def => {
            const file = s.file;
            const literal = file.tokens.get(token_handle).location(file.source);
            const constant = Constant.initSymbolLiteral(a.gpa, &a.constant_temp, literal);
            const interned_symbol_name = constant.intern(a.arena, a.gpa, &a.constants);
            s.expr.items(.resolved_constant)[symbol_def_expr] = interned_symbol_name;
            s.expr.items(.resolved_type)[symbol_def_expr] = .{ .symbol_def = {} };
            return interned_symbol_name;
        },
        .directive_symbol_def => |inner_expr| {
            var expr_resolved_constants = s.expr.items(.resolved_constant);
            if (expr_resolved_constants[inner_expr]) |interned_symbol_name| {
                expr_resolved_constants[symbol_def_expr] = interned_symbol_name;
                s.expr.items(.resolved_type)[symbol_def_expr] = .{ .symbol_def = {} };
                return interned_symbol_name;
            } else if (tryResolveExpressionType(a, s, inner_expr)) {
                const interned_symbol_name = expr_resolved_constants[inner_expr].?;
                expr_resolved_constants[symbol_def_expr] = interned_symbol_name;
                s.expr.items(.resolved_type)[symbol_def_expr] = .{ .symbol_def = {} };
                return interned_symbol_name;
            } else unreachable;
        },
        else => unreachable,
    }
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

fn tryResolveExpressionType(a: *Assembler, s: SourceFile.Slices, expr_handle: Expression.Handle) bool {
    const expr_infos = s.expr.items(.info);
    var expr_resolved_types = s.expr.items(.resolved_type);

    std.debug.assert(expr_resolved_types[expr_handle] == .unknown);

    const info = expr_infos[expr_handle];
    switch (info) {
        .list, .arrow_list => {
            expr_resolved_types[expr_handle] = .{ .poison = {} };
        },
        .literal_int, .literal_str => {
            const token_handle = s.expr.items(.token)[expr_handle];
            const token = s.file.tokens.get(token_handle);
            if (Constant.initLiteral(a.gpa, &a.constant_temp, token, s.file.source)) |constant| {
                expr_resolved_types[expr_handle] = .{ .constant = {} };
                s.expr.items(.resolved_constant)[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
            } else |err| switch (err) {
                error.Overflow => {
                    a.recordError(s.file.handle, token_handle, "Integer literal too large", .{});
                    expr_resolved_types[expr_handle] = .{ .poison = {} };
                },
                error.InvalidCharacter => {
                    a.recordError(s.file.handle, token_handle, "Invalid character in literal", .{});
                    expr_resolved_types[expr_handle] = .{ .poison = {} };
                },
                error.InvalidToken => unreachable,
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
            if (s.block.items(.section)[block_handle]) |section_handle| {
                expr_resolved_types[expr_handle] = switch (a.getSection(section_handle).kind) {
                    .code_user, .code_kernel, .entry_user, .entry_kernel => ExpressionType.relativeAddress(.insn, .{ .sr = .ip }),
                    .data_user, .data_kernel, .constant_user, .constant_kernel => ExpressionType.relativeAddress(.data, .{ .sr = .ip }),
                    .stack => ExpressionType.relativeAddress(.stack, .{ .sr = .sp }),
                    .info => ExpressionType.absoluteAddress(.data),
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
        .plus => |bin| {
            var builder = ie.ExpressionTypeBuilder{};
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
            var builder = ie.ExpressionTypeBuilder{};
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
                    const index = constant.asInt() catch {
                        const token = s.expr.items(.token)[expr_handle];
                        a.recordError(s.file.handle, token, "Operand out of range", .{});
                        break :t .{ .poison = {} };
                    };
                    if (index < 0 or index > 15) {
                        const token = s.expr.items(.token)[expr_handle];
                        a.recordError(s.file.handle, token, "Operand out of range", .{});
                        break :t .{ .poison = {} };
                    }

                    const reg = ie.IndexedRegister{
                        .index = @intCast(u4, index),
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
                    const constant = Constant.initInt(reg.index, null);
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
        .signed_cast, .unsigned_cast, .maybe_signed_cast => |inner_expr| {
            const inner_type = expr_resolved_types[inner_expr];
            expr_resolved_types[expr_handle] = switch (inner_type) {
                .unknown => return false,
                .poison => .{ .poison = {} },
                .reg8, .reg16, .reg32 => |reg| t: {
                    var new_reg = reg;
                    new_reg.signedness = switch (info) {
                        .signed_cast => .signed,
                        .unsigned_cast => .unsigned,
                        .maybe_signed_cast => null,
                        else => unreachable,
                    };

                    break :t switch (inner_type) {
                        .reg8 => .{ .reg8 = new_reg },
                        .reg16 => .{ .reg16 = new_reg },
                        .reg32 => .{ .reg32 = new_reg },
                        else => unreachable,
                    };
                },
                .raw_base_offset, .data_address, .insn_address, .stack_address,
                .constant, .symbol_def, .sr => t: {
                    const token = s.expr.items(.token)[expr_handle];
                    a.recordError(s.file.handle, token, "Operand must be a GPR expression", .{});
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
                    if (!std.meta.eql(bo.base, .{ .sr = .ip })) {
                        const token = s.expr.items(.token)[expr_handle];
                        a.recordError(s.file.handle, token, "Operand must be an IP-relative expression", .{});
                        break :t .{ .poison = {} };
                    }
                    var builder = ie.ExpressionTypeBuilder{};
                    builder.add(inner_type);
                    builder.subtract(.{ .sr = .ip });
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
                .reg8, .reg16, .reg32, .constant, .sr => .{ .data_address = ie.BaseOffsetType.init(inner_type, null) catch unreachable },
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
                .reg8, .reg16, .reg32, .constant, .sr => .{ .insn_address = ie.BaseOffsetType.init(inner_type, null) catch unreachable },
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
                .reg8, .reg16, .reg32, .constant, .sr => .{ .stack_address = ie.BaseOffsetType.init(inner_type, null) catch unreachable },
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

    if (a.lookupSymbol(s, token_handle, symbol)) |target| switch (target) {
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
            if (sym_file.blocks.items(.section)[block_handle]) |section_handle| {
                expr_resolved_types[expr_handle] = switch (a.getSection(section_handle).kind) {
                    .code_user, .code_kernel, .entry_user, .entry_kernel => ExpressionType.relativeAddress(.insn, .{ .sr = .ip }),
                    .data_user, .data_kernel, .constant_user, .constant_kernel => ExpressionType.relativeAddress(.data, .{ .sr = .ip }),
                    .stack => ExpressionType.relativeAddress(.stack, .{ .sr = .sp }),
                    .info => ExpressionType.absoluteAddress(.data),
                } catch unreachable;
            } else {
                // Block does not have a section handle, so it's an .info section by default
                expr_resolved_types[expr_handle] = ExpressionType.absoluteAddress(.data);
            }
            expr_flags[expr_handle].insert(.constant_depends_on_layout);
            return true;
        },
        .not_found => {
            a.recordError(s.file.handle, token_handle, "Reference to undefined symbol", .{});
            expr_resolved_types[expr_handle] = .{ .poison = {} };
            return true;
        },
    };

    return false;
}

pub fn checkInstructionsAndDirectives(a: *Assembler) void {
    for (a.files.items) |*file| {
        checkInstructionsAndDirectivesInFile(a, file.slices());
    }
}

fn checkInstructionsAndDirectivesInFile(a: *Assembler, s: SourceFile.Slices) void {
    var insn_flags = s.insn.items(.flags);

    var expr_resolved_types = s.expr.items(.resolved_type);

    for (s.insn.items(.operation), s.insn.items(.params), 0..) |op, maybe_params, insn_handle| {
        switch (op) {
            .none => std.debug.assert(maybe_params == null),
            .bound_insn => unreachable,
            .insn => {
                if (maybe_params) |params_handle| {
                    if (instructionHasLayoutDependentParams(s, params_handle)) {
                        insn_flags[insn_handle].insert(.encoding_depends_on_layout);
                    }
                }
            },
            .keep => if (maybe_params) |params_expr| {
                const token = s.expr.items(.token)[params_expr];
                a.recordError(s.file.handle, token, "Expected no parameters", .{});
            },
            .org => {
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
                            if (std.meta.eql(bo.base, .{ .sr = .ip }) and bo.offset == .constant) {
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
            },
            .@"align" => {
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
            },
            .undef => if (maybe_params) |params_expr| {
                checkUndefExpr(a, s, params_expr);
            } else {
                const token = s.insn.items(.token)[insn_handle];
                a.recordError(s.file.handle, token, "Expected at least one .def symbol name", .{});
            },

            .db, .dw, .dd => if (maybe_params) |params_expr| {
                checkDataDirectiveExpr(a, s, params_expr);
            } else {
                const token = s.insn.items(.token)[insn_handle];
                a.recordError(s.file.handle, token, "Expected at least one data expression", .{});
            },

            .push => {
                // TODO .push
            },
            .pop => {
                // TODO .pop
            },

            .def, .section, .stack,
            .code, .kcode, .entry, .kentry,
            .data, .kdata, .@"const", .kconst,
            => {}
        }

        // TODO check that instructions do not appear in data or stack sections
        // TODO check that data directives do not appear in code sections
        // TODO check for shadowed symbols
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

fn checkUndefExpr(a: *Assembler, s: SourceFile.Slices, expr_handle: Expression.Handle) void {
    const token = s.expr.items(.token)[expr_handle];
    const constant = switch (s.expr.items(.info)[expr_handle]) {
        .list => |bin| {
            checkUndefExpr(a, s, bin.left);
            checkUndefExpr(a, s, bin.right);
            return;
        },
        .literal_symbol_def => c: {
            const file = s.file;
            const raw_symbol = file.tokens.get(token).location(file.source);
            break :c Constant.initSymbolLiteral(a.gpa, &a.constant_temp, raw_symbol);
        },
        .directive_symbol_def => |literal_expr| s.expr.items(.resolved_constant)[literal_expr].?.*,
        else => unreachable,
    };

    if (a.lookupSymbol(s, token, constant.asString())) |target| switch (target) {
        .expression => {},
        .not_found => {
            a.recordError(s.file.handle, token, "Symbol must be defined before it can be un-defined", .{});
        },
        .instruction => {
            a.recordError(s.file.handle, token, "Labels cannot be un-defined", .{});
        },
    };
}

fn checkDataDirectiveExpr(a: *Assembler, s: SourceFile.Slices, expr_handle: Expression.Handle) void {
    switch (s.expr.items(.info)[expr_handle]) {
        .list => |bin| {
            checkDataDirectiveExpr(a, s, bin.left);
            checkDataDirectiveExpr(a, s, bin.right);
            return;
        },
        .arrow_list => |bin| {
            checkDataDirectiveExpr(a, s, bin.left);

            const token = s.expr.items(.token)[expr_handle];
            a.recordError(s.file.handle, token, "Expected ','", .{});
        },
        else => switch (s.expr.items(.resolved_type)[expr_handle]) {
            .unknown, .poison, .constant => {},
            .symbol_def => unreachable,
            .raw_base_offset, .data_address, .insn_address, .stack_address,
            .reg8, .reg16, .reg32, .sr => {
                const token = s.expr.items(.token)[expr_handle];
                a.recordError(s.file.handle, token, "Expected constant expression", .{});
            },
        },
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