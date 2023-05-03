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
    const insn_slice = file.instructions.slice();
    var insn_tokens = insn_slice.items(.token);
    var labels = insn_slice.items(.label);
    var operations = insn_slice.items(.operation);
    var params = insn_slice.items(.params);

    const expr_slice = file.expressions.slice();
    var expr_resolved_types = expr_slice.items(.resolved_type);
    var expr_resolved_constants = expr_slice.items(.resolved_constant);
    var expr_infos = expr_slice.items(.info);
    var expr_tokens = expr_slice.items(.token);
    var expr_flags = expr_slice.items(.flags);

    var block_sections = file.blocks.items(.section);

    var in_stack_section = false;
    for (labels, operations, 0..) |maybe_label_expr, op, insn_handle| {
        if (Instruction.isSectionDirective(op)) {
            in_stack_section = op == .stack;

            if (params[insn_handle]) |section_name_expr| {
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

                const symbol_constant = resolveSymbolDefExpr(a, file, section_name_expr, expr_tokens, expr_infos, expr_resolved_constants, expr_resolved_types, expr_flags);
                const section_name = symbol_constant.asString();
                const entry = a.sections.getOrPutValue(a.gpa, section_name, .{
                    .name = section_name,
                    .kind = kind,
                }) catch @panic("OOM");
                const found_kind = entry.value_ptr.kind;
                if (found_kind != kind) switch (found_kind) {
                    inline else => |k| a.recordError(file.handle, insn_tokens[insn_handle], "Section already exists; expected ." ++ @tagName(k), .{}),
                };
                const block_handle = file.findBlockByInstruction(@intCast(Instruction.Handle, insn_handle));
                block_sections[block_handle] = @intCast(Section.Handle, entry.index);
            } else {
                a.recordError(file.handle, insn_tokens[insn_handle], "Unnamed sections are not currently supported", .{});
            }
        }

        if (maybe_label_expr) |label_expr| {
            const symbol_constant = resolveSymbolDefExpr(a, file, label_expr, expr_tokens, expr_infos, expr_resolved_constants, expr_resolved_types, expr_flags);
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

fn resolveSymbolDefExpr(
    a: *Assembler,
    file: *SourceFile,
    symbol_def_expr: Expression.Handle,
    expr_tokens: []const lex.Token.Handle,
    expr_infos: []const Expression.Info,
    expr_resolved_constants: []?*const Constant,
    expr_resolved_types: []ExpressionType,
    expr_flags: []Expression.FlagSet,
) *const Constant {
    var token_handle = expr_tokens[symbol_def_expr];
    switch (expr_infos[symbol_def_expr]) {
        .literal_symbol_def => {
            const literal = file.tokens.get(token_handle).location(file.source);
            const constant = Constant.initSymbolLiteral(a.gpa, &a.constant_temp, literal);
            const interned_symbol_name = constant.intern(a.arena, a.gpa, &a.constants);
            expr_resolved_constants[symbol_def_expr] = interned_symbol_name;
            expr_resolved_types[symbol_def_expr] = .{ .symbol_def = {} };
            return interned_symbol_name;
        },
        .directive_symbol_def => |inner_expr| {
            if (expr_resolved_constants[inner_expr]) |interned_symbol_name| {
                expr_resolved_constants[symbol_def_expr] = interned_symbol_name;
                expr_resolved_types[symbol_def_expr] = .{ .symbol_def = {} };
                return interned_symbol_name;
            } else if (tryResolveExpressionType(a, file, inner_expr, expr_tokens, expr_infos, expr_resolved_constants, expr_resolved_types, expr_flags)) {
                const interned_symbol_name = expr_resolved_constants[inner_expr].?;
                expr_resolved_constants[symbol_def_expr] = interned_symbol_name;
                expr_resolved_types[symbol_def_expr] = .{ .symbol_def = {} };
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
            var expr_resolved_types = file.expressions.items(.resolved_type);
            var expr_resolved_constants = file.expressions.items(.resolved_constant);
            var expr_infos = file.expressions.items(.info);
            var expr_tokens = file.expressions.items(.token);
            var expr_flags = file.expressions.items(.flags);

            for (expr_resolved_types, 0..) |expr_type, handle| {
                if (expr_type == .unknown) {
                    if (tryResolveExpressionType(a, file, @intCast(Expression.Handle, handle),
                        expr_tokens, expr_infos, expr_resolved_constants, expr_resolved_types, expr_flags,
                    )) {
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
            const resolved_types = file.expressions.items(.resolved_type);
            const tokens = file.expressions.items(.token);
            for (resolved_types, 0..) |expr_type, handle| {
                if (expr_type == .unknown) {
                    resolved_types[handle] = .{ .poison = {} };
                    a.recordError(file.handle, tokens[handle], "Could not determine type for expression", .{});
                }
            }
        }
        return false;
    }

    return true;
}

fn tryResolveExpressionType(
    a: *Assembler,
    file: *SourceFile,
    expr_handle: Expression.Handle,
    expr_tokens: []const lex.Token.Handle,
    expr_infos: []const Expression.Info,
    expr_resolved_constants: []?*const Constant,
    expr_resolved_types: []ExpressionType,
    expr_flags: []Expression.FlagSet,
) bool {
    std.debug.assert(expr_resolved_types[expr_handle] == .unknown);

    const info = expr_infos[expr_handle];
    switch (info) {
        .list, .arrow_list => {
            expr_resolved_types[expr_handle] = .{ .poison = {} };
        },
        .literal_int, .literal_str => {
            const token_handle = expr_tokens[expr_handle];
            const token = file.tokens.get(token_handle);
            if (Constant.initLiteral(a.gpa, &a.constant_temp, token, file.source)) |constant| {
                expr_resolved_types[expr_handle] = .{ .constant = {} };
                expr_resolved_constants[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
            } else |err| switch (err) {
                error.Overflow => {
                    a.recordError(file.handle, token_handle, "Integer literal too large", .{});
                    expr_resolved_types[expr_handle] = .{ .poison = {} };
                },
                error.InvalidCharacter => {
                    a.recordError(file.handle, token_handle, "Invalid character in literal", .{});
                    expr_resolved_types[expr_handle] = .{ .poison = {} };
                },
                error.InvalidToken => unreachable,
            }
        },
        .literal_reg => {
            // SFR types are preassigned when parsed, so we can assume we're dealing with a GPR name.
            const token_handle = expr_tokens[expr_handle];
            const token = file.tokens.get(token_handle);
            const name = token.location(file.source);
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
            const block_handle = file.findBlockByToken(expr_tokens[expr_handle]);
            if (file.blocks.items(.section)[block_handle]) |section_handle| {
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
        .literal_symbol_ref => {
            const token_handle = expr_tokens[expr_handle];
            const symbol_literal = file.tokens.get(token_handle).location(file.source);
            const symbol_constant = Constant.initSymbolLiteral(a.gpa, &a.constant_temp, symbol_literal);
            return tryResolveSymbolType(a, file, expr_handle, token_handle, symbol_constant.asString(), expr_resolved_types, expr_flags);
        },
        .directive_symbol_ref => |inner_expr| {
            if (expr_resolved_constants[inner_expr]) |symbol| {
                return tryResolveSymbolType(a, file, expr_handle, expr_tokens[expr_handle], symbol.asString(), expr_resolved_types, expr_flags);
            } else return false;
        },
        .literal_symbol_def, .directive_symbol_def => {
            _ = resolveSymbolDefExpr(a, file, expr_handle, expr_tokens, expr_infos, expr_resolved_constants, expr_resolved_types, expr_flags);
        },
        .plus => |bin| {
            var builder = ie.ExpressionTypeBuilder{};
            builder.add(expr_resolved_types[bin.left]);
            builder.add(expr_resolved_types[bin.right]);
            const result_type: ExpressionType = builder.build() catch t: {
                a.recordError(file.handle, expr_tokens[expr_handle], "Cannot represent result of symbolic addition", .{});
                break :t .{ .poison = {} };
            };
            if (result_type == .unknown) return false;
            expr_resolved_types[expr_handle] = result_type;
            resolveConstantDependsOnLayoutBinary(expr_flags, expr_handle, bin);
        },
        .minus => |bin| {
            var builder = ie.ExpressionTypeBuilder{};
            builder.add(expr_resolved_types[bin.left]);
            builder.subtract(expr_resolved_types[bin.right]);
            const result_type: ExpressionType = builder.build() catch t: {
                a.recordError(file.handle, expr_tokens[expr_handle], "Cannot represent result of symbolic subtraction", .{});
                break :t .{ .poison = {} };
            };
            if (result_type == .unknown) return false;
            expr_resolved_types[expr_handle] = result_type;
            resolveConstantDependsOnLayoutBinary(expr_flags, expr_handle, bin);
        },
        .negate, .complement => |inner_expr| {
            expr_resolved_types[expr_handle] = switch (expr_resolved_types[inner_expr]) {
                .unknown => return false,
                .poison => .{ .poison = {} },
                .constant => .{ .constant = {} },
                .raw_base_offset, .data_address, .insn_address, .stack_address,
                .symbol_def, .reg8, .reg16, .reg32, .sr => t: {
                    a.recordError(file.handle, expr_tokens[expr_handle], "Operand must be a constant expression", .{});
                    break :t .{ .poison = {} };
                },
            };
            resolveConstantDependsOnLayout(expr_flags, expr_handle, inner_expr);
        },
        .index_to_reg8, .index_to_reg16, .index_to_reg32 => |inner_expr| {
            if (expr_flags[inner_expr].contains(.constant_depends_on_layout)) {
                a.recordError(file.handle, expr_tokens[expr_handle], "Operand cannot vary on memory layout", .{});
                expr_resolved_types[expr_handle] = .{ .poison = {} };
                return true;
            }
            expr_resolved_types[expr_handle] = switch (expr_resolved_types[inner_expr]) {
                .unknown => return false,
                .poison => .{ .poison = {} },
                .constant => t: {
                    const constant = layout.resolveExpressionConstant(a, file, 0, inner_expr) orelse {
                        break :t .{ .poison = {} };
                    };
                    const index = constant.asInt() catch {
                        a.recordError(file.handle, expr_tokens[expr_handle], "Operand out of range", .{});
                        break :t .{ .poison = {} };
                    };
                    if (index < 0 or index > 15) {
                        a.recordError(file.handle, expr_tokens[expr_handle], "Operand out of range", .{});
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
                    a.recordError(file.handle, expr_tokens[expr_handle], "Operand must be a constant expression", .{});
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
                    expr_resolved_constants[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
                    break :t .{ .constant = {} };
                },
                .raw_base_offset, .data_address, .insn_address, .stack_address,
                .constant, .symbol_def, .sr => t: {
                    a.recordError(file.handle, expr_tokens[expr_handle], "Operand must be a GPR expression", .{});
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
                a.recordError(file.handle, expr_tokens[expr_handle], "Both operands must be constant expressions", .{});
                break :t .{ .poison = {} };
            };
            resolveConstantDependsOnLayoutBinary(expr_flags, expr_handle, bin);
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
                    a.recordError(file.handle, expr_tokens[expr_handle], "Operand must be a GPR expression", .{});
                    break :t .{ .poison = {} };
                },
            };
            resolveConstantDependsOnLayout(expr_flags, expr_handle, inner_expr);
        },
        .absolute_address_cast => |inner_expr| {
            const inner_type = expr_resolved_types[inner_expr];
            expr_resolved_types[expr_handle] = switch (inner_type) {
                .unknown => return false,
                .poison => .{ .poison = {} },
                .raw_base_offset, .data_address, .insn_address, .stack_address => |bo| t: {
                    if (!std.meta.eql(bo.base, .{ .sr = .ip })) {
                        a.recordError(file.handle, expr_tokens[expr_handle], "Operand must be an IP-relative expression", .{});
                        break :t .{ .poison = {} };
                    }
                    var builder = ie.ExpressionTypeBuilder{};
                    builder.add(inner_type);
                    builder.subtract(.{ .sr = .ip });
                    break :t builder.build() catch unreachable;
                },
                .reg8, .reg16, .reg32, .constant, .symbol_def, .sr => t: {
                    a.recordError(file.handle, expr_tokens[expr_handle], "Operand must be an IP-relative expression", .{});
                    break :t .{ .poison = {} };
                },
            };
            resolveConstantDependsOnLayout(expr_flags, expr_handle, inner_expr);
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
                    a.recordError(file.handle, expr_tokens[expr_handle], "Casting directly between address spaces is not allowed.  Use `.d .raw` if you really want this.", .{});
                    break :t .{ .poison = {} };
                },
                .symbol_def => unreachable,
            };
            resolveConstantDependsOnLayout(expr_flags, expr_handle, inner_expr);
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
                    a.recordError(file.handle, expr_tokens[expr_handle], "Casting directly between address spaces is not allowed.  Use `.i .raw` if you really want this.", .{});
                    break :t .{ .poison = {} };
                },
                .symbol_def => unreachable,
            };
            resolveConstantDependsOnLayout(expr_flags, expr_handle, inner_expr);
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
                    a.recordError(file.handle, expr_tokens[expr_handle], "Casting directly between address spaces is not allowed.  Use `.s .raw` if you really want this.", .{});
                    break :t .{ .poison = {} };
                },
                .symbol_def => unreachable,
            };
            resolveConstantDependsOnLayout(expr_flags, expr_handle, inner_expr);
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
            resolveConstantDependsOnLayout(expr_flags, expr_handle, inner_expr);
        },
    }
    return true;
}

fn resolveConstantDependsOnLayout(expr_flags: []Expression.FlagSet, expr_handle: Expression.Handle, dependent_expr: Expression.Handle) void {
    if (expr_flags[dependent_expr].contains(.constant_depends_on_layout)) {
        expr_flags[expr_handle].insert(.constant_depends_on_layout);
    }
}

fn resolveConstantDependsOnLayoutBinary(expr_flags: []Expression.FlagSet, expr_handle: Expression.Handle, bin: Expression.Binary) void {
    if (expr_flags[bin.left].contains(.constant_depends_on_layout) or expr_flags[bin.right].contains(.constant_depends_on_layout)) {
        expr_flags[expr_handle].insert(.constant_depends_on_layout);
    }
}

fn tryResolveSymbolType(
    a: *Assembler,
    file: *const SourceFile,
    expr_handle: Expression.Handle,
    token_handle: Token.Handle,
    symbol: []const u8,
    expr_resolved_types: []ExpressionType,
    expr_flags: []Expression.FlagSet,
) bool {
    if (a.lookupSymbol(file, token_handle, symbol)) |target| switch (target) {
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
            a.recordError(file.handle, token_handle, "Reference to undefined symbol", .{});
            expr_resolved_types[expr_handle] = .{ .poison = {} };
            return true;
        },
    };

    return false;
}

pub fn checkInstructionsAndDirectives(a: *Assembler) void {
    for (a.files.items) |*file| {
        var insn_operations = file.instructions.items(.operation);
        var insn_params = file.instructions.items(.params);
        var insn_flags = file.instructions.items(.flags);
        var insn_tokens = file.expressions.items(.token);

        var expr_infos = file.expressions.items(.info);
        var expr_flags = file.expressions.items(.flags);
        var expr_tokens = file.expressions.items(.token);
        var expr_resolved_types = file.expressions.items(.resolved_type);

        for (insn_operations, insn_params, 0..) |op, maybe_params, insn_handle| {
            switch (op) {
                .none => std.debug.assert(maybe_params == null),
                .bound_insn => unreachable,
                .insn => {
                    if (maybe_params) |params_handle| {
                        if (instructionHasLayoutDependentParams(expr_flags, expr_infos, params_handle)) {
                            insn_flags[insn_handle].insert(.encoding_depends_on_layout);
                        }
                    }
                },
                .keep => if (maybe_params) |params_expr| {
                    const token = expr_tokens[params_expr];
                    a.recordError(file.handle, token, "Expected no parameters", .{});
                },
                .org => {
                    var params = [_]?Expression.Handle{null};
                    getParamHandles(a, file.handle, maybe_params, expr_infos, expr_tokens, &params);
                    if (params[0]) |address_expr| {
                        switch (expr_resolved_types[address_expr]) {
                            .unknown, .symbol_def => unreachable,
                            .poison, .constant => {},
                            .reg8, .reg16, .reg32, .sr => {
                                const token = expr_tokens[address_expr];
                                a.recordError(file.handle, token, "Expected constant or absolute address, not register", .{});
                            },
                            .raw_base_offset, .data_address, .insn_address, .stack_address => |bo| {
                                if (std.meta.eql(bo.base, .{ .sr = .ip }) and bo.offset == .constant) {
                                    const token = expr_tokens[address_expr];
                                    a.recordError(file.handle, token, "Expected absolute address, not relative; try using '@'", .{});
                                } else if (bo.base != .constant or bo.offset != .none) {
                                    const token = expr_tokens[address_expr];
                                    a.recordError(file.handle, token, "Expected constant or absolute address", .{});
                                }
                            },
                        }
                    } else {
                        const token = insn_tokens[insn_handle];
                        a.recordError(file.handle, token, ".org directive must be followed by address expression", .{});
                    }
                },
                .@"align" => {
                    var params = [_]?Expression.Handle{null} ** 2;
                    getParamHandles(a, file.handle, maybe_params, expr_infos, expr_tokens, &params);
                    if (params[0]) |align_expr| {
                        switch (expr_resolved_types[align_expr]) {
                            .unknown, .symbol_def => unreachable,
                            .poison, .constant => {},
                            .reg8, .reg16, .reg32, .sr,
                            .raw_base_offset, .data_address, .insn_address, .stack_address => {
                                const token = expr_tokens[align_expr];
                                a.recordError(file.handle, token, "Expected constant", .{});
                            },
                        }
                    } else {
                        const token = insn_tokens[insn_handle];
                        a.recordError(file.handle, token, ".align directive must be followed by constant expression", .{});
                    }

                    if (params[1]) |offset_expr| {
                        switch (expr_resolved_types[offset_expr]) {
                            .unknown, .symbol_def => unreachable,
                            .poison, .constant => {},
                            .reg8, .reg16, .reg32, .sr,
                            .raw_base_offset, .data_address, .insn_address, .stack_address => {
                                const token = expr_tokens[offset_expr];
                                a.recordError(file.handle, token, "Expected constant", .{});
                            },
                        }
                    }
                },
                .undef => if (maybe_params) |params_expr| {
                    checkUndefExpr(a, file, params_expr);
                } else {
                    const token = insn_tokens[insn_handle];
                    a.recordError(file.handle, token, "Expected at least one .def symbol name", .{});
                },

                .db, .dw, .dd,
                .push, .pop,


                .def, .section, .stack,
                .code, .kcode, .entry, .kentry,
                .data, .kdata, .@"const", .kconst,
                => {}
            }

            // TODO check that directives have the expected number and types of params
            // TODO check that instructions do not appear in data or stack sections
            // TODO check that data directives do not appear in code sections
            // TODO check for shadowed symbols
        }
    }
}

fn instructionHasLayoutDependentParams(expr_flags: []const Expression.FlagSet, expr_infos: []const Expression.Info, params_handle: Expression.Handle) bool {
    if (expr_flags[params_handle].contains(.constant_depends_on_layout)) return true;

    switch (expr_infos[params_handle]) {
        .list, .arrow_list => |bin| {
            return instructionHasLayoutDependentParams(expr_flags, expr_infos, bin.left)
                or instructionHasLayoutDependentParams(expr_flags, expr_infos, bin.right);
        },
        else => {},
    }
    return false;
}

fn checkUndefExpr(a: *Assembler, file: *const SourceFile, expr: Expression.Handle) void {
    _ = expr;
    _ = a;
    _ = file;
}

fn getParamHandles(
    a: *Assembler,
    file_handle: SourceFile.Handle,
    maybe_params_expr: ?Expression.Handle,
    expr_infos: []const Expression.Info,
    expr_tokens: []const lex.Token.Handle,
    out: []?Expression.Handle,
) void {
    if (out.len == 0) return;
    if (maybe_params_expr) |params_expr| {
        var expr_handle = params_expr;
        for (out) |*p| {
            switch (expr_infos[expr_handle]) {
                .list => |bin| {
                    p.* = bin.left;
                    expr_handle = bin.right;
                },
                .arrow_list => {
                    a.recordError(file_handle, expr_tokens[expr_handle], "Expected ',' as parameter separator", .{});
                    return;
                },
                else => {
                    p.* = expr_handle;
                    return;
                }
            }
        }
        a.recordError(file_handle, expr_tokens[expr_handle], "Too many parameters", .{});
    }
}