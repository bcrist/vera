const std = @import("std");
const lex = @import("lex.zig");
const ie = @import("instruction_encoding");
const Assembler = @import("Assembler.zig");
const SourceFile = @import("SourceFile.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const Constant = @import("Constant.zig");
const Section = @import("Section.zig");
const Token = lex.Token;
const ExpressionType = ie.ExpressionType;
const SectionBlock = SourceFile.SectionBlock;

// Set the resolved type for labels to .symbol_def.
// The resolved constant for these expressions is the raw symbol name.
// Any labels which do not begin with `_` and are not in a stack section will be added to the global symbol table.
// Create Sections for each named section block and set the SectionBlock.section handle to reference it.
pub fn processLabelsAndSections(a: *Assembler, file: *SourceFile, file_handle: SourceFile.Handle) void {
    var insn_tokens = file.instructions.items(.token);
    var labels = file.instructions.items(.label);
    var operations = file.instructions.items(.operation);
    var params = file.instructions.items(.params);

    var expr_resolved_types = file.expressions.items(.resolved_type);
    var expr_resolved_constants = file.expressions.items(.resolved_constant);
    var expr_infos = file.expressions.items(.info);
    var expr_tokens = file.expressions.items(.token);

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

                const symbol_constant = resolveSymbolDefExpr(a, file, file_handle, section_name_expr, expr_tokens, expr_infos, expr_resolved_constants, expr_resolved_types);
                const section_name = symbol_constant.asString();
                const entry = a.sections.getOrPutValue(a.gpa, section_name, .{
                    .name = section_name,
                    .kind = kind,
                }) catch @panic("OOM");
                const found_kind = entry.value_ptr.kind;
                if (found_kind != kind) switch (found_kind) {
                    inline else => |k| a.recordError(file_handle, insn_tokens[insn_handle], "Section already exists; expected ." ++ @tagName(k)),
                };
                const block_handle = file.findBlockByInstruction(@intCast(Instruction.Handle, insn_handle));
                block_sections[block_handle] = @intCast(Section.Handle, entry.index);
            } else {
                a.recordError(file_handle, insn_tokens[insn_handle], "Unnamed sections are not currently supported");
            }
        }

        if (maybe_label_expr) |label_expr| {
            const symbol_constant = resolveSymbolDefExpr(a, file, file_handle, label_expr, expr_tokens, expr_infos, expr_resolved_constants, expr_resolved_types);
            const symbol = symbol_constant.asString();
            if (!in_stack_section and !std.mem.startsWith(u8, symbol, "_")) {
                a.symbols.put(a.gpa, symbol, .{
                    .file = file_handle,
                    .instruction = @intCast(Instruction.Handle, insn_handle),
                }) catch @panic("OOM");
            }
        }
    }
}

fn resolveSymbolDefExpr(
    a: *Assembler,
    file: *SourceFile,
    file_handle: SourceFile.Handle,
    symbol_def_expr: Expression.Handle,
    expr_tokens: []const lex.Token.Handle,
    expr_infos: []const Expression.Info,
    expr_resolved_constants: []?*const Constant,
    expr_resolved_types: []ExpressionType,
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
            } else if (tryResolveExpressionType(a, file, file_handle, inner_expr, expr_tokens, expr_infos, expr_resolved_constants, expr_resolved_types)) {
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

        for (a.files.items, 0..) |*file, file_handle| {
            var expr_resolved_types = file.expressions.items(.resolved_type);
            var expr_resolved_constants = file.expressions.items(.resolved_constant);
            var expr_infos = file.expressions.items(.info);
            var expr_tokens = file.expressions.items(.token);

            for (expr_resolved_types, 0..) |expr_type, handle| {
                if (expr_type == .unknown) {
                    if (tryResolveExpressionType(a, file,
                        @intCast(SourceFile.Handle, file_handle),
                        @intCast(Expression.Handle, handle),
                        expr_tokens, expr_infos, expr_resolved_constants, expr_resolved_types,
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
        for (a.files.items, 0..) |*file, file_handle| {
            const resolved_types = file.expressions.items(.resolved_type);
            const tokens = file.expressions.items(.token);
            for (resolved_types, 0..) |expr_type, handle| {
                if (expr_type == .unknown) {
                    resolved_types[handle] = .{ .poison = {} };
                    a.recordError(@intCast(SourceFile.Handle, file_handle), tokens[handle], "Could not determine type for expression");
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
    file_handle: SourceFile.Handle,
    expr_handle: Expression.Handle,
    expr_tokens: []const lex.Token.Handle,
    expr_infos: []const Expression.Info,
    expr_resolved_constants: []?*const Constant,
    expr_resolved_types: []ExpressionType,
) bool {
    std.debug.assert(expr_resolved_types[expr_handle] == .unknown);

    switch (expr_infos[expr_handle]) {
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
                    a.recordError(file_handle, token_handle, "Integer literal too large");
                    expr_resolved_types[expr_handle] = .{ .poison = {} };
                },
                error.InvalidCharacter => {
                    a.recordError(file_handle, token_handle, "Invalid character in literal");
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
        .literal_symbol_ref => {
            const token_handle = expr_tokens[expr_handle];
            const symbol_literal = file.tokens.get(token_handle).location(file.source);
            const symbol_constant = Constant.initSymbolLiteral(a.gpa, &a.constant_temp, symbol_literal);
            const expr_type = tryResolveSymbolType(a, file, file_handle, token_handle, symbol_constant.asString(), expr_resolved_types);
            if (expr_type == .unknown) return false;
            expr_resolved_types[expr_handle] = expr_type;
        },
        .directive_symbol_ref => |inner_expr| {
            if (expr_resolved_constants[inner_expr]) |symbol| {
                const expr_type = tryResolveSymbolType(a, file, file_handle, expr_tokens[expr_handle], symbol.asString(), expr_resolved_types);
                if (expr_type == .unknown) return false;
                expr_resolved_types[expr_handle] = expr_type;
            } else return false;
        },
        .literal_symbol_def, .directive_symbol_def => {
            _ = resolveSymbolDefExpr(a, file, file_handle, expr_handle, expr_tokens, expr_infos, expr_resolved_constants, expr_resolved_types);
        },
    }
    return true;
}

fn tryResolveSymbolType(
    a: *Assembler,
    file: *const SourceFile,
    file_handle: SourceFile.Handle,
    token_handle: Token.Handle,
    symbol: []const u8,
    expr_resolved_types: []const ExpressionType,
) ExpressionType {
    if (a.lookupSymbol(file, file_handle, token_handle, symbol)) |target| switch (target) {
        .expression => |expr_handle| {
            return expr_resolved_types[expr_handle];
        },
        .instruction => |insn_ref| {
            const sym_file = a.getSource(insn_ref.file);
            const block_handle = sym_file.findBlockByInstruction(insn_ref.instruction);
            if (sym_file.blocks.items(.section)[block_handle]) |section_handle| {
                return switch (a.getSection(section_handle).kind) {
                    .code_user, .code_kernel, .entry_user, .entry_kernel => ExpressionType.relativeAddress(.insn, .{ .sr = .ip }),
                    .data_user, .data_kernel, .constant_user, .constant_kernel => ExpressionType.relativeAddress(.data, .{ .sr = .ip }),
                    .stack => ExpressionType.relativeAddress(.stack, .{ .sr = .sp }),
                    .info => ExpressionType.absoluteAddress(.data),
                } catch unreachable;
            } else {
                // Block does not have a section handle, so it's an .info section by default
                return ExpressionType.absoluteAddress(.data);
            }
        },
        .not_found => {
            a.recordError(file_handle, token_handle, "Reference to undefined symbol");
            return .{ .poison = {} };
        },
    };

    return .{ .unknown = {} };
}
