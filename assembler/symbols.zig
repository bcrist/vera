const std = @import("std");
const lex = @import("lex.zig");
const typechecking = @import("typechecking.zig");
const Assembler = @import("Assembler.zig");
const SourceFile = @import("SourceFile.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const Constant = @import("Constant.zig");

pub const SymbolTarget = union(enum) {
    not_found,
    expression: Expression.Handle, // always in the same file where it's being referenced
    instruction: InstructionRef,

    // If this is an instruction target, it must be in the same file as `s`
    pub fn getInstructionHandle(self: SymbolTarget, s: SourceFile.Slices) Instruction.Handle {
        return switch (self) {
            .expression => |expr_handle| s.file.findInstructionByExpr(expr_handle),
            .instruction => |insn_ref| i: {
                std.debug.assert(insn_ref.file == s.file.handle);
                break :i insn_ref.instruction;
            },
            .not_found => unreachable,
        };
    }
};

pub const InstructionRef = struct {
    file: SourceFile.Handle,
    instruction: Instruction.Handle,
};

pub fn parseSymbol(a: *Assembler, s: SourceFile.Slices, expr_handle: Expression.Handle) *const Constant {
    switch (s.expr.items(.info)[expr_handle]) {
        .literal_symbol_def, .literal_symbol_ref => {
            const file = s.file;
            const token_handle = s.expr.items(.token)[expr_handle];
            const literal = file.tokens.get(token_handle).location(file.source);
            const constant = Constant.initSymbolLiteral(a.gpa, &a.constant_temp, literal);
            return constant.intern(a.arena, a.gpa, &a.constants);
        },
        .directive_symbol_def, .directive_symbol_ref => |inner_expr| {
            var expr_resolved_constants = s.expr.items(.resolved_constant);
            if (expr_resolved_constants[inner_expr]) |interned_symbol_name| {
                return interned_symbol_name;
            } else if (typechecking.tryResolveExpressionType(a, s, inner_expr)) {
                return expr_resolved_constants[inner_expr].?;
            } else unreachable;
        },
        else => unreachable,
    }
}

pub fn lookupSymbol(a: *Assembler, s: SourceFile.Slices, symbol_token_handle: lex.Token.Handle, symbol: []const u8) SymbolTarget {
    const file = s.file;
    const operations = s.insn.items(.operation);
    const resolved_constants = s.expr.items(.resolved_constant);

    const insn_containing_symbol_expression = file.findInstructionByToken(symbol_token_handle);

    // 1. .def symbols
    var def_symbol_definition: ?Expression.Handle = null;
    const block_handle = file.findBlockByToken(symbol_token_handle);
    var block_iter = file.blockInstructions(block_handle);
    while (block_iter.next()) |insn_handle| {
        if (insn_handle >= insn_containing_symbol_expression) break;

        switch (operations[insn_handle]) {
            .def => {
                const params_expr = s.insn.items(.params)[insn_handle].?;
                const symbol_and_definition = s.expr.items(.info)[params_expr].list;
                const def_symbol_expr = symbol_and_definition.left;
                const def_symbol = resolved_constants[def_symbol_expr].?;
                if (std.mem.eql(u8, symbol, def_symbol.asString())) {
                    def_symbol_definition = symbol_and_definition.right;
                }
            },
            .undef => if (def_symbol_definition) |_| {
                if (s.insn.items(.params)[insn_handle]) |params_expr| {
                    if (undefListContainsSymbol(a, s, params_expr, symbol)) {
                        def_symbol_definition = null;
                    }
                }
            },
            else => {},
        }
    }
    if (def_symbol_definition) |expr_handle| {
        return .{ .expression = expr_handle };
    }

    // 2. private labels
    if (std.mem.startsWith(u8, symbol, "_")) {
        if (findPrivateLabel(s, block_handle, symbol)) |insn_handle| {
            return .{ .instruction = .{
                .file = s.file.handle,
                .instruction = insn_handle,
            }};
        }
    }

    // 3. stack labels from .pushed contexts
    // TODO stack sections

    // 4. .local symbols
    if (s.file.locals.get(symbol)) |local| {
        return local;
    }

    // 5. public labels
    if (a.public_labels.get(symbol)) |insn_ref| {
        return .{ .instruction = insn_ref };
    }

    return .{ .not_found = {} };
}

pub fn findPrivateLabel(s: SourceFile.Slices, block_handle: SourceFile.SectionBlock.Handle, symbol: []const u8) ?Instruction.Handle {
    const labels = s.insn.items(.label);
    const resolved_constants = s.expr.items(.resolved_constant);
    var block_iter = s.file.blockInstructions(block_handle);
    while (block_iter.next()) |insn_handle| {
        if (labels[insn_handle]) |label_expr| {
            const label_constant = resolved_constants[label_expr] orelse unreachable;
            if (std.mem.eql(u8, symbol, label_constant.asString())) {
                return insn_handle;
            }
        }
    }
    return null;
}

fn undefListContainsSymbol(a: *Assembler, s: SourceFile.Slices, expr_handle: Expression.Handle, symbol: []const u8) bool {
    switch (s.expr.items(.info)[expr_handle]) {
        .list => |bin| {
            return undefListContainsSymbol(a, s, bin.left, symbol)
                or undefListContainsSymbol(a, s, bin.right, symbol);
        },
        .literal_symbol_def => {
            const token_handle = s.expr.items(.token)[expr_handle];
            const raw_symbol = s.file.tokens.get(token_handle).location(s.file.source);
            const undef_symbol = Constant.initSymbolLiteral(a.gpa, &a.constant_temp, raw_symbol);
            return std.mem.eql(u8, symbol, undef_symbol.asString());
        },
        .directive_symbol_def => |literal_expr| {
            const undef_symbol = s.expr.items(.resolved_constant)[literal_expr].?;
            return std.mem.eql(u8, symbol, undef_symbol.asString());
        },
        else => unreachable,
    }
}
