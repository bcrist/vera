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
    instruction: InstructionRef,
    expression: Expression.Handle, // always in the same file where it's being referenced
    stack: StackRef,

    // If this is an instruction target, it must be in the same file as `s`
    pub fn getInstructionHandle(self: SymbolTarget, s: SourceFile.Slices) Instruction.Handle {
        return switch (self) {
            .expression => |expr_handle| s.file.findInstructionByExpr(expr_handle),
            .stack => |stack_ref| stack_ref.instruction,
            .instruction => |insn_ref| i: {
                std.debug.assert(insn_ref.file == s.file.handle);
                break :i insn_ref.instruction;
            },
            .not_found => unreachable,
        };
    }
};

pub const StackRef = struct {
    additional_sp_offset: u32,
    instruction: Instruction.Handle,
    stack_block_name: []const u8,
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
    const block_handle = file.findBlockByToken(symbol_token_handle);

    var def_symbol_definition: ?Expression.Handle = null;
    var maybe_stack_ref: ?StackRef = null;
    var block_iter = s.blockInstructions(block_handle);
    while (block_iter.next()) |insn_handle| {
        if (insn_handle >= insn_containing_symbol_expression) break;

        const op = operations[insn_handle];
        switch (op) {
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
            .push, .pop => {
                const expr_infos = s.expr.items(.info);
                var maybe_expr_handle = s.insn.items(.params)[insn_handle];
                while (maybe_expr_handle) |expr_handle| switch (expr_infos[expr_handle]) {
                    .list => |bin| {
                        maybe_stack_ref = processPushOrPop(a, s, op, maybe_stack_ref, bin.left, symbol);
                        maybe_expr_handle = bin.right;
                    },
                    else => {
                        maybe_stack_ref = processPushOrPop(a, s, op, maybe_stack_ref, expr_handle, symbol);
                        maybe_expr_handle = null;
                    }
                };
            },
            else => {},
        }
    }

    // 1. .def symbols
    if (def_symbol_definition) |expr_handle| {
        return .{ .expression = expr_handle };
    }

    // 2. private labels
    if (std.mem.startsWith(u8, symbol, "_")) {
        const private_labels = s.block.items(.labels)[block_handle];
        if (private_labels.get(symbol)) |insn_handle| {
            return .{ .instruction = .{
                .file = s.file.handle,
                .instruction = insn_handle,
            }};
        }
    }

    // 3. stack labels from .pushed contexts
    if (maybe_stack_ref) |stack_ref| {
        return .{ .stack = stack_ref };
    }

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

pub fn computePushOrPopSize(a: *Assembler, s: SourceFile.Slices, maybe_stack_name_list: ?Expression.Handle) u32 {
    const expr_infos = s.expr.items(.info);

    var result: u32 = 0;

    var maybe_expr_handle = maybe_stack_name_list;
    while (maybe_expr_handle) |expr_handle| switch (expr_infos[expr_handle]) {
        .list => |bin| {
            result += computeStackSize(a, s, bin.left);
            maybe_expr_handle = bin.right;
        },
        else => {
            result += computeStackSize(a, s, expr_handle);
            maybe_expr_handle = null;
        }
    };

    return result;
}
fn computeStackSize(a: *Assembler, s: SourceFile.Slices, expr_handle: Expression.Handle) u32 {
    const constant = parseSymbol(a, s, expr_handle);
    if (s.file.stacks.get(constant.asString())) |stack_block_handle| {
        const block_insns = s.blockInstructions(stack_block_handle);
        if (block_insns.begin != block_insns.end) {
            const insn_handle = block_insns.end - 1;
            const address = s.insn.items(.address)[insn_handle];
            const length = s.insn.items(.length)[insn_handle];
            return @intCast(u32, std.mem.alignForward(address + length, 2));
        }
    }
    return 0;
}

fn processPushOrPop(a: *Assembler, s: SourceFile.Slices, op: Instruction.OperationType, maybe_stack_ref: ?StackRef, expr_handle: Expression.Handle, symbol: []const u8) ?StackRef {
    if (maybe_stack_ref) |stack_ref| {
        var offset = stack_ref.additional_sp_offset;
        const stack_size = computeStackSize(a, s, expr_handle);
        if (op == .pop) {
            const stack_block_name = parseSymbol(a, s, expr_handle).asString();
            if (std.mem.eql(u8, stack_ref.stack_block_name, stack_block_name)) {
                // this was the push that contains our symbol
                return null;
            }

            if (offset >= stack_size) {
                offset -= stack_size;
            } else {
                offset = 0;
            }
        } else {
            offset += stack_size;
        }
        return .{
            .additional_sp_offset = offset,
            .instruction = stack_ref.instruction,
            .stack_block_name = stack_ref.stack_block_name,
        };
    }

    if (op == .push) {
        const stack_block_name = parseSymbol(a, s, expr_handle).asString();
        if (s.file.stacks.get(stack_block_name)) |stack_block_handle| {
            const stack_labels = s.block.items(.labels)[stack_block_handle];
            if (stack_labels.get(symbol)) |insn_handle| {
                return .{
                    .additional_sp_offset = 0,
                    .instruction = insn_handle,
                    .stack_block_name = stack_block_name,
                };
            }
        }
    }

    return maybe_stack_ref;
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
