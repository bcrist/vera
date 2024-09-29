pub const Symbol_Target = union (enum) {
    not_found,
    instruction: Instruction_Ref,
    expression: Expression.Handle, // always in the same file where it's being referenced
    stack: Stack_Ref,

    // If this is an instruction target, it must be in the same file as `s`
    pub fn instruction_handle(self: Symbol_Target, s: Source_File.Slices) Instruction.Handle {
        return switch (self) {
            .expression => |expr_handle| s.file.find_instruction_by_expr(expr_handle),
            .stack => |stack_ref| stack_ref.instruction,
            .instruction => |insn_ref| i: {
                std.debug.assert(insn_ref.file == s.file.handle);
                break :i insn_ref.instruction;
            },
            .not_found => unreachable,
        };
    }
};

pub const Stack_Ref = struct {
    additional_sp_offset: u32,
    instruction: Instruction.Handle,
    stack_block_name: []const u8,
};

pub const Instruction_Ref = struct {
    file: Source_File.Handle,
    instruction: Instruction.Handle,
};

pub fn parse_symbol(a: *Assembler, s: Source_File.Slices, expr_handle: Expression.Handle) *const Constant {
    switch (s.expr.items(.info)[expr_handle]) {
        .literal_symbol_def, .literal_symbol_ref => {
            const file = s.file;
            const token_handle = s.expr.items(.token)[expr_handle];
            const literal = file.tokens.get(token_handle).location(file.source);
            const constant = Constant.init_symbol_literal(a.gpa, &a.constant_temp, literal);
            return constant.intern(a.arena, a.gpa, &a.constants);
        },
        .directive_symbol_def, .directive_symbol_ref => |inner_expr| {
            const expr_resolved_constants = s.expr.items(.resolved_constant);
            if (expr_resolved_constants[inner_expr]) |interned_symbol_name| {
                return interned_symbol_name;
            } else if (typechecking.try_resolve_expr_type(a, s, inner_expr)) {
                return expr_resolved_constants[inner_expr].?;
            } else unreachable;
        },
        else => unreachable,
    }
}

pub fn lookup_symbol(a: *Assembler, s: Source_File.Slices, symbol_token_handle: lex.Token.Handle, symbol: []const u8, comptime check_ambiguity: bool) Symbol_Target {
    const file = s.file;
    const operations = s.insn.items(.operation);
    const resolved_constants = s.expr.items(.resolved_constant);

    const insn_containing_symbol_expression = file.find_instruction_by_token(symbol_token_handle);
    const block_handle = file.find_block_by_token(symbol_token_handle);

    // Look for lexically scoped symbols (.def expressions and stack labels)
    var def_symbol_definition: ?Expression.Handle = null;
    var maybe_stack_ref: ?Stack_Ref = null;
    var block_iter = s.block_instructions(block_handle);
    var dupe_strategy: Stack_Label_Dupe_Strategy = if (check_ambiguity) .report_both else .skip_check;
    while (block_iter.next()) |insn_handle| {
        if (insn_handle >= insn_containing_symbol_expression) break;

        const op = operations[insn_handle];
        switch (op) {
            .def => {
                const params_expr = s.insn.items(.params)[insn_handle].?;
                const symbol_and_definition = s.expr.items(.info)[params_expr].list;
                const def_symbol_expr = symbol_and_definition.left;
                const def_symbol = resolved_constants[def_symbol_expr].?;
                if (std.mem.eql(u8, symbol, def_symbol.as_string())) {
                    def_symbol_definition = symbol_and_definition.right;
                }
            },
            .undef => if (def_symbol_definition) |_| {
                if (s.insn.items(.params)[insn_handle]) |params_expr| {
                    if (undef_list_contains_symbol(a, s, params_expr, symbol)) {
                        def_symbol_definition = null;
                    }
                }
            },
            .push, .pop => {
                const expr_infos = s.expr.items(.info);
                var maybe_expr_handle = s.insn.items(.params)[insn_handle];
                while (maybe_expr_handle) |expr_handle| switch (expr_infos[expr_handle]) {
                    .list => |bin| {
                        maybe_stack_ref = process_push_or_pop(a, s, op, maybe_stack_ref, bin.left, symbol_token_handle, symbol, &dupe_strategy);
                        maybe_expr_handle = bin.right;
                    },
                    else => {
                        maybe_stack_ref = process_push_or_pop(a, s, op, maybe_stack_ref, expr_handle, symbol_token_handle, symbol, &dupe_strategy);
                        maybe_expr_handle = null;
                    }
                };
            },
            else => {},
        }
    }

    const maybe_private_label = if (std.mem.startsWith(u8, symbol, "_")) s.block.items(.labels)[block_handle].get(symbol) else null;
    const maybe_local = s.file.locals.get(symbol);
    const maybe_public = a.public_labels.get(symbol);

    if (check_ambiguity) {
        var num_matches: u32 = 0;
        if (def_symbol_definition != null) num_matches += 1;
        if (maybe_private_label != null) num_matches += 1;
        if (maybe_stack_ref != null) num_matches += 1;
        if (maybe_local != null) num_matches += 1;

        if (maybe_public) |insn_ref| {
            if (insn_ref.file != s.file.handle) {
                num_matches += 1;
            }
        }

        if (num_matches > 1) {
            const insn_line_numbers = s.insn.items(.line_number);

            if (def_symbol_definition) |expr_handle| {
                const insn_handle = s.file.find_instruction_by_expr(expr_handle);
                const line_number = insn_line_numbers[insn_handle];
                a.record_token_error_fmt(s.file.handle, symbol_token_handle, "Ambiguous symbol; could be .def symbol from line {}", .{ line_number }, .{});
            }

            if (maybe_private_label) |insn_handle| {
                const line_number = insn_line_numbers[insn_handle];
                a.record_token_error_fmt(s.file.handle, symbol_token_handle, "Ambiguous symbol; could be private label from line {}", .{ line_number }, .{});
            }

            if (maybe_stack_ref) |stack_ref| {
                if (dupe_strategy == .report_both) {
                    const line_number = insn_line_numbers[stack_ref.instruction];
                    a.record_token_error_fmt(s.file.handle, symbol_token_handle, "Ambiguous symbol; could be stack label from line {}", .{ line_number }, .{});
                }
            }

            if (maybe_local) |target| switch (target) {
                .not_found, .stack => unreachable,
                .instruction => |insn_ref| {
                    const line_number = insn_line_numbers[insn_ref.instruction];
                    a.record_token_error_fmt(s.file.handle, symbol_token_handle, "Ambiguous symbol; could be file-local label from line {}", .{ line_number }, .{});
                },
                .expression => |expr_handle| {
                    const insn_handle = s.file.find_instruction_by_expr(expr_handle);
                    const line_number = insn_line_numbers[insn_handle];
                    a.record_token_error_fmt(s.file.handle, symbol_token_handle, "Ambiguous symbol; could be .local symbol from line {}", .{ line_number }, .{});
                },
            };

            if (maybe_public) |insn_ref| {
                if (insn_ref.file != s.file.handle) {
                    const target_file = a.get_source(insn_ref.file);
                    const line_number = target_file.instructions.items(.line_number)[insn_ref.instruction];
                    a.record_token_error_fmt(s.file.handle, symbol_token_handle, "Ambiguous symbol; could be public label from line {} in {s}", .{ line_number, target_file.name }, .{});
                }
            }
        }
    }

    return if (def_symbol_definition) |expr_handle| .{ .expression = expr_handle }
        else if (maybe_private_label) |insn_handle| .{ .instruction = .{
            .file = s.file.handle,
            .instruction = insn_handle,
        }}
        else if (maybe_stack_ref) |stack_ref| .{ .stack = stack_ref }
        else if (maybe_local) |local| local
        else if (maybe_public) |insn_ref| .{ .instruction = insn_ref }
        else .not_found
        ;
}

pub fn compute_push_or_pop_size(a: *Assembler, s: Source_File.Slices, maybe_stack_name_list: ?Expression.Handle) u32 {
    const expr_infos = s.expr.items(.info);

    var result: u32 = 0;

    var maybe_expr_handle = maybe_stack_name_list;
    while (maybe_expr_handle) |expr_handle| switch (expr_infos[expr_handle]) {
        .list => |bin| {
            result += compute_stack_size(a, s, bin.left);
            maybe_expr_handle = bin.right;
        },
        else => {
            result += compute_stack_size(a, s, expr_handle);
            maybe_expr_handle = null;
        }
    };

    return result;
}
fn compute_stack_size(a: *Assembler, s: Source_File.Slices, expr_handle: Expression.Handle) u32 {
    const constant = parse_symbol(a, s, expr_handle);
    if (s.file.stacks.get(constant.as_string())) |stack_block_handle| {
        const block_insns = s.block_instructions(stack_block_handle);
        if (block_insns.begin != block_insns.end) {
            const insn_handle = block_insns.end - 1;
            const address = s.insn.items(.address)[insn_handle];
            const length = s.insn.items(.length)[insn_handle];
            return std.mem.alignForward(u32, address + length, 2);
        }
    }
    return 0;
}

const Stack_Label_Dupe_Strategy = enum {
    skip_check,
    report_both,
    report_dupe_only,
};
fn process_push_or_pop(a: *Assembler, s: Source_File.Slices, op: Instruction.Operation_Type, maybe_stack_ref: ?Stack_Ref, expr_handle: Expression.Handle, symbol_token_handle: lex.Token.Handle, symbol: []const u8, dupe_strategy: *Stack_Label_Dupe_Strategy) ?Stack_Ref {
    if (maybe_stack_ref) |stack_ref| {
        var offset = stack_ref.additional_sp_offset;
        const stack_size = compute_stack_size(a, s, expr_handle);
        if (op == .pop) {
            const stack_block_name = parse_symbol(a, s, expr_handle).as_string();
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
            if (dupe_strategy.* != .skip_check) {
                const stack_block_name = parse_symbol(a, s, expr_handle).as_string();
                if (s.file.stacks.get(stack_block_name)) |stack_block_handle| {
                    const stack_labels = s.block.items(.labels)[stack_block_handle];
                    if (stack_labels.get(symbol)) |insn_handle| {
                        const insn_line_numbers = s.insn.items(.line_number);

                        if (dupe_strategy.* == .report_both) {
                            const line_number = insn_line_numbers[stack_ref.instruction];
                            a.record_token_error_fmt(s.file.handle, symbol_token_handle, "Ambiguous symbol; could be stack label from line {}", .{ line_number }, .{});

                            dupe_strategy.* = .report_dupe_only;
                        }

                        const line_number = insn_line_numbers[insn_handle];
                        a.record_token_error_fmt(s.file.handle, symbol_token_handle, "Ambiguous symbol; could be stack label from line {}", .{ line_number }, .{});
                    }
                }
            }
            offset += stack_size;
        }
        return .{
            .additional_sp_offset = offset,
            .instruction = stack_ref.instruction,
            .stack_block_name = stack_ref.stack_block_name,
        };
    }

    if (op == .push) {
        const stack_block_name = parse_symbol(a, s, expr_handle).as_string();
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

fn undef_list_contains_symbol(a: *Assembler, s: Source_File.Slices, expr_handle: Expression.Handle, symbol: []const u8) bool {
    switch (s.expr.items(.info)[expr_handle]) {
        .list => |bin| {
            return undef_list_contains_symbol(a, s, bin.left, symbol)
                or undef_list_contains_symbol(a, s, bin.right, symbol);
        },
        .literal_symbol_def => {
            const token_handle = s.expr.items(.token)[expr_handle];
            const raw_symbol = s.file.tokens.get(token_handle).location(s.file.source);
            const undef_symbol = Constant.init_symbol_literal(a.gpa, &a.constant_temp, raw_symbol);
            return std.mem.eql(u8, symbol, undef_symbol.as_string());
        },
        .directive_symbol_def => |literal_expr| {
            const undef_symbol = s.expr.items(.resolved_constant)[literal_expr].?;
            return std.mem.eql(u8, symbol, undef_symbol.as_string());
        },
        else => unreachable,
    }
}

const typechecking = @import("typechecking.zig");
const Assembler = @import("Assembler.zig");
const Source_File = @import("Source_File.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const Constant = @import("Constant.zig");
const lex = isa.lex;
const isa = @import("isa");
const std = @import("std");
