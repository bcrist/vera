gpa: std.mem.Allocator,
handle: Source_File.Handle,
out: *Source_File,
errors: *std.ArrayListUnmanaged(Error),
next_token: Token.Handle,
sync_to_end_of_line: bool,
token_kinds: []Token_Kind,
token_offsets: []u32,

pub fn init(
    gpa: std.mem.Allocator,
    handle: Source_File.Handle,
    out: *Source_File,
    errors: *std.ArrayListUnmanaged(Error),
) Parser {
    return .{
        .gpa = gpa,
        .handle = handle,
        .out = out,
        .errors = errors,
        .next_token = 0,
        .sync_to_end_of_line = false,
        .token_kinds = out.tokens.items(.kind),
        .token_offsets = out.tokens.items(.offset),
    };
}

pub fn parse_instruction(self: *Parser) bool {
    const label_token = self.next_token;
    const label = self.parse_label();
    self.skip_linespace();
    self.sync_to_end_of_line = false;

    if (self.parse_directive()) |directive| {
        const directive_token = self.next_token - 1;
        switch (directive) {
            .def, .local => {
                if (self.parse_symbol_def()) |symbol| {
                    if (self.parse_expr()) |expr| {
                        const params = self.add_binary_expression(.list, directive_token, symbol, expr);
                        self.add_instruction(label, directive_token, switch (directive) {
                            .def => .def,
                            .local => .local,
                            else => unreachable,
                        }, params);
                    } else {
                        self.record_error("Expected expression for symbol definition");
                        self.sync_to_end_of_line = true;
                    }
                } else {
                    self.record_error("Expected symbol name");
                    self.sync_to_end_of_line = true;
                }
            },
            .undef => self.add_instruction(label, directive_token, .undef, self.parse_symbol_def_list()),

            .push => self.add_instruction(label, directive_token, .{ .push = 0 }, self.parse_symbol_def_list()),
            .pop => self.add_instruction(label, directive_token, .{ .pop = 0 }, self.parse_symbol_def_list()),

            .section  => self.add_instruction(label, directive_token, .section, self.parse_symbol_def()),
            .boot     => self.add_instruction(label, directive_token, .boot, self.parse_symbol_def()),
            .code     => self.add_instruction(label, directive_token, .code, self.parse_symbol_def()),
            .kcode,   => self.add_instruction(label, directive_token, .kcode, self.parse_symbol_def()),
            .entry    => self.add_instruction(label, directive_token, .entry, self.parse_symbol_def()),
            .kentry   => self.add_instruction(label, directive_token, .kentry, self.parse_symbol_def()),
            .data     => self.add_instruction(label, directive_token, .data, self.parse_symbol_def()),
            .kdata    => self.add_instruction(label, directive_token, .kdata, self.parse_symbol_def()),
            .@"const" => self.add_instruction(label, directive_token, .@"const", self.parse_symbol_def()),
            .kconst   => self.add_instruction(label, directive_token, .kconst, self.parse_symbol_def()),
            .stack    => self.add_instruction(label, directive_token, .stack, self.parse_symbol_def()),

            .none, .insn, .bound_insn => unreachable,

            inline else => |d| {
                const params = self.parse_expr_list(false);
                const op = @unionInit(Instruction.Operation, @tagName(d), {});
                self.add_instruction(label, directive_token, op, params);
            },
        }
    } else if (parse_helpers.parse_mnemonic(self)) |mnemonic| {
        const mnemonic_token = self.next_token - 1;
        var swap_params = false;
        const suffix = parse_helpers.parse_suffix(self, &swap_params);
        const params = self.parse_expr_list(swap_params);
        self.add_instruction(label, mnemonic_token, .{ .insn = .{
            .mnemonic = mnemonic,
            .suffix = suffix,
        }}, params);
    } else if (label) |_| {
        self.add_instruction(label, label_token, .none, null);
    }
    self.skip_linespace();
    _ = self.try_token(.comment);
    while (true) {
        if (self.try_token(.newline)) return true;
        if (self.try_token(.eof)) return false;
        if (!self.sync_to_end_of_line) {
            self.record_error("Expected end of line");
            self.sync_to_end_of_line = true;
        }
        self.next_token += 1;
    }
}

fn add_instruction(self: *Parser, label: ?Expression.Handle, token: Token.Handle, op: Instruction.Operation, params: ?Expression.Handle) void {
    var instructions = &self.out.instructions;
    var start_token: Token.Handle = 0;
    var start_line: u32 = 1;
    if (instructions.len > 0) {
        const prev_insn = instructions.get(instructions.len - 1);
        start_token = prev_insn.token;
        start_line = prev_insn.line_number;
    }
    while (start_token < token) : (start_token += 1) {
        const t = Token{
            .offset = self.token_offsets[start_token],
            .kind = self.token_kinds[start_token],
        };
        start_line += t.count_new_lines(self.out.source);
    }
    instructions.append(self.gpa, .{
        .label = label,
        .token = token,
        .operation = op,
        .params = params,
        .line_number = start_line,
    }) catch @panic("OOM");
}

fn parse_expr_list(self: *Parser, swap_params: bool) ?Expression.Handle {
    const begin = self.next_token;
    self.skip_linespace();
    if (self.try_token(.arrow)) {
        const token = self.next_token - 1;
        const lhs = self.add_typed_terminal_expression(.arrow, token, .arrow);
        if (self.parse_expr_list(false)) |rhs| {
            return self.add_binary_expression(.list, token, lhs, rhs);
        } else {
            return lhs;
        }
    } else if (self.parse_expr()) |lhs| {
        self.skip_linespace();
        if (self.try_token(.comma) or self.try_token(.arrow)) {
            const token = self.next_token - 1;
            if (self.parse_expr_list(false)) |rhs| {
                switch (self.token_kinds[token]) {
                    .comma => if (swap_params) {
                        return self.add_binary_expression(.list, token, rhs, lhs);
                    } else {
                        return self.add_binary_expression(.list, token, lhs, rhs);
                    },
                    .arrow => {
                        const arrow = self.add_typed_terminal_expression(.arrow, token, .arrow);
                        const arrow_rhs = self.add_binary_expression(.list, token, arrow, rhs);
                        return self.add_binary_expression(.list, token, lhs, arrow_rhs);
                    },
                    else => unreachable,
                }
            } else if (self.token_kinds[token] == .arrow) {
                const rhs = self.add_typed_terminal_expression(.arrow, token, .arrow);
                return self.add_binary_expression(.list, token, lhs, rhs);
            } else {
                if (!self.sync_to_end_of_line) {
                    self.record_error("Expected expression");
                    self.sync_to_end_of_line = true;
                }
                return lhs;
            }
        } else {
            return lhs;
        }
    }
    self.next_token = begin;
    return null;
}

fn parse_expr(self: *Parser) ?Expression.Handle {
    return self.parse_expr_pratt(0);
}

const Operator_Info = struct {
    token: Token.Handle,
    left_bp: u8,
    right_bp: ?u8, // null for postfix operators
    expr: Expression.Kind,
};
fn parse_prefix_operator(self: *Parser) ?Operator_Info {
    const begin = self.next_token;
    self.skip_linespace();
    var t = self.next_token;
    const info: Operator_Info = switch (self.token_kinds[t]) {
        .minus => .{ .token = t, .left_bp = 1, .right_bp = 0xFF, .expr = .negate },
        .tilde => .{ .token = t, .left_bp = 1, .right_bp = 0xFF, .expr = .complement },
        .at    => .{ .token = t, .left_bp = 1, .right_bp = 0xFF, .expr = .absolute_address_cast },
        .dot => {
            t += 1;
            self.next_token = t;
            if (self.try_keyword("d")) {
                return .{ .token = t, .left_bp = 1, .right_bp = 1, .expr = .data_address_cast };
            } else if (self.try_keyword("i")) {
                return .{ .token = t, .left_bp = 1, .right_bp = 1, .expr = .insn_address_cast };
            } else if (self.try_keyword("s")) {
                return .{ .token = t, .left_bp = 1, .right_bp = 1, .expr = .stack_address_cast };
            } else if (self.try_keyword("raw")) {
                return .{ .token = t, .left_bp = 1, .right_bp = 1, .expr = .remove_address_cast };
            } else if (self.try_keyword("r")) {
                return .{ .token = t, .left_bp = 1, .right_bp = 1, .expr = .index_to_reg };
            } else if (self.try_keyword("idx")) {
                return .{ .token = t, .left_bp = 1, .right_bp = 1, .expr = .reg_to_index };
            } else if (self.try_keyword("crlf")) {
                return .{ .token = t, .left_bp = 1, .right_bp = 1, .expr = .crlf_cast };
            } else if (self.try_keyword("lf")) {
                return .{ .token = t, .left_bp = 1, .right_bp = 1, .expr = .lf_cast };
            } else {
                self.next_token = begin;
                return null;
            }
        },
        else => {
            self.next_token = begin;
            return null;
        },
    };
    self.next_token += 1;
    return info;
}
fn parse_operator(self: *Parser) ?Operator_Info {
    const begin = self.next_token;
    self.skip_linespace();
    var t = self.next_token;
    const info: Operator_Info = switch (self.token_kinds[t]) {
        .bar        => .{ .token = t, .left_bp = 10, .right_bp = 11, .expr = .bitwise_or },
        .caret      => .{ .token = t, .left_bp = 12, .right_bp = 13, .expr = .bitwise_xor },
        .amp        => .{ .token = t, .left_bp = 14, .right_bp = 15, .expr = .bitwise_and },
        .plus       => .{ .token = t, .left_bp = 20, .right_bp = 21, .expr = .plus },
        .minus      => .{ .token = t, .left_bp = 20, .right_bp = 21, .expr = .minus },
        .star       => .{ .token = t, .left_bp = 22, .right_bp = 23, .expr = .multiply },
        .shl        => .{ .token = t, .left_bp = 30, .right_bp = 31, .expr = .shl },
        .shr        => .{ .token = t, .left_bp = 30, .right_bp = 31, .expr = .shr },
        .plus_plus  => .{ .token = t, .left_bp = 40, .right_bp = 41, .expr = .concat },
        .star_star  => .{ .token = t, .left_bp = 42, .right_bp = 43, .expr = .concat_repeat },
        .apostrophe => .{ .token = t, .left_bp = 52, .right_bp = 53, .expr = .length_cast },
        .dot => info: {
            t += 1;
            self.next_token = t;
            defer self.next_token = t;
            if (self.try_keyword("zx")) {
                break :info .{ .token = t, .left_bp = 50, .right_bp = 51, .expr = .zero_extend };
            } else if (self.try_keyword("sx")) {
                break :info .{ .token = t, .left_bp = 50, .right_bp = 51, .expr = .sign_extend };
            } else if (self.try_keyword("trunc")) {
                break :info .{ .token = t, .left_bp = 50, .right_bp = 51, .expr = .truncate };
            } else if (self.try_keyword("signed")) {
                break :info .{ .token = t, .left_bp = 50, .right_bp = null, .expr = .signed_cast };
            } else if (self.try_keyword("unsigned")) {
                break :info .{ .token = t, .left_bp = 50, .right_bp = null, .expr = .unsigned_cast };
            } else if (self.try_keyword("without_signedness")) {
                break :info .{ .token = t, .left_bp = 50, .right_bp = null, .expr = .remove_signedness_cast };
            } else {
                t = begin;
                return null;
            }
        },
        else => {
            self.next_token = begin;
            return null;
        },
    };
    self.next_token += 1;
    return info;
}

fn parse_expr_pratt(self: *Parser, min_binding_power: u8) ?Expression.Handle {
    if (self.sync_to_end_of_line) return null;

    const before_prefix_operator = self.next_token;
    var expr = if (self.parse_prefix_operator()) |operator| e: {
        if (operator.left_bp < min_binding_power) {
            self.next_token = before_prefix_operator;
            return null;
        }
        if (self.parse_expr_pratt(operator.right_bp.?)) |right| {
            break :e self.add_unary_expression(operator.expr, operator.token, right);
        } else if (!self.sync_to_end_of_line) {
            self.record_error("Expected expression");
            self.sync_to_end_of_line = true;
        }
        return null;
    } else self.parse_atom() orelse return null;

    while (true) {
        if (self.sync_to_end_of_line) return null;
        const before_operator = self.next_token;
        if (self.parse_operator()) |operator| {
            if (operator.left_bp < min_binding_power) {
                self.next_token = before_operator;
                break;
            }
            if (operator.right_bp) |binding_power| {
                if (self.parse_expr_pratt(binding_power)) |right| {
                    expr = self.add_binary_expression(operator.expr, operator.token, expr, right);
                } else if (!self.sync_to_end_of_line) {
                    self.record_error("Expected expression");
                    self.sync_to_end_of_line = true;
                    return null;
                }
            } else {
                expr = self.add_unary_expression(operator.expr, operator.token, expr);
            }
        } else break;
    }

    return expr;
}

fn parse_atom(self: *Parser) ?Expression.Handle {
    return self.parse_paren_expr()
        orelse self.parse_int_literal()
        orelse self.parse_string_literal()
        orelse self.parse_register_literal()
        orelse self.parse_special_literal()
        orelse self.parse_symbol_ref()
        ;
}

fn parse_paren_expr(self: *Parser) ?Expression.Handle {
    const begin = self.next_token;
    self.skip_linespace();
    if (self.try_token(.paren_open)) {
        if (self.parse_expr()) |expr| {
            if (!self.try_token(.paren_close)) {
                if (!self.sync_to_end_of_line) {
                    self.record_error("Expected ')'");
                    self.sync_to_end_of_line = true;
                }
            }
            return expr;
        } else {
            if (!self.sync_to_end_of_line) {
                self.record_error("Expected expression");
                self.sync_to_end_of_line = true;
            }
            _ = self.try_token(.paren_close);
            return null;
        }
    }
    self.next_token = begin;
    return null;
}

fn parse_int_literal(self: *Parser) ?Expression.Handle {
    const begin = self.next_token;
    self.skip_linespace();
    if (self.try_token(.int_literal)) {
        return self.add_terminal_expression(.literal_int, self.next_token - 1);
    }
    self.next_token = begin;
    return null;
}

fn parse_string_literal(self: *Parser) ?Expression.Handle {
    const begin = self.next_token;
    self.skip_linespace();
    const literal_token = self.next_token;
    if (self.try_token(.str_literal)) {
        return self.add_terminal_expression(.literal_str, literal_token);
    } else if (self.try_token(.str_literal_raw)) {
        while (true) {
            const end = self.next_token;
            self.skip_linespace();
            if (self.try_token(.str_literal_raw)) continue;

            self.next_token = end;
            break;
        }
        return self.add_terminal_expression(.literal_str, literal_token);
    }
    self.next_token = begin;
    return null;
}

fn parse_register_literal(self: *Parser) ?Expression.Handle {
    const begin = self.next_token;
    self.skip_linespace();
    if (self.try_token(.id)) {
        const token = self.next_token - 1;
        const id = self.token_span(token);
        if (is_gpr_literal(id)) {
            return self.add_terminal_expression(.literal_reg, token);
        }
        if (id.len >= 2 and id.len <= 4) {
            var buf = [_]u8 {0} ** 4;
            const lower = std.ascii.lowerString(&buf, id);
            if (lower[1] == 'p') {
                if (std.mem.eql(u8, lower, "ip")) {
                    return self.add_typed_terminal_expression(.literal_reg, token, Expression.Type.sr(.ip));
                } else if (std.mem.eql(u8, lower, "sp")) {
                    return self.add_typed_terminal_expression(.literal_reg, token, Expression.Type.sr(.sp));
                } else if (std.mem.eql(u8, lower, "rp")) {
                    return self.add_typed_terminal_expression(.literal_reg, token, Expression.Type.sr(.rp));
                } else if (std.mem.eql(u8, lower, "bp")) {
                    return self.add_typed_terminal_expression(.literal_reg, token, Expression.Type.sr(.bp));
                }
            } else if (std.mem.eql(u8, lower, "uxp")) {
                return self.add_typed_terminal_expression(.literal_reg, token, Expression.Type.sr(.uxp));
            } else if (std.mem.eql(u8, lower, "kxp")) {
                return self.add_typed_terminal_expression(.literal_reg, token, Expression.Type.sr(.kxp));
            } else if (std.mem.eql(u8, lower, "stat")) {
                return self.add_typed_terminal_expression(.literal_reg, token, Expression.Type.sr(.stat));
            } else if (std.mem.eql(u8, lower, "asn")) {
                return self.add_typed_terminal_expression(.literal_reg, token, Expression.Type.sr(.asn));
            }
        }
    }
    self.next_token = begin;
    return null;
}

fn parse_special_literal(self: *Parser) ?Expression.Handle {
    const begin = self.next_token;
    self.skip_linespace();
    const token = self.next_token;
    if (self.try_token(.money)) {
        return self.add_terminal_expression(.literal_current_address, token);
    }
    self.next_token = begin;
    return null;
}

fn is_gpr_literal(id: []const u8) bool {
    if (id.len < 2) return false;
    switch (id[0]) {
        'r', 'R', 'x', 'X', 'b', 'B' => {
            const index = std.fmt.parseUnsigned(u4, id[1..], 10) catch return false;
            if (index != 0 and id[1] == '0') return false; // don't treat R01, X000010, etc. as register names
            return true;
        },
        else => return false,
    }
}

fn parse_label(self: *Parser) ?Expression.Handle {
    const begin = self.next_token;
    const begin_expression_len = self.out.expressions.len;

    const local = self.parse_local_label_directive();
    if (self.parse_symbol_def()) |expr| {
        self.skip_linespace();
        if (self.try_token(.colon)) {
            return if (local) self.add_unary_expression(.local_label_def, begin + 1, expr) else expr;
        }
    }
    self.out.expressions.len = begin_expression_len;
    self.next_token = begin;
    return null;
}

fn parse_local_label_directive(self: *Parser) bool {
    const begin = self.next_token;
    if (self.try_token(.dot) and self.try_token(.id)) {
        const directive_str = self.token_span(self.next_token - 1);
        if (std.mem.eql(u8, directive_str, "local")) {
            return true;
        }
    }
    self.next_token = begin;
    return false;
}

fn parse_symbol_def_list(self: *Parser) ?Expression.Handle {
    const begin = self.next_token;
    if (self.parse_symbol_def()) |lhs| {
        self.skip_linespace();
        const token = self.next_token;
        if (self.try_token(.comma)) {
            if (self.parse_symbol_def_list()) |rhs| {
                return self.add_binary_expression(.list, token, lhs, rhs);
            } else {
                if (!self.sync_to_end_of_line) {
                    self.record_error("Expected symbol");
                    self.sync_to_end_of_line = true;
                }
                return lhs;
            }
        } else {
            return lhs;
        }
    }
    self.next_token = begin;
    return null;
}

fn parse_symbol_def(self: *Parser) ?Expression.Handle {
    const begin = self.next_token;
    self.skip_linespace();
    if (self.try_token(.id)) {
        return self.add_terminal_expression(.literal_symbol_def, self.next_token - 1);
    } else if (self.try_token(.dot) and self.try_keyword("sym")) {
        const sym_token = self.next_token - 1;
        if (self.parse_string_literal()) |expr| {
            return self.add_unary_expression(.directive_symbol_def, sym_token, expr);
        } else {
            self.record_error("Expected constant expression");
            self.sync_to_end_of_line = true;
        }
    }
    self.next_token = begin;
    return null;
}
fn parse_symbol_ref(self: *Parser) ?Expression.Handle {
    const begin = self.next_token;
    self.skip_linespace();
    if (self.try_token(.id)) {
        return self.add_terminal_expression(.literal_symbol_ref, self.next_token - 1);
    } else if (self.try_token(.dot) and self.try_keyword("sym")) {
        const sym_token = self.next_token - 1;
        if (self.parse_string_literal()) |expr| {
            return self.add_unary_expression(.directive_symbol_ref, sym_token, expr);
        } else {
            self.record_error("Expected constant expression");
            self.sync_to_end_of_line = true;
        }
    }
    self.next_token = begin;
    return null;
}

fn try_keyword(self: *Parser, comptime kw: []const u8) bool {
    const begin = self.next_token;
    if (self.try_token(.id)) {
        const str = self.token_span(begin);
        if (str.len == kw.len) {
            var buf = [_]u8 {0} ** kw.len;
            if (std.mem.eql(u8, std.ascii.lowerString(&buf, str), kw)) {
                return true;
            }
        }
    }
    self.next_token = begin;
    return false;
}

fn add_typed_terminal_expression(self: *Parser, comptime kind: Expression.Kind, token: Token.Handle, expr_type: Expression.Type) Expression.Handle {
    const handle: Expression.Handle = @intCast(self.out.expressions.len);
    self.out.expressions.append(self.gpa, .{
        .token = token,
        .info = @unionInit(Expression.Info, @tagName(kind), {}),
        .resolved_type = expr_type,
        .resolved_constant = null,
        .flags = .{},
    }) catch @panic("OOM");
    return handle;
}

fn add_expression_info(self: *Parser, token: Token.Handle, info: Expression.Info) Expression.Handle {
    const handle: Expression.Handle = @intCast(self.out.expressions.len);
    self.out.expressions.append(self.gpa, .{
        .token = token,
        .info = info,
        .resolved_type = .unknown,
        .resolved_constant = null,
        .flags = .{},
    }) catch @panic("OOM");
    return handle;
}

fn add_terminal_expression(self: *Parser, kind: Expression.Kind, token: Token.Handle) Expression.Handle {
    return self.add_expression_info(token, switch (kind) {
        .arrow => .arrow,
        .literal_int => .literal_int,
        .literal_str => .literal_str,
        .literal_reg => .literal_reg,
        .literal_symbol_def => .literal_symbol_def,
        .literal_symbol_ref => .literal_symbol_ref,
        .literal_current_address => .literal_current_address,

        .list,
        .directive_symbol_def,
        .directive_symbol_ref,
        .local_label_def,
        .plus,
        .minus,
        .negate,
        .multiply,
        .shl,
        .shr,
        .concat,
        .concat_repeat,
        .bitwise_or,
        .bitwise_xor,
        .bitwise_and,
        .complement,
        .length_cast,
        .sign_extend,
        .zero_extend,
        .truncate,
        .signed_cast,
        .unsigned_cast,
        .remove_signedness_cast,
        .absolute_address_cast,
        .data_address_cast,
        .insn_address_cast,
        .stack_address_cast,
        .remove_address_cast,
        .index_to_reg,
        .reg_to_index,
        .crlf_cast,
        .lf_cast,
        => unreachable,
    });
}

fn add_unary_expression(self: *Parser, kind: Expression.Kind, token: Token.Handle, inner: Expression.Handle) Expression.Handle {
    return self.add_expression_info(token, switch (kind) {
        .directive_symbol_def => .{ .directive_symbol_def = inner },
        .directive_symbol_ref => .{ .directive_symbol_ref = inner },
        .local_label_def => .{ .local_label_def = inner },
        .negate => .{ .negate = inner },
        .complement => .{ .complement = inner },
        .signed_cast => .{ .signed_cast = inner },
        .unsigned_cast => .{ .unsigned_cast = inner },
        .remove_signedness_cast => .{ .remove_signedness_cast = inner },
        .absolute_address_cast => .{ .absolute_address_cast = inner },
        .data_address_cast => .{ .data_address_cast = inner },
        .insn_address_cast => .{ .insn_address_cast = inner },
        .stack_address_cast => .{ .stack_address_cast = inner },
        .remove_address_cast => .{ .remove_address_cast = inner },
        .index_to_reg => .{ .index_to_reg = inner },
        .reg_to_index => .{ .reg_to_index = inner },
        .crlf_cast => .{ .crlf_cast = inner },
        .lf_cast => .{ .lf_cast = inner },

        .arrow,
        .literal_int,
        .literal_str,
        .literal_reg,
        .literal_symbol_def,
        .literal_symbol_ref,
        .literal_current_address,
        .list,
        .plus,
        .minus,
        .multiply,
        .shl,
        .shr,
        .concat,
        .concat_repeat,
        .bitwise_or,
        .bitwise_xor,
        .bitwise_and,
        .length_cast,
        .sign_extend,
        .zero_extend,
        .truncate,
        => unreachable,
    });
}

fn add_binary_expression(self: *Parser, kind: Expression.Kind, token: Token.Handle, lhs: Expression.Handle, rhs: Expression.Handle) Expression.Handle {
    const bin = Expression.Binary{
        .left = lhs,
        .right = rhs,
    };
    return self.add_expression_info(token, switch (kind) {
        .list => .{ .list = bin },
        .plus => .{ .plus = bin },
        .minus => .{ .minus = bin },
        .multiply => .{ .multiply = bin },
        .shl => .{ .shl = bin },
        .shr => .{ .shr = bin },
        .concat => .{ .concat = bin },
        .concat_repeat => .{ .concat_repeat = bin },
        .bitwise_or => .{ .bitwise_or = bin },
        .bitwise_xor => .{ .bitwise_xor = bin },
        .bitwise_and => .{ .bitwise_and = bin },
        .length_cast => .{ .length_cast = bin },
        .sign_extend => .{ .sign_extend = bin },
        .zero_extend => .{ .zero_extend = bin },
        .truncate => .{ .truncate = bin },

        .arrow,
        .directive_symbol_def,
        .directive_symbol_ref,
        .local_label_def,
        .negate,
        .complement,
        .signed_cast,
        .unsigned_cast,
        .remove_signedness_cast,
        .literal_int,
        .literal_str,
        .literal_reg,
        .literal_symbol_def,
        .literal_symbol_ref,
        .literal_current_address,
        .absolute_address_cast,
        .data_address_cast,
        .insn_address_cast,
        .stack_address_cast,
        .remove_address_cast,
        .index_to_reg,
        .reg_to_index,
        .crlf_cast,
        .lf_cast,
        => unreachable,
    });
}

fn parse_directive(self: *Parser) ?Instruction.Operation_Type {
    if (self.sync_to_end_of_line or !self.try_token(.dot)) return null;

    if (self.try_token(.id)) {
        const directive_str = self.token_span(self.next_token - 1);
        if (directive_map.get(directive_str)) |directive| {
            return directive;
        }
        self.record_error_rel("Unrecognized directive", -1);
    } else {
        self.record_error("Expected directive");
    }
    self.sync_to_end_of_line = true;
    return null;
}

pub fn skip_linespace(self: *Parser) void {
    _ = self.try_token(.linespace);
}

pub fn try_token(self: *Parser, kind: Token_Kind) bool {
    if (self.token_kinds[self.next_token] == kind) {
        self.next_token += 1;
        return true;
    } else {
        return false;
    }
}

pub fn record_error(self: *Parser, desc: []const u8) void {
    self.errors.append(self.gpa, .{
        .file = self.handle,
        .context = .{ .token = self.next_token },
        .desc = desc,
        .flags = .{},
    }) catch @panic("OOM");
}
pub fn record_error_rel(self: *Parser, desc: []const u8, token_offset: i8) void {
    self.errors.append(self.gpa, .{
        .file = self.handle,
        .context = .{ .token = @intCast(@as(i32, self.next_token) + token_offset) },
        .desc = desc,
        .flags = .{},
    }) catch @panic("OOM");
}

pub fn token_span(self: *Parser, handle: Token.Handle) []const u8 {
    return self.out.tokens.get(handle).span(self.out.source);
}

const directive_map = parse_helpers.case_insensitive_enum_map(Instruction.Operation_Type, .{
    .excluded_values = &.{ "none", "insn", "bound_insn" },
}, .{});

const Parser = @This();
const Source_File = @import("Source_File.zig");
const Error = @import("Error.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const parse_helpers = isa.parse_helpers;
const Token = lex.Token;
const Token_Kind = lex.Token_Kind;
const lex = isa.lex;
const Mnemonic = isa.Mnemonic;
const Mnemonic_Suffix = isa.Mnemonic_Suffix;
const isa = @import("isa");
const arch = @import("arch");
const std = @import("std");
