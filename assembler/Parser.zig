const std = @import("std");
const isa = @import("isa_types");
const ie = @import("isa_encoding");
const lex = @import("lex.zig");
const SourceFile = @import("SourceFile.zig");
const Error = @import("Error.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");

const Token = lex.Token;
const TokenKind = lex.TokenKind;
const Mnemonic = isa.Mnemonic;
const MnemonicSuffix = isa.MnemonicSuffix;
const ExpressionType = ie.Parameter.ExpressionType;
const Parser = @This();

temp: std.mem.Allocator,
gpa: std.mem.Allocator,
handle: SourceFile.Handle,
out: *SourceFile,
errors: *std.ArrayListUnmanaged(Error),
next_token: Token.Handle,
sync_to_end_of_line: bool,
token_kinds: []TokenKind,
token_offsets: []u32,

mnemonic_map: std.StringHashMapUnmanaged(Mnemonic),
suffix_map: std.StringHashMapUnmanaged(MnemonicSuffix),
directive_map: std.StringHashMapUnmanaged(Instruction.OperationType),

const max_mnemonic_length = 8;
const max_suffix_length = 4;
const max_directive_length = 8;

pub fn init(
    temp: std.mem.Allocator,
    gpa: std.mem.Allocator,
    handle: SourceFile.Handle,
    out: *SourceFile,
    errors: *std.ArrayListUnmanaged(Error),
) Parser {
    var mnemonic_map = std.StringHashMapUnmanaged(Mnemonic) {};
    var suffix_map = std.StringHashMapUnmanaged(MnemonicSuffix) {};
    var directive_map = std.StringHashMapUnmanaged(Instruction.OperationType) {};

    const mnemonics = comptime std.enums.values(Mnemonic);
    mnemonic_map.ensureUnusedCapacity(temp, mnemonics.len) catch @panic("OOM");
    errdefer mnemonic_map.deinit(temp);

    inline for (mnemonics) |mnemonic| {
        if (mnemonic == ._reserved) continue;
        const lower = comptime blk: {
            @setEvalBranchQuota(10_000);
            var buf = [_]u8{0} ** max_mnemonic_length;
            break :blk std.ascii.lowerString(&buf, @tagName(mnemonic));
        };
        mnemonic_map.putAssumeCapacityNoClobber(lower, mnemonic);
    }

    const suffixes = comptime std.enums.values(MnemonicSuffix);
    suffix_map.ensureUnusedCapacity(temp, @intCast(u32, suffixes.len + 20)) catch @panic("OOM");
    errdefer suffix_map.deinit(temp);

    inline for (suffixes) |suffix| {
        if (suffix == .none or comptime std.mem.indexOfScalar(u8, @tagName(suffix), '_') != null) continue;
        const lower = comptime blk: {
            var buf = [_]u8{0} ** max_suffix_length;
            break :blk std.ascii.lowerString(&buf, @tagName(suffix));
        };
        suffix_map.putAssumeCapacityNoClobber(lower, suffix);
    }

    suffix_map.putAssumeCapacityNoClobber("eq", .Z);
    suffix_map.putAssumeCapacityNoClobber("neq", .NZ);
    suffix_map.putAssumeCapacityNoClobber("ltu", .LU);
    suffix_map.putAssumeCapacityNoClobber("lts", .LS);
    suffix_map.putAssumeCapacityNoClobber("leu", .NGU);
    suffix_map.putAssumeCapacityNoClobber("les", .NGS);
    suffix_map.putAssumeCapacityNoClobber("gtu", .GU);
    suffix_map.putAssumeCapacityNoClobber("gts", .GS);
    suffix_map.putAssumeCapacityNoClobber("geu", .NLU);
    suffix_map.putAssumeCapacityNoClobber("ges", .NLS);
    suffix_map.putAssumeCapacityNoClobber("nltu", .NLU);
    suffix_map.putAssumeCapacityNoClobber("nlts", .NLS);
    suffix_map.putAssumeCapacityNoClobber("nleu", .GU);
    suffix_map.putAssumeCapacityNoClobber("nles", .GS);
    suffix_map.putAssumeCapacityNoClobber("ngtu", .NGU);
    suffix_map.putAssumeCapacityNoClobber("ngts", .NGS);
    suffix_map.putAssumeCapacityNoClobber("ngeu", .LU);
    suffix_map.putAssumeCapacityNoClobber("nges", .LS);
    suffix_map.putAssumeCapacityNoClobber("dw", .W);
    suffix_map.putAssumeCapacityNoClobber("dr", .R);

    const directives = comptime std.enums.values(Instruction.OperationType);
    directive_map.ensureUnusedCapacity(temp, directives.len) catch @panic("OOM");
    errdefer directive_map.deinit(temp);

    inline for (directives) |directive| {
        switch (directive) {
            .none, .insn, .bound_insn => {},
            inline else => |comptime_directive| {
                const lower = comptime blk: {
                    var buf = [_]u8{0} ** max_directive_length;
                    break :blk std.ascii.lowerString(&buf, @tagName(comptime_directive));
                };
                directive_map.putAssumeCapacityNoClobber(lower, directive);
            },
        }
    }

    return .{
        .temp = temp,
        .gpa = gpa,
        .handle = handle,
        .out = out,
        .errors = errors,
        .next_token = 0,
        .sync_to_end_of_line = false,
        .token_kinds = out.tokens.items(.kind),
        .token_offsets = out.tokens.items(.offset),
        .mnemonic_map = mnemonic_map,
        .suffix_map = suffix_map,
        .directive_map = directive_map,
    };
}

pub fn parseInstruction(self: *Parser) bool {
    const label_token = self.next_token;
    const label = self.parseLabel();
    self.skipLinespace();
    self.sync_to_end_of_line = false;

    if (self.parseDirective()) |directive| {
        const directive_token = self.next_token - 1;
        if (directive == .def or directive == .local) {
            if (self.parseSymbolDef()) |symbol| {
                if (self.parseExpr(false)) |expr| {
                    const params = self.addBinaryExpression(.list, directive_token, symbol, expr);
                    self.addInstruction(label, directive_token, switch (directive) {
                        .def => .{ .def = {} },
                        .local => .{ .local = {} },
                        else => unreachable,
                    }, params);
                } else {
                    self.recordError("Expected expression for symbol definition");
                    self.sync_to_end_of_line = true;
                }
            } else {
                self.recordError("Expected symbol name");
                self.sync_to_end_of_line = true;
            }
        } else if (directive == .undef) {
            const params = self.parseSymbolDefList();
            self.addInstruction(label, directive_token, .{ .undef = {} }, params);
        } else {
            const params = if (Instruction.isSectionDirective(directive)) self.parseSymbolDef() else self.parseExprList(true, false);
            switch (directive) {
                .none, .insn, .bound_insn => unreachable,
                inline else => |d| {
                    self.addInstruction(label, directive_token, @unionInit(Instruction.Operation, @tagName(d), {}), params);
                },
            }
        }
    } else if (self.parseMnemonic()) |mnemonic| {
        const mnemonic_token = self.next_token - 1;
        var swap_params = false;
        const suffix = self.parseSuffix(&swap_params);
        const params = self.parseExprList(true, swap_params);
        self.addInstruction(label, mnemonic_token, .{ .insn = .{
            .mnemonic = mnemonic,
            .suffix = suffix,
        }}, params);
    } else if (label) |_| {
        self.addInstruction(label, label_token, .{ .none = {} }, null);
    }
    self.skipLinespace();
    _ = self.tryToken(.comment);
    while (true) {
        if (self.tryToken(.newline)) return true;
        if (self.tryToken(.eof)) return false;
        if (!self.sync_to_end_of_line) {
            self.recordError("Expected end of line");
            self.sync_to_end_of_line = true;
        }
        self.next_token += 1;
    }
}

fn addInstruction(self: *Parser, label: ?Expression.Handle, token: Token.Handle, op: Instruction.Operation, params: ?Expression.Handle) void {
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
        start_line += t.countNewlines(self.out.source);
    }
    instructions.append(self.gpa, .{
        .label = label,
        .token = token,
        .operation = op,
        .params = params,
        .line_number = start_line,
    }) catch @panic("OOM");
}

fn trySuffix(self: *Parser) MnemonicSuffix {
    if (self.tryToken(.dot)) {
        if (self.tryToken(.id)) {
            const suffix_str = self.tokenLocation(self.next_token - 1);
            if (suffix_str.len <= max_suffix_length) {
                var buf = [_]u8 {0} ** max_suffix_length;
                const lower = std.ascii.lowerString(&buf, suffix_str);
                if (self.suffix_map.get(lower)) |suffix| {
                    return suffix;
                }
            }
            self.recordErrorRel("Unrecognized mnemonic suffix", -1);
        } else {
            self.recordError("Expected mnemonic suffix");
        }
    }
    return .none;
}

fn parseSuffix(self: *Parser, swap_params: *bool) MnemonicSuffix {
    const suffix1 = self.trySuffix();
    const suffix2 = self.trySuffix();
    return switch (suffix1) {
        .none => suffix2,
        .LU => switch (suffix2) {
            .none => suffix1,
            .GU => .LU_GU,
            .Z => .LU_Z,
            else => blk: {
                self.recordErrorRel("Invalid mnemonic suffix combination", -1);
                break :blk suffix1;
            },
        },
        .GU => switch (suffix2) {
            .none => suffix1,
            .LU => blk: {
                swap_params.* = true;
                break :blk .LU_GU;
            },
            .Z => .GU_Z,
            else => blk: {
                self.recordErrorRel("Invalid mnemonic suffix combination", -1);
                break :blk suffix1;
            },
        },
        .Z => switch (suffix2) {
            .none => suffix1,
            .LU => blk: {
                swap_params.* = true;
                break :blk .LU_Z;
            },
            .GU => blk: {
                swap_params.* = true;
                break :blk .GU_Z;
            },
            .LS => blk: {
                swap_params.* = true;
                break :blk .LS_Z;
            },
            .GS => blk: {
                swap_params.* = true;
                break :blk .GS_Z;
            },
            .N => blk: {
                swap_params.* = true;
                break :blk .N_Z;
            },
            .P => blk: {
                swap_params.* = true;
                break :blk .P_Z;
            },
            else => blk: {
                self.recordErrorRel("Invalid mnemonic suffix combination", -1);
                break :blk suffix1;
            },
        },
        .LS => switch (suffix2) {
            .none => suffix1,
            .GS => .LS_GS,
            .Z => .LS_Z,
            else => blk: {
                self.recordErrorRel("Invalid mnemonic suffix combination", -1);
                break :blk suffix1;
            },
        },
        .GS =>  switch (suffix2) {
            .none => suffix1,
            .LS => blk: {
                swap_params.* = true;
                break :blk .LS_GS;
            },
            .Z => .GS_Z,
            else => blk: {
                self.recordErrorRel("Invalid mnemonic suffix combination", -1);
                break :blk suffix1;
            },
        },
        .N => switch (suffix2) {
            .none => suffix1,
            .Z => .N_Z,
            .P => .N_P,
            else => blk: {
                self.recordErrorRel("Invalid mnemonic suffix combination", -1);
                break :blk suffix1;
            },
        },
        .P => switch (suffix2) {
            .none => suffix1,
            .Z => .P_Z,
            .N => blk: {
                swap_params.* = true;
                break :blk .N_P;
            },
            else => blk: {
                self.recordErrorRel("Invalid mnemonic suffix combination", -1);
                break :blk suffix1;
            },
        },
        else => switch (suffix2) {
            .none => suffix1,
            else => blk: {
                self.recordErrorRel("Invalid mnemonic suffix combination", -1);
                break :blk suffix1;
            },
        }
    };
}

fn parseExprList(self: *Parser, first: bool, swap_params: bool) ?Expression.Handle {
    const begin = self.next_token;
    if (self.parseExpr(first)) |lhs| {
        self.skipLinespace();
        if (self.tryToken(.comma) or self.tryToken(.arrow)) {
            const token = self.next_token - 1;
            if (self.parseExprList(false, false)) |rhs| {
                switch (self.token_kinds[token]) {
                    .comma => if (swap_params) {
                        return self.addBinaryExpression(.list, token, rhs, lhs);
                    } else {
                        return self.addBinaryExpression(.list, token, lhs, rhs);
                    },
                    .arrow => if (swap_params) {
                        return self.addBinaryExpression(.arrow_list, token, rhs, lhs);
                    } else {
                        return self.addBinaryExpression(.arrow_list, token, lhs, rhs);
                    },
                    else => unreachable,
                }
            } else {
                if (!self.sync_to_end_of_line) {
                    self.recordError("Expected expression");
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

fn parseExpr(self: *Parser, allow_arrow_prefix: bool) ?Expression.Handle {
    return self.parseExprPratt(if (allow_arrow_prefix) 0 else 1);
}

const OperatorInfo = struct {
    token: Token.Handle,
    left_bp: u8,
    right_bp: ?u8, // null for postfix operators
    expr: Expression.Kind,
};
fn parsePrefixOperator(self: *Parser) ?OperatorInfo {
    const begin = self.next_token;
    self.skipLinespace();
    var t = self.next_token;
    const info: OperatorInfo = switch (self.token_kinds[t]) {
        .arrow => .{ .token = t, .left_bp = 0, .right_bp = 1, .expr = .arrow_prefix },
        .minus => .{ .token = t, .left_bp = 1, .right_bp = 0xFF, .expr = .negate },
        .tilde => .{ .token = t, .left_bp = 1, .right_bp = 0xFF, .expr = .complement },
        .at    => .{ .token = t, .left_bp = 1, .right_bp = 0xFF, .expr = .absolute_address_cast },
        .dot => info: {
            t += 1;
            self.next_token = t;
            defer self.next_token = t;
            if (self.tryKeyword("d")) {
                break :info .{ .token = t, .left_bp = 1, .right_bp = 1, .expr = .data_address_cast };
            } else if (self.tryKeyword("i")) {
                break :info .{ .token = t, .left_bp = 1,  .right_bp = 1, .expr = .insn_address_cast };
            } else if (self.tryKeyword("s")) {
                break :info .{ .token = t, .left_bp = 1,  .right_bp = 1, .expr = .stack_address_cast };
            } else if (self.tryKeyword("raw")) {
                break :info .{ .token = t, .left_bp = 1,  .right_bp = 1, .expr = .remove_address_cast };
            } else if (self.tryKeyword("r")) {
                break :info .{ .token = t, .left_bp = 1,  .right_bp = 0xFF, .expr = .index_to_reg16 };
            } else if (self.tryKeyword("rx")) {
                break :info .{ .token = t, .left_bp = 1,  .right_bp = 0xFF, .expr = .index_to_reg32 };
            } else if (self.tryKeyword("rb")) {
                break :info .{ .token = t, .left_bp = 1,  .right_bp = 0xFF, .expr = .index_to_reg8 };
            } else if (self.tryKeyword("idx")) {
                break :info .{ .token = t, .left_bp = 1,  .right_bp = 0xFF, .expr = .reg_to_index };
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
fn parseOperator(self: *Parser) ?OperatorInfo {
    const begin = self.next_token;
    self.skipLinespace();
    var t = self.next_token;
    const info: OperatorInfo = switch (self.token_kinds[t]) {
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
            if (self.tryKeyword("zx")) {
                break :info .{ .token = t, .left_bp = 50, .right_bp = 51, .expr = .zero_extend };
            } else if (self.tryKeyword("sx")) {
                break :info .{ .token = t, .left_bp = 50, .right_bp = 51, .expr = .sign_extend };
            } else if (self.tryKeyword("trunc")) {
                break :info .{ .token = t, .left_bp = 50, .right_bp = 51, .expr = .truncate };
            } else if (self.tryKeyword("signed")) {
                break :info .{ .token = t, .left_bp = 50, .right_bp = null, .expr = .signed_cast };
            } else if (self.tryKeyword("unsigned")) {
                break :info .{ .token = t, .left_bp = 50, .right_bp = null, .expr = .unsigned_cast };
            } else if (self.tryKeyword("without_signedness")) {
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

fn parseExprPratt(self: *Parser, min_binding_power: u8) ?Expression.Handle {
    if (self.sync_to_end_of_line) return null;

    const before_prefix_operator = self.next_token;
    var expr = if (self.parsePrefixOperator()) |operator| e: {
        if (operator.left_bp < min_binding_power) {
            self.next_token = before_prefix_operator;
            return null;
        }
        if (self.parseExprPratt(operator.right_bp.?)) |right| {
            break :e self.addUnaryExpression(operator.expr, operator.token, right);
        } else if (!self.sync_to_end_of_line) {
            self.recordError("Expected expression");
            self.sync_to_end_of_line = true;
        }
        return null;
    } else self.parseAtom() orelse return null;

    while (true) {
        if (self.sync_to_end_of_line) return null;
        const before_operator = self.next_token;
        if (self.parseOperator()) |operator| {
            if (operator.left_bp < min_binding_power) {
                self.next_token = before_operator;
                break;
            }
            if (operator.right_bp) |binding_power| {
                if (self.parseExprPratt(binding_power)) |right| {
                    expr = self.addBinaryExpression(operator.expr, operator.token, expr, right);
                } else if (!self.sync_to_end_of_line) {
                    self.recordError("Expected expression");
                    self.sync_to_end_of_line = true;
                    return null;
                }
            } else {
                expr = self.addUnaryExpression(operator.expr, operator.token, expr);
            }
        } else break;
    }

    return expr;
}

fn parseAtom(self: *Parser) ?Expression.Handle {
    return self.parseParenExpr()
        orelse self.parseIntLiteral()
        orelse self.parseStringLiteral()
        orelse self.parseRegisterLiteral()
        orelse self.parseSpecialLiteral()
        orelse self.parseSymbolRef()
        ;
}

fn parseParenExpr(self: *Parser) ?Expression.Handle {
    const begin = self.next_token;
    self.skipLinespace();
    if (self.tryToken(.paren_open)) {
        if (self.parseExpr(false)) |expr| {
            if (!self.tryToken(.paren_close)) {
                if (!self.sync_to_end_of_line) {
                    self.recordError("Expected ')'");
                    self.sync_to_end_of_line = true;
                }
            }
            return expr;
        } else {
            if (!self.sync_to_end_of_line) {
                self.recordError("Expected expression");
                self.sync_to_end_of_line = true;
            }
            _ = self.tryToken(.paren_close);
            return null;
        }
    }
    self.next_token = begin;
    return null;
}

fn parseIntLiteral(self: *Parser) ?Expression.Handle {
    const begin = self.next_token;
    self.skipLinespace();
    if (self.tryToken(.int_literal)) {
        return self.addTerminalExpression(.literal_int, self.next_token - 1);
    }
    self.next_token = begin;
    return null;
}

fn parseStringLiteral(self: *Parser) ?Expression.Handle {
    const begin = self.next_token;
    self.skipLinespace();
    const literal_token = self.next_token;
    if (self.tryToken(.str_literal)) {
        return self.addTerminalExpression(.literal_str, literal_token);
    } else if (self.tryToken(.str_literal_raw)) {
        while (true) {
            const end = self.next_token;
            self.skipLinespace();
            if (self.tryToken(.str_literal_raw)) continue;

            self.next_token = end;
            break;
        }
        return self.addTerminalExpression(.literal_str, literal_token);
    }
    self.next_token = begin;
    return null;
}

fn parseRegisterLiteral(self: *Parser) ?Expression.Handle {
    const begin = self.next_token;
    self.skipLinespace();
    if (self.tryToken(.id)) {
        const token = self.next_token - 1;
        const id = self.tokenLocation(token);
        if (isGprLiteral(id)) {
            return self.addTerminalExpression(.literal_reg, token);
        }
        if (id.len >= 2 and id.len <= 4) {
            var buf = [_]u8 {0} ** 4;
            const lower = std.ascii.lowerString(&buf, id);
            if (lower[1] == 'p') {
                if (std.mem.eql(u8, lower, "ip")) {
                    return self.addTypedTerminalExpression(.literal_reg, token, .{ .sr = .IP });
                } else if (std.mem.eql(u8, lower, "sp")) {
                    return self.addTypedTerminalExpression(.literal_reg, token, .{ .sr = .SP });
                } else if (std.mem.eql(u8, lower, "rp")) {
                    return self.addTypedTerminalExpression(.literal_reg, token, .{ .sr = .RP });
                } else if (std.mem.eql(u8, lower, "bp")) {
                    return self.addTypedTerminalExpression(.literal_reg, token, .{ .sr = .BP });
                }
            } else if (std.mem.eql(u8, lower, "uxp")) {
                return self.addTypedTerminalExpression(.literal_reg, token, .{ .sr = .UXP });
            } else if (std.mem.eql(u8, lower, "kxp")) {
                return self.addTypedTerminalExpression(.literal_reg, token, .{ .sr = .KXP });
            } else if (std.mem.eql(u8, lower, "stat")) {
                return self.addTypedTerminalExpression(.literal_reg, token, .{ .sr = .STAT });
            } else if (std.mem.eql(u8, lower, "asn")) {
                return self.addTypedTerminalExpression(.literal_reg, token, .{ .sr = .ASN });
            }
        }
    }
    self.next_token = begin;
    return null;
}

fn parseSpecialLiteral(self: *Parser) ?Expression.Handle {
        const begin = self.next_token;
    self.skipLinespace();
    const token = self.next_token;
    if (self.tryToken(.money)) {
        return self.addTerminalExpression(.literal_current_address, token);
    }
    self.next_token = begin;
    return null;
}

fn isGprLiteral(id: []const u8) bool {
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

fn parseLabel(self: *Parser) ?Expression.Handle {
    const begin = self.next_token;
    const begin_expression_len = self.out.expressions.len;

    const local = self.parseLocalLabelDirective();
    if (self.parseSymbolDef()) |expr| {
        self.skipLinespace();
        if (self.tryToken(.colon)) {
            return if (local) self.addUnaryExpression(.local_label_def, begin + 1, expr) else expr;
        }
    }
    self.out.expressions.len = begin_expression_len;
    self.next_token = begin;
    return null;
}

fn parseLocalLabelDirective(self: *Parser) bool {
    const begin = self.next_token;
    if (self.tryToken(.dot) and self.tryToken(.id)) {
        const directive_str = self.tokenLocation(self.next_token - 1);
        if (std.mem.eql(u8, directive_str, "local")) {
            return true;
        }
    }
    self.next_token = begin;
    return false;
}

fn parseSymbolDefList(self: *Parser) ?Expression.Handle {
    const begin = self.next_token;
    if (self.parseSymbolDef()) |lhs| {
        self.skipLinespace();
        const token = self.next_token;
        if (self.tryToken(.comma)) {
            if (self.parseSymbolDefList()) |rhs| {
                return self.addBinaryExpression(.list, token, lhs, rhs);
            } else {
                if (!self.sync_to_end_of_line) {
                    self.recordError("Expected symbol");
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

fn parseSymbolDef(self: *Parser) ?Expression.Handle {
    const begin = self.next_token;
    self.skipLinespace();
    if (self.tryToken(.id)) {
        return self.addTerminalExpression(.literal_symbol_def, self.next_token - 1);
    } else if (self.tryToken(.dot) and self.tryKeyword("sym")) {
        const sym_token = self.next_token - 1;
        if (self.parseStringLiteral()) |expr| {
            return self.addUnaryExpression(.directive_symbol_def, sym_token, expr);
        } else {
            self.recordError("Expected constant expression");
            self.sync_to_end_of_line = true;
        }
    }
    self.next_token = begin;
    return null;
}
fn parseSymbolRef(self: *Parser) ?Expression.Handle {
    const begin = self.next_token;
    self.skipLinespace();
    if (self.tryToken(.id)) {
        return self.addTerminalExpression(.literal_symbol_ref, self.next_token - 1);
    } else if (self.tryToken(.dot) and self.tryKeyword("sym")) {
        const sym_token = self.next_token - 1;
        if (self.parseStringLiteral()) |expr| {
            return self.addUnaryExpression(.directive_symbol_ref, sym_token, expr);
        } else {
            self.recordError("Expected constant expression");
            self.sync_to_end_of_line = true;
        }
    }
    self.next_token = begin;
    return null;
}

fn tryKeyword(self: *Parser, comptime kw: []const u8) bool {
    const begin = self.next_token;
    if (self.tryToken(.id)) {
        const str = self.tokenLocation(begin);
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

fn addTypedTerminalExpression(self: *Parser, comptime kind: Expression.Kind, token: Token.Handle, expr_type: ExpressionType) Expression.Handle {
    const handle = @intCast(Expression.Handle, self.out.expressions.len);
    self.out.expressions.append(self.gpa, .{
        .token = token,
        .info = @unionInit(Expression.Info, @tagName(kind), {}),
        .resolved_type = expr_type,
        .resolved_constant = null,
        .flags = .{},
    }) catch @panic("OOM");
    return handle;
}

fn addExpressionInfo(self: *Parser, token: Token.Handle, info: Expression.Info) Expression.Handle {
    const handle = @intCast(Expression.Handle, self.out.expressions.len);
    self.out.expressions.append(self.gpa, .{
        .token = token,
        .info = info,
        .resolved_type = .{ .unknown = {} },
        .resolved_constant = null,
        .flags = .{},
    }) catch @panic("OOM");
    return handle;
}

fn addTerminalExpression(self: *Parser, kind: Expression.Kind, token: Token.Handle) Expression.Handle {
    return self.addExpressionInfo(token, switch (kind) {
        .literal_int => .{ .literal_int = {} },
        .literal_str => .{ .literal_str = {} },
        .literal_reg => .{ .literal_reg = {} },
        .literal_symbol_def => .{ .literal_symbol_def = {} },
        .literal_symbol_ref => .{ .literal_symbol_ref = {} },
        .literal_current_address => .{ .literal_current_address = {} },

        .list,
        .arrow_list,
        .arrow_prefix,
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
        .index_to_reg8,
        .index_to_reg16,
        .index_to_reg32,
        .reg_to_index,
        => unreachable,
    });
}

fn addUnaryExpression(self: *Parser,  kind: Expression.Kind, token: Token.Handle, inner: Expression.Handle) Expression.Handle {
    return self.addExpressionInfo(token, switch (kind) {
        .arrow_prefix => .{ .arrow_prefix = inner },
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
        .index_to_reg8 => .{ .index_to_reg8 = inner },
        .index_to_reg16 => .{ .index_to_reg16 = inner },
        .index_to_reg32 => .{ .index_to_reg32 = inner },
        .reg_to_index => .{ .reg_to_index = inner },

        .literal_int,
        .literal_str,
        .literal_reg,
        .literal_symbol_def,
        .literal_symbol_ref,
        .literal_current_address,
        .list,
        .arrow_list,
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

fn addBinaryExpression(self: *Parser, kind: Expression.Kind, token: Token.Handle, lhs: Expression.Handle, rhs: Expression.Handle) Expression.Handle {
    const bin = Expression.Binary{
        .left = lhs,
        .right = rhs,
    };
    return self.addExpressionInfo(token, switch (kind) {
        .list => .{ .list = bin },
        .arrow_list => .{ .arrow_list = bin },
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

        .arrow_prefix,
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
        .index_to_reg8,
        .index_to_reg16,
        .index_to_reg32,
        .reg_to_index,
        => unreachable,
    });
}

fn parseMnemonic(self: *Parser) ?Mnemonic {
    if (self.sync_to_end_of_line or !self.tryToken(.id)) return null;

    const mnemonic_str = self.tokenLocation(self.next_token - 1);
    if (mnemonic_str.len <= max_mnemonic_length) {
        var buf = [_]u8 {0} ** max_mnemonic_length;
        const lower = std.ascii.lowerString(&buf, mnemonic_str);
        if (self.mnemonic_map.get(lower)) |mnemonic| {
            return mnemonic;
        }
    }

    self.recordErrorRel("Unrecognized mnemonic", -1);
    self.sync_to_end_of_line = true;
    return null;
}

fn parseDirective(self: *Parser) ?Instruction.OperationType {
    if (self.sync_to_end_of_line or !self.tryToken(.dot)) return null;

    if (self.tryToken(.id)) {
        const directive_str = self.tokenLocation(self.next_token - 1);
        if (directive_str.len <= max_directive_length) {
            var buf = [_]u8 {0} ** max_directive_length;
            const lower = std.ascii.lowerString(&buf, directive_str);
            if (self.directive_map.get(lower)) |directive| {
                return directive;
            }
        }
        self.recordErrorRel("Unrecognized directive", -1);
    } else {
        self.recordError("Expected directive");
    }
    self.sync_to_end_of_line = true;
    return null;
}

fn skipLinespace(self: *Parser) void {
    _ = self.tryToken(.linespace);
}

fn tryToken(self: *Parser, kind: TokenKind) bool {
    if (self.token_kinds[self.next_token] == kind) {
        self.next_token += 1;
        return true;
    } else {
        return false;
    }
}

fn recordError(self: *Parser, desc: []const u8) void {
    self.errors.append(self.gpa, .{
        .file = self.handle,
        .token = self.next_token,
        .desc = desc,
        .flags = .{},
    }) catch @panic("OOM");
}
fn recordErrorAbs(self: *Parser, desc: []const u8, token: Token.Handle) void {
    self.errors.append(self.gpa, .{
        .file = self.handle,
        .token = token,
        .desc = desc,
        .flags = .{},
    }) catch @panic("OOM");
}
fn recordErrorRel(self: *Parser, desc: []const u8, token_offset: i8) void {
    self.errors.append(self.gpa, .{
        .file = self.handle,
        .token = @intCast(Token.Handle, @as(i32, self.next_token) + token_offset),
        .desc = desc,
        .flags = .{},
    }) catch @panic("OOM");
}

fn tokenLocation(self: *Parser, handle: Token.Handle) []const u8 {
    return self.out.tokens.get(handle).location(self.out.source);
}
