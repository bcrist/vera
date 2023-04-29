const std = @import("std");
const lex = @import("lex.zig");
const ie = @import("instruction_encoding");
const Assembler = @import("Assembler.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const Section = @import("Section.zig");
const Error = @import("Error.zig");
const ErrorList = std.ArrayListUnmanaged(Error);
const ExpressionType = ie.ExpressionType;

const SourceFile = @This();

name: []const u8,
source: []const u8,
tokens: lex.TokenList,
instructions: std.MultiArrayList(Instruction),
expressions: std.MultiArrayList(Expression),
blocks: std.MultiArrayList(SectionBlock),

pub const SectionBlock = struct {
    first_token: lex.Token.Handle,
    first_insn: Instruction.Handle,
    end_insn: Instruction.Handle,
    section: ?Section.Handle,
    keep: bool,

    pub const Handle = u32;
};

pub const Handle = u32;

const Token = lex.Token;
const TokenKind = lex.TokenKind;
const TokenList = lex.TokenList;

const Mnemonic = ie.Mnemonic;
const MnemonicSuffix = ie.MnemonicSuffix;

const max_mnemonic_length = 8;
const max_suffix_length = 4;
const max_directive_length = 8;

pub fn parse(gpa: std.mem.Allocator, handle: Handle, name: []const u8, source: []const u8, errors: *ErrorList) SourceFile {
    const tokens = lex.lex(gpa, source);
    errdefer tokens.deinit(gpa);

    var file = SourceFile{
        .name = name,
        .source = source,
        .tokens = tokens,
        .instructions = .{},
        .expressions = .{},
        .blocks = .{},
    };

    var temp = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer temp.deinit();

    var p = Parser.init(temp.allocator(), gpa, handle, &file, errors);
    while (p.parseInstruction()) {}

    const operations = file.instructions.items(.operation);
    var block_begin: Instruction.Handle = 0;
    for (operations, 0..) |op, insn_handle| {
        if (Instruction.isSectionDirective(op)) {
            const new_block_begin = backtrackLabels(operations, @intCast(Instruction.Handle, insn_handle));
            file.tryAddBlock(gpa, block_begin, new_block_begin);
            block_begin = new_block_begin;
        }
    }

    file.tryAddBlock(gpa, block_begin, @intCast(Instruction.Handle, operations.len));

    return file;
}

fn tryAddBlock(self: *SourceFile, gpa: std.mem.Allocator, first_insn: Instruction.Handle, end_insn: Instruction.Handle) void {
    if (first_insn >= end_insn) return;

    var first_token = self.instructions.items(.token)[first_insn];
    if (self.instructions.items(.label)[first_insn]) |label_expr| {
        first_token = self.expressions.items(.token)[label_expr];
    }

    self.blocks.append(gpa, .{
        .first_token = first_token,
        .first_insn = first_insn,
        .end_insn = end_insn,
        .section = null,
        .keep = false,
    }) catch @panic("OOM");
}

pub fn deinit(self: SourceFile, gpa: std.mem.Allocator, maybe_arena: ?std.mem.Allocator) void {
    self.tokens.deinit(gpa);
    self.instructions.deinit(gpa);
    self.expressions.deinit(gpa);
    self.blocks.deinit(gpa);
    if (maybe_arena) |arena| {
        arena.free(self.name);
        arena.free(self.source);
    }
}

pub fn blockInstructions(self: *const SourceFile, block_handle: SectionBlock.Handle) Instruction.Iterator {
    return .{
        .begin = self.blocks.items(.first_insn)[block_handle],
        .end = self.blocks.items(.end_insn)[block_handle],
    };
}

pub const Chunk = struct {
    section: ?Section.Handle,
    file: SourceFile.Handle,
    instructions: Instruction.Iterator,
};

pub fn collectChunks(
    self: *const SourceFile,
    file_handle: SourceFile.Handle,
    gpa: std.mem.Allocator,
    fixed_org: *std.ArrayListUnmanaged(Chunk),
    auto_org: *std.ArrayListUnmanaged(Chunk),
) void {
    const operations = self.instructions.items(.operation);
    for (self.blocks.items(.keep), self.blocks.items(.section), self.blocks.items(.first_insn), self.blocks.items(.end_insn)) |keep, section, begin, end| {
        if (!keep) continue;

        var block_iter = Instruction.Iterator{
            .begin = begin,
            .end = end,
        };
        var chunk_begin = begin;
        var dest = auto_org;
        while (block_iter.next()) |insn_handle| {
            switch (operations[insn_handle]) {
                .org => {
                    const new_chunk_begin = backtrackOrgHeaders(operations, insn_handle);
                    if (chunk_begin < new_chunk_begin) {
                        dest.append(gpa, .{
                            .section = section,
                            .file = file_handle,
                            .instructions = .{
                                .begin = chunk_begin,
                                .end = new_chunk_begin,
                            },
                        }) catch @panic("OOM");
                    }
                    chunk_begin = new_chunk_begin;
                    dest = fixed_org;
                },
                .insn => |i| if (ie.getBranchKind(i.mnemonic, i.suffix) == .unconditional) {
                    // Control will never flow past an unconditional branch, so we can treat anything after as a different chunk
                    const new_chunk_begin = insn_handle + 1;
                    if (chunk_begin < new_chunk_begin) {
                        dest.append(gpa, .{
                            .section = section,
                            .file = file_handle,
                            .instructions = .{
                                .begin = chunk_begin,
                                .end = new_chunk_begin,
                            },
                        }) catch @panic("OOM");
                    }
                    chunk_begin = new_chunk_begin;
                    dest = auto_org;
                },
                .bound_insn => unreachable, // instructions should never be bound before we've collected chunks
                else => {},
            }
        }

        if (chunk_begin < end) {
            dest.append(gpa, .{
                .section = section,
                .file = file_handle,
                .instructions = .{
                    .begin = chunk_begin,
                    .end = end,
                },
            }) catch @panic("OOM");
        }
    }
}

// Note this may not work correctly for a needle token in a label, which preceeds the main Instruction.token unless there is only a label on that line.
// It's meant to be used when you know the token is from Instruction.token or an Expression.token being used as a parameter.
// It will only work for tokens within a label expression if the operation for that line is `.none`
pub fn findInstructionByToken(self: *const SourceFile, token_handle: Token.Handle) Instruction.Handle {
    var haystack = self.instructions.items(.token);
    var base: usize = 0;

    while (haystack.len > 8) {
        const mid = haystack.len / 2;
        if (haystack[mid] <= token_handle) {
            base += mid;
            haystack = haystack[mid..];
        } else {
            haystack = haystack[0..mid];
        }
    }

    var insn_index: ?usize = null;
    for (haystack, 0..) |insn_token_handle, offset| {
        if (insn_token_handle <= token_handle) {
            insn_index = base + offset;
        } else break;
    }
    return @intCast(Instruction.Handle, insn_index orelse unreachable);
}

pub fn findBlockByToken(self: *const SourceFile, token_handle: Token.Handle) SectionBlock.Handle {
    var haystack = self.blocks.items(.first_token);
    var base: usize = 0;

    while (haystack.len > 8) {
        const mid = haystack.len / 2;
        if (haystack[mid] <= token_handle) {
            base += mid;
            haystack = haystack[mid..];
        } else {
            haystack = haystack[0..mid];
        }
    }

    var block_index: ?usize = null;
    for (haystack, 0..) |haystack_token, offset| {
        if (haystack_token <= token_handle) {
            block_index = base + offset;
        } else break;
    }

    return @intCast(SectionBlock.Handle, block_index orelse unreachable);
}

pub fn findBlockByInstruction(self: *const SourceFile, insn_handle: Instruction.Handle) SectionBlock.Handle {
    var haystack = self.blocks.items(.first_insn);
    var base: usize = 0;

    while (haystack.len > 8) {
        const mid = haystack.len / 2;
        if (haystack[mid] <= insn_handle) {
            base += mid;
            haystack = haystack[mid..];
        } else {
            haystack = haystack[0..mid];
        }
    }

    var block_index: ?usize = null;
    for (haystack, 0..) |haystack_insn, offset| {
        if (haystack_insn <= insn_handle) {
            block_index = base + offset;
        } else break;
    }

    return @intCast(SectionBlock.Handle, block_index orelse unreachable);
}

fn backtrackLabels(operations: []const Instruction.Operation, handle: Instruction.Handle) Instruction.Handle {
    var result = handle;
    while (result > 0 and operations[result - 1] == .none) {
        result -= 1;
    }
    return result;
}

fn backtrackOrgHeaders(operations: []const Instruction.Operation, handle: Instruction.Handle) Instruction.Handle {
    var result = handle;
    while (result > 0 and Instruction.isOrgHeader(operations[result - 1])) {
        result -= 1;
    }
    return result;
}

const Parser = struct {
    temp: std.mem.Allocator,
    gpa: std.mem.Allocator,
    handle: Handle,
    out: *SourceFile,
    errors: *ErrorList,
    next_token: Token.Handle,
    sync_to_end_of_line: bool,
    token_kinds: []TokenKind,

    mnemonic_map: std.StringHashMapUnmanaged(Mnemonic),
    suffix_map: std.StringHashMapUnmanaged(MnemonicSuffix),
    directive_map: std.StringHashMapUnmanaged(Instruction.OperationType),

    fn init(
        temp: std.mem.Allocator,
        gpa: std.mem.Allocator,
        handle: Handle,
        out: *SourceFile,
        errors: *ErrorList,
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
            .mnemonic_map = mnemonic_map,
            .suffix_map = suffix_map,
            .directive_map = directive_map,
        };
    }

    fn parseInstruction(self: *Parser) bool {
        const label_token = self.next_token;
        const label = self.parseLabel();
        self.skipLinespace();
        self.sync_to_end_of_line = false;

        if (self.parseDirective()) |directive| {
            const directive_token = self.next_token - 1;
            if (directive == .def) {
                if (self.parseSymbolDef()) |symbol| {
                    if (self.parseExpr()) |expr| {
                        const params = self.addBinaryExpression(.list, directive_token, symbol, expr);
                        self.out.instructions.append(self.gpa, .{
                            .label = label,
                            .token = directive_token,
                            .operation = .{ .def = {} },
                            .params = params,
                        }) catch @panic("OOM");
                    } else {
                        self.recordError("Expected expression for symbol definition");
                    }
                } else {
                    self.recordError("Expected symbol name");
                }
            } else {
                const params = if (Instruction.isSectionDirective(directive)) self.parseSymbolDef() else self.parseExprList(false);
                switch (directive) {
                    .none, .insn, .bound_insn => unreachable,
                    inline else => |d| {
                        self.out.instructions.append(self.gpa, .{
                            .label = label,
                            .token = directive_token,
                            .operation = @unionInit(Instruction.Operation, @tagName(d), {}),
                            .params = params,
                        }) catch @panic("OOM");
                    },
                }
            }
        } else if (self.parseMnemonic()) |mnemonic| {
            const mnemonic_token = self.next_token - 1;
            var swap_params = false;
            const suffix = self.parseSuffix(&swap_params);
            const params = self.parseExprList(swap_params);

            self.out.instructions.append(self.gpa, .{
                .label = label,
                .token = mnemonic_token,
                .operation = .{ .insn = .{
                    .mnemonic = mnemonic,
                    .suffix = suffix,
                }},
                .params = params,
            }) catch @panic("OOM");
        } else if (label) |_| {
            self.out.instructions.append(self.gpa, .{
                .label = label,
                .token = label_token,
                .operation = .{ .none = {} },
                .params = null,
            }) catch @panic("OOM");
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

    fn parseExprList(self: *Parser, swap_params: bool) ?Expression.Handle {
        const begin = self.next_token;
        if (self.parseExpr()) |lhs| {
            self.skipLinespace();
            if (self.tryToken(.comma) or self.tryToken(.arrow)) {
                const token = self.next_token - 1;
                if (self.parseExprList(false)) |rhs| {
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

    fn parseExpr(self: *Parser) ?Expression.Handle {
        return self.parseExprPratt(0);
    }

    const OperatorInfo = struct {
        token: Token.Handle,
        left_bp: u8,
        right_bp: ?u8, // null for postfix operators
        expr: Expression.Kind,
    };
    const PrefixOperatorInfo = struct {
        token: Token.Handle,
        right_bp: u8,
        expr: Expression.Kind,
    };
    fn parsePrefixOperator(self: *Parser) ?PrefixOperatorInfo {
        const begin = self.next_token;
        self.skipLinespace();
        var t = self.next_token;
        const info: PrefixOperatorInfo = switch (self.token_kinds[t]) {
            .minus => .{ .token = t, .right_bp = 0xFF, .expr = .negate },
            .tilde => .{ .token = t, .right_bp = 0xFF, .expr = .complement },
            .at    => .{ .token = t, .right_bp = 0xFF, .expr = .absolute_address_cast },
            .dot => info: {
                t += 1;
                self.next_token = t;
                defer self.next_token = t;
                if (self.tryKeyword("d")) {
                    break :info .{ .token = t, .right_bp = 0, .expr = .data_address_cast };
                } else if (self.tryKeyword("i")) {
                    break :info .{ .token = t, .right_bp = 0, .expr = .insn_address_cast };
                } else if (self.tryKeyword("s")) {
                    break :info .{ .token = t, .right_bp = 0, .expr = .stack_address_cast };
                } else if (self.tryKeyword("raw")) {
                    break :info .{ .token = t, .right_bp = 0, .expr = .remove_address_cast };
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
                } else if (self.tryKeyword("maybe_signed")) {
                    break :info .{ .token = t, .left_bp = 50, .right_bp = null, .expr = .maybe_signed_cast };
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

        var expr = if (self.parsePrefixOperator()) |operator| e: {
            if (self.parseExprPratt(operator.right_bp)) |right| {
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
            orelse self.parseSymbolRef()
            ;
    }

    fn parseParenExpr(self: *Parser) ?Expression.Handle {
        const begin = self.next_token;
        self.skipLinespace();
        if (self.tryToken(.paren_open)) {
            if (self.parseExpr()) |expr| {
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
        if (self.tryToken(.str_literal)) {
            return self.addTerminalExpression(.literal_str, self.next_token - 1);
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
                        return self.addTypedTerminalExpression(.literal_reg, token, .{ .sr = .ip });
                    } else if (std.mem.eql(u8, lower, "sp")) {
                        return self.addTypedTerminalExpression(.literal_reg, token, .{ .sr = .sp });
                    } else if (std.mem.eql(u8, lower, "rp")) {
                        return self.addTypedTerminalExpression(.literal_reg, token, .{ .sr = .rp });
                    } else if (std.mem.eql(u8, lower, "bp")) {
                        return self.addTypedTerminalExpression(.literal_reg, token, .{ .sr = .bp });
                    }
                } else if (std.mem.eql(u8, lower, "uxp")) {
                    return self.addTypedTerminalExpression(.literal_reg, token, .{ .sr = .uxp });
                } else if (std.mem.eql(u8, lower, "kxp")) {
                    return self.addTypedTerminalExpression(.literal_reg, token, .{ .sr = .kxp });
                } else if (std.mem.eql(u8, lower, "stat")) {
                    return self.addTypedTerminalExpression(.literal_reg, token, .{ .sr = .stat });
                } else if (std.mem.eql(u8, lower, "asn")) {
                    return self.addTypedTerminalExpression(.literal_reg, token, .{ .sr = .asn });
                }
            }
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
        if (self.parseSymbolDef()) |expr| {
            self.skipLinespace();
            if (self.tryToken(.colon)) {
                return expr;
            }
        }
        self.out.expressions.len = begin_expression_len;
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

            .list,
            .arrow_list,
            .directive_symbol_def,
            .directive_symbol_ref,
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
            .maybe_signed_cast,
            .absolute_address_cast,
            .data_address_cast,
            .insn_address_cast,
            .stack_address_cast,
            .remove_address_cast,
            => unreachable,
        });
    }

    fn addUnaryExpression(self: *Parser,  kind: Expression.Kind, token: Token.Handle, inner: Expression.Handle) Expression.Handle {
        return self.addExpressionInfo(token, switch (kind) {
            .directive_symbol_def => .{ .directive_symbol_def = inner },
            .directive_symbol_ref => .{ .directive_symbol_ref = inner },
            .negate => .{ .negate = inner },
            .complement => .{ .complement = inner },
            .signed_cast => .{ .signed_cast = inner },
            .unsigned_cast => .{ .unsigned_cast = inner },
            .maybe_signed_cast => .{ .maybe_signed_cast = inner },
            .absolute_address_cast => .{ .absolute_address_cast = inner },
            .data_address_cast => .{ .data_address_cast = inner },
            .insn_address_cast => .{ .insn_address_cast = inner },
            .stack_address_cast => .{ .stack_address_cast = inner },
            .remove_address_cast => .{ .remove_address_cast = inner },

            .literal_int,
            .literal_str,
            .literal_reg,
            .literal_symbol_def,
            .literal_symbol_ref,
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

            .directive_symbol_def,
            .directive_symbol_ref,
            .negate,
            .complement,
            .signed_cast,
            .unsigned_cast,
            .maybe_signed_cast,
            .literal_int,
            .literal_str,
            .literal_reg,
            .literal_symbol_def,
            .literal_symbol_ref,
            .absolute_address_cast,
            .data_address_cast,
            .insn_address_cast,
            .stack_address_cast,
            .remove_address_cast,
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
        }) catch @panic("OOM");
    }
    fn recordErrorAbs(self: *Parser, desc: []const u8, token: Token.Handle) void {
        self.errors.append(self.gpa, .{
            .file = self.handle,
            .token = token,
            .desc = desc,
        }) catch @panic("OOM");
    }
    fn recordErrorRel(self: *Parser, desc: []const u8, token_offset: i8) void {
        self.errors.append(self.gpa, .{
            .file = self.handle,
            .token = @intCast(Token.Handle, @as(i32, self.next_token) + token_offset),
            .desc = desc,
        }) catch @panic("OOM");
    }

    fn tokenLocation(self: *Parser, handle: Token.Handle) []const u8 {
        return self.out.tokens.get(handle).location(self.out.source);
    }

    test "Parser" {
        const src =
            \\label:
            \\   nop //comment
            \\   sync
            \\   fret
            \\
            \\asdf: PARK
        ;
        var token_list = lex.lex(std.testing.allocator, src);
        defer token_list.deinit(std.testing.allocator);
        var results = parse(std.testing.allocator, src, token_list);
        defer results.deinit(std.testing.allocator);
        try std.testing.expectEqual(@as(usize, 0), results.errors.items.len);
        try std.testing.expectEqual(@as(usize, 5), results.instructions.items.len);

        var insn = results.instructions.items[0];
        try std.testing.expect(insn.label != null);
        try std.testing.expectEqualStrings("label", insn.label.?);
        try std.testing.expectEqual(Mnemonic._reserved, insn.mnemonic);

        insn = results.instructions.items[1];
        try std.testing.expect(insn.label == null);
        try std.testing.expectEqual(Mnemonic.NOP, insn.mnemonic);

        insn = results.instructions.items[2];
        try std.testing.expect(insn.label == null);
        try std.testing.expectEqual(Mnemonic.SYNC, insn.mnemonic);

        insn = results.instructions.items[3];
        try std.testing.expect(insn.label == null);
        try std.testing.expectEqual(Mnemonic.FRET, insn.mnemonic);

        insn = results.instructions.items[4];
        try std.testing.expect(insn.label != null);
        try std.testing.expectEqualStrings("asdf", insn.label.?);
        try std.testing.expectEqual(Mnemonic.PARK, insn.mnemonic);
    }
};
