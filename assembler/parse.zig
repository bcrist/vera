const lex = @import("lex.zig");
const std = @import("std");
const ie = @import("instruction_encoding");

const Token = lex.Token;
const TokenKind = lex.TokenKind;
const TokenList = lex.TokenList;

const Mnemonic = ie.Mnemonic;
const MnemonicSuffix = ie.MnemonicSuffix;

var lookup_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
var mnemonic_lookup: std.StringHashMapUnmanaged(Mnemonic) = .{};
var mnemonic_suffix_lookup: std.StringHashMapUnmanaged(MnemonicSuffix) = .{};

fn addSuffix(comptime text: []const u8, suffix: MnemonicSuffix) !void {
    mnemonic_suffix_lookup.putAssumeCapacity(text, suffix);
    const lower_text = comptime blk: {
        var buf = [_]u8{0}**text.len;
        break :blk std.ascii.lowerString(&buf, text);
    };
    mnemonic_suffix_lookup.putAssumeCapacity(lower_text, suffix);
}

pub fn init() !void {
    @setEvalBranchQuota(10_000);
    var alloc = lookup_arena.allocator();
    try mnemonic_lookup.ensureTotalCapacity(alloc, 500);
    try mnemonic_suffix_lookup.ensureTotalCapacity(alloc, 300);
    inline for (comptime std.enums.values(Mnemonic)) |mnemonic| {
        if (mnemonic == ._reserved) continue;
        const str = comptime @tagName(mnemonic);
        try mnemonic_lookup.put(alloc, str, mnemonic);
        const lower_str = comptime blk: {
            comptime var buf = [_]u8{0} ** str.len;
            break :blk std.ascii.lowerString(&buf, str);
        };
        try mnemonic_lookup.put(alloc, lower_str, mnemonic);
    }

    try addSuffix("Z", .Z); // zero
    try addSuffix("EQ", .Z);
    try addSuffix("NZ", .NZ); // not zero
    try addSuffix("NEQ", .NZ);
    try addSuffix("LU", .LU); // less (unsigned)
    try addSuffix("NLU", .NLU); // not less (unsigned)
    try addSuffix("GU", .GU); // greater (unsigned)
    try addSuffix("NGU", .NGU); // not greater (unsigned)
    try addSuffix("N", .N); // negative
    try addSuffix("NN", .NN); // not negative
    try addSuffix("C", .C); // carry
    try addSuffix("NC", .NC); // not carry
    try addSuffix("V", .V); // overflow
    try addSuffix("NV", .NV); // not overflow
    try addSuffix("LS", .LS); // less (signed)
    try addSuffix("NLS", .NLS); // not less (signed)
    try addSuffix("GS", .GS); // greater (signed)
    try addSuffix("NGS", .NGS); // not greater (signed)
    try addSuffix("P", .P); // positive
    try addSuffix("NP", .NP); // not positive
    try addSuffix("I", .I); // insn section
    try addSuffix("S", .S); // stack section
    try addSuffix("D", .D); // data section
    try addSuffix("W", .W); // data write section
    try addSuffix("DW", .W);
    try addSuffix("R", .R); // data read section
    try addSuffix("DR", .R);
}

pub fn deinit() void {
    var alloc = lookup_arena.allocator();
    mnemonic_lookup.deinit(alloc);
    mnemonic_suffix_lookup.deinit(alloc);
    lookup_arena.deinit();
}

pub const AstInstruction = struct {
    label: ?[]const u8,
    token: ?Token.Handle,
    mnemonic: Mnemonic,
    suffix: MnemonicSuffix,
    params: ?AstExpr.Handle,
    encoding: ?ie.InstructionEncoding = null,
    length: ?u7 = null,
    address: ?u32 = null,
};

pub const ATSection = enum {
    insn,
    stack,
    data,
};
pub const RegWidth = enum {
    b, // 8 bit
    r, // 16 bit
    x, // 32 bit
};
pub const ExprType = union(enum) {
    constant: i64,
    stat,
    ip,
    rp,
    sp,
    bp,
    asn,
    kxp,
    uxp,
    gpr: struct {
        index: ?u4,
        width: RegWidth,
        signedness: ?std.builtin.Signedness,
    },
    pointer_gpr: struct {
        index: ?u4,
        section: ATSection,
    },
    // relative: struct {
    //     base: *ExprType,
    //     offset: *ExprType,
    // },
};

pub const AstExpr = union(enum) {
    token: Token.Handle,
    type: ?ExprType,
    extra: union(enum) {
        list: Binary,
        arrow_list: Binary,
        int_literal,
        register_literal,
    },

    pub const Handle = u31;

    const Binary = struct {
        left: AstExpr.Handle,
        right: AstExpr.Handle,
    };
};

pub const ParseError = struct {
    token: Token.Handle,
    desc: []const u8,
};

pub const ParseResult = struct {

    instructions: std.ArrayListUnmanaged(AstInstruction) = .{},
    // types: std.AutoHashMapUnmanaged(ExprType) = .{},
    errors: std.ArrayListUnmanaged(ParseError) = .{},

    pub fn deinit(self: *ParseResult, alloc: std.mem.Allocator) void {
        self.instructions.deinit(alloc);
        self.errors.deinit(alloc);
    }
};

pub fn parse(alloc: std.mem.Allocator, source: []const u8, tokens: TokenList) !ParseResult {
    var parser = Parser {
        .source = source,
        .tokens = tokens,
        .next_token = 0,
        .alloc = alloc,
        .results = .{},
    };
    try parser.parseInstructions();
    return parser.results;
}

const Parser = struct {
    source: []const u8,
    tokens: TokenList,
    next_token: Token.Handle,
    alloc: std.mem.Allocator,
    results: ParseResult,

    fn skipLinespace(self: *Parser) void {
        _ = self.tryToken(.linespace);
    }

    fn tryToken(self: *Parser, kind: TokenKind) bool {
        if (self.tokens.items(.kind)[self.next_token] == kind) {
            self.next_token += 1;
            return true;
        } else {
            return false;
        }
    }

    fn recordError(self: *Parser, desc: []const u8) !void {
        try self.results.errors.append(self.alloc, .{
            .token = self.next_token,
            .desc = desc,
        });
    }
    fn recordErrorAbs(self: *Parser, desc: []const u8, token: Token.Handle) !void {
        try self.results.errors.append(self.alloc, .{
            .token = token,
            .desc = desc,
        });
    }
    fn recordErrorRel(self: *Parser, desc: []const u8, token_offset: i8) !void {
        try self.results.errors.append(self.alloc, .{
            .token = @intCast(Token.Handle, @as(i32, self.next_token) + token_offset),
            .desc = desc,
        });
    }

    fn tokenLocation(self: *Parser, handle: Token.Handle) []const u8 {
        return self.tokens.get(handle).location(self.source);
    }

    pub fn parseInstructions(self: *Parser) !void {
        while (try self.parseInstruction()) {}
    }

    fn parseInstruction(self: *Parser) !bool {
        const label = self.parseLabel();
        self.skipLinespace();
        if (self.tryToken(.dot)) {
            if (self.tryToken(.id)) {
                //const directive = self.tokenLocation(self.next_token - 1);
                try self.recordError("Directives not yet implemented");
            } else {
                try self.recordError("Expected directive");
            }
        } else if (self.tryToken(.id)) {
            const mnemonic_token = self.next_token - 1;
            const mnemonic_str = self.tokenLocation(mnemonic_token);
            const mnemonic = mnemonic_lookup.get(mnemonic_str) orelse ._reserved;

            const suffix1 = try self.parseSuffix();
            const suffix2 = try self.parseSuffix();

            var swap_params = false;

            var suffix = switch (suffix1) {
                .none => suffix2,
                .LU => switch (suffix2) {
                    .none => suffix1,
                    .GU => .LU_GU,
                    .Z => .LU_Z,
                    else => blk: {
                        try self.recordErrorRel("Invalid mnemonic suffix", -1);
                        break :blk suffix1;
                    },
                },
                .GU => switch (suffix2) {
                    .none => suffix1,
                    .LU => blk: {
                        swap_params = true;
                        break :blk .LU_GU;
                    },
                    .Z => .GU_Z,
                    else => blk: {
                        try self.recordErrorRel("Invalid mnemonic suffix", -1);
                        break :blk suffix1;
                    },
                },
                .Z => switch (suffix2) {
                    .none => suffix1,
                    .LU => blk: {
                        swap_params = true;
                        break :blk .LU_Z;
                    },
                    .GU => blk: {
                        swap_params = true;
                        break :blk .GU_Z;
                    },
                    .LS => blk: {
                        swap_params = true;
                        break :blk .LS_Z;
                    },
                    .GS => blk: {
                        swap_params = true;
                        break :blk .GS_Z;
                    },
                    .N => blk: {
                        swap_params = true;
                        break :blk .N_Z;
                    },
                    .P => blk: {
                        swap_params = true;
                        break :blk .P_Z;
                    },
                    else => blk: {
                        try self.recordErrorRel("Invalid mnemonic suffix", -1);
                        break :blk suffix1;
                    },
                },
                .LS => switch (suffix2) {
                    .none => suffix1,
                    .GS => .LS_GS,
                    .Z => .LS_Z,
                    else => blk: {
                        try self.recordErrorRel("Invalid mnemonic suffix", -1);
                        break :blk suffix1;
                    },
                },
                .GS =>  switch (suffix2) {
                    .none => suffix1,
                    .LS => blk: {
                        swap_params = true;
                        break :blk .LS_GS;
                    },
                    .Z => .GS_Z,
                    else => blk: {
                        try self.recordErrorRel("Invalid mnemonic suffix", -1);
                        break :blk suffix1;
                    },
                },
                .N => switch (suffix2) {
                    .none => suffix1,
                    .Z => .N_Z,
                    .P => .N_P,
                    else => blk: {
                        try self.recordErrorRel("Invalid mnemonic suffix", -1);
                        break :blk suffix1;
                    },
                },
                .P => switch (suffix2) {
                    .none => suffix1,
                    .Z => .P_Z,
                    .N => blk: {
                        swap_params = true;
                        break :blk .N_P;
                    },
                    else => blk: {
                        try self.recordErrorRel("Invalid mnemonic suffix", -1);
                        break :blk suffix1;
                    },
                },
                else => switch (suffix2) {
                    .none => suffix1,
                    else => blk: {
                        try self.recordErrorRel("Invalid mnemonic suffix", -1);
                        break :blk suffix1;
                    },
                }
            };

            const params = try self.parseExpr(swap_params);

            if (mnemonic == ._reserved) {
                try self.recordErrorAbs("Unrecognized mnemonic", mnemonic_token);
            } else {
                try self.results.instructions.append(self.alloc, .{
                    .label = label,
                    .token = mnemonic_token,
                    .mnemonic = mnemonic,
                    .suffix = suffix,
                    .params = params,
                });
            }
        } else if (label) |_| {
            try self.results.instructions.append(self.alloc, .{
                .label = label,
                .token = null,
                .mnemonic = ._reserved,
                .suffix = .none,
                .params = null,
            });
        }

        _ = self.tryToken(.linespace);
        _ = self.tryToken(.comment);
        while (true) {
            if (self.tryToken(.newline)) return true;
            if (self.tryToken(.eof)) return false;
            try self.recordError("Expected end of line");
            self.next_token += 1;
        }
    }

    fn parseSuffix(self: *Parser) !MnemonicSuffix {
        if (self.tryToken(.dot)) {
            if (self.tryToken(.id)) {
                const suffix_str = self.tokenLocation(self.next_token - 1);
                if (mnemonic_suffix_lookup.get(suffix_str)) |suffix| {
                    return suffix;
                } else {
                    try self.recordErrorRel("Unrecognized mnemonic suffix", -1);
                }
            } else {
                try self.recordError("Expected mnemonic suffix");
            }
        }
        return .none;
    }

    fn parseLabel(self: *Parser) ?[]const u8 {
        const begin = self.next_token;
        if (self.tryToken(.id) and self.tryToken(.colon)) {
            return self.tokens.get(begin).location(self.source);
        }
        self.next_token = begin;
        return null;
    }

    fn parseExpr(self: *Parser, swap_params: bool) !?AstExpr.Handle {
        _ = self;
        _ = swap_params;
        return null;
    }
};

test "Parser" {
    try init();
    defer deinit();
    const src =
        \\label:
        \\   nop //comment
        \\   sync
        \\   fret
        \\
        \\asdf: WFI
    ;
    var token_list = try lex.lex(std.testing.allocator, src);
    defer token_list.deinit(std.testing.allocator);
    var results = try parse(std.testing.allocator, src, token_list);
    defer results.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 0), results.errors.items.len);
    try std.testing.expectEqual(@as(usize, 5), results.instructions.items.len);

    var insn = results.instructions.items[0];
    try std.testing.expect(insn.label != null);
    try std.testing.expectEqualStrings("label", insn.label.?);
    try std.testing.expect(insn.token == null);
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
    try std.testing.expectEqual(Mnemonic.WFI, insn.mnemonic);
}
