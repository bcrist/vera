const std = @import("std");

pub const TokenKind = enum(u8) {
    reserved,
    eof,
    linespace,
    newline,
    comment,
    int_literal,
    str_literal,
    str_literal_raw,
    id,
    dot,
    comma,
    colon,
    arrow,
    paren_open,
    paren_close,
    plus,
    plus_plus,
    minus,
    star,
    star_star,
    shl,
    shr,
    amp,
    bar,
    caret,
    tilde,
    apostrophe,
    at,
    money,
};

pub const Token = struct {
    offset: u32,
    kind: TokenKind,

    pub const Handle = u31;
    pub const Range = struct {
        first: Handle,
        last: Handle,

        pub fn expand(self: ?Range, t: Handle) Range {
            return if (self) |s| .{
                .first = @min(s.first, t),
                .last = @max(s.last, t),
            } else .{
                .first = t,
                .last = t,
            };
        }
    };

    pub fn countNewlines(self: Token, source: []const u8) u32 {
        var remaining = source[self.offset..];
        return switch (self.kind) {
            .newline => 1,

            .reserved,
            .dot,
            .comma,
            .colon,
            .paren_open,
            .paren_close,
            .plus,
            .minus,
            .star,
            .amp,
            .bar,
            .caret,
            .tilde,
            .apostrophe,
            .at,
            .money,
            .arrow,
            .shl,
            .shr,
            .plus_plus,
            .star_star,
            .eof,
            .id,
            .comment,
            .int_literal,
            .str_literal,
            => 0,

            .str_literal_raw => blk: {
                var end: usize = 2;
                while (end < remaining.len) : (end += 1) {
                    switch (remaining[end]) {
                        '\n' => break :blk 1,
                        else => {},
                    }
                }
                break :blk 0;
            },

            .linespace => blk: {
                var newlines: u32 = 0;
                var consume_newline = remaining[0] == '\\';
                var end: usize = 1;
                while (end < remaining.len) : (end += 1) {
                    switch (remaining[end]) {
                        0...9, 11...' ', 127 => {},
                        '\\' => {
                            if (remaining[end - 1] == '\\') {
                                break;
                            }
                            consume_newline = true;
                        },
                        '\n' => {
                            if (consume_newline) {
                                consume_newline = false;
                                newlines += 1;
                            } else {
                                break;
                            }
                        },
                        else => break,
                    }
                }
                break :blk newlines;
            },
        };
    }

    pub fn location(self: Token, source: []const u8) []const u8 {
        var remaining = source[self.offset..];
        var token_len = switch (self.kind) {
            .eof => 0,

            .reserved,
            .newline,
            .dot,
            .comma,
            .colon,
            .paren_open,
            .paren_close,
            .plus,
            .minus,
            .star,
            .amp,
            .bar,
            .caret,
            .tilde,
            .apostrophe,
            .at,
            .money,
            => 1,

            .arrow,
            .shl,
            .shr,
            .plus_plus,
            .star_star,
            => 2,

            .id => blk: {
                var consume_linespace = false;
                var end: usize = 1;
                while (end < remaining.len) : (end += 1) {
                    switch (remaining[end]) {
                        'A'...'Z', 'a'...'z', '0'...'9', '_', 128...255 => {
                            consume_linespace = false;
                        },
                        '\\' => {
                            consume_linespace = true;
                        },
                        0...9, 11...' ', 127 => if (!consume_linespace) break,
                        else => break,
                    }
                }
                break :blk end;
            },

            .linespace => blk: {
                var consume_newline = remaining[0] == '\\';
                var end: usize = 1;
                while (end < remaining.len) : (end += 1) {
                    switch (remaining[end]) {
                        0...9, 11...' ', 127 => {},
                        '\\' => {
                            if (remaining[end - 1] == '\\') {
                                break :blk end - 1;
                            }
                            consume_newline = true;
                        },
                        '\n' => {
                            if (consume_newline) {
                                consume_newline = false;
                            } else {
                                break;
                            }
                        },
                        else => break,
                    }
                }
                break :blk end;
            },

            .comment => blk: {
                var end: usize = 2;
                while (end < remaining.len) : (end += 1) {
                    switch (remaining[end]) {
                        '\n' => break,
                        else => {},
                    }
                }
                break :blk end;
            },

            .str_literal_raw => blk: {
                var end: usize = 2;
                while (end < remaining.len) : (end += 1) {
                    switch (remaining[end]) {
                        '\n' => break :blk end + 1,
                        else => {},
                    }
                }
                break :blk end;
            },

            .int_literal => if (remaining.len <= 1) 1 else blk: {
                var end: usize = 1;
                while (end < remaining.len) : (end += 1) {
                    switch (remaining[end]) {
                        '0'...'9', 'A'...'Z', 'a'...'z', '_' => {},
                        else => break,
                    }
                }
                break :blk end;
            },

            .str_literal => blk: {
                var end: usize = 1;
                var escape: enum {
                    none,
                    single,
                    paren,
                } = .none;
                while (end < remaining.len) : (end += 1) {
                    const ch = remaining[end];
                    switch (escape) {
                        .none => switch (ch) {
                            '"' => break :blk end + 1,
                            '\n' => break :blk end,
                            '\\' => escape = .single,
                            else => {},
                        },
                        .single => switch (ch) {
                            '(' => escape = .paren,
                            '\n' => break :blk end,
                            else => escape = .none,
                        },
                        .paren => switch (ch) {
                            '"' => break :blk end + 1,
                            ')' => escape = .none,
                            '\n' => break :blk end,
                            else => {},
                        },
                    }
                }
                break :blk end;
            },
        };

        return remaining[0..token_len];
    }
};
pub const TokenList = std.MultiArrayList(Token);

pub fn lex(allocator: std.mem.Allocator, source: []const u8) TokenList {
    var tokens = TokenList{};
    tokens.setCapacity(allocator, source.len / 2 + 100) catch @panic("OOM");

    var i: u32 = 0;
    while (i < source.len) {
        var token = Token{
            .offset = i,
            .kind = undefined,
        };
        token.kind = switch (source[i]) {
            'A'...'Z', 'a'...'z', '_', 128...255 => .id,
            '\n' => .newline,
            '0'...'9' => .int_literal,
            '.' => .dot,
            ',' => .comma,
            ':' => .colon,
            '(' => .paren_open,
            ')' => .paren_close,
            '"' => .str_literal,
            '&' => .amp,
            '|' => .bar,
            '^' => .caret,
            '~' => .tilde,
            '\'' => .apostrophe,
            '@' => .at,
            '$' => .money,
            '+' => if (source.len <= i + 1) .plus else switch (source[i + 1]) {
                '+' => .plus_plus,
                else => .plus,
            },
            '*' => if (source.len <= i + 1) .star else switch (source[i + 1]) {
                '*' => .star_star,
                else => .star,
            },
            '-' => if (source.len <= i + 1) .minus else switch (source[i + 1]) {
                '>' => .arrow,
                else => .minus,
            },
            '/' => if (source.len <= i + 1) .reserved else switch (source[i + 1]) {
                '/' => .comment,
                else => .reserved,
            },
            '<' => if (source.len <= i + 1) .reserved else switch (source[i + 1]) {
                '<' => .shl,
                else => .reserved,
            },
            '>' => if (source.len <= i + 1) .reserved else switch (source[i + 1]) {
                '>' => .shr,
                else => .reserved,
            },
            '\\' => if (source.len <= i + 1) .linespace else switch (source[i + 1]) {
                '\\' => .str_literal_raw,
                else => .linespace,
            },
            0...9, 11...' ', 127 => .linespace,
            else => .reserved,
        };

        tokens.append(allocator, token) catch @panic("OOM");
        i += @intCast(u32, token.location(source).len);
    }

    tokens.append(allocator, .{
        .offset = @intCast(u32, source.len),
        .kind = .eof,
    }) catch @panic("OOM");

    if (tokens.capacity > (tokens.len / 4) * 5) blk: {
        var old = tokens;
        tokens = tokens.clone(allocator) catch break :blk;
        old.deinit(allocator);
    }
    return tokens;
}

fn testLex(src: []const u8, expected_tokens: []const TokenKind) !void {
    // try std.io.getStdOut().writer().print("Testing lex of '{s}'\n", .{ src });
    var tokens = lex(std.testing.allocator, src);
    defer tokens.deinit(std.testing.allocator);
    try std.testing.expectEqualSlices(TokenKind, expected_tokens, tokens.items(.kind));
}

test "Lexer linespace" {
    try testLex(" ", &.{ .linespace, .eof });
    try testLex("\t", &.{ .linespace, .eof });
    try testLex("\x00", &.{ .linespace, .eof });
    try testLex("   \t", &.{ .linespace, .eof });
    try testLex("  \\\n  ", &.{ .linespace, .eof });
    try testLex("  \\ \t \n  ", &.{ .linespace, .eof });
}

test "Lexer newline" {
    try testLex("\n", &.{ .newline, .eof });
    try testLex("\n\n", &.{ .newline, .newline, .eof });
}

test "Lexer ids" {
    try testLex("abcd", &.{ .id, .eof });
    try testLex("ab\\cd", &.{ .id, .eof });
    try testLex("ab\\  \tcd", &.{ .id, .eof });
    try testLex("abcd efg", &.{ .id, .linespace, .id, .eof });
}

test "Lexer integers" {
    try testLex("1234", &.{ .int_literal, .eof });
    try testLex("0123 0x10fff", &.{ .int_literal, .linespace, .int_literal, .eof });
    try testLex("0b1010111010101", &.{ .int_literal, .eof });
    try testLex("0o14777", &.{ .int_literal, .eof });
}

test "Lexer comments" {
    try testLex("// comment", &.{ .comment, .eof });
    try testLex(
        \\//aasdf
        \\//asdf asdf a;sdlfkjasdf@#$%&^#@()(*&)
    , &.{ .comment, .newline, .comment, .eof });
}

test "Lexer strings" {
    try testLex("\"abc\"", &.{ .str_literal, .eof });
    try testLex("\"\\\"\"", &.{ .str_literal, .eof });
    try testLex("\"\\( U+123 0x55 0b1010 =0123 )\"", &.{ .str_literal, .eof });

    try testLex(
        \\    \\
        \\    \\
        \\\\
        , &.{ .linespace, .str_literal_raw, .linespace, .str_literal_raw, .str_literal_raw, .eof });

    try testLex(
        \\\\
        \\\\
        \\
        , &.{ .str_literal_raw, .str_literal_raw, .eof });
}

test "Lexer operators" {
    try testLex(".,:->", &.{
        .dot,
        .comma,
        .colon,
        .arrow,
        .eof,
    });
}

test "Lexer examples" {
    try testLex(
        \\label:
        \\   addc r0, 15 -> r0 //comment
        \\   neg r0
        \\   b label
        \\
        \\asdf: .db 0 1 2 0x54
    , &.{
        .id, .colon, .newline,
        .linespace, .id, .linespace, .id, .comma, .linespace, .int_literal, .linespace, .arrow, .linespace, .id, .linespace, .comment, .newline,
        .linespace, .id, .linespace, .id, .newline,
        .linespace, .id, .linespace, .id, .newline,
        .newline,
        .id, .colon, .linespace, .dot, .id, .linespace, .int_literal, .linespace, .int_literal, .linespace, .int_literal, .linespace, .int_literal, .eof
    });
}
