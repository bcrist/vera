const std = @import("std");

pub const TokenKind = enum(u8) {
    reserved,
    eof,
    linespace,
    newline,
    comment,
    int_literal,
    id,
    dot,
    comma,
    colon,
    arrow,

    pub fn isIdentifier(self: TokenKind) bool {
        return switch (self) {
            .id => true,

            .reserved,
            .eof,
            .linespace,
            .newline,
            .comment,
            .int_literal,
            .dot,
            .comma,
            .colon,
            .arrow,
            => false,
        };
    }
};

pub const Token = struct {
    offset: u32,
    kind: TokenKind,

    pub const Handle = u31;

    pub fn location(self: Token, source: []const u8) []const u8 {
        var remaining = source[self.offset..];
        var token_len = switch (self.kind) {
            .eof => 0,

            .reserved,
            .newline,
            .dot,
            .comma,
            .colon,
            => 1,

            .arrow => 2,

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

            .int_literal => if (remaining.len <= 1) 1 else blk: {
                var consume_linespace = false;
                var end: usize = 1;
                while (end < remaining.len) : (end += 1) {
                    switch (remaining[end]) {
                        '0'...'9', 'A'...'Z', 'a'...'z', '_', 128...255 => {
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
        };

        return remaining[0..token_len];
    }

    pub fn int_value(self: Token, source: []const u8) !i64 {
        if (self.kind != .int_literal) return error.NotAnInteger;

        var remaining = self.location(source);
        std.debug.assert(remaining.len > 0);

        var radix: i64 = 10;
        if (remaining.len > 2 and remaining[0] == '0') {
            switch (remaining[1]) {
                'x', 'X' => {
                    radix = 16;
                    remaining = remaining[2..];
                },
                'b', 'B' => {
                    radix = 2;
                    remaining = remaining[2..];
                },
                'o', 'O' => {
                    radix = 8;
                    remaining = remaining[2..];
                },
            }
        }

        var accumulator: i64 = 0;
        for (remaining) |ch| {
            const digit = switch (ch) {
                '0'...'9' => ch - '0',
                'A'...'Z' => ch - 'A' + 10,
                'a'...'z' => ch - 'a' + 10,
                '_' => continue,
                else => return error.InvalidCharacter,
            };
            if (digit >= radix) {
                return error.InvalidCharacter;
            }
            var tmp = @mulWithOverflow(accumulator, radix);
            if (tmp[1]) return error.IntegerLiteralTooLarge;
            tmp = @addWithOverflow(tmp[0], digit);
            if (tmp[1]) return error.IntegerLiteralTooLarge;
            accumulator = tmp[0];
        }

        return accumulator;
    }
};
pub const TokenList = std.MultiArrayList(Token);

pub fn lex(allocator: std.mem.Allocator, source: []const u8) !TokenList {
    var tokens = TokenList{};
    try tokens.setCapacity(allocator, source.len / 2 + 100);

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
            '-' => if (source.len <= i + 1) .reserved else switch (source[i + 1]) {
                '>' => .arrow,
                else => .reserved,
            },
            '/' => if (source.len <= i + 1) .reserved else switch (source[i + 1]) {
                '/' => .comment,
                else => .reserved,
            },
            0...9, 11...' ', '\\', 127 => .linespace,
            else => .reserved,
        };

        try tokens.append(allocator, token);
        i += @intCast(u32, token.location(source).len);
    }

    try tokens.append(allocator, .{
        .offset = @intCast(u32, source.len),
        .kind = .eof,
    });

    if (tokens.capacity > (tokens.len / 4) * 5) blk: {
        var old = tokens;
        tokens = tokens.clone(allocator) catch break :blk;
        old.deinit(allocator);
    }
    return tokens;
}

fn testLex(src: []const u8, expected_tokens: []const TokenKind) !void {
    var tokens = try lex(std.testing.allocator, src);
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
