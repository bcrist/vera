/// A lexer for Vera assembly syntax
/// See usage in assembler.Parser and compile.Spec_Parser

pub const Token_Kind = enum (u8) {
    reserved,
    eof,
    linespace,
    newline,
    comment,
    int_literal,
    str_literal,
    str_literal_raw,
    id,
    comma,
    colon,
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
    kind: Token_Kind,

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

    pub fn count_new_lines(self: Token, source: []const u8) u32 {
        const remaining = source[self.offset..];
        return switch (self.kind) {
            .newline => 1,

            .reserved,
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

    pub fn span(self: Token, source: []const u8) []const u8 {
        var remaining = source[self.offset..];
        const token_len = switch (self.kind) {
            .eof => 0,

            .reserved,
            .newline,
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
                        'A'...'Z', 'a'...'z', '0'...'9', '_', '.', 128...255 => {
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
pub const Token_List = std.MultiArrayList(Token);

pub fn lex(allocator: std.mem.Allocator, source: []const u8) Token_List {
    var tokens = Token_List{};
    tokens.setCapacity(allocator, source.len / 2 + 100) catch @panic("OOM");

    var i: u32 = 0;
    while (i < source.len) {
        var token = Token{
            .offset = i,
            .kind = undefined,
        };
        token.kind = switch (source[i]) {
            'A'...'Z', 'a'...'z', '_', '.', 128...255 => .id,
            '\n' => .newline,
            '0'...'9' => .int_literal,
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
            '-' => .minus,
            '+' => if (source.len <= i + 1) .plus else switch (source[i + 1]) {
                '+' => .plus_plus,
                else => .plus,
            },
            '*' => if (source.len <= i + 1) .star else switch (source[i + 1]) {
                '*' => .star_star,
                else => .star,
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
        i += @intCast(token.span(source).len);
    }

    tokens.append(allocator, .{
        .offset = @intCast(source.len),
        .kind = .eof,
    }) catch @panic("OOM");

    if (tokens.capacity > (tokens.len / 4) * 5) blk: {
        var old = tokens;
        tokens = tokens.clone(allocator) catch break :blk;
        old.deinit(allocator);
    }
    return tokens;
}

pub const Case_Insensitive_Enum_Map_Options = struct {
    excluded_values: []const []const u8 = &.{},
    excluded_chars: []const u8 = "",
};
pub fn case_insensitive_enum_map(comptime T: type, comptime options: Case_Insensitive_Enum_Map_Options, comptime extra_entries: anytype) std.StaticStringMapWithEql(T, std.ascii.eqlIgnoreCase) {
    comptime var tuple_types: []const type = &.{};

    outer: inline for (comptime std.enums.values(T)) |val| {
        for (options.excluded_values) |excluded| {
            if (std.mem.eql(u8, @tagName(val), excluded)) {
                continue :outer;
            }
        }
        if (options.excluded_chars.len > 0) {
            if (std.mem.indexOfAny(u8, @tagName(val), options.excluded_chars) != null) {
                continue :outer;
            }
        }

        tuple_types = tuple_types ++ .{ @TypeOf(.{ @tagName(val), val }) };
    }
    inline for (extra_entries) |entry| {
        tuple_types = tuple_types ++ .{ @TypeOf(entry) };
    }

    comptime var entries: std.meta.Tuple(tuple_types) = undefined;
    comptime var n = 0;
    outer: inline for (comptime std.enums.values(T)) |val| {
        for (options.excluded_values) |excluded| {
            if (std.mem.eql(u8, @tagName(val), excluded)) {
                continue :outer;
            }
        }
        if (options.excluded_chars.len > 0) {
            if (std.mem.indexOfAny(u8, @tagName(val), options.excluded_chars) != null) {
                continue :outer;
            }
        }

        entries[n] = .{ @tagName(val), val };
        n += 1;
    }
    inline for (extra_entries) |entry| {
        entries[n] = entry;
        n += 1;
    }

    return std.StaticStringMapWithEql(T, std.ascii.eqlIgnoreCase).initComptime(entries);
}

const std = @import("std");
