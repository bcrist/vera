
fn test_lex(src: []const u8, expected_tokens: []const lex.Token_Kind) !void {
    // try std.io.getStdOut().writer().print("Testing lex of '{s}'\n", .{ src });
    var tokens = lex.lex(std.testing.allocator, src);
    defer tokens.deinit(std.testing.allocator);
    try std.testing.expectEqualSlices(lex.Token_Kind, expected_tokens, tokens.items(.kind));
}

test "Lexer linespace" {
    try test_lex(" ", &.{ .linespace, .eof });
    try test_lex("\t", &.{ .linespace, .eof });
    try test_lex("\x00", &.{ .linespace, .eof });
    try test_lex("   \t", &.{ .linespace, .eof });
    try test_lex("  \\\n  ", &.{ .linespace, .eof });
    try test_lex("  \\ \t \n  ", &.{ .linespace, .eof });
}

test "Lexer newline" {
    try test_lex("\n", &.{ .newline, .eof });
    try test_lex("\n\n", &.{ .newline, .newline, .eof });
}

test "Lexer ids" {
    try test_lex("abcd", &.{ .id, .eof });
    try test_lex("ab\\cd", &.{ .id, .eof });
    try test_lex("ab\\  \tcd", &.{ .id, .eof });
    try test_lex("abcd efg", &.{ .id, .linespace, .id, .eof });
}

test "Lexer integers" {
    try test_lex("1234", &.{ .int_literal, .eof });
    try test_lex("0123 0x10fff", &.{ .int_literal, .linespace, .int_literal, .eof });
    try test_lex("0b1010111010101", &.{ .int_literal, .eof });
    try test_lex("0o14777", &.{ .int_literal, .eof });
}

test "Lexer comments" {
    try test_lex("// comment", &.{ .comment, .eof });
    try test_lex(
        \\//aasdf
        \\//asdf asdf a;sdlfkjasdf@#$%&^#@()(*&)
    , &.{ .comment, .newline, .comment, .eof });
}

test "Lexer strings" {
    try test_lex("\"abc\"", &.{ .str_literal, .eof });
    try test_lex("\"\\\"\"", &.{ .str_literal, .eof });
    try test_lex("\"\\( U+123 0x55 0b1010 =0123 )\"", &.{ .str_literal, .eof });

    try test_lex(
        \\    \\
        \\    \\
        \\\\
        , &.{ .linespace, .str_literal_raw, .linespace, .str_literal_raw, .str_literal_raw, .eof });

    try test_lex(
        \\\\
        \\\\
        \\
        , &.{ .str_literal_raw, .str_literal_raw, .eof });
}

test "Lexer operators" {
    try test_lex(".,:->", &.{
        .dot,
        .comma,
        .colon,
        .arrow,
        .eof,
    });
}

test "Lexer examples" {
    try test_lex(
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

const lex = isa.lex;
const isa = @import("isa");
const std = @import("std");