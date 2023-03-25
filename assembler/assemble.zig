const std = @import("std");
const ie = @import("instruction_encoding");
const ie_data = @import("instruction_encoding_data").data;
const lex = @import("lex.zig");
const parse = @import("parse.zig");
const infer = @import("infer.zig");
const encode = @import("encode.zig");

const ParseError = parse.ParseError;

const Result = struct {
    tokens: lex.TokenList,
    sections: std.StringHashMapUnmanaged(encode.SectionData),
    errors: std.ArrayListUnmanaged(ParseError),
};

pub fn assemble(alloc: std.mem.Allocator, edb: ie.EncoderDatabase, source: []const u8, origin_address: u32) !Result {
    const tokens = try lex.lex(alloc, source);
    var parsed = try parse.parse(alloc, source, tokens);
    try infer.inferTypes(&parsed);
    try infer.inferEncodings(&parsed, alloc, edb, origin_address);
    return .{
        .tokens = tokens,
        .errors = parsed.errors,
        .sections = try encode.encode(alloc, parsed),
    };
}

test {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var temp = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const edb = try ie.EncoderDatabase.init(arena.allocator(), ie_data, temp.allocator());
    const ddb = try ie.DecoderDatabase.init(arena.allocator(), ie_data, temp.allocator());
    temp.deinit();

    const src =
        \\label:
        \\   nop //comment
        \\   sync.D
        \\   fret
        \\
        \\asdf: WFI
    ;

    var stderr = std.io.getStdErr().writer();

    var results = try assemble(arena.allocator(), edb, src, 0);
    for (results.errors.items) |err| {
        const token = results.tokens.get(err.token);
        try token.printContext(src, stderr, 160);
        try stderr.print("{s}\n", .{ err.desc });
    }
    try std.testing.expectEqual(@as(usize, 0), results.errors.items.len);

    _ = ddb;
}