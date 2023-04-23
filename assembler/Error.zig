const std = @import("std");
const lex = @import("lex.zig");
const Assembler = @import("Assembler.zig");
const SourceFile = @import("SourceFile.zig");
const Token = lex.Token;

const Error = @This();

file: SourceFile.Handle,
token: Token.Handle,
desc: []const u8,

pub fn print(self: Error, assembler: Assembler, writer: anytype) !void {
    const file = assembler.files.items[self.file];
    try writer.print("{s}: {s}\n", .{ file.name, self.desc });
    const token = file.tokens.get(self.token);
    try token.printContext(file.source, writer, 160);
}
