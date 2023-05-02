const std = @import("std");
const lex = @import("lex.zig");
const Assembler = @import("Assembler.zig");
const SourceFile = @import("SourceFile.zig");
const Token = lex.Token;

const Error = @This();

file: SourceFile.Handle,
token: Token.Handle,
desc: []const u8,
flags: FlagSet,

pub const FlagSet = std.EnumSet(Flags);

pub const Flags = enum {
    remove_on_layout_reset,
    desc_is_allocated,
};

pub fn init(gpa: std.mem.Allocator, file: SourceFile.Handle, token: Token.Handle, comptime fmt: []const u8, args: anytype, flags: FlagSet) Error {
    const desc = std.fmt.allocPrint(gpa, fmt, args) catch @panic("OOM");
    var mutable_flags = flags;
    mutable_flags.insert(.desc_is_allocated);
    return .{
        .file = file,
        .token = token,
        .desc = desc,
        .flags = mutable_flags,
    };
}

pub fn print(self: Error, assembler: *const Assembler, writer: anytype) !void {
    const file = assembler.files.items[self.file];
    try writer.print("\n{s}: {s}\n", .{ file.name, self.desc });
    const token = file.tokens.get(self.token);
    try token.printContext(file.source, writer, 160);
}
