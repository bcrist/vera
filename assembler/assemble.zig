const std = @import("std");
const ie = @import("instruction_encoding");
const ie_data = @import("instruction_encoding_data").data;
const Assembler = @import("Assembler.zig");
const typechecking = @import("typechecking.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};

    var temp = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const edb = try ie.EncoderDatabase.init(arena.allocator(), ie_data, temp.allocator());

    var a = Assembler.init(gpa.allocator(), arena.allocator(), edb);

    var arg_iter = try std.process.ArgIterator.initWithAllocator(temp.allocator());
    _ = arg_iter.next(); // ignore assemble command name
    while (arg_iter.next()) |arg| {
        const source = try std.fs.cwd().readFileAlloc(arena.allocator(), arg, 100_000_000);
        _ = a.adoptSource(try arena.allocator().dupe(u8, arg), source);
    }
    temp.deinit();

    a.assemble();

    try a.dump(std.io.getStdOut().writer());

}
