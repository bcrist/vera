const std = @import("std");
const ie = @import("isa_encoding");
const Assembler = @import("Assembler.zig");
const dump = @import("dump.zig");
const output = @import("output.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};

    var temp = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const edb = try ie.data.EncoderDatabase.init(arena.allocator(), temp.allocator());

    var a = Assembler.init(gpa.allocator(), arena.allocator(), edb);

    var output_file = std.ArrayList(u8).init(gpa.allocator());

    var arg_iter = try std.process.ArgIterator.initWithAllocator(temp.allocator());
    _ = arg_iter.next(); // ignore assemble command name
    while (arg_iter.next()) |arg| {
        const source = try std.fs.cwd().readFileAlloc(arena.allocator(), arg, 100_000_000);
        _ = a.adoptSource(try arena.allocator().dupe(u8, arg), source);
        if (output_file.items.len == 0) {
            try output_file.appendSlice(arg);
        } else {
            output_file.clearRetainingCapacity();
            try output_file.appendSlice("a.out");
        }
    }
    temp.deinit();

    a.assemble();

    var list = output.createListing(&a, gpa.allocator(), .{});
    defer list.deinit();

    try list.writeAll(*Assembler, &a, std.io.getStdOut().writer());

    //try output.writeListing(&a, std.io.getStdOut().writer(), .{ .ordering = .address });

    // try dump.dump(&a, std.io.getStdOut().writer());

    try a.printErrors(std.io.getStdErr().writer());

    // try output.writeHex(&a, std.fs.cwd(), output_file.items, .{
    //     .merge_all_sections = true,
    // });

}
