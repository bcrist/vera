// --ssx filename.ext --ssx-no-list
// --srec filename.ext --split <num>:<width> --offset <address offset> --range min-max --merge --pretty
// --ihex filename.ext
// --list filename.ext --order file|address

pub fn main() !void {
    try console.init();
    defer console.deinit();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var temp = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var gpa = std.heap.GeneralPurposeAllocator(.{}) {};

    var pd: iedb.read_database.Parser_Data = .{
        .arena = arena.allocator(),
        .temp = gpa.allocator(),
    };

    const edb = try iedb.read_database.parse_encoding_db(&pd, @embedFile("iedb.sx"));

    var a = Assembler.init(gpa.allocator(), arena.allocator(), edb);

    var output_file = std.array_list.Managed(u8).init(gpa.allocator());

    var arg_iter = try std.process.ArgIterator.initWithAllocator(temp.allocator());
    _ = arg_iter.next(); // ignore assemble command name
    while (arg_iter.next()) |arg| {
        const source = try std.fs.cwd().readFileAlloc(arena.allocator(), arg, 100_000_000);
        _ = a.adopt_source(try arena.allocator().dupe(u8, arg), source);
        if (output_file.items.len == 0) {
            try output_file.appendSlice(arg);
        } else {
            output_file.clearRetainingCapacity();
            try output_file.appendSlice("a.out");
        }
    }
    temp.deinit();

    a.assemble();

    var list = output.create_listing(&a, gpa.allocator(), .{});
    defer list.deinit();

    try list.write_all(*Assembler, &a, std.io.getStdOut().writer());

    //try dump.dump(&a, std.io.getStdOut().writer());

    try a.print_errors(std.io.getStdErr().writer());

    // try output.write_hex(&a, std.fs.cwd(), output_file.items, .{
    //     .merge_all_sections = true,
    // });

    const simsx_filename = try std.fmt.allocPrint(arena.allocator(), "{s}.ssx", .{ output_file.items });
    var f = try std.fs.cwd().createFile(simsx_filename, .{});
    defer f.close();
    const writer = f.writer();
    try output.write_sim_sx(&a, gpa.allocator(), writer.any(), .{});

}

const arch = @import("arch");
const isa = @import("isa");
const iedb = @import("iedb");
const dump = assembler.dump;
const output = assembler.output;
const Assembler = assembler.Assembler;
const assembler = @import("assembler");
const console = @import("console");
const std = @import("std");
