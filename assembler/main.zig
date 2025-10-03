// --ssx filename.ext --ssx-no-list
// --srec filename.ext --split <num>:<width> --offset <address offset> --range min-max --merge --pretty
// --ihex filename.ext
// --list filename.ext --order file|address

pub fn main() !void {
    try console.init();
    defer console.deinit();

    const edb = try iedb.Encoding_Database.init(gpa);

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var temp = std.heap.ArenaAllocator.init(std.heap.page_allocator);

    var a = Assembler.init(gpa, arena.allocator(), edb);

    var output_file = std.io.Writer.Allocating.init(gpa);
    defer output_file.deinit();

    var arg_iter = try std.process.ArgIterator.initWithAllocator(temp.allocator());
    _ = arg_iter.next(); // ignore assemble command name
    while (arg_iter.next()) |arg| {
        const source = try std.fs.cwd().readFileAlloc(arena.allocator(), arg, 100_000_000);
        _ = a.adopt_source(try arena.allocator().dupe(u8, arg), source);
        if (output_file.writer.end == 0) {
            try output_file.writer.writeAll(arg);
        } else {
            output_file.writer.end = 0;
            try output_file.writer.writeAll("a.out");
        }
    }
    temp.deinit();

    a.assemble();

    var list = output.create_listing(&a, gpa, .{});
    defer list.deinit();

    var stdout_buf: [64]u8 = undefined;
    var stdout = std.fs.File.stdout().writer(&stdout_buf);

    try list.write_all(*Assembler, &a, &stdout.interface);
    try stdout.interface.flush();


    var stderr_buf: [64]u8 = undefined;
    var stderr = std.fs.File.stderr().writer(&stderr_buf);

    // try dump.dump(&a, &stderr.interface);

    try a.print_errors(&stderr.interface);
    try stderr.interface.flush();

    try output.write_hex(&a, std.fs.cwd(), output_file.written(), .{
        .merge_all_sections = true,
    });

    try output_file.writer.writeAll(".ssx");
    var f = try std.fs.cwd().createFile(output_file.written(), .{});
    defer f.close();
    var buf: [4096]u8 = undefined;
    var w = f.writer(&buf);
    try output.write_sim_sx(&a, gpa, &w.interface, .{});
    try w.interface.flush();
}

const gpa = std.heap.smp_allocator;

const arch = @import("arch");
const isa = @import("isa");
const iedb = @import("iedb");
const dump = assembler.dump;
const output = assembler.output;
const Assembler = assembler.Assembler;
const assembler = @import("assembler");
const console = @import("console");
const std = @import("std");
