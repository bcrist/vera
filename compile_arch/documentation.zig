pub fn generate(gpa: std.mem.Allocator, processor: *const Processor) !void {
    _ = gpa;
    _ = processor;

    try generate_encoding_table();
}

const style = @embedFile("style.css");

fn generate_encoding_table() !void {
    var f = try std.fs.cwd().createFile("doc/isa/encoding_table.html", .{});
    defer f.close();

    var w = f.writer();
    try w.print(
        \\<html>
        \\<head>
        \\<style>
        \\{s}
        \\</style>
        \\</head>
        \\<body>
        \\<h1>Instruction Encodings by Initial Byte</h1>
        \\<table class="encoding_table">
        \\<tr><th></th>
        , .{ style });

    for (0..0x10) |col| {
        try w.print(
            \\<th><code>+{X:0>1}</code></th>
            , .{ col });
    }

    var temp_buf: [1024]u8 = undefined;

    for (0..0x10) |row_usize| {
        const row: u4 = @intCast(row_usize);
        try w.print(
            \\</tr>
            \\<tr><th><code>{X:0>1}0</code></th>
            , .{ row });

        for (0..0x10) |col_usize| {
            const col: u4 = @intCast(col_usize);

            const byte = bits.concat(.{ col, row });

            var title: []const u8 = "";
            var class: []const u8 = "";

            for (std.enums.values(opcodes.Lo8)) |opcode| {
                if (@intFromEnum(opcode) == byte) {
                    title = try std.fmt.bufPrint(&temp_buf, "0x{X:0>2}: {s}", .{ byte, @tagName(opcode) });
                    class = "lo8";
                    break;
                }
            } else for (std.enums.values(opcodes.Lo12)) |opcode| {
                const truncated: u8 = @truncate(@intFromEnum(opcode));
                if (truncated == byte) {
                    title = try std.fmt.bufPrint(&temp_buf, "0x{X:0>2}", .{ byte });
                    class = "lo12";
                    break;
                }
            } else for (std.enums.values(opcodes.Lo16)) |opcode| {
                const truncated: u8 = @truncate(@intFromEnum(opcode));
                if (truncated == byte) {
                    title = try std.fmt.bufPrint(&temp_buf, "0x{X:0>2}", .{ byte });
                    class = "lo16";
                    break;
                }
            }

            try w.print(
                \\<td class="{s}" title="{s}">
                , .{ class, title });

            if (class.len > 0) {
                try w.print("<a href=\"encoding_table_{x:0>2}.html\"></a>", .{ byte });
                try generate_encoding_table_inner(byte);
            }

            try w.writeAll("</td>");
        }
    }

    try w.writeAll(
        \\</tr>
        \\</table>
        \\</body>
        \\</html>
        );
}

fn generate_encoding_table_inner(prefix_byte: u8) !void {
    var temp_buf: [1024]u8 = undefined;

    const filename = try std.fmt.bufPrint(&temp_buf, "doc/isa/encoding_table_{x:0>2}.html", .{ prefix_byte });

    var f = try std.fs.cwd().createFile(filename, .{});
    defer f.close();

    var w = f.writer();
    try w.print(
        \\<html>
        \\<head>
        \\<style>
        \\{s}
        \\</style>
        \\</head>
        \\<body>
        \\<h1>Instruction Encodings with Initial Byte 0x{X:0>2}</h1>
        \\<table class="encoding_table">
        \\<tr><th></th>
        , .{ style, prefix_byte });

    for (0..0x10) |col| {
        try w.print(
            \\<th><code>+{X:0>1}00</code></th>
            , .{ col });
    }


    for (0..0x10) |row_usize| {
        const row: u4 = @intCast(row_usize);
        try w.print(
            \\</tr>
            \\<tr><th><code>{X:0>1}0{X:0>2}</code></th>
            , .{ row, prefix_byte });

        for (0..0x10) |col_usize| {
            const col: u4 = @intCast(col_usize);

            const second_byte = bits.concat(.{ col, row });

            var title: []const u8 = "";
            var class: []const u8 = "";

            for (std.enums.values(opcodes.Lo8)) |opcode| {
                if (@intFromEnum(opcode) == prefix_byte) {
                    title = try std.fmt.bufPrint(&temp_buf, "0x{X:0>2}{X:0>2}: {s}", .{ second_byte, prefix_byte, @tagName(opcode) });
                    class = "lo8";
                    break;
                }
            } else for (std.enums.values(opcodes.Lo12)) |opcode| {
                const truncated: u8 = @truncate(@intFromEnum(opcode));
                if (truncated == prefix_byte) {
                    const remaining: u4 = @intCast(@intFromEnum(opcode) >> 8);
                    if (remaining == @as(u4, @truncate(second_byte))) {
                        title = try std.fmt.bufPrint(&temp_buf, "0x{X:0>2}{X:0>2}: {s}", .{ second_byte, prefix_byte, @tagName(opcode) });
                        class = "lo12";
                        break;
                    }
                }
            } else for (std.enums.values(opcodes.Lo16)) |opcode| {
                const truncated: u8 = @truncate(@intFromEnum(opcode));
                if (truncated == prefix_byte) {
                    const remaining: u8 = @intCast(@intFromEnum(opcode) >> 8);
                    if (remaining == second_byte) {
                        title = try std.fmt.bufPrint(&temp_buf, "0x{X:0>2}{X:0>2}: {s}", .{ second_byte, prefix_byte, @tagName(opcode) });
                        class = "lo16";
                        break;
                    }
                }
            }

            try w.print(
                \\<td class="{s}" title="{s}">
                , .{ class, title });

            try w.writeAll("</td>");
        }
    }

    try w.writeAll(
        \\</tr>
        \\</table>
        \\</body>
        \\</html>
        );
}


const Processor = @import("Processor.zig");
const opcodes = @import("opcodes.zig");
const bits = @import("bits");
const std = @import("std");
