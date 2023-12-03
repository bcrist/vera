pub fn generate(gpa: std.mem.Allocator, processor: *Processor, microcode: []const ?Control_Signals) !void {
    _ = gpa;

    try generate_encoding_table();
    try generate_control_signal_analysis(processor, microcode);
}

fn generate_control_signal_analysis(processor: *Processor, microcode: []const ?Control_Signals) !void {
    _ = microcode;


    var f = try std.fs.cwd().createFile("doc/isa/control_signal_usage.html", .{});
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
        \\<h1>Control Signal Usage Analysis</h1>
        , .{ style });

    inline for (comptime std.enums.values(Control_Signal)) |signal| {
        try analyze_control_signal_usage(processor, &.{ signal }, w);
    }

    try analyze_control_signal_usage(processor, &.{ .c_ij, .c_ik }, w);
    try analyze_control_signal_usage(processor, &.{ .c_ij, .c_ik, .c_iw }, w);
    try analyze_control_signal_usage(processor, &.{ .jl_src, .jh_src }, w);
    try analyze_control_signal_usage(processor, &.{ .sr1_ri, .sr2_ri }, w);
    try analyze_control_signal_usage(processor, &.{ .sr1_wi, .sr2_wi }, w);
    try analyze_control_signal_usage(processor, &.{ .sr1_wsrc, .sr2_wsrc }, w);
    try analyze_control_signal_usage(processor, &.{ .sr1_wi, .sr1_wsrc }, w);
    try analyze_control_signal_usage(processor, &.{ .sr2_wi, .sr2_wsrc }, w);
    try analyze_control_signal_usage(processor, &.{ .addr_space, .bus_width, .bus_dir }, w);
    try analyze_control_signal_usage(processor, &.{ .ij_op, .ik_op, .iw_op }, w);
    try analyze_control_signal_usage(processor, &.{ .bus_dir, .ij_op, .ik_op, .iw_op }, w);
    try analyze_control_signal_usage(processor, &.{ .seq_op, .special }, w);
    try analyze_control_signal_usage(processor, &.{ .allow_int, .special }, w);
    try analyze_control_signal_usage(processor, &.{ .allow_int, .seq_op }, w);
    try analyze_control_signal_usage(processor, &.{ .ll_src, .lh_src }, w);

    try w.writeAll(
        \\</body>
        \\</html>
        );
}

pub fn analyze_control_signal_usage(processor: *Processor, comptime signals: []const Control_Signal, writer: anytype) !void {
    processor.temp.reset();
    var temp = std.ArrayList(u8).init(processor.temp.allocator());
    var data = std.StringHashMap(u16).init(processor.temp.allocator());

    for (processor.microcode.cycles.items, 0..) |cycle, ua| {
        _ = ua;
    
        temp.clearRetainingCapacity();
        inline for (signals) |signal| {
            if (cycle.assigned_signals.contains(signal)) {
                const v = @field(cycle.signals, @tagName(signal));
                var w = temp.writer();
                switch (@typeInfo(@TypeOf(v))) {
                    .Enum  => {
                        if (std.enums.tagName(@TypeOf(v), v)) |tag| {
                            try w.print("{s}|", .{ tag });
                        } else {
                            try w.print("{}|", .{ @intFromEnum(v) });
                        }
                    },
                    .Bool  => try w.print("{}|", .{ v }),
                    .Union => try w.print("{X}|", .{ v.raw() }),
                    .Int   => try w.print("0x{X}|", .{ v }),
                    else   => try temp.appendSlice("???|"),
                }
            } else {
                try temp.appendSlice("---|");
            }
        }

        temp.items.len -= 1;

        var result = try data.getOrPut(temp.items);
        if (result.found_existing) {
            result.value_ptr.* += 1;
        } else {
            result.key_ptr.* = try processor.temp.allocator().dupe(u8, temp.items);
            result.value_ptr.* = 1;
        }
    }

    try writer.writeAll(
        \\
        \\<table class="control_signal_freq">
        \\  <tr>
        );
    inline for (signals) |signal| {
        try writer.print("<th class=\"signal\">{s}</th>", .{ @tagName(signal) });
    }
    try writer.writeAll(
        \\<th>Frequency</th></tr>
        );

    var iter = data.iterator();
    while (iter.next()) |entry| {
        try writer.writeAll(
            \\
            \\  <tr>
            );

        var column_iter = std.mem.splitScalar(u8, entry.key_ptr.*, '|');
        while (column_iter.next()) |item| {
            try writer.print("<td class=\"signal\">{s}</td>", .{ item });
        }

        try writer.print("<td>{}</td></tr>", .{ entry.value_ptr.* });
    }
    try writer.writeAll(
        \\
        \\</table>
        \\
    );
}

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
                if (opcode.value() == byte) {
                    title = try std.fmt.bufPrint(&temp_buf, "0x{X:0>2}: {s}", .{ byte, @tagName(opcode) });
                    class = "lo8";
                    break;
                }
            } else for (std.enums.values(opcodes.Lo12)) |opcode| {
                const truncated: u8 = @truncate(opcode.value());
                if (truncated == byte) {
                    title = try std.fmt.bufPrint(&temp_buf, "0x{X:0>2}", .{ byte });
                    class = "lo12";
                    break;
                }
            } else for (std.enums.values(opcodes.Lo16)) |opcode| {
                const truncated: u8 = @truncate(opcode.value());
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
                if (opcode.value() == prefix_byte) {
                    title = try std.fmt.bufPrint(&temp_buf, "0x{X:0>2}{X:0>2}: {s}", .{ second_byte, prefix_byte, @tagName(opcode) });
                    class = "lo8";
                    break;
                }
            } else for (std.enums.values(opcodes.Lo12)) |opcode| {
                const truncated: u8 = @truncate(opcode.value());
                if (truncated == prefix_byte) {
                    const remaining: u4 = @intCast(opcode.value() >> 8);
                    if (remaining == @as(u4, @truncate(second_byte))) {
                        title = try std.fmt.bufPrint(&temp_buf, "0x{X:0>2}{X:0>2}: {s}", .{ second_byte, prefix_byte, @tagName(opcode) });
                        class = "lo12";
                        break;
                    }
                }
            } else for (std.enums.values(opcodes.Lo16)) |opcode| {
                const truncated: u8 = @truncate(opcode.value());
                if (truncated == prefix_byte) {
                    const remaining: u8 = @intCast(opcode.value() >> 8);
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

const style = @embedFile("style.css");

const Processor = @import("Processor.zig");
const opcodes = @import("opcodes.zig");
const Control_Signals = hw.Control_Signals;
const Control_Signal = hw.Control_Signal;
const hw = arch.hw;
const arch = @import("lib_arch");
const bits = @import("bits");
const std = @import("std");
