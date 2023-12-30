pub fn generate(gpa: std.mem.Allocator, processor: *Processor, microcode: []const ?Control_Signals, decode_rom: []const hw.decode.Result) !void {
    _ = gpa;

    try std.fs.cwd().makePath("doc/isa");

    try generate_encoding_table(processor, decode_rom);
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

        const result = try data.getOrPut(temp.items);
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

fn mnemonic_color(mnemonic: isa.Mnemonic) u24 {
    var hasher = std.hash.Wyhash.init(0);
    std.hash.autoHash(&hasher, mnemonic);
    const hash = hasher.final();
    const hue: u8 = @truncate(hash >> 7);
    const hue_f32: f32 = @floatFromInt(hue);
    const lightness: u8 = @truncate(hash >> 16);
    const lightness_f32: f32 = @floatFromInt(lightness);
    const sat: u8 = @truncate(hash >> 24);
    const sat_f32: f32 = @floatFromInt(sat);
    return hsl_to_rgb(hue_f32 / 256, 0.4 + 0.5 * sat_f32 / 256, 0.5 + 0.3 * lightness_f32 / 256);
}

fn hsl_to_rgb(h: f32, s: f32, l: f32) u24 {
    if (s == 0) {
        const v: u8 = @intFromFloat(@min(255, l * 256));
        return bits.concat(.{ v, v, v });
    } else {
        const q = if (l < 0.5) l * (1 + s) else l + s - l * s;
        const p = 2 * l - q;
        const r = hue_to_rgb(p, q, h + 1.0/3.0);
        const g = hue_to_rgb(p, q, h);
        const b = hue_to_rgb(p, q, h - 1.0/3.0);
        return bits.concat(.{ r, g, b });
    }
}

fn hue_to_rgb(p: f32, q: f32, t: f32) u8 {
    var w = t;
    if (w < 0) w += 1;
    if (w > 1) w -= 1;
    if (w < 1.0/6.0) {
        w = p + (q - p) * 6 * w;
    } else if (w < 1.0/2.0) {
        w = q;
    } else if (w < 2.0/3.0) {
        w = p + (q - p) * (2.0/3.0 - w) * 6;
    }
    return @intFromFloat(@min(255, w * 256));
}

fn generate_encoding_table(processor: *Processor, decode_rom: []const hw.decode.Result) !void {
    var f = try std.fs.cwd().createFile("doc/isa/encoding_table.html", .{});
    defer f.close();

    const Cell_Info = struct {
        used_slots: u9 = 0,
        multiple_encodings: bool = false,
        instruction_encoding: ?Instruction_Encoding = null,
        mnemonic: Mnemonic = ._reserved,
        suffix: Mnemonic_Suffix = .none,
    };

    var cells: [256]Cell_Info = .{ .{} } ** 256;

    for (0.., processor.decode_rom.entries[0..0x10000], decode_rom[0..0x10000]) |addr, entry, result| {
        if (result.slot != .invalid_instruction) {
            const high_byte: u8 = @truncate(addr >> 8);
            const cell = &cells[high_byte];
            cell.used_slots += 1;

            if (!cell.multiple_encodings) {
                if (entry.instruction_encoding) |ie| {
                    if (cell.instruction_encoding) |existing| {
                        if (!ie.eql(existing)) {
                            cell.multiple_encodings = true;
                            cell.instruction_encoding = null;
                        }
                    } else {
                        cell.instruction_encoding = ie;
                        cell.mnemonic = ie.signature.mnemonic;
                        cell.suffix = ie.signature.suffix;
                    }
                }
            }
            if (entry.instruction_encoding) |ie| {
                if (ie.signature.mnemonic != cell.mnemonic) {
                    cell.mnemonic = ._reserved;
                    cell.suffix = .none;
                }
                if (ie.signature.suffix != cell.suffix) {
                    cell.suffix = .none;
                }
            }
        }
    }

    var w = f.writer();
    try w.print(
        \\<html>
        \\<head>
        \\<style>
        \\{s}
        \\</style>
        \\</head>
        \\<body>
        \\<h1>Instruction Encodings by Initial Word MSB</h1>
        \\<table class="encoding_table">
        \\<tr><th></th>
        , .{ style });

    for (0..0x10) |col| {
        try w.print(
            \\<th><code>+{X:0>1}xx</code></th>
            , .{ col });
    }

    for (0..0x10) |row_usize| {
        const row: u4 = @intCast(row_usize);
        try w.print(
            \\</tr>
            \\<tr><th><code>{X:0>1}000</code></th>
            , .{ row });

        for (0..0x10) |col_usize| {
            const col: u4 = @intCast(col_usize);

            const byte = bits.concat(.{ col, row });
            const cell = cells[byte];

            const color: u24 = c: {
                // if (cell.mnemonic != ._reserved) {
                //     break :c mnemonic_color(cell.mnemonic);
                // } else if (cell.has_slots) {
                //     break :c 0x777777;
                // } else {
                //     break :c 0xffffff;
                // }
                var red_factor: u24 = cell.used_slots / 2;
                var green_factor: u24 = 0;
                if (cell.used_slots > 0) {
                    red_factor += 0x30;
                }
                if (cell.used_slots == 256) {
                    green_factor += 0x50;
                }
                break :c 0xffffff - red_factor * 0x10000 - green_factor * 0x100;
            };

            try w.print(
                \\<td style="background:#{x:0>6}"
                , .{ color });

            if (cell.instruction_encoding) |ie| {
                try w.writeAll(" title=\"");
                try isa.print.print_encoding(ie, w);
                try w.writeAll("\"");
            }

            try w.writeAll(">");

            var end: []const u8 = "";
            if (cell.used_slots > 0) {
                try w.print("<a href=\"encoding_table_{x:0>2}.html\">", .{ byte });
                end = "</a>";
                try generate_encoding_table_inner(processor, decode_rom, byte);
            }

            if (cell.mnemonic != ._reserved) {
                try w.writeAll(@tagName(cell.mnemonic));
                if (cell.suffix != .none) {
                    try w.writeByte('.');
                    try w.writeAll(@tagName(cell.suffix));
                }
            }

            try w.writeAll(end);
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

fn generate_encoding_table_inner(processor: *Processor, decode_rom: []const hw.decode.Result, msb: u8) !void {
    const Cell_Info = struct {
        slot: hw.microcode.Slot = .invalid_instruction,
        instruction_encoding: ?Instruction_Encoding = null,
    };

    var cells: [256]Cell_Info = .{ .{} } ** 256;

    for (0..256) |byte| {
        const addr = bits.concat(.{ @as(u8, @intCast(byte)), msb });
        const entry = processor.decode_rom.entries[addr];
        const result = decode_rom[addr];
        cells[byte] = .{
            .slot = result.slot,
            .instruction_encoding = entry.instruction_encoding,
        };
    }

    var temp_buf: [1024]u8 = undefined;

    const filename = try std.fmt.bufPrint(&temp_buf, "doc/isa/encoding_table_{x:0>2}.html", .{ msb });

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
        \\<h1>Instruction Encodings with Initial Word 0x{X:0>2}xx</h1>
        \\<table class="encoding_table">
        \\<tr><th></th>
        , .{ style, msb });

    for (0..0x10) |col| {
        try w.print(
            \\<th><code>+{X:0>1}</code></th>
            , .{ col });
    }

    for (0..0x10) |row_usize| {
        const row: u4 = @intCast(row_usize);
        try w.print(
            \\</tr>
            \\<tr><th><code>{X:0>2}{X:0>1}0</code></th>
            , .{ msb, row });

        for (0..0x10) |col_usize| {
            const col: u4 = @intCast(col_usize);

            const byte = bits.concat(.{ col, row });
            const cell = cells[byte];

            const color: u24 = c: {
                if (cell.instruction_encoding) |ie| {
                    break :c mnemonic_color(ie.signature.mnemonic);
                } else {
                    break :c 0xffffff;
                }
            };

            try w.print(
                \\<td style="background:#{x:0>6}" title="Slot {}
                , .{ color, cell.slot.raw() });

            if (cell.instruction_encoding) |ie| {
                try w.writeAll(": ");
                try isa.print.print_encoding(ie, w);
            }

            try w.writeAll("\">");

            if (cell.instruction_encoding) |ie| {
                try w.writeAll(@tagName(ie.signature.mnemonic));
                if (ie.signature.suffix != .none) {
                    try w.writeByte('.');
                    try w.writeAll(@tagName(ie.signature.suffix));
                }
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

const style = @embedFile("style.css");

const Processor = @import("Processor.zig");
const Instruction_Encoding = isa.Instruction_Encoding;
const Mnemonic = isa.Mnemonic;
const Mnemonic_Suffix = isa.Mnemonic_Suffix;
const isa = arch.isa;
const Control_Signals = hw.Control_Signals;
const Control_Signal = hw.Control_Signal;
const hw = arch.hw;
const arch = @import("lib_arch");
const bits = @import("bits");
const std = @import("std");
