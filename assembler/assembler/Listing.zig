gpa: std.mem.Allocator,
arena: std.heap.ArenaAllocator,
lines: Lines,
insns: Instructions,

const Lines = std.MultiArrayList(Line);
const Line_Index = u32;

const Instructions = std.ArrayListUnmanaged(Instruction);
const Instruction_Index = u31;

const listing_width = 80;

pub const Line = struct {
    kind: Kind,
    address: u32,
    length: u32,
    line_number: u32,
    source: []const u8,
    insn_index: ?Instruction_Index,

    pub const Kind = enum {
        filename, // source is filename
        empty,
        instruction,
        data8,
        data16,
        data32,
        alignment, // address is modulo, length is offset
        org,
        data_space,
        stack_space,
        insn_space,
    };
};

pub const Instruction = struct {
    form: isa.Instruction.Form,
    params: []const isa.Parameter,
};

pub fn init(gpa: std.mem.Allocator) Listing {
    return .{
        .gpa = gpa,
        .arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
        .lines = .{},
        .insns = .{},
    };
}

pub fn deinit(self: *Listing) void {
    self.arena.deinit();
    self.lines.deinit(self.gpa);
    self.insns.deinit(self.gpa);
}

fn maybe_dupe_string(self: *Listing, str: []const u8, dupe: bool) []const u8 {
    if (dupe) {
        return self.arena.allocator().dupe(u8, str) catch @panic("OOM");
    } else {
        return str;
    }
}

pub fn add_filename_line(self: *Listing, filename: []const u8, copy_to_arena: bool) void {
    self.lines.append(self.gpa, .{
        .kind = .filename,
        .address = 0,
        .length = 0,
        .line_number = 0,
        .source = self.maybe_dupe_string(filename, copy_to_arena),
        .insn_index = null,
    }) catch @panic("OOM");
}

pub fn add_non_output_line(self: *Listing, kind: Line.Kind, line_number: u32, source: []const u8, copy_source_to_arena: bool) void {
    switch (kind) {
        .empty, .insn_space, .data_space, .stack_space => {},
        else => unreachable,
    }

    self.lines.append(self.gpa, .{
        .kind = kind,
        .address = 0,
        .length = 0,
        .line_number = line_number,
        .source = self.maybe_dupe_string(source, copy_source_to_arena),
        .insn_index = null,
    }) catch @panic("OOM");
}

pub fn add_instruction_line(self: *Listing, address: u32, length: u32, insn: Instruction, line_number: u32, source: []const u8, copy_source_to_arena: bool) void {
    var final_insn = insn;
    final_insn.params = self.arena.allocator().dupe(isa.Parameter, insn.params) catch @panic("OOM");

    const insn_index = self.insns.items.len;
    self.insns.append(self.gpa, final_insn) catch @panic("OOM");

    self.lines.append(self.gpa, .{
        .kind = .instruction,
        .address = address,
        .length = length,
        .line_number = line_number,
        .source = self.maybe_dupe_string(source, copy_source_to_arena),
        .insn_index = @intCast(insn_index),
    }) catch @panic("OOM");
}

pub fn add_data_line(self: *Listing, address: u32, length: u32, kind: Line.Kind, line_number: u32, source: []const u8, copy_source_to_arena: bool) void {
    switch (kind) {
        .data8, .data16, .data32 => {},
        else => unreachable,
    }

    self.lines.append(self.gpa, .{
        .kind = kind,
        .address = address,
        .length = length,
        .line_number = line_number,
        .source = self.maybe_dupe_string(source, copy_source_to_arena),
        .insn_index = null,
    }) catch @panic("OOM");
}

pub fn add_alignment_line(self: *Listing, alignment: Assembler.Alignment, line_number: u32, source: []const u8, copy_source_to_arena: bool) void {
    self.lines.append(self.gpa, .{
        .kind = .alignment,
        .address = alignment.modulo,
        .length = alignment.offset,
        .line_number = line_number,
        .source = self.maybe_dupe_string(source, copy_source_to_arena),
        .insn_index = null,
    }) catch @panic("OOM");
}

pub fn add_org_line(self: *Listing, address: u32, line_number: u32, source: []const u8, copy_source_to_arena: bool) void {
    self.lines.append(self.gpa, .{
        .kind = .org,
        .address = address,
        .length = 0,
        .line_number = line_number,
        .source = self.maybe_dupe_string(source, copy_source_to_arena),
        .insn_index = null,
    }) catch @panic("OOM");
}

pub fn write_all(self: *const Listing, comptime MemoryContext: type, ctx: MemoryContext, writer: *std.io.Writer) !void {
    try write(self.lines.slice(), self.insns.items, MemoryContext, ctx, 0, @intCast(self.lines.len), writer);
}

pub fn write(lines: Lines.Slice, insns: []const Instruction, comptime MemoryContext: type, ctx: MemoryContext, begin: Line_Index, end: Line_Index, writer: *std.io.Writer) !void {
    const addresses = lines.items(.address);
    const lengths = lines.items(.length);
    const insn_indices = lines.items(.insn_index);

    var address_space = isa.Address_Space.data;

    for (begin..,
        lines.items(.kind)[begin..end],
        lines.items(.line_number)[begin..end],
        lines.items(.source)
    ) |line_index, line_kind, line_number, source| {
        switch (line_kind) {
            .filename => {
                try write_filename_line(source, writer);
            },
            .empty => {
                try write_empty_line(line_number, source, writer, false);
            },
            .alignment => {
                const modulo = addresses[line_index];
                const offset = lengths[line_index];
                try write_alignment_line(modulo, offset, line_number, source, writer);
            },
            .org => {
                try write_org_line(addresses[line_index], line_number, source, writer);
            },
            .instruction => {
                const address = addresses[line_index];
                const length = lengths[line_index];
                const insn = insns[insn_indices[line_index].?];
                try write_instruction_line(address, length, address_space, MemoryContext, ctx, insn, line_number, source, writer);
            },
            .data8 => {
                const address = addresses[line_index];
                const length = lengths[line_index];
                try write_data_line(u8, address, length, address_space, MemoryContext, ctx, line_number, source, writer);
            },
            .data16 => {
                const address = addresses[line_index];
                const length = lengths[line_index];
                try write_data_line(u16, address, length, address_space, MemoryContext, ctx, line_number, source, writer);
            },
            .data32 => {
                const address = addresses[line_index];
                const length = lengths[line_index];
                try write_data_line(u32, address, length, address_space, MemoryContext, ctx, line_number, source, writer);
            },
            .data_space => {
                address_space = .data;
                try write_address_space_line(address_space, line_number, source, writer);
            },
            .stack_space => {
                address_space = .stack;
                try write_address_space_line(address_space, line_number, source, writer);
            },

            .insn_space => {
                address_space = .insn;
                try write_address_space_line(address_space, line_number, source, writer);
            },
        }
    }
}

pub fn write_all_source(self: *Listing, writer: *std.io.Writer) !void {
    try write_source(self.lines.slice(), 0, @intCast(self.lines.len), writer);
}

pub fn write_source(lines: Lines.Slice, begin: Line_Index, end: Line_Index, writer: *std.io.Writer) !void {
    const line_numbers = lines.items(.line_number);
    const source = lines.items(.source);
    for (begin.., lines.items(.kind)[begin..end]) |line_index, line_kind| {
        switch (line_kind) {
            .filename => {
                const src = source[line_index];
                try write_filename_line(src, writer);
            },
            .empty, .alignment, .org, .instruction, .data8, .data16, .data32, .data_space, .stack_space, .insn_space => {
                const line_number = line_numbers[line_index];
                const src = source[line_index];
                try write_empty_line(line_number, src, writer, true);
            },
        }
    }
}

fn write_filename_line(filename: []const u8, writer: *std.io.Writer) !void {
    try writer.print(".source {f}\n", .{ std.ascii.hexEscape(filename, .upper) });
}

fn write_empty_line(line_number: u32, source: []const u8, writer: *std.io.Writer, source_only: bool) !void {
    try write_remaining_source_lines(0, line_number, std.mem.splitScalar(u8, source, '\n'), writer, source_only);
}

fn write_address_space_line(address_space: isa.Address_Space, line_number: u32, source: []const u8, writer: *std.io.Writer) !void {
    switch (address_space) {
        .data => try writer.writeAll(".dspace"),
        .stack => try writer.writeAll(".sspace"),
        .insn => try writer.writeAll(".ispace"),
    }
    try write_remaining_source_lines(7, line_number, std.mem.splitScalar(u8, source, '\n'), writer, false);
}

fn write_alignment_line(modulo: u32, offset: u32, line_number: u32, source: []const u8, writer: *std.io.Writer) !void {
    var buf: [listing_width]u8 = undefined;
    var align_text: []const u8 = undefined;
    if (offset > 0) {
        align_text = try std.fmt.bufPrint(&buf, ".align {}, {}", .{ modulo, offset });
    } else {
        align_text = try std.fmt.bufPrint(&buf, ".align {}", .{ modulo });
    }

    try writer.writeAll(align_text);
    try write_remaining_source_lines(@intCast(align_text.len), line_number, std.mem.splitScalar(u8, source, '\n'), writer, false);
}

fn write_org_line(address: u32, line_number: u32, source: []const u8, writer: *std.io.Writer) !void {
    var buf: [listing_width]u8 = undefined;
    const align_text = try std.fmt.bufPrint(&buf, ".org 0x{X}", .{ address });
    try writer.writeAll(align_text);
    try write_remaining_source_lines(@intCast(align_text.len), line_number, std.mem.splitScalar(u8, source, '\n'), writer, false);
}

fn write_instruction_line(
    address: u32,
    length: u32,
    address_space: isa.Address_Space,
    comptime MemoryContext: type,
    ctx: MemoryContext,
    insn: Instruction,
    line_number: u32,
    source: []const u8,
    writer: *std.io.Writer,
) !void {
    var buf: [listing_width]u8 = undefined;
    var buf_writer = std.io.Writer.fixed(&buf);

    try buf_writer.print("{X:0>8} ", .{ address });
    const isa_insn = isa.Instruction{
        .mnemonic = insn.form.signature.mnemonic,
        .params = insn.params,
    };

    isa.fmt.print_instruction(isa_insn, address, &buf_writer) catch {};
    buf_writer.writeByte(' ') catch {};

    try writer.writeAll(buf_writer.buffered());

    const line_cursor: u32 = @intCast(buf_writer.end);

    var d = [_]u8{0}**8;
    for (0..length) |offset| {
        d[offset] = ctx.read_byte(@intCast(address + offset), address_space);
    }

    return write_grouped_data_and_source(address, line_cursor, &d, &.{ @intCast(length) }, line_number, source, writer);
}

fn write_data_line(
    comptime T: type,
    initial_address: u32,
    length: u32,
    address_space: isa.Address_Space,
    comptime MemoryContext: type,
    ctx: MemoryContext,
    line_number: u32,
    source: []const u8,
    writer: *std.io.Writer,
) !void {
    var address = initial_address;
    var remaining: i64 = length;
    var cur_line_number = line_number;
    var source_iter = std.mem.splitScalar(u8, source, '\n');

    const word_size: u32 = @sizeOf(T);
    const chars_per_word = word_size * 2 + 1;
    const max_words_per_line = comptime wpl: {
        const available_chars = listing_width - 8 - 3;
        break :wpl available_chars / chars_per_word;
    };
    const max_bytes_per_line = max_words_per_line * word_size;

    var temp = [_]u8{0} ** max_bytes_per_line;
    while (remaining > 0) {
        const words_on_line = @min(max_words_per_line, @as(u32, @intCast(remaining + word_size - 1)) / word_size);
        const bytes_on_line = words_on_line * word_size;
        for (0..bytes_on_line) |i| {
            temp[bytes_on_line - 1 - i] = ctx.read_byte(@intCast(address + i), address_space);
        }

        const padding = listing_width - 8 - 1 - words_on_line * chars_per_word - 1;

        try writer.print("{X:0>8}", .{ address });
        try writer.splatByteAll(' ', padding);
        try writer.writeAll("!");

        for (temp[0..bytes_on_line], 0..) |b, i| {
            if (i % word_size == 0) {
                try writer.writeByte(' ');
            }
            try writer.print("{X:0>2}", .{ b });
        }

        try writer.writeByte(' ');

        try write_source_for_line(cur_line_number, source_iter.next() orelse "", writer, false);
        cur_line_number += 1;
        address += bytes_on_line;
        remaining -= bytes_on_line;
    }

    try write_remaining_source_lines(0, cur_line_number, source_iter, writer, false);
}

fn write_grouped_data_and_source(address: u32, line_cursor: u32, data: []const u8, groups: []const u8, line_number: u32, source: []const u8, writer: *std.io.Writer) !void {
    var cursor = line_cursor;
    var remaining_data = data;
    var remaining_groups = groups;
    var cur_line_number = line_number;
    var cur_address = address;
    var source_iter = std.mem.splitScalar(u8, source, '\n');

    while (remaining_groups.len > 0) {
        var num_groups_for_this_line = for (0..remaining_groups.len + 1) |groups_to_skip| {
            var end = cursor + 2;
            if (cursor == 0) end += 9;
            const groups_on_line = remaining_groups.len - groups_to_skip;
            for (remaining_groups[0..groups_on_line]) |group_size| {
                end += group_size * 2 + 1;
            }
            if (end <= listing_width) break groups_on_line;
        } else unreachable;

        if (num_groups_for_this_line == 0 and cursor == 0) {
            num_groups_for_this_line = 1;
        }

        var num_bytes_for_this_line: usize = 0;
        if (num_groups_for_this_line > 0) {
            if (cursor == 0) {
                try writer.print("{X:0>8} ", .{ cur_address });
                cursor += 9;
            }

            var buf: [listing_width]u8 = undefined;
            var buf_writer = std.io.Writer.fixed(&buf);

            try buf_writer.writeAll("! ");
            cursor += 2;

            for (0..num_groups_for_this_line) |line_group_index| {
                const group_index = num_groups_for_this_line - line_group_index - 1;

                const group_size = remaining_groups[group_index];

                var data_offset: u32 = 0;
                for (remaining_groups[0..group_index]) |gs| {
                    data_offset += gs;
                }

                for (0..group_size) |group_line_byte_index| {
                    const byte_index = group_size - group_line_byte_index - 1;
                    try buf_writer.print("{X:0>2}", .{ remaining_data[data_offset + byte_index] });
                    cursor += 2;
                    num_bytes_for_this_line += 1;
                }

                try buf_writer.writeByte(' ');
                cursor += 1;
            }

            if (cursor < listing_width) {
                try writer.splatByteAll(' ', listing_width - cursor);
            }
            try writer.writeAll(buf_writer.buffered());
        } else if (cursor < listing_width) {
            try writer.splatByteAll(' ', listing_width - cursor);
        }

        try write_source_for_line(cur_line_number, source_iter.next() orelse "", writer, false);

        cur_address += @intCast(num_bytes_for_this_line);
        remaining_data = remaining_data[num_bytes_for_this_line..];
        remaining_groups = remaining_groups[num_groups_for_this_line..];
        cur_line_number += 1;
        cursor = 0;
    }

    try write_remaining_source_lines(cursor, cur_line_number, source_iter, writer, false);
}

fn write_remaining_source_lines(line_cursor: u32, line_number: u32, line_iter: std.mem.SplitIterator(u8, .scalar), writer: *std.io.Writer, source_only: bool) !void {
    var iter = line_iter;
    const first_line = iter.next() orelse return;
    var i = line_number;
    if (first_line.len > 0) {
        if (!source_only and line_cursor < listing_width) {
            try writer.splatByteAll(' ', listing_width - line_cursor);
        }
        try write_source_for_line(i, first_line, writer, source_only);
        i += 1;
        while (iter.next()) |line| {
            if (!source_only) {
                try writer.splatByteAll(' ', listing_width);
            }
            try write_source_for_line(i, line, writer, source_only);
            if (i != 0) i += 1;
        }
    } else {
        try writer.writeByte('\n');
    }
}

fn write_source_for_line(line_number: u32, line_source: []const u8, writer: *std.io.Writer, source_only: bool) !void {
    const source = if (std.mem.endsWith(u8, line_source, "\r")) line_source[0..line_source.len - 1] else line_source;
    if (source.len == 0) {
        if (line_number == 0) {
            try writer.writeByte('\n');
        } else {
            if (!source_only) {
                try writer.writeAll("//");
            }
            try writer.print("{:>5} |\n", .{ line_number } );
        }
    } else {
        if (!source_only) {
            try writer.writeAll("//");
        }
        if (line_number == 0) {
            try writer.print("{f}\n", .{ std.ascii.hexEscape(source, .upper) } );
        } else {
            try writer.print("{:>5} | {f}\n", .{ line_number, std.ascii.hexEscape(source, .upper) } );
        }
    }
}

const Listing = @This();
const Assembler = @import("Assembler.zig");
const isa = @import("isa");
const arch = @import("arch");
const std = @import("std");
