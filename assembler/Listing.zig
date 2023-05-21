const std = @import("std");
const isa = @import("isa_types");
const ie = @import("isa_encoding");
const Assembler = @import("Assembler.zig");

gpa: std.mem.Allocator,
arena: std.heap.ArenaAllocator,
lines: Lines,
insns: Instructions,

const Listing = @This();

const Lines = std.MultiArrayList(Line);
const LineIndex = u32;

const Instructions = std.ArrayListUnmanaged(Instruction);
const InstructionIndex = u31;

const listing_width = 80;

pub const Line = struct {
    kind: Kind,
    address: u32,
    length: u32,
    line_number: u32,
    source: []const u8,
    insn_index: ?InstructionIndex,

    pub const Kind = enum {
        filename, // source is filename
        empty,
        instruction,
        data8,
        data16,
        data32,
        alignment, // address is modulo, length is offset
        data_space,
        stack_space,
        insn_space,
    };
};

pub const Instruction = struct {
    encoding: ie.InstructionEncoding,
    params: []const ie.Parameter,
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

fn maybeDupeString(self: *Listing, str: []const u8, dupe: bool) []const u8 {
    if (dupe) {
        return self.arena.allocator().dupe(u8, str) catch @panic("OOM");
    } else {
        return str;
    }
}

pub fn addFilenameLine(self: *Listing, filename: []const u8, copy_to_arena: bool) void {
    self.lines.append(self.gpa, .{
        .kind = .filename,
        .address = 0,
        .length = 0,
        .line_number = 0,
        .source = self.maybeDupeString(filename, copy_to_arena),
        .insn_index = null,
    }) catch @panic("OOM");
}

pub fn addNonOutputLine(self: *Listing, kind: Line.Kind, line_number: u32, source: []const u8, copy_source_to_arena: bool) void {
    switch (kind) {
        .empty, .insn_space, .data_space, .stack_space => {},
        else => unreachable,
    }

    self.lines.append(self.gpa, .{
        .kind = kind,
        .address = 0,
        .length = 0,
        .line_number = line_number,
        .source = self.maybeDupeString(source, copy_source_to_arena),
        .insn_index = null,
    }) catch @panic("OOM");
}

pub fn addInstructionLine(self: *Listing, address: u32, length: u32, insn: Instruction, line_number: u32, source: []const u8, copy_source_to_arena: bool) void {
    var final_insn = insn;
    final_insn.params = self.arena.allocator().dupe(ie.Parameter, insn.params) catch @panic("OOM");

    const insn_index = self.insns.items.len;
    self.insns.append(self.gpa, final_insn) catch @panic("OOM");

    self.lines.append(self.gpa, .{
        .kind = .instruction,
        .address = address,
        .length = length,
        .line_number = line_number,
        .source = self.maybeDupeString(source, copy_source_to_arena),
        .insn_index = @intCast(u31, insn_index),
    }) catch @panic("OOM");
}

pub fn addDataLine(self: *Listing, address: u32, length: u32, kind: Line.Kind, line_number: u32, source: []const u8, copy_source_to_arena: bool) void {
    switch (kind) {
        .data8, .data16, .data32 => {},
        else => unreachable,
    }

    self.lines.append(self.gpa, .{
        .kind = kind,
        .address = address,
        .length = length,
        .line_number = line_number,
        .source = self.maybeDupeString(source, copy_source_to_arena),
        .insn_index = null,
    }) catch @panic("OOM");
}

pub fn addAlignmentLine(self: *Listing, alignment: Assembler.Alignment, line_number: u32, source: []const u8, copy_source_to_arena: bool) void {
    self.lines.append(self.gpa, .{
        .kind = .alignment,
        .address = alignment.modulo,
        .length = alignment.offset,
        .line_number = line_number,
        .source = self.maybeDupeString(source, copy_source_to_arena),
        .insn_index = null,
    }) catch @panic("OOM");
}

pub fn writeAll(self: *const Listing, comptime MemoryContext: type, ctx: MemoryContext, writer: anytype) !void {
    try write(self.lines.slice(), self.insns.items, MemoryContext, ctx, 0, @intCast(LineIndex, self.lines.len), writer);
}

pub fn write(lines: Lines.Slice, insns: []const Instruction, comptime MemoryContext: type, ctx: MemoryContext, begin: LineIndex, end: LineIndex, writer: anytype) !void {
    const addresses = lines.items(.address);
    const lengths = lines.items(.length);
    const insn_indices = lines.items(.insn_index);

    var address_space = isa.AddressSpace.data;

    for (begin..,
        lines.items(.kind)[begin..end],
        lines.items(.line_number)[begin..end],
        lines.items(.source)
    ) |line_index, line_kind, line_number, source| {
        switch (line_kind) {
            .filename => {
                try writeFilenameLine(source, writer);
            },
            .empty => {
                try writeEmptyLine(line_number, source, writer, false);
            },
            .alignment => {
                const modulo = addresses[line_index];
                const offset = lengths[line_index];
                try writeAlignmentLine(modulo, offset, line_number, source, writer);
            },
            .instruction => {
                const address = addresses[line_index];
                const length = lengths[line_index];
                const insn = insns[insn_indices[line_index].?];
                try writeInstructionLine(address, length, address_space, MemoryContext, ctx, insn, line_number, source, writer);
            },
            .data8 => {
                const address = addresses[line_index];
                const length = lengths[line_index];
                try writeDataLine(u8, address, length, address_space, MemoryContext, ctx, line_number, source, writer);
            },
            .data16 => {
                const address = addresses[line_index];
                const length = lengths[line_index];
                try writeDataLine(u16, address, length, address_space, MemoryContext, ctx, line_number, source, writer);
            },
            .data32 => {
                const address = addresses[line_index];
                const length = lengths[line_index];
                try writeDataLine(u32, address, length, address_space, MemoryContext, ctx, line_number, source, writer);
            },
            .data_space => {
                address_space = .data;
                try writeAddressSpaceLine(address_space, line_number, source, writer);
            },
            .stack_space => {
                address_space = .stack;
                try writeAddressSpaceLine(address_space, line_number, source, writer);
            },

            .insn_space => {
                address_space = .insn;
                try writeAddressSpaceLine(address_space, line_number, source, writer);
            },
        }
    }
}

pub fn writeAllSource(self: *Listing, comptime MemoryContext: type, ctx: MemoryContext, writer: anytype) !void {
    try writeSource(self.lines.slice(), self.insns.items, MemoryContext, ctx, 0, @intCast(LineIndex, self.lines.len), writer);
}

pub fn writeSource(lines: Lines.Slice, insns: []const Instruction, comptime MemoryContext: type, ctx: MemoryContext, begin: LineIndex, end: LineIndex, writer: anytype) !void {
    _ = ctx;
    _ = insns;
    const line_numbers = lines.items(.line_number);
    const source = lines.items(.source);
    for (begin.., lines.items(.kind)[begin..end]) |line_index, line_kind| {
        switch (line_kind) {
            .filename => {
                const src = source[line_index];
                try writeFilenameLine(src, writer);
            },
            .empty, .alignment, .instruction, .data8, .data16, .data32, .data_space, .stack_space, .insn_space => {
                const line_number = line_numbers[line_index];
                const src = source[line_index];
                try writeEmptyLine(line_number, src, writer, true);
            },
        }
    }
}

fn writeFilenameLine(filename: []const u8, writer: anytype) !void {
    try writer.print(".source {s}\n", .{ std.fmt.fmtSliceEscapeUpper(filename) });
}

fn writeEmptyLine(line_number: u32, source: []const u8, writer: anytype, source_only: bool) !void {
    try writeRemainingSourceLines(0, line_number, std.mem.split(u8, source, "\n"), writer, source_only);
}

fn writeAddressSpaceLine(address_space: isa.AddressSpace, line_number: u32, source: []const u8, writer: anytype) !void {
    switch (address_space) {
        .data => try writer.writeAll(".dspace"),
        .stack => try writer.writeAll(".sspace"),
        .insn => try writer.writeAll(".ispace"),
    }
    try writeRemainingSourceLines(7, line_number, std.mem.split(u8, source, "\n"), writer, false);
}

fn writeAlignmentLine(modulo: u32, offset: u32, line_number: u32, source: []const u8, writer: anytype) !void {
    var buf: [listing_width]u8 = undefined;
    var align_text: []const u8 = undefined;
    if (offset > 0) {
        align_text = try std.fmt.bufPrint(&buf, ".align {}, {}", .{ modulo, offset });
    } else {
        align_text = try std.fmt.bufPrint(&buf, ".align {}", .{ modulo });
    }

    try writer.writeAll(align_text);
    try writeRemainingSourceLines(@intCast(u32, align_text.len), line_number, std.mem.split(u8, source, "\n"), writer, false);
}

fn writeInstructionLine(
    address: u32,
    length: u32,
    address_space: isa.AddressSpace,
    comptime MemoryContext: type,
    ctx: MemoryContext,
    insn: Instruction,
    line_number: u32,
    source: []const u8,
    writer: anytype
) !void {
    var buf: [listing_width]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    var buf_writer = stream.writer();

    try buf_writer.print("{X:0>8} ", .{ address });
    const ie_insn = ie.Instruction{
        .mnemonic = insn.encoding.mnemonic,
        .suffix = insn.encoding.suffix,
        .params = insn.params,
    };

    ie_insn.print(buf_writer, address) catch |err| switch (err) {
        error.NoSpaceLeft => {},
        else => return err,
    };
    buf_writer.writeByte(' ') catch {};

    try writer.writeAll(stream.getWritten());

    var line_cursor = @intCast(u32, stream.pos);

    var d = [_]u8{0}**8;
    for (0..length) |offset| {
        d[offset] = ctx.readByte(@intCast(u32, address + offset), address_space);
    }

    var needs_ip_plus_2_byte = false;
    var needs_ip_plus_2_word = false;
    var needs_ip_plus_2_dword = false;
    var needs_ip_plus_4_word = false;

    for (insn.encoding.params) |param_encoding| {
        switch (param_encoding.base_src) {
            .implicit, .opcode, .OA, .OB, .OB_OA => {},
            .IP_plus_2_OA, .IP_plus_2_OB, .IP_plus_2_8 => needs_ip_plus_2_byte = true,
            .IP_plus_2_16 => needs_ip_plus_2_word = true,
            .IP_plus_2_32 => needs_ip_plus_2_dword = true,
            .IP_plus_4_16 => needs_ip_plus_4_word = true,
        }
        switch (param_encoding.offset_src) {
            .implicit, .opcode, .OA, .OB, .OB_OA => {},
            .IP_plus_2_OA, .IP_plus_2_OB, .IP_plus_2_8 => needs_ip_plus_2_byte = true,
            .IP_plus_2_16 => needs_ip_plus_2_word = true,
            .IP_plus_2_32 => needs_ip_plus_2_dword = true,
            .IP_plus_4_16 => needs_ip_plus_4_word = true,
        }
    }

    if (needs_ip_plus_2_dword) {
        return writeGroupedDataAndSource(address, line_cursor, &d, &.{ 2, 4 }, line_number, source, writer);
    } else if (needs_ip_plus_4_word) {
        return writeGroupedDataAndSource(address, line_cursor, &d, &.{ 2, 2, 2 }, line_number, source, writer);
    } else if (needs_ip_plus_2_word) {
        return writeGroupedDataAndSource(address, line_cursor, &d, &.{ 2, 2 }, line_number, source, writer);
    } else if (needs_ip_plus_2_byte) {
        return writeGroupedDataAndSource(address, line_cursor, &d, &.{ 2, 1 }, line_number, source, writer);
    } else {
        return writeGroupedDataAndSource(address, line_cursor, &d, &.{ 2 }, line_number, source, writer);
    }
}

fn writeDataLine(
    comptime T: type,
    initial_address: u32,
    length: u32,
    address_space: isa.AddressSpace,
    comptime MemoryContext: type,
    ctx: MemoryContext,
    line_number: u32,
    source: []const u8,
    writer: anytype,
) !void {
    var address = initial_address;
    var remaining: i64 = length;
    var cur_line_number = line_number;
    var source_iter = std.mem.split(u8, source, "\n");

    const word_size = @sizeOf(T);
    const chars_per_word = word_size * 2 + 1;
    const max_words_per_line = comptime wpl: {
        const available_chars = listing_width - 8 - 3;
        break :wpl available_chars / chars_per_word;
    };
    const max_bytes_per_line = max_words_per_line * word_size;

    var temp = [_]u8{0} ** max_bytes_per_line;
    while (remaining > 0) {
        const words_on_line = @min(max_words_per_line, @intCast(u32, (remaining + word_size - 1)) / word_size);
        const bytes_on_line = words_on_line * word_size;
        for (0..bytes_on_line) |i| {
            temp[bytes_on_line - 1 - i] = ctx.readByte(@intCast(u32, address + i), address_space);
        }

        try writer.print("{X:0>8}", .{ address });
        try writer.writeByteNTimes(' ', listing_width - 8 - 1 - words_on_line * chars_per_word - 1);
        try writer.writeAll("!");

        for (temp[0..bytes_on_line], 0..) |b, i| {
            if (i % word_size == 0) {
                try writer.writeByte(' ');
            }
            try writer.print("{X:0>2}", .{ b });
        }

        try writer.writeByte(' ');

        try writeSourceForLine(cur_line_number, source_iter.next() orelse "", writer, false);
        cur_line_number += 1;
        address += bytes_on_line;
        remaining -= bytes_on_line;
    }

    try writeRemainingSourceLines(0, cur_line_number, source_iter, writer, false);
}

fn writeGroupedDataAndSource(address: u32, line_cursor: u32, data: []const u8, groups: []const u8, line_number: u32, source: []const u8, writer: anytype) !void {
    var cursor = line_cursor;
    var remaining_data = data;
    var remaining_groups = groups;
    var cur_line_number = line_number;
    var cur_address = address;
    var source_iter = std.mem.split(u8, source, "\n");

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
            var stream = std.io.fixedBufferStream(&buf);
            var buf_writer = stream.writer();

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
                try writer.writeByteNTimes(' ', listing_width - cursor);
            }
            try writer.writeAll(stream.getWritten());
        } else if (cursor < listing_width) {
            try writer.writeByteNTimes(' ', listing_width - cursor);
        }

        try writeSourceForLine(cur_line_number, source_iter.next() orelse "", writer, false);

        cur_address += @intCast(u32, num_bytes_for_this_line);
        remaining_data = remaining_data[num_bytes_for_this_line..];
        remaining_groups = remaining_groups[num_groups_for_this_line..];
        cur_line_number += 1;
        cursor = 0;
    }

    try writeRemainingSourceLines(cursor, cur_line_number, source_iter, writer, false);
}

fn writeRemainingSourceLines(line_cursor: u32, line_number: u32, line_iter: std.mem.SplitIterator(u8), writer: anytype, source_only: bool) !void {
    var iter = line_iter;
    var first_line = iter.next() orelse return;
    var i = line_number;
    if (first_line.len > 0) {
        if (!source_only and line_cursor < listing_width) {
            try writer.writeByteNTimes(' ', listing_width - line_cursor);
        }
        try writeSourceForLine(i, first_line, writer, source_only);
        i += 1;
        while (iter.next()) |line| {
            if (!source_only) {
                try writer.writeByteNTimes(' ', listing_width);
            }
            try writeSourceForLine(i, line, writer, source_only);
            if (i != 0) i += 1;
        }
    } else {
        try writer.writeByte('\n');
    }
}

fn writeSourceForLine(line_number: u32, line_source: []const u8, writer: anytype, source_only: bool) !void {
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
            try writer.print("{s}\n", .{ std.fmt.fmtSliceEscapeUpper(source) } );
        } else {
            try writer.print("{:>5} | {s}\n", .{ line_number, std.fmt.fmtSliceEscapeUpper(source) } );
        }
    }
}
