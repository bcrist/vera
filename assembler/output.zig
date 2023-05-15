const std = @import("std");
const ie = @import("isa_encoding");
const srec = @import("srec");
const ihex = @import("ihex");
const bus = @import("bus_types");
const layout = @import("layout.zig");
const Assembler = @import("Assembler.zig");
const SourceFile = @import("SourceFile.zig");
const PageData = @import("PageData.zig");
const Instruction = @import("Instruction.zig");
const Section = @import("Section.zig");

pub const HexOptions = struct {
    format: enum {
        motorola,
        intel,
    } = .intel,
    file_extension: []const u8 = "hex",
    num_roms: u8 = 1,           // e.g. 4 for boot rom
    rom_width_bytes: u8 = 2,    // e.g. 2 for boot rom
    address_offset: i64 = 0,    // added to addresses before being placed in roms
    merge_all_sections: bool = true,
    add_spaces: bool = true,    // add spaces between fields in the hex records, making them more readable, but longer
};
pub fn writeHex(a: *const Assembler, dir: std.fs.Dir, output_filename_prefix: []const u8, options: HexOptions) !void {
    if (options.merge_all_sections) {
        try writeHexFilesForSection(a, null, dir, output_filename_prefix, options);
    } else {
        for (0.., a.sections.values()) |section_handle_usize, section| {
            if (section.has_chunks) {
                const section_handle = @intCast(Section.Handle, section_handle_usize);
                try writeHexFilesForSection(a, section_handle, dir, output_filename_prefix, options);
            }
        }
    }
}

pub fn writeHexFilesForSection(a: *const Assembler, maybe_section_handle: ?Section.Handle, dir: std.fs.Dir, output_filename_prefix: []const u8, options: HexOptions) !void {
    var filename_temp = std.ArrayList(u8).init(a.gpa);
    defer filename_temp.deinit();

    try filename_temp.appendSlice(output_filename_prefix);
    if (maybe_section_handle) |section_handle| {
        try filename_temp.append('.');
        const section = a.sections.values()[section_handle];
        for (section.name) |ch| {
            try filename_temp.append(switch (ch) {
                'a'...'z', 'A'...'Z', '0'...'9' => ch,
                else => '_',
            });
        }
    }

    const base_length = filename_temp.items.len;

    if (options.num_roms > 1) {
        for (0..options.num_roms) |rom_number| {
            filename_temp.items.len = base_length;
            try filename_temp.writer().print(".{}.{s}", .{ rom_number, options.file_extension });
            var f = try dir.createFile(filename_temp.items, .{});
            defer f.close();
            var writer = f.writer();
            try writeHexForSection(a, maybe_section_handle, writer, @intCast(u8, rom_number), options);
        }
    } else {
        filename_temp.items.len = base_length;
        try filename_temp.writer().print(".{s}", .{ options.file_extension });
        var f = try dir.createFile(filename_temp.items, .{});
        defer f.close();
        var writer = f.writer();
        try writeHexForSection(a, maybe_section_handle, writer, 0, options);
    }
}

pub fn writeHexForSection(a: *const Assembler, maybe_section_handle: ?Section.Handle, writer: anytype, rom_number: u8, options: HexOptions) !void {
    switch (options.format) {
        .motorola => {
            var buf: [8]u8 = undefined;
            const header = try std.fmt.bufPrint(&buf, "rom {}", .{ rom_number });
            var hex_writer = try srec.writer(u32, writer, header, options.add_spaces);
            try writeHexForSectionInner(a, maybe_section_handle, &hex_writer, rom_number, options);
            try hex_writer.finish(0);
        },
        .intel => {
            var hex_writer = ihex.writer(u32, writer, options.add_spaces);
            try writeHexForSectionInner(a, maybe_section_handle, &hex_writer, rom_number, options);
            try hex_writer.finish(null);
        },
    }
}

// writer should be srec.Writer or ihex.Writer
fn writeHexForSectionInner(a: *const Assembler, maybe_section_handle: ?Section.Handle, writer: anytype, rom_number: u8, options: HexOptions) !void {
    const page_data = a.pages.items(.data);

    var temp = std.ArrayListUnmanaged(u8){};
    defer temp.deinit(a.gpa);

    for (a.chunks.items) |chunk_and_address| {
        const chunk = chunk_and_address.chunk;
        if (!options.merge_all_sections) {
            if (!std.meta.eql(maybe_section_handle, chunk.section)) continue;
        }

        const address_range = chunk.getAddressRange(a);

        var begin = address_range.begin;
        while (begin < address_range.end) {
            const page = @truncate(bus.Page, begin >> @bitSizeOf(bus.PageOffset));
            const page_end = (@as(u64, page) + 1) << @bitSizeOf(bus.PageOffset);
            const end = @intCast(u32, @min(page_end, address_range.end));

            if (a.page_lookup.get(page)) |page_data_handle| {
                const begin_offset = @truncate(bus.PageOffset, begin);
                const end_offset = @truncate(bus.PageOffset, end);

                var data: []const u8 = &page_data[page_data_handle];
                if (end_offset == 0) {
                    data = data[begin_offset..];
                } else {
                    data = data[begin_offset..end_offset];
                }

                const offset_address = @bitCast(u32, @truncate(i32, options.address_offset +% begin));

                if (options.num_roms <= 1) {
                    try writer.write(offset_address, data);
                } else {
                    const span = options.num_roms * options.rom_width_bytes;
                    const offset_begin = rom_number * options.rom_width_bytes;
                    const offset_end = offset_begin + options.rom_width_bytes;

                    // Addresses in the hex file should only count bytes that would actually appear in that rom
                    var encoded_address = offset_address / span * options.rom_width_bytes;
                    var span_offset = offset_address % span;
                    if (span_offset >= offset_end) {
                        encoded_address += options.rom_width_bytes;
                    } else if (span_offset > offset_begin) {
                        encoded_address += span_offset - offset_begin;
                    }

                    temp.clearRetainingCapacity();
                    for (data) |b| {
                        if (span_offset >= offset_begin and span_offset < offset_end) {
                            temp.append(a.gpa, b) catch @panic("OOM");
                        }
                        span_offset += 1;
                        if (span_offset == span) span_offset = 0;
                    }
                    if (temp.items.len > 0) {
                        try writer.write(encoded_address, temp.items);
                    }
                    temp.clearRetainingCapacity();
                }
            }

            begin = end;
        }
    }
}

pub const SimSxOptions = struct {
    address_offset: i64 = 0,    // added to addresses before being placed in roms
};
pub fn writeSimSx() !void {
    // TODO
}

pub const ListOptions = struct {
    ordering: enum {
        file,
        address,
    } = .address,
};
pub fn writeListing(a: *Assembler, writer: anytype, options: ListOptions) !void {
    switch (options.ordering) {
        .file => {
            var last_file: ?SourceFile.Handle = null;
            for (a.files.items) |*file| {
                const blocks = file.blocks.slice();
                for (blocks.items(.first_insn),
                    blocks.items(.end_insn),
                    blocks.items(.section),
                    blocks.items(.keep),
                ) |begin, end, section, keep| {
                    try writeListingForChunk(a, writer, .{
                        .section = section,
                        .file = file.handle,
                        .instructions = .{
                            .begin = begin,
                            .end = end,
                        },
                    }, keep, last_file);
                    last_file = file.handle;
                }
            }
        },
        .address => {
            var last_file: ?SourceFile.Handle = null;
            for (a.chunks.items) |chunk_and_address| {
                try writeListingForChunk(a, writer, chunk_and_address.chunk, true, last_file);
                last_file = chunk_and_address.chunk.file;
            }
        },
    }
}

const listing_width = 80;

fn writeListingForChunk(a: *Assembler, writer: anytype, chunk: SourceFile.Chunk, keep: bool, last_file: ?SourceFile.Handle) !void {
    const file = a.getSource(chunk.file);
    const s = file.slices();

    const insn_tokens = s.insn.items(.token);
    const insn_params = s.insn.items(.params);
    const insn_operations = s.insn.items(.operation);
    const insn_addresses = s.insn.items(.address);
    const insn_lengths = s.insn.items(.length);

    const tokens = file.tokens.slice();
    const token_kinds = tokens.items(.kind);
    const token_offsets = tokens.items(.offset);

    if (chunk.file != last_file) {
        try writer.print(".source {s}\n", .{ file.name });
    }

    var iter = chunk.instructions;
    while (iter.next()) |insn_handle| {
        var start_of_line = insn_tokens[insn_handle];
        var end_of_line = start_of_line;
        while (start_of_line > 0 and token_kinds[start_of_line - 1] != .newline) {
            start_of_line -= 1;
        }
        while (token_kinds[end_of_line] != .newline and token_kinds[end_of_line] != .eof) {
            end_of_line += 1;
            std.debug.assert(end_of_line <= file.tokens.len);
        }
        const line = file.source[token_offsets[start_of_line] .. token_offsets[end_of_line]];

        if (!keep) {
            try writer.writeByteNTimes(' ', listing_width);
            try writer.print("// {s}\n", .{ line } );
            continue;
        }

        switch (insn_operations[insn_handle]) {
            .section, .code, .kcode, .entry, .kentry,
            .data, .kdata, .@"const", .kconst, .stack,
            .none, .insn, .keep, .def, .undef,
            => {
                try writer.writeByteNTimes(' ', listing_width);
                try writer.print("// {s}\n", .{ line } );
            },

            .bound_insn => |encoding| {
                try writeInstructionListing(a, s, insn_handle, encoding.*, line, writer);
            },

            .push => {
                // TODO stack sections
            },

            .pop => {
                // TODO stack sections
            },

            .org => {
                if (insn_params[insn_handle]) |expr_handle| {
                    if (s.expr.items(.resolved_constant)[expr_handle]) |constant| {
                        a.constant_temp.clearRetainingCapacity();
                        var temp_writer = a.constant_temp.writer(a.gpa);
                        try temp_writer.print(".org 0x{X}", .{ constant.asInt(u32) catch 0 });
                        try writer.writeAll(a.constant_temp.items);
                        try writer.writeByteNTimes(' ', listing_width - a.constant_temp.items.len);
                        try writer.print("// {s}\n", .{ line } );
                        a.constant_temp.clearRetainingCapacity();
                    }
                }
            },

            .@"align" => {
                if (layout.getResolvedAlignment(s, insn_params[insn_handle])) |alignment| {
                    a.constant_temp.clearRetainingCapacity();
                    var temp_writer = a.constant_temp.writer(a.gpa);
                    try temp_writer.print(".align {}", .{ alignment.modulo });
                    if (alignment.offset > 0) {
                        try temp_writer.print(", {}", .{ alignment.offset });
                    }
                    try writer.writeAll(a.constant_temp.items);
                    try writer.writeByteNTimes(' ', listing_width - a.constant_temp.items.len);
                    a.constant_temp.clearRetainingCapacity();
                } else {
                    try writer.writeByteNTimes(' ', 80);
                }
                try writer.print("// {s}\n", .{ line } );
            },

            .db => try writeDataListing(u8, a, insn_addresses[insn_handle], insn_lengths[insn_handle], line, writer),
            .dw => try writeDataListing(u16, a, insn_addresses[insn_handle], insn_lengths[insn_handle], line, writer),
            .dd => try writeDataListing(u32, a, insn_addresses[insn_handle], insn_lengths[insn_handle], line, writer),
        }
    }
}

fn writeDataListing(comptime W: type, a: *Assembler, initial_address: u32, length: u32, line: []const u8, writer: anytype) !void {
    var address = initial_address;
    var remaining = length;
    var write_line = true;
    var data_iter = PageData.DataIterator.init(a, address);

    const word_size = @sizeOf(W);
    const chars_per_word = word_size * 2 + 1;
    const max_words_per_line = comptime wpl: {
        const available_chars = listing_width - 8 - 3;
        break :wpl available_chars / chars_per_word;
    };
    const max_bytes_per_line = max_words_per_line * word_size;

    var temp = [_]u8{0} ** max_bytes_per_line;
    while (remaining > max_bytes_per_line) {
        try writer.print("{X:0>8}", .{ address });
        try writer.writeByteNTimes(' ', listing_width - 8 - 1 - max_words_per_line * chars_per_word - 1);
        try writer.writeAll("!");
        for (0..max_bytes_per_line) |i| {
            temp[max_bytes_per_line - 1 - i] = data_iter.next();
        }
        address += max_bytes_per_line;
        remaining -= max_bytes_per_line;

        for (temp, 0..) |b, i| {
            if (i % word_size == 0) {
                try writer.writeByte(' ');
            }
            try writer.print("{X:0>2}", .{ b });
        }

        if (write_line) {
            try writer.print(" // {s}\n", .{ line } );
            write_line = false;
        } else try writer.writeAll("\n");
    }

    a.constant_temp.clearRetainingCapacity();
    const words_remaining = (remaining + word_size - 1) / word_size;
    const bytes_remaining = words_remaining * word_size;
    for (0..bytes_remaining) |i| {
        temp[bytes_remaining - 1 - i] = data_iter.next();
    }

    var temp_writer = a.constant_temp.writer(a.gpa);
    try temp_writer.writeAll("!");
    for (temp[0..bytes_remaining], 0..) |b, i| {
        if (i % word_size == 0) {
            try temp_writer.writeByte(' ');
        }
        try temp_writer.print("{X:0>2}", .{ b });
    }

    try writer.print("{X:0>8}", .{ address });
    try writer.writeByteNTimes(' ', listing_width - 8 - a.constant_temp.items.len - 1);
    try writer.writeAll(a.constant_temp.items);
    a.constant_temp.clearRetainingCapacity();

    if (write_line) {
        try writer.print(" // {s}\n", .{ line } );
    } else {
        try writer.writeAll("\n");
    }
}

fn writeInstructionListing(a: *Assembler, s: SourceFile.Slices, insn_handle: Instruction.Handle, encoding: ie.InstructionEncoding, line: []const u8, writer: anytype) !void {
    const address = s.insn.items(.address)[insn_handle];
    const params = s.insn.items(.params)[insn_handle];

    a.constant_temp.clearRetainingCapacity();
    var temp_writer = a.constant_temp.writer(a.gpa);

    const insn = a.buildInstruction(s, address, encoding.mnemonic, encoding.suffix, params, false).?;
    try insn.print(temp_writer, address);
    try writer.print("{X:0>8} ", .{ address });
    try writer.writeAll(a.constant_temp.items);
    try writer.writeByte(' ');

    var line_cursor = 9 + a.constant_temp.items.len + 1;
    a.constant_temp.clearRetainingCapacity();

    var d = [_]u8{0}**8;
    _ = insn.write(encoding, &d);

    var needs_ip_plus_2_byte = false;
    var needs_ip_plus_2_word = false;
    var needs_ip_plus_2_dword = false;
    var needs_ip_plus_4_word = false;

    for (encoding.params) |param_encoding| {
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
        const cursor_with_pad = line_cursor + 3;

        var end = cursor_with_pad + 8 + 1 + 4;
        if (end <= listing_width) {
            try writer.writeByteNTimes(' ', listing_width - end);
            try writer.print("! {X:0>2}{X:0>2}{X:0>2}{X:0>2} {X:0>2}{X:0>2} // {s}\n", .{
                d[5], d[4], d[3], d[2], d[1], d[0], line
            });
            return;
        }

        end = cursor_with_pad + 4;
        if (end <= listing_width) {
            try writer.writeByteNTimes(' ', listing_width - end);
            try writer.print("! {X:0>2}{X:0>2} // {s}\n{X:0>8}", .{ d[1], d[0], line, address + 2 });
            try writer.writeByteNTimes(' ', listing_width - 8 - 3 - 8);
            try writer.print("! {X:0>2}{X:0>2}{X:0>2}{X:0>2}\n", .{ d[5], d[4], d[3], d[2] });
            return;
        }

        try writer.writeByteNTimes(' ', listing_width - line_cursor);
        try writer.print("// {s}\n", .{ line });
        try writer.writeByteNTimes(' ', listing_width - 3 - 8 - 1 - 4);
        try writer.print("! {X:0>2}{X:0>2}{X:0>2}{X:0>2} {X:0>2}{X:0>2}\n", .{
            d[5], d[4], d[3], d[2], d[1], d[0]
        });
        return;
    }

    if (needs_ip_plus_4_word) {
        const cursor_with_pad = line_cursor + 3;

        var end = cursor_with_pad + 4 + 1 + 4 + 1 + 4;
        if (end <= listing_width) {
            try writer.writeByteNTimes(' ', listing_width - end);
            try writer.print("! {X:0>2}{X:0>2} {X:0>2}{X:0>2} {X:0>2}{X:0>2} // {s}\n", .{
                d[5], d[4], d[3], d[2], d[1], d[0], line
            });
            return;
        }

        end = cursor_with_pad + 4 + 1 + 4;
        if (end <= listing_width) {
            try writer.writeByteNTimes(' ', listing_width - end);
            try writer.print("! {X:0>2}{X:0>2} {X:0>2}{X:0>2} // {s}\n{X:0>8}", .{
                d[3], d[2], d[1], d[0], line, address + 4
            });
            try writer.writeByteNTimes(' ', listing_width - 8 - 3 - 4);
            try writer.print("! {X:0>2}{X:0>2}\n", .{ d[5], d[4] });
            return;
        }

        end = cursor_with_pad + 4;
        if (end <= listing_width) {
            try writer.writeByteNTimes(' ', listing_width - end);
            try writer.print("! {X:0>2}{X:0>2} // {s}\n{X:0>8}", .{ d[1], d[0], line, address + 2 });
            try writer.writeByteNTimes(' ', listing_width - 8 - 3 - 4 - 1 - 4);
            try writer.print("! {X:0>2}{X:0>2} {X:0>2}{X:0>2}\n", .{ d[5], d[4], d[3], d[2] });
            return;
        }

        try writer.writeByteNTimes(' ', listing_width - line_cursor);
        try writer.print("// {s}\n", .{ line });
        try writer.writeByteNTimes(' ', listing_width - 3 - 4 - 1 - 4 - 1 - 4);
        try writer.print("! {X:0>2}{X:0>2} {X:0>2}{X:0>2} {X:0>2}{X:0>2}\n", .{
            d[5], d[4], d[3], d[2], d[1], d[0]
        });
        return;
    }

    if (needs_ip_plus_2_word) {
        const cursor_with_pad = line_cursor + 3;

        var end = cursor_with_pad + 4 + 1 + 4;
        if (end <= listing_width) {
            try writer.writeByteNTimes(' ', listing_width - end);
            try writer.print("! {X:0>2}{X:0>2} {X:0>2}{X:0>2} // {s}\n", .{ d[3], d[2], d[1], d[0], line });
            return;
        }

        end = cursor_with_pad + 4;
        if (end <= listing_width) {
            try writer.writeByteNTimes(' ', listing_width - end);
            try writer.print("! {X:0>2}{X:0>2} // {s}\n{X:0>8}", .{ d[1], d[0], line, address + 2 });
            try writer.writeByteNTimes(' ', listing_width - 8 - 3 - 4);
            try writer.print("! {X:0>2}{X:0>2}\n", .{ d[3], d[2] });
            return;
        }

        try writer.writeByteNTimes(' ', listing_width - line_cursor);
        try writer.print("// {s}\n", .{ line });
        try writer.writeByteNTimes(' ', listing_width - 3 - 4 - 1 - 4);
        try writer.print("! {X:0>2}{X:0>2} {X:0>2}{X:0>2}\n", .{ d[3], d[2], d[1], d[0] });
        return;
    }

    if (needs_ip_plus_2_byte) {
        const cursor_with_pad = line_cursor + 3;

        var end = cursor_with_pad + 2 + 1 + 4;
        if (end <= listing_width) {
            try writer.writeByteNTimes(' ', listing_width - end);
            try writer.print("! {X:0>2} {X:0>2}{X:0>2} // {s}\n", .{ d[2], d[1], d[0], line });
            return;
        }

        end = cursor_with_pad + 4;
        if (end <= listing_width) {
            try writer.writeByteNTimes(' ', listing_width - end);
            try writer.print("! {X:0>2}{X:0>2} // {s}\n{X:0>8}", .{ d[1], d[0], line, address + 2 });
            try writer.writeByteNTimes(' ', listing_width - 8 - 3 - 2);
            try writer.print("! {X:0>2}\n", .{ d[2] });
            return;
        }

        try writer.writeByteNTimes(' ', listing_width - line_cursor);
        try writer.print("// {s}\n", .{ line });
        try writer.writeByteNTimes(' ', listing_width - 3 - 2 - 1 - 4);
        try writer.print("! {X:0>2} {X:0>2}{X:0>2}\n", .{ d[2], d[1], d[0] });
        return;
    }

    const cursor_with_pad = line_cursor + 3;

    var end = cursor_with_pad + 4;
    if (end <= listing_width) {
        try writer.writeByteNTimes(' ', listing_width - end);
        try writer.print("! {X:0>2}{X:0>2} // {s}\n", .{ d[1], d[0], line });
        return;
    }

    try writer.writeByteNTimes(' ', listing_width - line_cursor);
    try writer.print("// {s}\n", .{ line });
    try writer.writeByteNTimes(' ', listing_width - 3 - 4);
    try writer.print("! {X:0>2}{X:0>2}\n", .{ d[1], d[0] });
}
