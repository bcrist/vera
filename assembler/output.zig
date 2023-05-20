const std = @import("std");
const ie = @import("isa_encoding");
const srec = @import("srec");
const ihex = @import("ihex");
const sx = @import("sx");
const bus = @import("bus_types");
const layout = @import("layout.zig");
const Assembler = @import("Assembler.zig");
const SourceFile = @import("SourceFile.zig");
const PageData = @import("PageData.zig");
const Instruction = @import("Instruction.zig");
const Section = @import("Section.zig");
const Listing = @import("Listing.zig");

pub const HexOptions = struct {
    format: enum {
        motorola,
        intel,
    } = .intel,
    file_extension: []const u8 = "hex",
    num_roms: u8 = 1,           // e.g. 4 for boot rom
    rom_width_bytes: u8 = 2,    // e.g. 2 for boot rom
    address_offset: i64 = 0,    // added to addresses before being placed in roms
    address_range: Assembler.AddressRange = .{
        .first = 0,
        .len = 0x1_0000_0000,
    },
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

        const address_range = chunk.getAddressRange(a).intersectionWith(options.address_range);
        if (address_range.len == 0) {
            continue;
        }

        var address: usize = address_range.first;
        const last_address = address_range.last();
        while (address < last_address) {
            const page = @truncate(bus.Page, address >> @bitSizeOf(bus.PageOffset));
            const page_end = (@as(u64, page) + 1) << @bitSizeOf(bus.PageOffset);
            const end = @intCast(u32, @min(page_end, last_address + 1));

            if (a.page_lookup.get(page)) |page_data_handle| {
                const begin_offset = @truncate(bus.PageOffset, address);
                const end_offset = @truncate(bus.PageOffset, end);

                var data: []const u8 = &page_data[page_data_handle];
                if (end_offset == 0) {
                    data = data[begin_offset..];
                } else {
                    data = data[begin_offset..end_offset];
                }

                const offset_address = @bitCast(u32, @truncate(i32, options.address_offset +% @intCast(i64, address)));

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

            address = end;
        }
    }
}

pub const SimSxOptions = struct {
    include_listing: bool = true,
};
pub fn writeSimSx(a: *Assembler, temp_alloc: std.mem.Allocator, writer: anytype, options: SimSxOptions) !void {
    var sx_writer = sx.writer(temp_alloc, writer);
    defer sx_writer.deinit();

    try sx_writer.openExpanded();

    try writeSimSxPage(a, 0, &sx_writer);
    var last_page_written: bus.Page = 0;

    for (a.chunks.items) |chunk_and_address| {
        const address_range = chunk_and_address.chunk.getAddressRange(a);
        if (address_range.len == 0) {
            continue;
        }
        const first_page = address_range.first >> @bitSizeOf(bus.PageOffset);
        const last_page = (address_range.first + @intCast(u32, address_range.len - 1)) >> @bitSizeOf(bus.PageOffset);

        for (first_page..last_page + 1) |page_usize| {
            const page = @intCast(bus.Page, page_usize);
            if (page > last_page_written) {
                try writeSimSxPage(a, page, &sx_writer);
                last_page_written = page;
            }
        }
    }

    if (options.include_listing) {
        var listing = createListing(a, temp_alloc, .{});
        defer listing.deinit();

        const lines_slice = listing.lines.slice();
        const addresses = lines_slice.items(.address);
        const lengths = lines_slice.items(.length);
        const insn_indices = lines_slice.items(.insn_index);

        try sx_writer.expressionExpanded("listing");
        for (0.., lines_slice.items(.kind), lines_slice.items(.line_number), lines_slice.items(.source)) |l, kind, line_number, source| {
            switch (kind) {
                .filename => {
                    try sx_writer.expression("source");
                    try sx_writer.string(source);
                    try sx_writer.close();
                },
                .empty, .data_space, .stack_space, .insn_space => {
                    try sx_writer.expression(switch (kind) {
                        .empty => "_",
                        .data_space => "dspace",
                        .stack_space => "sspace",
                        .insn_space => "ispace",
                        else => unreachable,
                    });
                    try writeListingSxLineAndSource(line_number, source, &sx_writer);
                    try sx_writer.close();
                },
                .instruction => {
                    const insn = listing.insns.items[insn_indices[l].?];
                    try sx_writer.expression("insn");
                    try sx_writer.printValue("{X:0>8}", .{ addresses[l] });
                    try sx_writer.int(lengths[l], 10);
                    try ie.data.writeInstructionEncoding(@TypeOf(writer), &sx_writer, insn.encoding, null, true);
                    try writeListingSxLineAndSource(line_number, source, &sx_writer);
                    try sx_writer.close();
                },
                .data8, .data16, .data32 => {
                    try sx_writer.expression(switch (kind) {
                        .data8 => "db",
                        .data16 => "dw",
                        .data32 => "dd",
                        else => unreachable,
                    });
                    try sx_writer.printValue("{X:0>8}", .{ addresses[l] });
                    try sx_writer.int(lengths[l], 10);
                    try writeListingSxLineAndSource(line_number, source, &sx_writer);
                    try sx_writer.close();
                },
                .alignment => {
                    try sx_writer.expression("align");
                    try sx_writer.int(addresses[l], 10);
                    const offset = lengths[l];
                    if (offset > 0) {
                        try sx_writer.int(offset, 10);
                    }
                    try writeListingSxLineAndSource(line_number, source, &sx_writer);
                    try sx_writer.close();
                },
            }
        }
        try sx_writer.close();
    }

    try sx_writer.done();
}

fn writeListingSxLineAndSource(line_number: u32, source: []const u8, sx_writer: anytype) !void {
    if (line_number > 0) {
        try sx_writer.expression("line");
        try sx_writer.int(line_number, 10);
        try sx_writer.close();
    }
    if (source.len > 0) {
        try sx_writer.expression("src");
        try sx_writer.string(source);
        try sx_writer.close();
    }
}

pub fn writeSimSxPage(a: *const Assembler, page: bus.Page, sx_writer: anytype) !void {
    if (a.page_lookup.get(page)) |page_data_handle| {
        const pages_slice = a.pages.slice();
        const page_data = pages_slice.items(.data);
        const page_usage = pages_slice.items(.usage);
        const page_sections = pages_slice.items(.section);

        const section = a.sections.values()[page_sections[page_data_handle]];
        const full_page_data = &page_data[page_data_handle];

        try sx_writer.expression("page");
        if (section.kind == .boot) {
            try sx_writer.string("flash");
            try sx_writer.printValue("{X:0>3}", .{ page + 1008 });
        } else {
            try sx_writer.string("ram");
            try sx_writer.printValue("{X:0>5}", .{ page });
        }
        sx_writer.setCompact(false);

        var used = page_usage[page_data_handle];
        var unused = used;
        unused.toggleAll();

        var begin: usize = 0;
        while (used.findFirstSet()) |used_range_begin| {
            unused.setRangeValue(.{
                .start = begin,
                .end = used_range_begin,
            }, false);

            const max_bytes_per_line = 64;
            const max_base64_length = comptime std.base64.standard.Encoder.calcSize(max_bytes_per_line);

            var used_range_end = unused.findFirstSet() orelse PageData.page_size;
            if (used_range_end - used_range_begin > max_bytes_per_line) {
                used_range_end = used_range_begin + max_bytes_per_line;
            }

            const data = full_page_data[used_range_begin..used_range_end];
            var buf: [max_base64_length]u8 = undefined;
            const encoded = std.base64.standard.Encoder.encode(&buf, data);

            try sx_writer.open();
            try sx_writer.printValue("{X:0>3}", .{ used_range_begin });
            try sx_writer.string(encoded);
            try sx_writer.close();

            used.setRangeValue(.{
                .start = begin,
                .end = used_range_end,
            }, false);
            begin = used_range_end;
        }

        try sx_writer.close();
    }
}

pub const ListOptions = struct {
    ordering: enum {
        file,
        address,
    } = .address,
    clone_source_strings: bool = false,
};
pub fn createListing(a: *Assembler, gpa: std.mem.Allocator, options: ListOptions) Listing {
    var listing = Listing.init(gpa);
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
                    addListingForChunk(a, &listing, .{
                        .section = section,
                        .file = file.handle,
                        .instructions = .{
                            .begin = begin,
                            .end = end,
                        },
                    }, keep, last_file, options);
                    last_file = file.handle;
                }
            }
        },
        .address => {
            var last_file: ?SourceFile.Handle = null;
            for (a.chunks.items) |chunk_and_address| {
                addListingForChunk(a, &listing, chunk_and_address.chunk, true, last_file, options);
                last_file = chunk_and_address.chunk.file;
            }
        },
    }
    return listing;
}

fn addListingForChunk(a: *Assembler, listing: *Listing, chunk: SourceFile.Chunk, keep: bool, last_file: ?SourceFile.Handle, options: ListOptions) void {
    const file = a.getSource(chunk.file);
    const s = file.slices();

    const tokens = s.insn.items(.token);
    const params = s.insn.items(.params);
    const operations = s.insn.items(.operation);
    const addresses = s.insn.items(.address);
    const lengths = s.insn.items(.length);
    const line_numbers = s.insn.items(.line_number);

    const token_slice = file.tokens.slice();
    const token_kinds = token_slice.items(.kind);
    const token_offsets = token_slice.items(.offset);

    if (chunk.file != last_file) {
        listing.addFilenameLine(file.name, options.clone_source_strings);
    }

    var iter = chunk.instructions;
    while (iter.next()) |i| {
        var start_of_line = tokens[i];
        var end_of_line = start_of_line;
        while (start_of_line > 0 and token_kinds[start_of_line - 1] != .newline) {
            start_of_line -= 1;
        }
        while (token_kinds[end_of_line] != .newline and token_kinds[end_of_line] != .eof) {
            end_of_line += 1;
            std.debug.assert(end_of_line <= file.tokens.len);
        }
        var line_source = file.source[token_offsets[start_of_line] .. token_offsets[end_of_line]];
        if (line_source.len > 0 and line_source[line_source.len - 1] == '\r') {
            line_source.len -= 1;
        }

        if (!keep) {
            listing.addNonOutputLine(.empty, line_numbers[i], line_source, options.clone_source_strings);
            continue;
        }

        switch (operations[i]) {
            .boot, .code, .kcode, .entry, .kentry => {
                listing.addNonOutputLine(.insn_space, line_numbers[i], line_source, options.clone_source_strings);
            },
            .data, .kdata, .@"const", .kconst, .section => {
                listing.addNonOutputLine(.data_space, line_numbers[i], line_source, options.clone_source_strings);
            },
            .stack => {
                listing.addNonOutputLine(.stack_space, line_numbers[i], line_source, options.clone_source_strings);
            },
            .none, .insn, .keep, .def, .undef, .org => {
                listing.addNonOutputLine(.empty, line_numbers[i], line_source, options.clone_source_strings);
            },

            .bound_insn => |encoding| {
                const insn = a.buildInstruction(s, addresses[i], encoding.mnemonic, encoding.suffix, params[i], false).?;
                listing.addInstructionLine(addresses[i], lengths[i], .{
                    .encoding = encoding.*,
                    .params = insn.params,
                }, line_numbers[i], line_source, options.clone_source_strings);
            },

            .push => {
                // TODO stack sections
            },

            .pop => {
                // TODO stack sections
            },

            .@"align" => {
                if (layout.getResolvedAlignment(s, params[i])) |alignment| {
                    listing.addAlignmentLine(alignment, line_numbers[i], line_source, options.clone_source_strings);
                } else {
                    listing.addNonOutputLine(.empty, line_numbers[i], line_source, options.clone_source_strings);
                }
            },

            .db => listing.addDataLine(addresses[i], lengths[i], .data8, line_numbers[i], line_source, options.clone_source_strings),
            .dw => listing.addDataLine(addresses[i], lengths[i], .data16, line_numbers[i], line_source, options.clone_source_strings),
            .dd => listing.addDataLine(addresses[i], lengths[i], .data32, line_numbers[i], line_source, options.clone_source_strings),
        }
    }
}
