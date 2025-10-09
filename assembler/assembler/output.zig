pub fn copy_memory(a: *const Assembler, maybe_section_handle: ?Section.Handle, allocator: std.mem.Allocator) ![]u8 {
    const page_data = a.pages.items(.data);

    var max_addr: u32 = 0;

    for (a.chunks.items) |chunk_and_address| {
        const chunk = chunk_and_address.chunk;
        if (maybe_section_handle) |_| {
            if (!std.meta.eql(maybe_section_handle, chunk.section)) continue;
        }

        const address_range = chunk.address_range(a);
        if (address_range.len == 0) {
            continue;
        }

        max_addr = @max(max_addr, address_range.last());
    }

    const memory = try allocator.alloc(u8, max_addr + 1);
    @memset(memory, 0);

    for (a.chunks.items) |chunk_and_address| {
        const chunk = chunk_and_address.chunk;
        if (maybe_section_handle) |_| {
            if (!std.meta.eql(maybe_section_handle, chunk.section)) continue;
        }

        const address_range = chunk.address_range(a);
        if (address_range.len == 0) {
            continue;
        }

        var address: usize = address_range.first;
        const last_address = address_range.last();
        while (address < last_address) {
            const page = arch.addr.Page.init(@truncate(address >> @bitSizeOf(arch.addr.Page.Offset)));
            const page_end = (@as(u64, page.raw()) + 1) << @bitSizeOf(arch.addr.Page.Offset);
            const end: u32 = @intCast(@min(page_end, last_address + 1));

            if (a.page_lookup.get(page)) |page_data_handle| {
                const begin_offset = arch.addr.Page.Offset.init(@truncate(address));
                const end_offset = arch.addr.Page.Offset.init(@truncate(end));

                var data: []const u8 = &page_data[page_data_handle];
                if (end_offset.raw() == 0) {
                    data = data[begin_offset.raw()..];
                } else {
                    data = data[begin_offset.raw()..end_offset.raw()];
                }

                const offset_address_i32: i32 = @truncate(@as(i64, @intCast(address)));
                const offset_address: u32 = @bitCast(offset_address_i32);

                @memcpy(memory[offset_address..].ptr, data);
            }

            address = end;
        }
    }

    return memory;
}

pub const Hex_Options = struct {
    format: enum {
        motorola,
        intel,
    } = .intel,
    file_extension: []const u8 = "hex",
    num_roms: u8 = 1,           // e.g. 4 for boot rom
    rom_width_bytes: u8 = 2,    // e.g. 2 for boot rom
    address_offset: i64 = 0,    // added to addresses before being placed in roms
    address_range: Assembler.Address_Range = .{
        .first = 0,
        .len = 0x1_0000_0000,
    },
    merge_all_sections: bool = true,
    add_spaces: bool = true,    // add spaces between fields in the hex records, making them more readable, but longer
};
pub fn write_hex(a: *const Assembler, dir: std.fs.Dir, output_filename_prefix: []const u8, options: Hex_Options) !void {
    if (options.merge_all_sections) {
        try write_hex_files_for_section(a, null, dir, output_filename_prefix, options);
    } else {
        for (0.., a.sections.values()) |section_handle_usize, section| {
            if (section.has_chunks) {
                const section_handle: Section.Handle = @intCast(section_handle_usize);
                try write_hex_files_for_section(a, section_handle, dir, output_filename_prefix, options);
            }
        }
    }
}

pub fn write_hex_files_for_section(a: *const Assembler, maybe_section_handle: ?Section.Handle, dir: std.fs.Dir, output_filename_prefix: []const u8, options: Hex_Options) !void {
    var filename_temp = std.io.Writer.Allocating.init(a.gpa);
    defer filename_temp.deinit();

    try filename_temp.writer.writeAll(output_filename_prefix);
    if (maybe_section_handle) |section_handle| {
        try filename_temp.writer.writeByte('.');
        const section = a.sections.values()[section_handle];
        for (section.name) |ch| {
            try filename_temp.writer.writeByte(switch (ch) {
                'a'...'z', 'A'...'Z', '0'...'9' => ch,
                else => '_',
            });
        }
    }

    const base_length = filename_temp.written().len;

    if (options.num_roms > 1) {
        for (0..options.num_roms) |rom_number| {
            filename_temp.writer.end = base_length;
            try filename_temp.writer.print(".{}.{s}", .{ rom_number, options.file_extension });
            var f = try dir.createFile(filename_temp.written(), .{});
            defer f.close();
            var buf: [4096]u8 = undefined;
            var writer = f.writer(&buf);
            try write_hex_for_section(a, maybe_section_handle, &writer.interface, @intCast(rom_number), options);
            try writer.interface.flush();
        }
    } else {
        filename_temp.writer.end = base_length;
        try filename_temp.writer.print(".{s}", .{ options.file_extension });
        var f = try dir.createFile(filename_temp.written(), .{});
        defer f.close();
        var buf: [4096]u8 = undefined;
        var writer = f.writer(&buf);
        try write_hex_for_section(a, maybe_section_handle, &writer.interface, 0, options);
        try writer.interface.flush();
    }
}

pub fn write_hex_for_section(a: *const Assembler, maybe_section_handle: ?Section.Handle, writer: *std.io.Writer, rom_number: u8, options: Hex_Options) !void {
    switch (options.format) {
        .motorola => {
            var buf: [8]u8 = undefined;
            const header = try std.fmt.bufPrint(&buf, "rom {}", .{ rom_number });
            var hex_writer = try srec.writer(u32, writer, .{
                .header_data = header,
                .pretty = options.add_spaces,
            });
            try write_hex_for_section_inner(a, maybe_section_handle, &hex_writer, rom_number, options);
            try hex_writer.finish(0);
        },
        .intel => {
            var hex_writer = ihex.writer(u32, writer, .{
                .pretty = options.add_spaces,
            });
            try write_hex_for_section_inner(a, maybe_section_handle, &hex_writer, rom_number, options);
            try hex_writer.finish(null);
        },
    }
}

// writer should be srec.Writer or ihex.Writer
fn write_hex_for_section_inner(a: *const Assembler, maybe_section_handle: ?Section.Handle, writer: anytype, rom_number: u8, options: Hex_Options) !void {
    const page_data = a.pages.items(.data);

    var temp = std.ArrayListUnmanaged(u8){};
    defer temp.deinit(a.gpa);

    for (a.chunks.items) |chunk_and_address| {
        const chunk = chunk_and_address.chunk;
        if (!options.merge_all_sections) {
            if (!std.meta.eql(maybe_section_handle, chunk.section)) continue;
        }

        const address_range = chunk.address_range(a).intersection_with(options.address_range);
        if (address_range.len == 0) {
            continue;
        }

        var address: usize = address_range.first;
        const last_address = address_range.last();
        while (address < last_address) {
            const page: arch.addr.Page = .init(@truncate(address >> @bitSizeOf(arch.addr.Page.Offset)));
            const page_end = (@as(u64, page.raw()) + 1) << @bitSizeOf(arch.addr.Page.Offset);
            const end: u32 = @intCast(@min(page_end, last_address + 1));

            if (a.page_lookup.get(page)) |page_data_handle| {
                const begin_offset: arch.addr.Page.Offset.Raw = @truncate(address);
                const end_offset: arch.addr.Page.Offset.Raw = @truncate(end);

                var data: []const u8 = &page_data[page_data_handle];
                if (end_offset == 0) {
                    data = data[begin_offset..];
                } else {
                    data = data[begin_offset..end_offset];
                }

                const offset_address_i32: i32 = @truncate(options.address_offset +% @as(i64, @intCast(address)));
                const offset_address: u32 = @bitCast(offset_address_i32);

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

pub const Sim_Sx_Options = struct {
    include_listing: bool = true,
};
pub fn write_sim_sx(a: *Assembler, temp_alloc: std.mem.Allocator, writer: *std.io.Writer, options: Sim_Sx_Options) !void {
    var sx_writer = sx.writer(temp_alloc, writer);
    defer sx_writer.deinit();

    try sx_writer.open_expanded();

    var last_page_written = arch.addr.Page.init(0);
    try write_sim_sx_page(a, last_page_written, &sx_writer);

    for (a.chunks.items) |chunk_and_address| {
        const address_range = chunk_and_address.chunk.address_range(a);
        if (address_range.len == 0) {
            continue;
        }
        const first_page = address_range.first >> @bitSizeOf(arch.addr.Page.Offset);
        const last_page = (address_range.first + @as(u32, @intCast(address_range.len - 1))) >> @bitSizeOf(arch.addr.Page.Offset);

        for (first_page..last_page + 1) |page_usize| {
            const page = arch.addr.Page.init(@intCast(page_usize));
            if (page.raw() > last_page_written.raw()) {
                try write_sim_sx_page(a, page, &sx_writer);
                last_page_written = page;
            }
        }
    }

    if (options.include_listing) {
        var listing = create_listing(a, temp_alloc, .{});
        defer listing.deinit();

        const lines_slice = listing.lines.slice();
        const addresses = lines_slice.items(.address);
        const lengths = lines_slice.items(.length);
        // const insn_indices = lines_slice.items(.insn_index);

        try sx_writer.expression_expanded("listing");
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
                    try write_listing_sx_line_and_source(line_number, source, &sx_writer);
                    try sx_writer.close();
                },
                .instruction => {
                    //const insn = listing.insns.items[insn_indices[l].?];
                    try sx_writer.expression("insn");
                    try sx_writer.print_value("{X:0>8}", .{ addresses[l] });
                    try sx_writer.int(lengths[l], 10);
                    // TODO fixme!
                    //try iedb.write_database.write_encoding(&sx_writer, true, insn.encoding);
                    try write_listing_sx_line_and_source(line_number, source, &sx_writer);
                    try sx_writer.close();
                },
                .data8, .data16, .data32 => {
                    try sx_writer.expression(switch (kind) {
                        .data8 => "db",
                        .data16 => "dw",
                        .data32 => "dd",
                        else => unreachable,
                    });
                    try sx_writer.print_value("{X:0>8}", .{ addresses[l] });
                    try sx_writer.int(lengths[l], 10);
                    try write_listing_sx_line_and_source(line_number, source, &sx_writer);
                    try sx_writer.close();
                },
                .alignment => {
                    try sx_writer.expression("align");
                    try sx_writer.int(addresses[l], 10);
                    const offset = lengths[l];
                    if (offset > 0) {
                        try sx_writer.int(offset, 10);
                    }
                    try write_listing_sx_line_and_source(line_number, source, &sx_writer);
                    try sx_writer.close();
                },
                .org => {
                    try sx_writer.expression("org");
                    try sx_writer.int(addresses[l], 10);
                    try write_listing_sx_line_and_source(line_number, source, &sx_writer);
                    try sx_writer.close();
                },
            }
        }
        try sx_writer.close();
    }

    try sx_writer.done();
}

fn write_listing_sx_line_and_source(line_number: u32, source: []const u8, sx_writer: *sx.Writer) !void {
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

pub fn write_sim_sx_page(a: *const Assembler, page: arch.addr.Page, sx_writer: *sx.Writer) !void {
    if (a.page_lookup.get(page)) |page_data_handle| {
        const pages_slice = a.pages.slice();
        const page_data = pages_slice.items(.data);
        const page_usage = pages_slice.items(.usage);
        const page_sections = pages_slice.items(.section);
        const page_access = pages_slice.items(.access);

        const section = a.get_section(page_sections[page_data_handle]);
        const full_page_data = &page_data[page_data_handle];

        try sx_writer.expression("page");
        if (section.kind == .boot) {
            try sx_writer.string("flash");
            try sx_writer.print_value("{X:0>3}", .{ page.raw() + 1008 });
        } else {
            try sx_writer.string("ram");
            try sx_writer.print_value("{X:0>5}", .{ page.raw() });
        }

        try sx_writer.open();
        try sx_writer.tag(section.kind);
        try sx_writer.string(section.name);
        try sx_writer.close();

        const access = page_access[page_data_handle];

        try sx_writer.expression("read");
        if (access.read) |read| {
            try sx_writer.tag(read);
        } else {
            try sx_writer.string("none");
        }
        try sx_writer.close();

        try sx_writer.expression("write");
        if (access.write) |write| {
            try sx_writer.tag(write);
        } else {
            try sx_writer.string("none");
        }
        try sx_writer.close();

        try sx_writer.expression("exec");
        if (access.execute) |exec| {
            try sx_writer.tag(exec);
        } else {
            try sx_writer.string("none");
        }
        try sx_writer.close();

        sx_writer.set_compact(false);

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

            var used_range_end = unused.findFirstSet() orelse Page_Data.page_size;
            if (used_range_end - used_range_begin > max_bytes_per_line) {
                used_range_end = used_range_begin + max_bytes_per_line;
            }

            const data = full_page_data[used_range_begin..used_range_end];
            var buf: [max_base64_length]u8 = undefined;
            const encoded = std.base64.standard.Encoder.encode(&buf, data);

            try sx_writer.open();
            try sx_writer.print_value("{X:0>3}", .{ used_range_begin });
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

pub const List_Options = struct {
    ordering: enum {
        file,
        address,
    } = .address,
    clone_source_strings: bool = false,
};
pub fn create_listing(a: *Assembler, gpa: std.mem.Allocator, options: List_Options) Listing {
    var listing = Listing.init(gpa);
    switch (options.ordering) {
        .file => {
            var last_file: ?Source_File.Handle = null;
            for (a.files.items) |*file| {
                const blocks = file.blocks.slice();
                for (blocks.items(.first_insn),
                    blocks.items(.end_insn),
                    blocks.items(.section),
                    blocks.items(.keep),
                ) |begin, end, section, keep| {
                    add_listing_for_chunk(a, &listing, .{
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
            var last_file: ?Source_File.Handle = null;
            for (a.chunks.items) |chunk_and_address| {
                add_listing_for_chunk(a, &listing, chunk_and_address.chunk, true, last_file, options);
                last_file = chunk_and_address.chunk.file;
            }
        },
    }
    return listing;
}

fn add_listing_for_chunk(a: *Assembler, listing: *Listing, chunk: Source_File.Chunk, keep: bool, last_file: ?Source_File.Handle, options: List_Options) void {
    const file = a.get_source(chunk.file);
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
        listing.add_filename_line(file.name, options.clone_source_strings);
    }

    var iter = chunk.instructions;
    while (iter.next()) |i| {
        const token_range = Instruction.token_range(i, tokens, token_kinds);
        var line_source = file.source[token_offsets[token_range.first] .. token_offsets[token_range.last + 1]];
        if (line_source.len > 0 and line_source[line_source.len - 1] == '\r') {
            line_source.len -= 1;
        }

        if (!keep) {
            listing.add_non_output_line(.empty, line_numbers[i], line_source, options.clone_source_strings);
            continue;
        }

        switch (operations[i]) {
            .boot, .code, .kcode, .entry, .kentry => {
                listing.add_non_output_line(.insn_space, line_numbers[i], line_source, options.clone_source_strings);
            },
            .data, .kdata, .@"const", .kconst, .section => {
                listing.add_non_output_line(.data_space, line_numbers[i], line_source, options.clone_source_strings);
            },
            .stack => {
                listing.add_non_output_line(.stack_space, line_numbers[i], line_source, options.clone_source_strings);
            },
            .none, .nil, .insn, .keep, .def, .undef, .local, .range => {
                listing.add_non_output_line(.empty, line_numbers[i], line_source, options.clone_source_strings);
            },
            .org => {
                listing.add_org_line(addresses[i], line_numbers[i], line_source, options.clone_source_strings);
            },

            .bound_insn => |id| {
                const form = iedb.get(id);
                const insn = a.build_instruction(s, addresses[i], form.signature.mnemonic, params[i], false).?;
                listing.add_instruction_line(addresses[i], lengths[i], .{
                    .form = form,
                    .params = insn.params,
                }, line_numbers[i], line_source, options.clone_source_strings);
            },

            .push, .pop => |stack_size| {
                const insn = isa.Instruction{
                    .mnemonic = switch (operations[i]) {
                        .push => .frame,
                        .pop => .unframe,
                        else => unreachable,
                    },
                    .params = &.{
                        .{
                            .signature = Expression.Type.constant().param_signature(),
                            .base_gpr_offset = .init(0),
                            .offset_gpr_offset = .init(0),
                            .constant = stack_size,
                        },
                    },
                };
                if (layout.find_best_insn_form(a, insn)) |form| {
                    listing.add_instruction_line(addresses[i], lengths[i], .{
                        .form = form,
                        .params = insn.params,
                    }, line_numbers[i], line_source, options.clone_source_strings);
                } else {
                    listing.add_non_output_line(.empty, line_numbers[i], line_source, options.clone_source_strings);
                }
            },

            .@"align" => {
                if (layout.get_resolved_alignment(s, params[i])) |alignment| {
                    listing.add_alignment_line(alignment, line_numbers[i], line_source, options.clone_source_strings);
                } else {
                    listing.add_non_output_line(.empty, line_numbers[i], line_source, options.clone_source_strings);
                }
            },

            .db, .zb => listing.add_data_line(addresses[i], lengths[i], .data8, line_numbers[i], line_source, options.clone_source_strings),
            .dh, .zh => listing.add_data_line(addresses[i], lengths[i], .data16, line_numbers[i], line_source, options.clone_source_strings),
            .dw, .zw => listing.add_data_line(addresses[i], lengths[i], .data32, line_numbers[i], line_source, options.clone_source_strings),
        }
    }
}

const layout = @import("layout.zig");
const Expression = @import("Expression.zig");
const Assembler = @import("Assembler.zig");
const Source_File = @import("Source_File.zig");
const Page_Data = @import("Page_Data.zig");
const Instruction = @import("Instruction.zig");
const Section = @import("Section.zig");
const Listing = @import("Listing.zig");
const iedb = @import("iedb");
const isa = @import("isa");
const arch = @import("arch");
const srec = @import("srec");
const ihex = @import("ihex");
const sx = @import("sx");
const std = @import("std");
