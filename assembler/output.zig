const std = @import("std");
const ie = @import("isa_encoding");
const layout = @import("layout.zig");
const Assembler = @import("Assembler.zig");
const SourceFile = @import("SourceFile.zig");
const PageData = @import("PageData.zig");
const Instruction = @import("Instruction.zig");

pub const IntelHexOptions = struct {
    num_roms: u8 = 1,           // e.g. 4 for boot rom
    rom_width_bytes: u8 = 2,    // e.g. 2 for boot rom
    address_offset: i64 = 0,    // added to addresses before being placed in roms
    merge_all_sections: bool = true,
};
pub fn writeIntelHex() !void {
    // TODO
}

pub const MotorolaSRecOptions = struct {
    num_roms: u8 = 1,           // e.g. 4 for boot rom
    rom_width_bytes: u8 = 2,    // e.g. 2 for boot rom
    address_offset: i64 = 0,    // added to addresses before being placed in roms
    merge_all_sections: bool = true,
};
pub fn writeMotorolaSRec(self: *Assembler, options: MotorolaSRecOptions) !void {
    _ = options;

    // var safe_section_names_used = std.StringHashMap(void).init(self.gpa);
    // defer safe_section_names_used.deinit();

    for (self.chunks.items) |chunk_and_address| {
        const addr = chunk_and_address.address;
        const chunk = chunk_and_address.chunk;

        std.debug.print("{}: section {?} #{}-{}\n", .{ addr, chunk.section, chunk.instructions.begin, chunk.instructions.end });
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
