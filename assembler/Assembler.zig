const std = @import("std");
const ie = @import("instruction_encoding");
const ie_data = @import("instruction_encoding_data");
const bus = @import("bus_types");
const lex = @import("lex.zig");
const typechecking = @import("typechecking.zig");
const dead_code = @import("dead_code.zig");
const layout = @import("layout.zig");
const Constant = @import("Constant.zig");
const Section = @import("Section.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const SourceFile = @import("SourceFile.zig");
const Error = @import("Error.zig");
const InsnEncodingError = @import("InsnEncodingError.zig");
const PageData = @import("PageData.zig");

const Assembler = @This();

edb: ie_data.EncoderDatabase,
gpa: std.mem.Allocator,
arena: std.mem.Allocator,
files: std.ArrayListUnmanaged(SourceFile) = .{},
errors: std.ArrayListUnmanaged(Error) = .{},
insn_encoding_errors: std.ArrayListUnmanaged(InsnEncodingError) = .{},
overlapping_chunks: std.AutoArrayHashMapUnmanaged(SourceFile.ChunkPair, void) = .{},
symbols: std.StringHashMapUnmanaged(InstructionRef) = .{},
sections: std.StringArrayHashMapUnmanaged(Section) = .{},
chunks: std.ArrayListUnmanaged(ChunkWithAddress) = .{},
pages: std.MultiArrayList(PageData) = .{},
page_lookup: std.AutoHashMapUnmanaged(bus.Page, PageData.Handle) = .{},
constant_temp: std.ArrayListUnmanaged(u8) = .{},
params_temp: std.ArrayListUnmanaged(ie.Parameter) = .{},
constants: Constant.InternPool = .{},

pub const InstructionRef = struct {
    file: SourceFile.Handle,
    instruction: Instruction.Handle,
};

pub const ChunkWithAddress = struct {
    chunk: SourceFile.Chunk,
    address: u32,

    fn lessThan(_: void, lhs: ChunkWithAddress, rhs: ChunkWithAddress) bool {
        return lhs.address < rhs.address;
    }
};

pub const Alignment = struct {
    modulo: u32,
    offset: u32,
};

pub const AddressRange = struct {
    begin: u32,
    end: u32,

    pub fn intersectionWith(self: AddressRange, other: AddressRange) AddressRange {
        const begin = @max(self.begin, other.begin);
        return .{
            .begin = begin,
            .end = @max(begin, @min(self.end, other.end)),
        };
    }
};

pub fn init(gpa: std.mem.Allocator, arena: std.mem.Allocator, edb: ie_data.EncoderDatabase) Assembler {
    // It's possible to use the same allocator for gpa and arena,
    // but nothing allocated in the arena allocator will be freed
    // until Assembler.deinit() is called, so it's a good candidate
    // for std.heap.ArenaAllocator use.

    // Note also that the EncoderDatabase is not owned by the Assembler
    // but its lifetime must not be less than the Assembler lifetime.

    var self = Assembler{
        .edb = edb,
        .gpa = gpa,
        .arena = arena,
    };

    return self;
}

pub fn deinit(self: *Assembler, deinit_arena: bool) void {
    var maybe_arena = if (deinit_arena) self.arena else null;

    if (deinit_arena) {
        var constant_iter = self.constants.keyIterator();
        while (constant_iter.next()) |constant| {
            constant.deinit(self.arena);
            self.arena.destroy(constant);
        }

        for (self.pages.items(.data)) |buf| {
            self.arena.free(buf);
        }
    }

    for (self.files.items) |file| {
        file.deinit(self.gpa, maybe_arena);
    }

    for (self.pages.items(.chunks)) |chunks| {
        chunks.deinit(self.gpa);
    }

    for (self.errors.items) |err| {
        if (err.flags.contains(.desc_is_allocated)) {
            self.gpa.free(err.desc);
        }
    }

    self.files.deinit(self.gpa);
    self.errors.deinit(self.gpa);
    self.insn_encoding_errors.deinit(self.gpa);
    self.overlapping_chunks.deinit(self.gpa);
    self.symbols.deinit(self.gpa);
    self.sections.deinit(self.gpa);
    self.chunks.deinit(self.gpa);
    self.pages.deinit(self.gpa);
    self.page_lookup.deinit(self.gpa);
    self.constant_temp.deinit(self.gpa);
    self.params_temp.deinit(self.gpa);
    self.constants.deinit(self.gpa);
}

pub fn addSource(self: *Assembler, name: []const u8, source: []const u8) SourceFile.Handle {
    const owned_name = self.arena.dupe(name) catch @panic("OOM");
    const owned_source = self.arena.dupe(source) catch @panic("OOM");
    return self.adoptSource(owned_name, owned_source);
}

// Takes ownership of name and source; assumed to have been allocated with self.arena
pub fn adoptSource(self: *Assembler, name: []const u8, source: []const u8) SourceFile.Handle {
    const handle = @intCast(SourceFile.Handle, self.files.items.len);
    const file = SourceFile.parse(self.gpa, handle, name, source, &self.errors);
    const ptr = self.files.addOne(self.gpa) catch @panic("OOM");
    ptr.* = file;
    return handle;
}

pub fn assemble(self: *Assembler) void {
    for (self.files.items) |*file| {
        typechecking.processLabelsAndSections(self, file);
    }

    if (!typechecking.resolveExpressionTypes(self)) return;

    typechecking.checkInstructionsAndDirectives(self);

    for (self.files.items) |*file| {
        dead_code.markBlocksToKeep(self, file);
    }

    var fixed_org_chunks = std.ArrayListUnmanaged(SourceFile.Chunk) {};
    var auto_org_chunks = std.ArrayListUnmanaged(SourceFile.Chunk) {};
    defer fixed_org_chunks.deinit(self.gpa);
    defer auto_org_chunks.deinit(self.gpa);
    for (self.files.items) |*file| {
        file.collectChunks(self.gpa, &fixed_org_chunks, &auto_org_chunks);
    }

    var attempts: usize = 0;
    const max_attempts = 100;
    while (attempts < max_attempts) : (attempts += 1) {
        self.resetLayout();

        var try_again = layout.doFixedOrgLayout(self, fixed_org_chunks.items);
        try_again = layout.doAutoOrgLayout(self, auto_org_chunks.items) or try_again;

        // TODO better handling of degenerate/recursive cases where the layout never reaches an equilibrium?
        if (!try_again) break;
    }

    if (attempts == max_attempts) {
        std.debug.print("Failed to find a stable layout after {} iterations!\n", .{ attempts });
    }

    layout.populatePageChunks(self, fixed_org_chunks.items);
    layout.populatePageChunks(self, auto_org_chunks.items);

    std.sort.sort(ChunkWithAddress, self.chunks.items, {}, ChunkWithAddress.lessThan);

    for (self.pages.items(.chunks)) |chunks| {
        layout.findOverlappingChunks(self, chunks.items);
    }

    for (self.files.items) |*file| {
        layout.encodePageData(self, file);
    }
}

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
pub fn writeListing(self: *Assembler, writer: anytype, options: ListOptions) !void {
    switch (options.ordering) {
        .file => {
            var last_file: ?SourceFile.Handle = null;
            for (self.files.items) |*file| {
                const blocks = file.blocks.slice();
                for (blocks.items(.first_insn),
                    blocks.items(.end_insn),
                    blocks.items(.section),
                    blocks.items(.keep),
                ) |begin, end, section, keep| {
                    try self.writeListingForChunk(writer, .{
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
            for (self.chunks.items) |chunk_and_address| {
                try self.writeListingForChunk(writer, chunk_and_address.chunk, true, last_file);
                last_file = chunk_and_address.chunk.file;
            }
        },
    }
}

fn writeListingForChunk(self: *Assembler, writer: anytype, chunk: SourceFile.Chunk, keep: bool, last_file: ?SourceFile.Handle) !void {
    const file = self.getSource(chunk.file);
    const s = file.slices();

    const insn_tokens = s.insn.items(.token);
    const insn_params = s.insn.items(.params);
    const insn_operations = s.insn.items(.operation);
    const insn_addresses = s.insn.items(.address);
    const insn_lengths = s.insn.items(.length);

    //const expr_infos = s.expr.items(.info);
    //const expr_constants = s.expr.items(.resolved_constant);
    //const expr_types = s.expr.items(.resolved_type);

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
            try writer.writeByteNTimes(' ', 80);
            try writer.print("// {s}\n", .{ line } );
            continue;
        }

        switch (insn_operations[insn_handle]) {
            .section, .code, .kcode, .entry, .kentry,
            .data, .kdata, .@"const", .kconst, .stack,
            .none, .insn, .keep, .def, .undef,
            => {
                try writer.writeByteNTimes(' ', 80);
                try writer.print("// {s}\n", .{ line } );
            },

            .bound_insn => |encoding| {
                const address = insn_addresses[insn_handle];
                const params = insn_params[insn_handle];

                try writer.print("{X:0>16} ", .{ address });

                const insn = self.buildInstruction(s, address, encoding.mnemonic, encoding.suffix, params, false).?;
                self.constant_temp.clearRetainingCapacity();
                var temp_writer = self.constant_temp.writer(self.gpa);
                try insn.print(temp_writer, address);
                const insn_space = self.constant_temp.items.len + 1;
                var space_remaining = if (insn_space >= 63) 0 else 63 - @intCast(u8, insn_space);
                try writer.writeAll(self.constant_temp.items);
                try writer.writeByte(' ');
                self.constant_temp.clearRetainingCapacity();

                var d = [_]u8{0}**8;
                _ = ie.encodeInstruction(insn, encoding.*, &d);

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
                    if (space_remaining >= 16) {
                        try writer.writeByteNTimes(' ', space_remaining - 16);
                        try writer.print("# {X:0>2}{X:0>2}{X:0>2}{X:0>2} {X:0>2}{X:0>2} // {s}\n", .{
                            d[5], d[4], d[3], d[2], d[1], d[0], line
                        });
                    } else if (space_remaining >= 7) {
                        try writer.writeByteNTimes(' ', space_remaining - 7);
                        try writer.print("# {X:0>2}{X:0>2} // {s}\n{X:0>16}", .{ d[1], d[0], line, address + 2 });
                        try writer.writeByteNTimes(' ', 53);
                        try writer.print("# {X:0>2}{X:0>2}{X:0>2}{X:0>2}\n", .{ d[5], d[4], d[3], d[2] });
                    } else {
                        try writer.writeByteNTimes(' ', space_remaining);
                        try writer.print("// {s}\n", .{ line });
                        try writer.writeByteNTimes(' ', 64);
                        try writer.print("# {X:0>2}{X:0>2}{X:0>2}{X:0>2} {X:0>2}{X:0>2}\n", .{
                            d[5], d[4], d[3], d[2], d[1], d[0]
                        });
                    }
                } else if (needs_ip_plus_4_word) {
                    if (space_remaining >= 17) {
                        try writer.writeByteNTimes(' ', space_remaining - 17);
                        try writer.print("# {X:0>2}{X:0>2} {X:0>2}{X:0>2} {X:0>2}{X:0>2} // {s}\n", .{
                            d[5], d[4], d[3], d[2], d[1], d[0], line
                        });
                    } else if (space_remaining >= 12) {
                        try writer.writeByteNTimes(' ', space_remaining - 12);
                        try writer.print("# {X:0>2}{X:0>2} {X:0>2}{X:0>2} // {s}\n{X:0>16}", .{ d[3], d[2], d[1], d[0], line, address + 4 });
                        try writer.writeByteNTimes(' ', 57);
                        try writer.print("# {X:0>2}{X:0>2}\n", .{ d[5], d[4] });
                    } else if (space_remaining >= 7) {
                        try writer.writeByteNTimes(' ', space_remaining - 7);
                        try writer.print("# {X:0>2}{X:0>2} // {s}\n{X:0>16}", .{ d[1], d[0], line, address + 2 });
                        try writer.writeByteNTimes(' ', 52);
                        try writer.print("# {X:0>2}{X:0>2} {X:0>2}{X:0>2}\n", .{ d[5], d[4], d[3], d[2] });
                    } else {
                        try writer.writeByteNTimes(' ', space_remaining);
                        try writer.print("// {s}\n", .{ line });
                        try writer.writeByteNTimes(' ', 63);
                        try writer.print("# {X:0>2}{X:0>2} {X:0>2}{X:0>2} {X:0>2}{X:0>2}\n", .{
                            d[5], d[4], d[3], d[2], d[1], d[0]
                        });
                    }
                } else if (needs_ip_plus_2_word) {
                    if (space_remaining >= 12) {
                        try writer.writeByteNTimes(' ', space_remaining - 12);
                        try writer.print("# {X:0>2}{X:0>2} {X:0>2}{X:0>2} // {s}\n", .{ d[3], d[2], d[1], d[0], line });
                    } else if (space_remaining >= 7) {
                        try writer.writeByteNTimes(' ', space_remaining - 7);
                        try writer.print("# {X:0>2}{X:0>2} // {s}\n{X:0>16}", .{ d[1], d[0], line, address + 2 });
                        try writer.writeByteNTimes(' ', 57);
                        try writer.print("# {X:0>2}{X:0>2}\n", .{ d[3], d[2] });
                    } else {
                        try writer.writeByteNTimes(' ', space_remaining);
                        try writer.print("// {s}\n", .{ line });
                        try writer.writeByteNTimes(' ', 68);
                        try writer.print("# {X:0>2}{X:0>2} {X:0>2}{X:0>2}\n", .{ d[3], d[2], d[1], d[0] });
                    }
                } else if (needs_ip_plus_2_byte) {
                    if (space_remaining >= 10) {
                        try writer.writeByteNTimes(' ', space_remaining - 10);
                        try writer.print("# {X:0>2} {X:0>2}{X:0>2} // {s}\n", .{ d[2], d[1], d[0], line });
                    } else if (space_remaining >= 7) {
                        try writer.writeByteNTimes(' ', space_remaining - 7);
                        try writer.print("# {X:0>2}{X:0>2} // {s}\n{X:0>16}", .{ d[1], d[0], line, address + 2 });
                        try writer.writeByteNTimes(' ', 59);
                        try writer.print("# {X:0>2}\n", .{ d[2] });
                    } else {
                        try writer.writeByteNTimes(' ', space_remaining);
                        try writer.print("// {s}\n", .{ line });
                        try writer.writeByteNTimes(' ', 70);
                        try writer.print("# {X:0>2} {X:0>2}{X:0>2}\n", .{ d[2], d[1], d[0] });
                    }
                } else {
                    if (space_remaining >= 7) {
                        try writer.writeByteNTimes(' ', space_remaining - 7);
                        try writer.print("# {X:0>2}{X:0>2} // {s}\n", .{ d[1], d[0], line });
                    } else {
                        try writer.writeByteNTimes(' ', space_remaining);
                        try writer.print("// {s}\n", .{ line });
                        try writer.writeByteNTimes(' ', 73);
                        try writer.print("# {X:0>2}{X:0>2}\n", .{ d[1], d[0] });
                    }
                }
            },

            .push => {
                // TODO stack sections
            },

            .pop => {
                // TODO stack sections
            },

            .org => {
                self.constant_temp.clearRetainingCapacity();
                var temp_writer = self.constant_temp.writer(self.gpa);
                try temp_writer.print(".org 0x{X}", .{ insn_addresses[insn_handle] });
                try writer.print("{s:<80}", .{ self.constant_temp.items });
                try writer.print("// {s}\n", .{ line } );
                self.constant_temp.clearRetainingCapacity();
            },

            .@"align" => {
                if (layout.getResolvedAlignment(s, insn_params[insn_handle])) |alignment| {
                    self.constant_temp.clearRetainingCapacity();
                    var temp_writer = self.constant_temp.writer(self.gpa);
                    try temp_writer.print(".align {}", .{ alignment.modulo });
                    if (alignment.offset > 0) {
                        try temp_writer.print(", {}", .{ alignment.offset });
                    }
                    try writer.print("{s:<80}", .{ self.constant_temp.items });
                    self.constant_temp.clearRetainingCapacity();
                } else {
                    try writer.writeByteNTimes(' ', 80);
                }
                try writer.print("// {s}\n", .{ line } );
            },

            .db => {
                var address = insn_addresses[insn_handle];
                var length = insn_lengths[insn_handle];
                var write_line = true;
                var data_iter = PageData.DataIterator.init(self, address);
                const bytes_per_line = 20;
                var temp = [_]u8{0} ** bytes_per_line;
                while (length > bytes_per_line) {
                    try writer.print("{X:0>16}  #", .{ address });
                    for (0..bytes_per_line) |i| {
                        temp[bytes_per_line - 1 - i] = data_iter.next();
                    }
                    address += bytes_per_line;
                    length -= bytes_per_line;

                    for (temp) |b| {
                        try writer.print(" {X:0>2}", .{ b });
                    }

                    if (write_line) {
                        try writer.print(" // {s}\n", .{ line } );
                        write_line = false;
                    } else try writer.writeAll("\n");
                }

                self.constant_temp.clearRetainingCapacity();
                for (0..length) |i| {
                    temp[length - 1 - i] = data_iter.next();
                }

                var temp_writer = self.constant_temp.writer(self.gpa);
                try temp_writer.writeAll("#");
                for (temp[0..length]) |b| {
                    try temp_writer.print(" {X:0>2}", .{ b });
                }

                try writer.print("{X:0>16}{s:>63}", .{ address, self.constant_temp.items });
                self.constant_temp.clearRetainingCapacity();
                if (write_line) {
                    try writer.print(" // {s}\n", .{ line } );
                } else try writer.writeAll("\n");
            },
            .dw => {
                var address = insn_addresses[insn_handle];
                var length = insn_lengths[insn_handle];
                var write_line = true;
                var data_iter = PageData.DataIterator.init(self, address);
                const words_per_line = 12;
                var temp = [_]u16{0} ** words_per_line;
                while (length > words_per_line * 2) {
                    try writer.print("{X:0>16}  #", .{ address });
                    for (0..words_per_line) |i| {
                        var word: u16 = data_iter.next();
                        word |= @as(u16, data_iter.next()) << 8;
                        temp[words_per_line - 1 - i] = word;
                    }
                    address += words_per_line * 2;
                    length -= words_per_line * 2;

                    for (temp) |w| {
                        try writer.print(" {X:0>4}", .{ w });
                    }

                    if (write_line) {
                        try writer.print(" // {s}\n", .{ line } );
                        write_line = false;
                    } else try writer.writeAll("\n");
                }

                self.constant_temp.clearRetainingCapacity();
                const word_length = (length + 1) / 2;
                for (0..word_length) |i| {
                    var word: u16 = data_iter.next();
                    word |= @as(u16, data_iter.next()) << 8;
                    temp[word_length - 1 - i] = word;
                }

                var temp_writer = self.constant_temp.writer(self.gpa);
                try temp_writer.writeAll("#");
                for (temp[0..word_length]) |w| {
                    try temp_writer.print(" {X:0>4}", .{ w });
                }

                try writer.print("{X:0>16}{s:>63}", .{ address, self.constant_temp.items });
                self.constant_temp.clearRetainingCapacity();
                if (write_line) {
                    try writer.print(" // {s}\n", .{ line } );
                } else try writer.writeAll("\n");
            },
            .dd => {
                var address = insn_addresses[insn_handle];
                var length = insn_lengths[insn_handle];
                var write_line = true;
                var data_iter = PageData.DataIterator.init(self, address);
                const dwords_per_line = 6;
                var temp = [_]u32{0} ** dwords_per_line;
                while (length > dwords_per_line * 4) {
                    try writer.print("{X:0>16}        #", .{ address });
                    for (0..dwords_per_line) |i| {
                        var dword: u32 = data_iter.next();
                        dword |= @as(u32, data_iter.next()) << 8;
                        dword |= @as(u32, data_iter.next()) << 16;
                        dword |= @as(u32, data_iter.next()) << 24;
                        temp[dwords_per_line - 1 - i] = dword;
                    }
                    address += dwords_per_line * 4;
                    length -= dwords_per_line * 4;

                    for (temp) |dw| {
                        try writer.print(" {X:0>8}", .{ dw });
                    }

                    if (write_line) {
                        try writer.print(" // {s}\n", .{ line } );
                        write_line = false;
                    } else try writer.writeAll("\n");
                }

                self.constant_temp.clearRetainingCapacity();
                const dword_length = (length + 3) / 4;
                for (0..dword_length) |i| {
                    var dword: u32 = data_iter.next();
                    dword |= @as(u32, data_iter.next()) << 8;
                    dword |= @as(u32, data_iter.next()) << 16;
                    dword |= @as(u32, data_iter.next()) << 24;
                    temp[dword_length - 1 - i] = dword;
                }

                var temp_writer = self.constant_temp.writer(self.gpa);
                try temp_writer.writeAll("#");
                for (temp[0..dword_length]) |dw| {
                    try temp_writer.print(" {X:0>8}", .{ dw });
                }

                try writer.print("{X:0>16}{s:>63}", .{ address, self.constant_temp.items });
                self.constant_temp.clearRetainingCapacity();
                if (write_line) {
                    try writer.print(" // {s}\n", .{ line } );
                } else try writer.writeAll("\n");
            },
        }
    }
}

pub fn countErrors(self: *Assembler) usize {
    return self.errors.items.len
        + self.insn_encoding_errors.items.len
        + self.overlapping_chunks.count()
        ;
}

pub fn printErrors(self: *Assembler, writer: anytype) !void {
    for (self.errors.items) |err| {
        try err.print(self, writer);
    }
    for (self.insn_encoding_errors.items) |err| {
        try err.print(self, writer);
    }
    for (self.overlapping_chunks.entries.items(.key)) |chunk_pair| {
        const a_range = chunk_pair.a.getAddressRange(self);
        const b_range = chunk_pair.b.getAddressRange(self);
        const intersection = a_range.intersectionWith(b_range);

        try writer.print("Found chunks overlapping on address range [{X:0>16},{X:0>16}]\n", .{
            intersection.begin, intersection.end - 1,
        });
    }
}

fn resetLayout(self: *Assembler) void {
    // std.debug.print("Resetting Layout\n", .{});
    for (self.files.items) |*file| {
        var expr_resolved_constants = file.expressions.items(.resolved_constant);
        for (file.expressions.items(.flags), 0..) |flags, expr_handle| {
            if (flags.contains(.constant_depends_on_layout)) {
                expr_resolved_constants[expr_handle] = null;
            }
        }
    }

    var errors = self.errors.items;
    var i = errors.len;
    while (i > 0) {
        i -= 1;
        const flags = errors[i].flags;
        if (flags.contains(.remove_on_layout_reset)) {
            if (flags.contains(.desc_is_allocated)) {
                self.gpa.free(errors[i].desc);
            }
            _ = self.errors.swapRemove(i);
        }
    }

    var insn_encoding_errors = self.insn_encoding_errors.items;
    i = insn_encoding_errors.len;
    while (i > 0) {
        i -= 1;
        if (insn_encoding_errors[i].flags.contains(.remove_on_layout_reset)) {
            _ = self.insn_encoding_errors.swapRemove(i);
        }
    }

    for (self.pages.items(.chunks)) |*chunks| {
        chunks.deinit(self.gpa);
    }
    self.pages.len = 0;
    self.page_lookup.clearRetainingCapacity();
}

pub fn getSource(self: *Assembler, handle: SourceFile.Handle) *SourceFile {
    return &self.files.items[handle];
}

pub fn getSection(self: *Assembler, handle: Section.Handle) Section {
    return self.sections.entries.items(.value)[handle];
}

pub fn findOrCreatePage(self: *Assembler, page: bus.Page, section: Section.Handle) PageData.Handle {
    if (self.page_lookup.get(page)) |handle| return handle;

    const handle = @intCast(PageData.Handle, self.pages.len);
    self.page_lookup.ensureUnusedCapacity(self.gpa, 1) catch @panic("OOM");
    self.pages.append(self.gpa, PageData.init(page, section)) catch @panic("OOM");
    self.page_lookup.putAssumeCapacity(page, handle);
    // std.debug.print("{X:0>13}: Created page for section {}\n", .{ page, section });
    return handle;
}

pub fn recordError(self: *Assembler, file_handle: SourceFile.Handle, token: lex.Token.Handle, desc: []const u8, flags: Error.FlagSet) void {
    self.errors.append(self.gpa, .{
        .file = file_handle,
        .token = token,
        .desc = desc,
        .flags = flags,
    }) catch @panic("OOM");
}

pub fn recordExpressionLayoutError(self: *Assembler, file_handle: SourceFile.Handle, ctx_expr_handle: Expression.Handle, token_expr_handle: Expression.Handle, desc: []const u8, flags: Error.FlagSet) void {
    var mutable_flags = flags;
    const file = self.files.items[file_handle];
    const token_handle = file.expressions.items(.token)[token_expr_handle];
    const expr_flags = file.expressions.items(.flags)[ctx_expr_handle];
    if (expr_flags.contains(.constant_depends_on_layout)) {
        mutable_flags.insert(.remove_on_layout_reset);
    }
    self.recordError(file_handle, token_handle, desc, mutable_flags);
}

pub fn recordInsnEncodingError(self: *Assembler, file_handle: SourceFile.Handle, insn_handle: Instruction.Handle, flags: InsnEncodingError.FlagSet) void {
    self.insn_encoding_errors.append(self.gpa, .{
        .file = file_handle,
        .insn = insn_handle,
        .flags = flags,
    }) catch @panic("OOM");
}

pub const SymbolTarget = union(enum) {
    not_found,
    expression: Expression.Handle, // always in the same file where it's being referenced
    instruction: InstructionRef,
};
pub fn lookupSymbol(self: *Assembler, s: SourceFile.Slices, symbol_token_handle: lex.Token.Handle, symbol: []const u8) ?SymbolTarget {
    const file = s.file;
    const operations = s.insn.items(.operation);
    const resolved_constants = s.expr.items(.resolved_constant);

    const insn_containing_symbol_expression = file.findInstructionByToken(symbol_token_handle);

    // 1. .def symbols
    var def_symbol_definition: ?Expression.Handle = null;
    const block_handle = file.findBlockByToken(symbol_token_handle);
    var block_iter = file.blockInstructions(block_handle);
    while (block_iter.next()) |insn_handle| {
        if (insn_handle >= insn_containing_symbol_expression) break;

        switch (operations[insn_handle]) {
            .def => {
                const params_expr = s.insn.items(.params)[insn_handle].?;
                const symbol_and_definition = s.expr.items(.info)[params_expr].list;
                const def_symbol_expr = symbol_and_definition.left;
                const def_symbol = resolved_constants[def_symbol_expr].?;
                if (std.mem.eql(u8, symbol, def_symbol.asString())) {
                    def_symbol_definition = symbol_and_definition.right;
                }
            },
            .undef => if (def_symbol_definition) |_| {
                if (s.insn.items(.params)[insn_handle]) |params_expr| {
                    if (undefListContainsSymbol(self, s, params_expr, symbol)) {
                        def_symbol_definition = null;
                    }
                }
            },
            else => {},
        }
    }
    if (def_symbol_definition) |expr_handle| {
        return .{ .expression = expr_handle };
    }

    // 2. private labels
    if (std.mem.startsWith(u8, symbol, "_")) {
        const labels = s.insn.items(.label);
        block_iter = file.blockInstructions(block_handle);
        while (block_iter.next()) |insn_handle| {
            if (labels[insn_handle]) |label_expr| {
                const label_constant = resolved_constants[label_expr] orelse unreachable;
                if (std.mem.eql(u8, symbol, label_constant.asString())) {
                    return .{ .instruction = .{
                        .file = file.handle,
                        .instruction = insn_handle,
                    }};
                }
            }
        }
    }

    // 3. stack labels from .pushed contexts
    // TODO stack sections

    // 4. global labels
    if (self.symbols.get(symbol)) |insn_ref| {
        return .{ .instruction = insn_ref };
    }

    return .{ .not_found = {} };
}

fn undefListContainsSymbol(self: *Assembler, s: SourceFile.Slices, expr_handle: Expression.Handle, symbol: []const u8) bool {
    switch (s.expr.items(.info)[expr_handle]) {
        .list => |bin| {
            return undefListContainsSymbol(self, s, bin.left, symbol)
                or undefListContainsSymbol(self, s, bin.right, symbol);
        },
        .literal_symbol_def => {
            const token_handle = s.expr.items(.token)[expr_handle];
            const raw_symbol = s.file.tokens.get(token_handle).location(s.file.source);
            const undef_symbol = Constant.initSymbolLiteral(self.gpa, &self.constant_temp, raw_symbol);
            return std.mem.eql(u8, symbol, undef_symbol.asString());
        },
        .directive_symbol_def => |literal_expr| {
            const undef_symbol = s.expr.items(.resolved_constant)[literal_expr].?;
            return std.mem.eql(u8, symbol, undef_symbol.asString());
        },
        else => unreachable,
    }
}

pub fn buildInstruction(self: *Assembler, s: SourceFile.Slices, ip: u32, mnemonic: ie.Mnemonic, suffix: ie.MnemonicSuffix, params: ?Expression.Handle, record_errors: bool) ?ie.Instruction {
    self.params_temp.clearRetainingCapacity();
    if (params) |base_expr_handle| {
        const expr_infos = s.expr.items(.info);
        const expr_resolved_types = s.expr.items(.resolved_type);

        var expr_handle = base_expr_handle;
        var is_arrow = false; // TODO want a way to have is_arrow on the first parameter
        while (true) {
            const info = expr_infos[expr_handle];
            switch (info) {
                .list, .arrow_list => |bin| {
                    const expr_type = expr_resolved_types[bin.left];
                    if (!self.buildInstructionParameter(s, ip, bin.left, is_arrow, expr_type, record_errors)) {
                        return null;
                    }
                    is_arrow = info == .arrow_list;
                    expr_handle = bin.right;
                },
                else => {
                    const expr_type = expr_resolved_types[expr_handle];
                    if (!self.buildInstructionParameter(s, ip, expr_handle, is_arrow, expr_type, record_errors)) {
                        return null;
                    }
                    break;
                },
            }
        }
    }

    return .{
        .mnemonic = mnemonic,
        .suffix = suffix,
        .params = self.params_temp.items,
    };
}

fn buildInstructionParameter(
    self: *Assembler,
    s: SourceFile.Slices,
    ip: u32,
    expr_handle: Expression.Handle,
    is_arrow: bool,
    resolved_type: ie.ExpressionType,
    record_errors: bool,
) bool {
    if (resolved_type == .poison) {
        return false;
    }
    const constant_value = v: {
        const constant = layout.resolveExpressionConstant(self, s, ip, expr_handle) orelse break :v 0;
        break :v constant.asInt() catch {
            if (record_errors) {
                const expr_token = s.expr.items(.token)[expr_handle];
                const expr_flags = s.expr.items(.flags)[expr_handle];
                var err_flags = Error.FlagSet{};
                if (expr_flags.contains(.constant_depends_on_layout)) {
                    err_flags.insert(.remove_on_layout_reset);
                }
                self.recordError(s.file.handle, expr_token, "Parameter constant too large (must fit in i64)", err_flags);
            }
            return false;
        };
    };
    self.params_temp.append(self.gpa, .{
        .arrow = is_arrow,
        .expr_type = resolved_type,
        .constant = constant_value,
    }) catch @panic("OOM");
    return true;
}
