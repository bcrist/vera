const std = @import("std");
const ie = @import("instruction_encoding");
const ie_data = @import("instruction_encoding_data").data;
const bus = @import("bus_types");
const types = @import("types.zig");
const lex = @import("lex.zig");
const typechecking = @import("typechecking.zig");
const dead_code = @import("dead_code.zig"); // TODO maybe consider merging with layout.zig
const layout = @import("layout.zig");
const Constant = @import("Constant.zig");
const Section = @import("Section.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const SourceFile = @import("SourceFile.zig");
const Error = @import("Error.zig");
const PageData = @import("PageData.zig");

const Assembler = @This();

edb: ie.EncoderDatabase,
gpa: std.mem.Allocator,
arena: std.mem.Allocator,
files: std.ArrayListUnmanaged(SourceFile) = .{},
errors: std.ArrayListUnmanaged(Error) = .{},
symbols: std.StringHashMapUnmanaged(InstructionRef) = .{},
sections: std.StringArrayHashMapUnmanaged(Section) = .{},
pages: std.MultiArrayList(PageData) = .{},
page_lookup: std.AutoHashMapUnmanaged(bus.Page, PageData.Handle) = .{},
invalid_layout: bool = false,
constant_temp: std.ArrayListUnmanaged(u8) = .{},
constants: Constant.InternPool = .{},
types: types.InternPool = .{},

pub const InstructionRef = struct {
    file: SourceFile.Handle,
    instruction: Instruction.Handle,
};

pub fn init(gpa: std.mem.Allocator, arena: std.mem.Allocator, edb: ie.EncoderDatabase) Assembler {
    // It's possible to use the same allocator for gpa and arena,
    // but nothing allocated in the arena allocator will be freed
    // until Assembler.deinit() is called, so it's a good candidate
    // for std.heap.ArenaAllocator use.

    // Note also that the EncoderDatabase is not owned by the Assembler
    // but its lifetime must not be less than the Assembler lifetime.

    return .{
        .edb = edb,
        .gpa = gpa,
        .arena = arena,
    };
}

pub fn deinit(self: *Assembler, deinit_arena: bool) void {
    var maybe_arena = if (deinit_arena) self.arena else null;

    if (deinit_arena) {
        var constant_iter = self.constants.keyIterator();
        while (constant_iter.next()) |constant| {
            constant.deinit(self.arena);
            self.arena.destroy(constant);
        }

        var type_iter = self.types.keyIterator();
        while (type_iter.next()) |t| {
            self.arena.destroy(t);
        }
    }

    for (self.files.items) |file| {
        file.deinit(self.gpa, maybe_arena);
    }

    self.files.deinit(self.gpa);
    self.errors.deinit(self.gpa);
    self.symbols.deinit(self.gpa);
    self.sections.deinit(self.gpa);
    self.pages.deinit(self.gpa);
    self.page_lookup.deinit(self.gpa);
    self.constant_temp.deinit(self.gpa);
    self.constants.deinit(self.gpa);
    self.types.deinit(self.gpa);
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

pub fn getSource(self: *Assembler, handle: SourceFile.Handle) *SourceFile {
    return &self.files.items[handle];
}

pub fn getSection(self: *Assembler, handle: Section.Handle) Section {
    return self.sections.entries.items(.value)[handle];
}

pub fn dump(self: *Assembler, writer: anytype) !void {
    try writer.writeAll("Files:\n");
    for (self.files.items) |file| {
        try writer.print("   {s}\n", .{ file.name });
        try writer.print("      Blocks:\n", .{});
        for (0..,
            file.blocks.items(.first_insn),
            file.blocks.items(.end_insn),
            file.blocks.items(.section),
            file.blocks.items(.keep),
        ) |handle, begin, end, maybe_section, keep| {
            try writer.print("         #{}: #{} - #{}", .{ handle, begin, end });
            if (maybe_section) |section| {
                try writer.print(" section#{}", .{ section });
            }
            if (keep) {
                try writer.writeAll(" keep");
            }
            try writer.writeAll("\n");
        }
        try writer.print("      Instructions:\n", .{});
        for (0..,
            file.instructions.items(.label),
            file.instructions.items(.token),
            file.instructions.items(.operation),
            file.instructions.items(.params),
            file.instructions.items(.address),
        ) |handle, label_handle, token_handle, operation, params_handle, maybe_address| {
            try writer.print("         #{}:", .{ handle });
            if (maybe_address) |address| {
                try writer.print(" {X:0>8}", .{ address });
            }
            if (label_handle) |label| {
                try writer.print(" #{}:", .{ label });
            }
            switch (operation) {
                .insn => |insn| {
                    try writer.print(" {s}", .{ @tagName(insn.mnemonic) });
                    if (insn.suffix != .none) {
                        try writer.print(".{s}", .{ @tagName(insn.suffix) });
                    }
                },
                .bound_insn => {
                    try writer.writeAll(" bound_insn");
                },
                else => {
                    try writer.print(" {s}", .{ @tagName(operation) });
                },
            }
            if (params_handle) |params| {
                try writer.print(" #{}", .{ params });
            }
            const token = file.tokens.get(token_handle);
            try writer.print(" '{s}'", .{ token.location(file.source) });
            try writer.writeAll("\n");
            switch (operation) {
                .bound_insn => |encoding| {
                    try encoding.print(writer, "            ");
                },
                else => {},
            }
        }
        try writer.print("      Expressions:\n", .{});
        for (0..,
            file.expressions.items(.token),
            file.expressions.items(.resolved_type),
            file.expressions.items(.resolved_constant),
            file.expressions.items(.info),
        ) |handle, token_handle, maybe_type, maybe_constant, info| {
            try writer.print("         #{}: {s}", .{ handle, @tagName(info) });
             switch (info) {
                .list, .arrow_list => |binary| {
                    try writer.print(" #{}, #{}", .{ binary.left, binary.right });
                },
                .directive_symbol_def, .directive_symbol_ref => |unary| {
                    try writer.print(" #{}", .{ unary });
                },
                .literal_int, .literal_str, .literal_reg, .literal_symbol_def, .literal_symbol_ref => {},
            }
            const token = file.tokens.get(token_handle);
            try writer.print(" '{s}'", .{ token.location(file.source) });
            if (maybe_type) |expr_type| {
                try dumpType(writer, expr_type.*, "  Type: ");
            }
            if (maybe_constant) |constant| {
                try writer.print("  Constant: {X} - {}b", .{ @ptrToInt(constant), constant.bit_count });
            }
            try writer.writeAll("\n");
        }
    }

    try writer.writeAll("Symbols:\n");
    var symbol_iter = self.symbols.iterator();
    while (symbol_iter.next()) |entry| {
        const symbol = entry.key_ptr.*;
        const file_handle = entry.value_ptr.file;
        const insn_handle = entry.value_ptr.instruction;
        const file_name = self.getSource(file_handle).name;
        try writer.print("   {s} #{} <- {s}\n", .{ file_name, insn_handle, symbol });
    }

    try writer.writeAll("Sections:\n");
    for (0.., self.sections.entries.items(.value)) |section_handle, section| {
        try writer.print("   #{}: {s} : {s}\n", .{ section_handle, section.name, @tagName(section.kind) });
    }

    // TODO pages
    // TODO invalid_layout

    try writer.writeAll("Errors:\n");
    for (self.errors.items) |err| {
        try err.print(self.*, writer);
    }
}

fn dumpType(writer: anytype, expr_type: types.Type, prefix: []const u8) !void {
    try writer.print("{s}{s}", .{ prefix, @tagName(expr_type) });
    switch (expr_type) {
        .constant => |constant| {
            if (constant.signedness) |sign| {
                try writer.print(" {s}", .{ @tagName(sign) });
            }
        },
        .reg8, .reg16, .reg32 => |reg| {
            try writer.print(" #{}", .{ reg.index });
            if (reg.signedness) |sign| {
                try writer.print(" {s}\n", .{ @tagName(sign) });
            }
        },
        .sr => |sr| {
            try writer.print(" {s}", .{ @tagName(sr) });
        },
        .data_address, .insn_address, .stack_address => |addr| {
            try dumpType(writer, addr.base.*, " ");
            try dumpType(writer, addr.offset.*, " + ");
        },
        else => {},
    }
}

pub fn recordError(self: *Assembler, file_handle: SourceFile.Handle, token: lex.Token.Handle, desc: []const u8) void {
    self.errors.append(self.gpa, .{
        .file = file_handle,
        .token = token,
        .desc = desc,
    }) catch @panic("OOM");
}

pub const SymbolTarget = union(enum) {
    not_found,
    expression: Expression.Handle, // always in the same file where it's being referenced
    address: InstructionRef,
};
pub fn lookupSymbol(self: *Assembler, file: *const SourceFile, file_handle: SourceFile.Handle, symbol_token_handle: lex.Token.Handle, symbol: []const u8) ?SymbolTarget {
    var operations = file.instructions.items(.operation);
    var params = file.instructions.items(.params);
    var labels = file.instructions.items(.label);

    var expr_info = file.expressions.items(.info);
    var resolved_constants = file.expressions.items(.resolved_constant);

    const insn_containing_symbol_expression = file.findInstructionByToken(symbol_token_handle);

    // 1. .def symbols
    var def_symbol_definition: ?Expression.Handle = null;
    var block_handle = file.findBlockByToken(symbol_token_handle);
    var block_iter = file.blockInstructions(block_handle);
    while (block_iter.next()) |insn_handle| {
        if (insn_handle >= insn_containing_symbol_expression) break;

        switch (operations[insn_handle]) {
            .def => switch (expr_info[params[insn_handle] orelse unreachable]) {
                .list => |symbol_and_definition| {
                    const def_symbol_expr = symbol_and_definition.left;
                    if (resolved_constants[def_symbol_expr]) |def_symbol| {
                        if (std.mem.eql(u8, symbol, def_symbol.asString())) {
                            def_symbol_definition = symbol_and_definition.right;
                        }
                    } else {
                        // .def name hasn't been processed yet, maybe try again later.
                        return null;
                    }
                },
                else => unreachable,
            },
            .undef => {
                if (resolved_constants[params[insn_handle] orelse unreachable]) |def_symbol| {
                    if (std.mem.eql(u8, symbol, def_symbol.asString())) {
                        def_symbol_definition = null;
                    }
                } else {
                    // .undef name hasn't been processed yet, maybe try again later.
                    return null;
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
        block_handle = file.findBlockByToken(symbol_token_handle);
        block_iter = file.blockInstructions(block_handle);
        while (block_iter.next()) |insn_handle| {
            if (labels[insn_handle]) |label_expr| {
                const label_constant = resolved_constants[label_expr] orelse unreachable;
                if (std.mem.eql(u8, symbol, label_constant.asString())) {
                    return .{ .address = .{
                        .file = file_handle,
                        .instruction = insn_handle,
                    }};
                }
            }
        }
    }

    // 3. stack labels from .pushed contexts
    // TODO

    // 4. global labels
    if (self.symbols.get(symbol)) |insn_ref| {
        return .{ .address = insn_ref };
    }

    return .{ .not_found = {} };
}


pub fn assemble(self: *Assembler) void {
    for (self.files.items, 0..) |*file, file_handle| {
        typechecking.processLabelsAndSections(self, file, @intCast(SourceFile.Handle, file_handle));
    }

    if (!typechecking.resolveExpressionTypes(self)) return;

    // TODO check that directives have the expected number and types of params
    // TODO check that instructions do not appear in data or stack sections
    // TODO check that data directives do not appear in code sections
    // TODO check for duplicate labels

    for (self.files.items, 0..) |*file, file_handle| {
        dead_code.markBlocksToKeep(self, file, @intCast(SourceFile.Handle, file_handle));
    }

    var fixed_org_chunks = std.ArrayListUnmanaged(SourceFile.Chunk) {};
    var auto_org_chunks = std.ArrayListUnmanaged(SourceFile.Chunk) {};
    defer fixed_org_chunks.deinit(self.gpa);
    defer auto_org_chunks.deinit(self.gpa);
    for (self.files.items, 0..) |*file, file_handle| {
        file.collectChunks(@intCast(SourceFile.Handle, file_handle), self.gpa, &fixed_org_chunks, &auto_org_chunks);
    }

    var try_again = true;
    while (try_again) {
        self.pages.len = 0;
        self.page_lookup.clearRetainingCapacity();

        try_again = layout.doFixedOrgLayout(self, fixed_org_chunks);
        try_again = layout.doAutoOrgLayout(self, auto_org_chunks) or try_again;
        if (self.invalid_layout) return;
    }

    // TODO encode instructions into PageData
}

// TODO functions for outputting assembled data to file or in-memory buffer




// test {
//     var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     defer arena.deinit();

//     var temp = std.heap.ArenaAllocator.init(std.heap.page_allocator);
//     const edb = try ie.EncoderDatabase.init(arena.allocator(), ie_data, temp.allocator());
//     const ddb = try ie.DecoderDatabase.init(arena.allocator(), ie_data, temp.allocator());
//     temp.deinit();

//     try parse.init();
//     defer parse.deinit();

//     const src =
//         \\label:
//         \\   nop //comment
//         \\   sync
//         \\   fret
//         \\
//         \\asdf: WFI
//     ;

//     var stderr = std.io.getStdErr().writer();

//     var results = try assemble(arena.allocator(), edb, src, 0);
//     for (results.errors.items) |err| {
//         const token = results.tokens.get(err.token);
//         try token.printContext(src, stderr, 160);
//         try stderr.print("{s}\n", .{ err.desc });
//     }
//     try std.testing.expectEqual(@as(usize, 0), results.errors.items.len);

//     _ = ddb;
// }
