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
pages: std.MultiArrayList(PageData) = .{},
page_lookup: std.AutoHashMapUnmanaged(bus.Page, PageData.Handle) = .{},
constant_temp: std.ArrayListUnmanaged(u8) = .{},
params_temp: std.ArrayListUnmanaged(ie.Parameter) = .{},
constants: Constant.InternPool = .{},

pub const InstructionRef = struct {
    file: SourceFile.Handle,
    instruction: Instruction.Handle,
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

    self.files.deinit(self.gpa);
    self.errors.deinit(self.gpa);
    self.insn_encoding_errors.deinit(self.gpa);
    self.overlapping_chunks.deinit(self.gpa);
    self.symbols.deinit(self.gpa);
    self.sections.deinit(self.gpa);
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
    for (self.files.items, 0..) |*file, file_handle| {
        typechecking.processLabelsAndSections(self, file, @intCast(SourceFile.Handle, file_handle));
    }

    if (!typechecking.resolveExpressionTypes(self)) return;

    typechecking.checkInstructionsAndDirectives(self);

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

    var attempts: usize = 0;
    const max_attempts = 100;
    while (attempts < max_attempts) : (attempts += 1) {
        self.resetLayout();

        var try_again = layout.doFixedOrgLayout(self, fixed_org_chunks.items);
        try_again = layout.doAutoOrgLayout(self, auto_org_chunks.items) or try_again;

        // TODO better handling of degenerate/recursive cases where the layout never reaches an equilibrium?
        if (self.insn_encoding_errors.items.len > 0 or !try_again) break;
    }

    if (attempts == max_attempts) {
        std.debug.print("Failed to find a stable layout after {} iterations!\n", .{ attempts });
        //self.invalid_program = true;
    }

    layout.populatePageChunks(self, fixed_org_chunks.items);
    layout.populatePageChunks(self, auto_org_chunks.items);

    for (self.pages.items(.chunks)) |chunks| {
        layout.findOverlappingChunks(self, chunks.items);
    }

    for (self.files.items, 0..) |*file, file_handle| {
        layout.encodePageData(self, file, @intCast(SourceFile.Handle, file_handle));
    }
}

// TODO functions for outputting assembled data to file or in-memory buffer

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
        if (errors[i].flags.contains(.remove_on_layout_reset)) {
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

pub fn recordExpressionLayoutError(self: *Assembler, file_handle: SourceFile.Handle, expr_handle: Expression.Handle, desc: []const u8, flags: Error.FlagSet) void {
    var mutable_flags = flags;
    const file = self.files.items[file_handle];
    const token_handle = file.expressions.items(.token)[expr_handle];
    const expr_flags = file.expressions.items(.flags)[expr_handle];
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
                    return .{ .instruction = .{
                        .file = file_handle,
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

pub fn buildInstruction(self: *Assembler, file: *SourceFile, file_handle: SourceFile.Handle, ip: u32, mnemonic: ie.Mnemonic, suffix: ie.MnemonicSuffix, params: ?Expression.Handle, record_errors: bool) ?ie.Instruction {
    self.params_temp.clearRetainingCapacity();
    if (params) |base_expr_handle| {
        const expr_infos = file.expressions.items(.info);
        const expr_resolved_types = file.expressions.items(.resolved_type);

        var expr_handle = base_expr_handle;
        var is_arrow = false; // TODO want a way to have is_arrow on the first parameter
        while (true) {
            const info = expr_infos[expr_handle];
            switch (info) {
                .list, .arrow_list => |bin| {
                    const expr_type = expr_resolved_types[bin.left];
                    if (!self.buildInstructionParameter(file, file_handle, ip, bin.left, is_arrow, expr_type, record_errors)) {
                        return null;
                    }
                    is_arrow = info == .arrow_list;
                    expr_handle = bin.right;
                },
                else => {
                    const expr_type = expr_resolved_types[expr_handle];
                    if (!self.buildInstructionParameter(file, file_handle, ip, expr_handle, is_arrow, expr_type, record_errors)) {
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
    file: *SourceFile,
    file_handle: SourceFile.Handle,
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
        const constant = layout.resolveExpressionConstant(self, file, file_handle, ip, expr_handle) orelse break :v 0;
        break :v constant.asInt() catch {
            if (record_errors) {
                const expr_token = file.expressions.items(.token)[expr_handle];
                const expr_flags = file.expressions.items(.flags)[expr_handle];
                var err_flags = Error.FlagSet{};
                if (expr_flags.contains(.constant_depends_on_layout)) {
                    err_flags.insert(.remove_on_layout_reset);
                }
                self.recordError(file_handle, expr_token, "Parameter constant too large (must fit in i64)", err_flags);
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
