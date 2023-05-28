const std = @import("std");
const isa = @import("isa_types");
const ie = @import("isa_encoding");
const bus = @import("bus_types");
const lex = @import("lex.zig");
const typechecking = @import("typechecking.zig");
const dead_code = @import("dead_code.zig");
const layout = @import("layout.zig");
const symbols = @import("symbols.zig");
const Constant = @import("Constant.zig");
const Section = @import("Section.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const SourceFile = @import("SourceFile.zig");
const Error = @import("Error.zig");
const PageData = @import("PageData.zig");
const ExpressionType = ie.Parameter.ExpressionType;
const Mnemonic = isa.Mnemonic;
const MnemonicSuffix = isa.MnemonicSuffix;

const Assembler = @This();

edb: ie.data.EncoderDatabase,
gpa: std.mem.Allocator,
arena: std.mem.Allocator,
files: std.ArrayListUnmanaged(SourceFile) = .{},
errors: std.ArrayListUnmanaged(Error) = .{},
overlapping_chunks: std.AutoArrayHashMapUnmanaged(SourceFile.ChunkPair, void) = .{},
public_labels: std.StringHashMapUnmanaged(symbols.InstructionRef) = .{},
sections: std.StringArrayHashMapUnmanaged(Section) = .{},
chunks: std.ArrayListUnmanaged(ChunkWithAddress) = .{},
pages: std.MultiArrayList(PageData) = .{},
page_lookup: std.AutoHashMapUnmanaged(bus.Page, PageData.Handle) = .{},
constant_temp: std.ArrayListUnmanaged(u8) = .{},
params_temp: std.ArrayListUnmanaged(ie.Parameter) = .{},
constants: Constant.InternPool = .{},

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
    first: u32,
    len: usize,

    pub fn last(self: AddressRange) u32 {
        return self.first + @intCast(u32, self.len - 1);
    }

    pub fn intersectionWith(self: AddressRange, other: AddressRange) AddressRange {
        const begin = @max(self.first, other.first);
        const end = @max(begin, @min(self.first + self.len, other.first + other.len));
        return .{
            .first = begin,
            .len = end - begin,
        };
    }
};

pub fn init(gpa: std.mem.Allocator, arena: std.mem.Allocator, edb: ie.data.EncoderDatabase) Assembler {
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

    for (self.files.items) |*file| {
        const s = file.slices();
        typechecking.checkInstructionsAndDirectivesInFile(self, s);
        typechecking.checkSymbolAmbiguityInFile(self, s);
        dead_code.markBlocksToKeep(self, s);
    }

    var fixed_org_chunks = std.ArrayListUnmanaged(SourceFile.Chunk) {};
    var auto_org_chunks = std.ArrayListUnmanaged(SourceFile.Chunk) {};
    defer fixed_org_chunks.deinit(self.gpa);
    defer auto_org_chunks.deinit(self.gpa);
    for (self.files.items) |*file| {
        file.collectChunks(self, &fixed_org_chunks, &auto_org_chunks);
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
    for (self.overlapping_chunks.entries.items(.key)) |chunk_pair| {
        const a_range = chunk_pair.a.getAddressRange(self);
        const b_range = chunk_pair.b.getAddressRange(self);
        const intersection = a_range.intersectionWith(b_range);

        try writer.print("Found chunks overlapping on address range [{X:0>16},{X:0>16}]\n", .{
            intersection.first, intersection.last(),
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

    for (self.pages.items(.chunks)) |*chunks| {
        chunks.deinit(self.gpa);
    }
    self.pages.len = 0;
    self.page_lookup.clearRetainingCapacity();
}

pub fn getSource(self: *Assembler, handle: SourceFile.Handle) *SourceFile {
    return &self.files.items[handle];
}

pub fn getSection(self: *const Assembler, handle: Section.Handle) Section {
    return self.sections.values()[handle];
}

pub fn getSectionPtr(self: *Assembler, handle: Section.Handle) *Section {
    return &self.sections.values()[handle];
}

pub fn findOrCreatePage(self: *Assembler, page: bus.Page, access: Section.AccessPolicies, section: Section.Handle) PageData.Handle {
    if (self.page_lookup.get(page)) |handle| return handle;

    const handle = @intCast(PageData.Handle, self.pages.len);
    self.page_lookup.ensureUnusedCapacity(self.gpa, 1) catch @panic("OOM");
    self.pages.append(self.gpa, PageData.init(page, access, section)) catch @panic("OOM");
    self.page_lookup.putAssumeCapacity(page, handle);
    // std.debug.print("{X:0>13}: Created page for section {}\n", .{ page, section });
    return handle;
}

pub fn readByte(self: *Assembler, address: u32, address_space: isa.AddressSpace) u8 {
    _ = address_space;
    var iter = PageData.DataIterator.init(self, address);
    return iter.next();
}

pub fn recordTokenError(self: *Assembler, file_handle: SourceFile.Handle, token: lex.Token.Handle, desc: []const u8, flags: Error.FlagSet) void {
    self.errors.append(self.gpa, .{
        .file = file_handle,
        .context = .{ .token = token },
        .desc = desc,
        .flags = flags,
    }) catch @panic("OOM");
}

pub fn recordInsnError(self: *Assembler, file_handle: SourceFile.Handle, insn_handle: Instruction.Handle, desc: []const u8, flags: Error.FlagSet) void {
    self.errors.append(self.gpa, .{
        .file = file_handle,
        .context = .{ .instruction = insn_handle },
        .desc = desc,
        .flags = flags,
    }) catch @panic("OOM");
}

pub fn recordInsnErrorFmt(self: *Assembler, file_handle: SourceFile.Handle, insn_handle: Instruction.Handle, comptime fmt: []const u8, args: anytype, flags: Error.FlagSet) void {
    const desc = std.fmt.allocPrint(self.gpa, fmt, args) catch @panic("OOM");
    var mutable_flags = flags;
    mutable_flags.insert(.desc_is_allocated);
    self.errors.append(self.gpa, .{
        .file = file_handle,
        .context = .{ .instruction = insn_handle },
        .desc = desc,
        .flags = mutable_flags,
    }) catch @panic("OOM");
}

pub fn recordExprError(self: *Assembler, file_handle: SourceFile.Handle, expr_handle: Expression.Handle, desc: []const u8, flags: Error.FlagSet) void {
    self.errors.append(self.gpa, .{
        .file = file_handle,
        .context = .{ .expression = expr_handle },
        .desc = desc,
        .flags = flags,
    }) catch @panic("OOM");
}

pub fn recordExprErrorFmt(self: *Assembler, file_handle: SourceFile.Handle, expr_handle: Expression.Handle, comptime fmt: []const u8, args: anytype, flags: Error.FlagSet) void {
    const desc = std.fmt.allocPrint(self.gpa, fmt, args) catch @panic("OOM");
    var mutable_flags = flags;
    mutable_flags.insert(.desc_is_allocated);
    self.errors.append(self.gpa, .{
        .file = file_handle,
        .context = .{ .expression = expr_handle },
        .desc = desc,
        .flags = mutable_flags,
    }) catch @panic("OOM");
}

pub fn recordExpressionLayoutError(self: *Assembler, file_handle: SourceFile.Handle, outer_expr_handle: Expression.Handle, context_expr_handle: Expression.Handle, desc: []const u8, flags: Error.FlagSet) void {
    var mutable_flags = flags;
    const file = self.files.items[file_handle];
    const expr_flags = file.expressions.items(.flags)[outer_expr_handle];
    if (expr_flags.contains(.constant_depends_on_layout)) {
        mutable_flags.insert(.remove_on_layout_reset);
    }
    self.recordExprError(file_handle, context_expr_handle, desc, mutable_flags);
}

pub fn recordInsnEncodingError(self: *Assembler, file_handle: SourceFile.Handle, insn_handle: Instruction.Handle, flags: Error.FlagSet) void {
    var mutable_flags = flags;
    mutable_flags.insert(.is_instruction_encoding_error);
    self.errors.append(self.gpa, .{
        .file = file_handle,
        .context = .{ .instruction = insn_handle },
        .desc = "No encodings found matching instruction signature: ",
        .flags = mutable_flags,
    }) catch @panic("OOM");
}

pub fn buildInstruction(self: *Assembler, s: SourceFile.Slices, ip: u32, mnemonic: Mnemonic, suffix: MnemonicSuffix, params: ?Expression.Handle, record_errors: bool) ?ie.Instruction {
    self.params_temp.clearRetainingCapacity();
    if (params) |base_expr_handle| {
        const expr_infos = s.expr.items(.info);
        const expr_resolved_types = s.expr.items(.resolved_type);

        var expr_handle = base_expr_handle;
        var is_arrow = false;
        while (true) {
            const info = expr_infos[expr_handle];
            switch (info) {
                .list, .arrow_list => |bin| {
                    const param_expr_handle = switch (expr_infos[bin.left]) {
                        .arrow_prefix => |inner| inner,
                        else => bin.left,
                    };
                    const expr_type = expr_resolved_types[param_expr_handle];
                    if (!self.buildInstructionParameter(s, ip, param_expr_handle, is_arrow, expr_type, record_errors)) {
                        return null;
                    }
                    is_arrow = info == .arrow_list;
                    expr_handle = bin.right;
                },
                .arrow_prefix => |inner| {
                    is_arrow = true;
                    expr_handle = inner;
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
    resolved_type: ExpressionType,
    record_errors: bool,
) bool {
    if (resolved_type == .poison) {
        return false;
    }
    const constant_value = v: {
        const constant = layout.resolveExpressionConstant(self, s, ip, expr_handle) orelse break :v 0;
        break :v constant.asInt(i64) catch {
            if (record_errors) {
                self.recordExpressionLayoutError(s.file.handle, expr_handle, expr_handle, "Parameter constant too large (must fit in i64)", .{});
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
