edb: iedb.Encoding_Database,
gpa: std.mem.Allocator,
arena: std.mem.Allocator,
files: std.ArrayListUnmanaged(Source_File) = .{},
errors: std.ArrayListUnmanaged(Error) = .{},
overlapping_chunks: std.AutoArrayHashMapUnmanaged(Source_File.Chunk_Pair, void) = .{},
public_labels: std.StringHashMapUnmanaged(symbols.Instruction_Ref) = .{},
sections: std.StringArrayHashMapUnmanaged(Section) = .{},
chunks: std.ArrayListUnmanaged(Chunk_With_Address) = .{},
pages: std.MultiArrayList(Page_Data) = .{},
page_lookup: std.AutoHashMapUnmanaged(arch.addr.Page, Page_Data.Handle) = .{},
constant_temp: std.ArrayListUnmanaged(u8) = .{},
params_temp: std.ArrayListUnmanaged(isa.Parameter) = .{},
constants: Constant.Intern_Pool = .{},

pub const Chunk_With_Address = struct {
    chunk: Source_File.Chunk,
    address: u32,

    fn less_than(_: void, lhs: Chunk_With_Address, rhs: Chunk_With_Address) bool {
        return lhs.address < rhs.address;
    }
};

pub const Alignment = struct {
    modulo: u32,
    offset: u32,
};

pub const Address_Range = struct {
    first: u32,
    len: usize,

    pub fn last(self: Address_Range) u32 {
        return self.first + @as(u32, @intCast(self.len - 1));
    }

    pub fn intersection_with(self: Address_Range, other: Address_Range) Address_Range {
        const begin = @max(self.first, other.first);
        const end = @max(begin, @min(self.first + self.len, other.first + other.len));
        return .{
            .first = begin,
            .len = end - begin,
        };
    }
};

pub fn init(gpa: std.mem.Allocator, arena: std.mem.Allocator, edb: iedb.Encoding_Database) Assembler {
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
    const maybe_arena = if (deinit_arena) self.arena else null;

    if (deinit_arena) {
        var constant_iter = self.constants.keyIterator();
        while (constant_iter.next()) |constant| {
            self.arena.destroy(constant);
        }

        for (self.pages.items(.data)) |*buf| {
            self.arena.free(buf);
        }
    }

    for (self.files.items) |*file| {
        file.deinit(self.gpa, maybe_arena);
    }

    for (self.pages.items(.chunks)) |*chunks| {
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
    self.public_labels.deinit(self.gpa);
    self.sections.deinit(self.gpa);
    self.chunks.deinit(self.gpa);
    self.pages.deinit(self.gpa);
    self.page_lookup.deinit(self.gpa);
    self.constant_temp.deinit(self.gpa);
    self.params_temp.deinit(self.gpa);
    self.constants.deinit(self.gpa);
}

pub fn add_source(self: *Assembler, name: []const u8, source: []const u8) Source_File.Handle {
    const owned_name = self.arena.dupe(u8, name) catch @panic("OOM");
    const owned_source = self.arena.dupe(u8, source) catch @panic("OOM");
    return self.adopt_source(owned_name, owned_source);
}

// Takes ownership of name and source; assumed to have been allocated with self.arena
pub fn adopt_source(self: *Assembler, name: []const u8, source: []const u8) Source_File.Handle {
    const handle: Source_File.Handle = @intCast(self.files.items.len);
    const file = Source_File.parse(self.gpa, handle, name, source, &self.errors);
    const ptr = self.files.addOne(self.gpa) catch @panic("OOM");
    ptr.* = file;
    return handle;
}

pub fn assemble(self: *Assembler) void {
    for (self.files.items) |*file| {
        typechecking.process_labels_and_sections(self, file);
    }

    if (!typechecking.resolve_expression_types(self)) return;

    for (self.files.items) |*file| {
        const s = file.slices();
        typechecking.check_instructions_and_directives_in_file(self, s);
        typechecking.check_symbol_ambiguity_in_file(self, s);
        dead_code.mark_blocks_to_keep(self, s);
    }

    var fixed_org_chunks = std.ArrayListUnmanaged(Source_File.Chunk) {};
    var auto_org_chunks = std.ArrayListUnmanaged(Source_File.Chunk) {};
    defer fixed_org_chunks.deinit(self.gpa);
    defer auto_org_chunks.deinit(self.gpa);
    for (self.files.items) |*file| {
        file.collect_chunks(self, &fixed_org_chunks, &auto_org_chunks);
    }

    var attempts: usize = 0;
    const max_attempts = 100;
    while (attempts < max_attempts) : (attempts += 1) {
        self.reset_layout();

        var try_again = layout.do_fixed_org_layout(self, fixed_org_chunks.items);
        try_again = layout.do_auto_org_layout(self, auto_org_chunks.items) or try_again;

        if (!try_again) break;
    }

    if (attempts == max_attempts) {
        std.debug.print("Failed to find a stable layout after {} iterations!\n", .{ attempts });
    }

    layout.populate_page_chunks(self, fixed_org_chunks.items);
    layout.populate_page_chunks(self, auto_org_chunks.items);

    std.sort.pdq(Chunk_With_Address, self.chunks.items, {}, Chunk_With_Address.less_than);

    for (self.pages.items(.chunks)) |chunks| {
        layout.find_overlapping_chunks(self, chunks.items);
    }

    for (self.files.items) |*file| {
        layout.encode_page_data(self, file);
    }
}

pub fn print_errors(self: *Assembler, writer: anytype) !void {
    for (self.errors.items) |err| {
        try err.print(self, writer);
    }
    for (self.overlapping_chunks.entries.items(.key)) |chunk_pair| {
        const a_range = chunk_pair.a.address_range(self);
        const b_range = chunk_pair.b.address_range(self);
        const intersection = a_range.intersection_with(b_range);

        try writer.print("Found chunks overlapping on address range [{X:0>16},{X:0>16}]\n", .{
            intersection.first, intersection.last(),
        });
    }
}

fn reset_layout(self: *Assembler) void {
    // std.debug.print("Resetting Layout\n", .{});
    for (self.files.items) |*file| {
        var expr_resolved_constants = file.expressions.items(.resolved_constant);
        for (file.expressions.items(.flags), 0..) |flags, expr_handle| {
            if (flags.contains(.constant_depends_on_layout)) {
                expr_resolved_constants[expr_handle] = null;
            }
        }
    }

    const errors = self.errors.items;
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

pub fn get_source(self: *Assembler, handle: Source_File.Handle) *Source_File {
    return &self.files.items[handle];
}

pub fn get_section(self: *const Assembler, handle: Section.Handle) Section {
    return self.sections.values()[handle];
}

pub fn get_section_ptr(self: *Assembler, handle: Section.Handle) *Section {
    return &self.sections.values()[handle];
}

pub fn find_or_create_page(self: *Assembler, page: arch.addr.Page, access: Section.Access_Policies, section: Section.Handle) Page_Data.Handle {
    if (self.page_lookup.get(page)) |handle| return handle;

    const handle: Page_Data.Handle = @intCast(self.pages.len);
    self.page_lookup.ensureUnusedCapacity(self.gpa, 1) catch @panic("OOM");
    self.pages.append(self.gpa, Page_Data.init(page, access, section)) catch @panic("OOM");
    self.page_lookup.putAssumeCapacity(page, handle);
    // std.debug.print("{X:0>13}: Created page for section {}\n", .{ page, section });
    return handle;
}

pub fn read_byte(self: *Assembler, address: u32, address_space: isa.Address_Space) u8 {
    _ = address_space;
    var iter = Page_Data.Data_Iterator.init(self, address);
    return iter.next();
}

pub fn record_error(self: *Assembler, file_handle: Source_File.Handle, context: Error.Context, desc: []const u8, flags: Error.Flag_Set) void {
    self.errors.append(self.gpa, .{
        .file = file_handle,
        .context = context,
        .desc = desc,
        .flags = flags,
    }) catch @panic("OOM");
}

pub fn record_error_fmt(self: *Assembler, file_handle: Source_File.Handle, context: Error.Context, comptime fmt: []const u8, args: anytype, flags: Error.Flag_Set) void {
    const desc = std.fmt.allocPrint(self.gpa, fmt, args) catch @panic("OOM");
    var mutable_flags = flags;
    mutable_flags.insert(.desc_is_allocated);
    self.errors.append(self.gpa, .{
        .file = file_handle,
        .context = context,
        .desc = desc,
        .flags = mutable_flags,
    }) catch @panic("OOM");
}

pub fn record_token_error(self: *Assembler, file_handle: Source_File.Handle, token: lex.Token.Handle, desc: []const u8, flags: Error.Flag_Set) void {
    record_error(self, file_handle, .{ .token = token }, desc, flags);
}

pub fn record_token_error_fmt(self: *Assembler, file_handle: Source_File.Handle, token: lex.Token.Handle, comptime fmt: []const u8, args: anytype, flags: Error.Flag_Set) void {
    record_error_fmt(self, file_handle, .{ .token = token }, fmt, args, flags);
}

pub fn record_insn_error(self: *Assembler, file_handle: Source_File.Handle, insn_handle: Instruction.Handle, desc: []const u8, flags: Error.Flag_Set) void {
    record_error(self, file_handle, .{ .instruction = insn_handle }, desc, flags);
}

pub fn record_insn_error_fmt(self: *Assembler, file_handle: Source_File.Handle, insn_handle: Instruction.Handle, comptime fmt: []const u8, args: anytype, flags: Error.Flag_Set) void {
    record_error_fmt(self, file_handle, .{ .instruction = insn_handle }, fmt, args, flags);
}

pub fn record_expr_error(self: *Assembler, file_handle: Source_File.Handle, expr_handle: Expression.Handle, desc: []const u8, flags: Error.Flag_Set) void {
    record_error(self, file_handle, .{ .expression = expr_handle }, desc, flags);
}

pub fn record_expr_error_fmt(self: *Assembler, file_handle: Source_File.Handle, expr_handle: Expression.Handle, comptime fmt: []const u8, args: anytype, flags: Error.Flag_Set) void {
    record_error_fmt(self, file_handle, .{ .expression = expr_handle }, fmt, args, flags);
}

pub fn record_expr_layout_error(self: *Assembler, file_handle: Source_File.Handle, outer_expr_handle: Expression.Handle, context_expr_handle: Expression.Handle, desc: []const u8, flags: Error.Flag_Set) void {
    var mutable_flags = flags;
    const file = self.files.items[file_handle];
    const expr_flags = file.expressions.items(.flags)[outer_expr_handle];
    if (expr_flags.contains(.constant_depends_on_layout)) {
        mutable_flags.insert(.remove_on_layout_reset);
    }
    self.record_expr_error(file_handle, context_expr_handle, desc, mutable_flags);
}

pub fn record_insn_encoding_error(self: *Assembler, file_handle: Source_File.Handle, insn_handle: Instruction.Handle, flags: Error.Flag_Set) void {
    var mutable_flags = flags;
    mutable_flags.insert(.is_instruction_encoding_error);
    self.errors.append(self.gpa, .{
        .file = file_handle,
        .context = .{ .instruction = insn_handle },
        .desc = "No encodings found matching instruction signature: ",
        .flags = mutable_flags,
    }) catch @panic("OOM");
}

pub fn build_instruction(self: *Assembler, s: Source_File.Slices, ip: u32, mnemonic: Mnemonic, suffix: Mnemonic_Suffix, params: ?Expression.Handle, record_errors: bool) ?isa.Instruction {
    self.params_temp.clearRetainingCapacity();
    if (params) |base_expr_handle| {
        const expr_infos = s.expr.items(.info);
        const expr_resolved_types = s.expr.items(.resolved_type);

        var expr_handle = base_expr_handle;
        while (true) {
            const info = expr_infos[expr_handle];
            const param_expr_handle = switch (info) {
                .list => |bin| h: {
                    expr_handle = bin.right;
                    break :h bin.left;
                },
                else => expr_handle,
            };
            const expr_type = expr_resolved_types[param_expr_handle];
            if (!self.build_instruction_parameter(s, ip, param_expr_handle, expr_type, record_errors)) {
                return null;
            }
        }
    }

    return .{
        .mnemonic = mnemonic,
        .suffix = suffix,
        .params = self.params_temp.items,
    };
}

fn build_instruction_parameter(
    self: *Assembler,
    s: Source_File.Slices,
    ip: u32,
    expr_handle: Expression.Handle,
    resolved_type: Expression.Type,
    record_errors: bool,
) bool {
    switch (resolved_type) {
        .unknown, .poison, .symbol_def => return false,
        else => {},
    }
    const constant_value = v: {
        const constant = layout.resolve_expression_constant(self, s, ip, expr_handle) orelse break :v 0;
        break :v constant.as_int(i64) catch {
            if (record_errors) {
                self.record_expr_layout_error(s.file.handle, expr_handle, expr_handle, "Parameter constant too large (must fit in i64)", .{});
            }
            return false;
        };
    };
    self.params_temp.append(self.gpa, isa.Parameter{
        .signature = resolved_type.param_signature(),
        .base_register_index = resolved_type.param_base_register_index(),
        .offset_register_index = resolved_type.param_offset_register_index(),
        .constant = constant_value,
    }) catch @panic("OOM");
    return true;
}

const Assembler = @This();

const typechecking = @import("typechecking.zig");
const dead_code = @import("dead_code.zig");
const layout = @import("layout.zig");
const symbols = @import("symbols.zig");
const Constant = @import("Constant.zig");
const Section = @import("Section.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const Source_File = @import("Source_File.zig");
const Error = @import("Error.zig");
const Page_Data = @import("Page_Data.zig");
const Mnemonic = isa.Mnemonic;
const Mnemonic_Suffix = isa.Mnemonic_Suffix;
const iedb = @import("iedb");
const lex = isa.lex;
const isa = @import("isa");
const arch = @import("arch");
const std = @import("std");
