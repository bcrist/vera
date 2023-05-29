const std = @import("std");
const lex = @import("lex.zig");
const isa = @import("isa_types");
const ie = @import("isa_encoding");
const symbols = @import("symbols.zig");
const Assembler = @import("Assembler.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const Section = @import("Section.zig");
const Error = @import("Error.zig");
const Parser = @import("Parser.zig");

const Mnemonic = isa.Mnemonic;
const SourceFile = @This();

handle: Handle,
name: []const u8,
source: []const u8,
tokens: lex.TokenList,
instructions: std.MultiArrayList(Instruction),
expressions: std.MultiArrayList(Expression),
blocks: std.MultiArrayList(Block),

// not populated during parsing:
locals: std.StringHashMapUnmanaged(symbols.SymbolTarget),
stacks: std.StringHashMapUnmanaged(Block.Handle),

pub const Handle = u32;

pub fn parse(gpa: std.mem.Allocator, handle: Handle, name: []const u8, source: []const u8, errors: *std.ArrayListUnmanaged(Error)) SourceFile {
    const tokens = lex.lex(gpa, source);
    errdefer tokens.deinit(gpa);

    var file = SourceFile{
        .handle = handle,
        .name = name,
        .source = source,
        .tokens = tokens,
        .instructions = .{},
        .expressions = .{},
        .blocks = .{},
        .locals = .{},
        .stacks = .{},
    };

    var temp = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer temp.deinit();

    var p = Parser.init(temp.allocator(), gpa, handle, &file, errors);
    while (p.parseInstruction()) {}

    const operations = file.instructions.items(.operation);
    var block_begin: Instruction.Handle = 0;
    for (operations, 0..) |op, insn_handle| switch (op) {
        .section, .boot, .code, .kcode, .entry, .kentry,
        .data, .kdata, .@"const", .kconst, .stack,
        => {
            const new_block_begin = backtrackLabels(operations, @intCast(Instruction.Handle, insn_handle));
            file.tryAddBlock(gpa, block_begin, new_block_begin);
            block_begin = new_block_begin;
        },
        .none, .nil, .org, .@"align", .keep,
        .local, .insn, .bound_insn,
        .db, .dw, .dd, .zb, .zw, .zd,
        .push, .pop, .range, .def, .undef,
        => {},
    };

    file.tryAddBlock(gpa, block_begin, @intCast(Instruction.Handle, operations.len));

    return file;
}

pub fn deinit(self: SourceFile, gpa: std.mem.Allocator, maybe_arena: ?std.mem.Allocator) void {
    for (self.blocks.items(.labels)) |*map| {
        map.deinit(gpa);
    }
    self.tokens.deinit(gpa);
    self.instructions.deinit(gpa);
    self.expressions.deinit(gpa);
    self.blocks.deinit(gpa);
    self.locals.deinit(gpa);
    self.stacks.deinit(gpa);
    if (maybe_arena) |arena| {
        arena.free(self.name);
        arena.free(self.source);
    }
}

pub const Block = struct {
    first_token: lex.Token.Handle,
    first_insn: Instruction.Handle,
    end_insn: Instruction.Handle,

    // not populated during parsing:
    block_type: ?Instruction.OperationType = null,
    section: ?Section.Handle = null,
    keep: bool = false,
    labels: std.StringHashMapUnmanaged(Instruction.Handle) = .{}, // only private and stack labels; not public labels

    pub const Handle = u32;
};
fn tryAddBlock(self: *SourceFile, gpa: std.mem.Allocator, first_insn: Instruction.Handle, end_insn: Instruction.Handle) void {
    if (first_insn >= end_insn) return;

    var first_token = self.instructions.items(.token)[first_insn];
    if (self.instructions.items(.label)[first_insn]) |label_expr| {
        first_token = self.expressions.items(.token)[label_expr];
    }

    self.blocks.append(gpa, .{
        .first_token = first_token,
        .first_insn = first_insn,
        .end_insn = end_insn,
    }) catch @panic("OOM");
}

pub const Slices = struct {
    file: *SourceFile,
    insn: std.MultiArrayList(Instruction).Slice,
    expr: std.MultiArrayList(Expression).Slice,
    block: std.MultiArrayList(Block).Slice,

    pub fn blockInstructions(self: Slices, block_handle: Block.Handle) Instruction.Iterator {
        return .{
            .begin = self.block.items(.first_insn)[block_handle],
            .end = self.block.items(.end_insn)[block_handle],
        };
    }
};
pub fn slices(self: *SourceFile) Slices {
    return .{
        .file = self,
        .insn = self.instructions.slice(),
        .expr = self.expressions.slice(),
        .block = self.blocks.slice(),
    };
}

pub const Chunk = struct {
    section: ?Section.Handle,
    file: SourceFile.Handle,
    instructions: Instruction.Iterator,

    pub fn getAddressRange(self: Chunk, a: *const Assembler) Assembler.AddressRange {
        const file = &a.files.items[self.file];
        const addresses = file.instructions.items(.address);
        const lengths = file.instructions.items(.length);

        const begin = addresses[self.instructions.begin];
        const end = @as(usize, addresses[self.instructions.end - 1]) + lengths[self.instructions.end - 1];

        return .{
            .first = begin,
            .len = end - begin,
        };
    }

};
pub const ChunkPair = struct {
    a: Chunk,
    b: Chunk,

    pub fn init(a: Chunk, b: Chunk) ChunkPair {
        var self: ChunkPair = undefined;
        if (a.file < b.file or a.file == b.file and a.instructions.begin < b.instructions.begin) {
            self.a = a;
            self.b = b;
        } else {
            self.a = b;
            self.b = a;
        }
        return self;
    }
};

pub fn collectChunks(
    self: *const SourceFile,
    a: *Assembler,
    fixed_org: *std.ArrayListUnmanaged(Chunk),
    auto_org: *std.ArrayListUnmanaged(Chunk),
) void {
    const ChunkType = enum {
        data, code,
    };

    const State = struct {
        file: *const SourceFile,
        a: *Assembler,
        fixed_org_chunks: *std.ArrayListUnmanaged(Chunk),
        auto_org_chunks: *std.ArrayListUnmanaged(Chunk),

        section_handle: ?Section.Handle = null,

        is_fixed: bool = false,
        chunk_type: ?ChunkType = null,
        chunk_begin: Instruction.Handle = undefined,

        fn tryAddChunk(state: *@This(), chunk_end: Instruction.Handle, ended_by_unconditional_control_flow: bool) void {
            const is_fixed = state.is_fixed or state.section_handle == null;
            const dest = if (is_fixed) state.fixed_org_chunks else state.auto_org_chunks;
            if (chunk_end > state.chunk_begin) {
                dest.append(state.a.gpa, .{
                    .section = state.section_handle,
                    .file = state.file.handle,
                    .instructions = .{
                        .begin = state.chunk_begin,
                        .end = chunk_end,
                    },
                }) catch @panic("OOM");

                if (state.chunk_type) |t| if (t == .code and !ended_by_unconditional_control_flow) {
                    state.a.recordInsnError(state.file.handle, chunk_end - 1, "Expected unconditional control flow to terminate this chunk", .{});
                };
            }

            state.is_fixed = false;
            state.chunk_type = null;
            state.chunk_begin = chunk_end;
        }

        fn checkChunkType(state: *@This(), new_type: ChunkType, insn: Instruction.Handle) void {
            if (state.chunk_type) |t| {
                if (t != new_type) {
                    state.tryAddChunk(insn, false);
                    state.chunk_type = new_type;
                }
            } else {
                state.chunk_type = new_type;
            }
        }
    };

    var state = State{
        .file = self,
        .a = a,
        .fixed_org_chunks = fixed_org,
        .auto_org_chunks = auto_org,
    };

    const s = self.blocks.slice();
    const operations = self.instructions.items(.operation);
    for (s.items(.keep), s.items(.section), s.items(.first_insn), s.items(.end_insn)) |keep, section, begin, end| {
        if (!keep) continue;

        state.section_handle = section;

        var block_iter = Instruction.Iterator{
            .begin = begin,
            .end = end,
        };

        state.chunk_begin = begin;
        while (block_iter.next()) |insn_handle| {
            switch (operations[insn_handle]) {
                .org => {
                    const new_chunk_begin = backtrackOrgHeaders(operations, insn_handle);
                    state.tryAddChunk(new_chunk_begin, false);
                    state.is_fixed = true;
                },
                .insn => |i| {
                    state.checkChunkType(.code, insn_handle);
                    if (isa.getBranchKind(i.mnemonic, i.suffix) == .unconditional) {
                        state.tryAddChunk(insn_handle + 1, true);
                    }
                },
                .bound_insn => unreachable, // instructions should never be bound before we've collected chunks
                .push, .pop => {
                    state.checkChunkType(.code, insn_handle);
                },
                .db, .dw, .dd, .zb, .zw, .zd => {
                    state.checkChunkType(.data, insn_handle);
                },

                .none, .nil, .@"align", .keep, .def, .undef, .local, .range,
                .section, .boot, .code, .kcode, .entry, .kentry, .data, .kdata, .@"const", .kconst, .stack
                => {},
            }
        }

        if (state.chunk_begin < end) {
            state.tryAddChunk(end, false);
        }
    }
}

// Note this may not work correctly for a needle token in a label, which preceeds the main Instruction.token unless there is only a label on that line.
// It's meant to be used when you know the token is from Instruction.token or an Expression.token being used as a parameter.
// It will only work for tokens within a label expression if the operation for that line is `.none`
pub fn findInstructionByToken(self: *const SourceFile, token_handle: lex.Token.Handle) Instruction.Handle {
    var haystack = self.instructions.items(.token);
    var base: usize = 0;

    while (haystack.len > 8) {
        const mid = haystack.len / 2;
        if (haystack[mid] <= token_handle) {
            base += mid;
            haystack = haystack[mid..];
        } else {
            haystack = haystack[0..mid];
        }
    }

    var insn_index: ?usize = null;
    for (haystack, 0..) |insn_token_handle, offset| {
        if (insn_token_handle <= token_handle) {
            insn_index = base + offset;
        } else break;
    }
    return @intCast(Instruction.Handle, insn_index orelse unreachable);
}

// Note this may not work correctly for a needle expression in a label, which preceeds the main Instruction.token.
// It's meant to be used when you know the expression is from an instruction's parameter or symbol definition expression.
pub fn findInstructionByExpr(self: *const SourceFile, expr_handle: Expression.Handle) Instruction.Handle {
    const token_handle = self.expressions.items(.token)[expr_handle];
    return self.findInstructionByToken(token_handle);
}

pub fn findBlockByToken(self: *const SourceFile, token_handle: lex.Token.Handle) Block.Handle {
    var haystack = self.blocks.items(.first_token);
    var base: usize = 0;

    while (haystack.len > 8) {
        const mid = haystack.len / 2;
        if (haystack[mid] <= token_handle) {
            base += mid;
            haystack = haystack[mid..];
        } else {
            haystack = haystack[0..mid];
        }
    }

    var block_index: ?usize = null;
    for (haystack, 0..) |haystack_token, offset| {
        if (haystack_token <= token_handle) {
            block_index = base + offset;
        } else break;
    }

    return @intCast(Block.Handle, block_index orelse unreachable);
}

pub fn findBlockByInstruction(self: *const SourceFile, insn_handle: Instruction.Handle) Block.Handle {
    var haystack = self.blocks.items(.first_insn);
    var base: usize = 0;

    while (haystack.len > 8) {
        const mid = haystack.len / 2;
        if (haystack[mid] <= insn_handle) {
            base += mid;
            haystack = haystack[mid..];
        } else {
            haystack = haystack[0..mid];
        }
    }

    var block_index: ?usize = null;
    for (haystack, 0..) |haystack_insn, offset| {
        if (haystack_insn <= insn_handle) {
            block_index = base + offset;
        } else break;
    }

    return @intCast(Block.Handle, block_index orelse unreachable);
}

fn backtrackLabels(operations: []const Instruction.Operation, handle: Instruction.Handle) Instruction.Handle {
    var result = handle;
    while (result > 0 and operations[result - 1] == .none) {
        result -= 1;
    }
    return result;
}

fn backtrackOrgHeaders(operations: []const Instruction.Operation, handle: Instruction.Handle) Instruction.Handle {
    var result = handle;
    while (result > 0 and Instruction.isOrgHeader(operations[result - 1])) {
        result -= 1;
    }
    return result;
}

test "Parser" {
    const src =
        \\label:
        \\   nop //comment
        \\   sync
        \\   fret
        \\
        \\asdf: PARK
    ;
    var token_list = lex.lex(std.testing.allocator, src);
    defer token_list.deinit(std.testing.allocator);
    var results = parse(std.testing.allocator, src, token_list);
    defer results.deinit(std.testing.allocator);
    try std.testing.expectEqual(@as(usize, 0), results.errors.items.len);
    try std.testing.expectEqual(@as(usize, 5), results.instructions.items.len);

    var insn = results.instructions.items[0];
    try std.testing.expect(insn.label != null);
    try std.testing.expectEqualStrings("label", insn.label.?);
    try std.testing.expectEqual(Mnemonic._reserved, insn.mnemonic);

    insn = results.instructions.items[1];
    try std.testing.expect(insn.label == null);
    try std.testing.expectEqual(Mnemonic.NOP, insn.mnemonic);

    insn = results.instructions.items[2];
    try std.testing.expect(insn.label == null);
    try std.testing.expectEqual(Mnemonic.SYNC, insn.mnemonic);

    insn = results.instructions.items[3];
    try std.testing.expect(insn.label == null);
    try std.testing.expectEqual(Mnemonic.FRET, insn.mnemonic);

    insn = results.instructions.items[4];
    try std.testing.expect(insn.label != null);
    try std.testing.expectEqualStrings("asdf", insn.label.?);
    try std.testing.expectEqual(Mnemonic.PARK, insn.mnemonic);
}
