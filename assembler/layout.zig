const std = @import("std");
const ie = @import("instruction_encoding");
const bus = @import("bus_types");
const lex = @import("lex.zig");
const Assembler = @import("Assembler.zig");
const SourceFile = @import("SourceFile.zig");
const Section = @import("Section.zig");
const Constant = @import("Constant.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const PageData = @import("PageData.zig");

pub fn resetLayoutDependentExpressions(a: *Assembler) void {
    // std.debug.print("Resetting Layout\n", .{});
    for (a.files.items) |*file| {
        var expr_resolved_constants = file.expressions.items(.resolved_constant);
        for (file.expressions.items(.flags), 0..) |flags, expr_handle| {
            if (flags.contains(.constant_depends_on_layout)) {
                expr_resolved_constants[expr_handle] = null;
            }
        }
    }
}

pub fn doFixedOrgLayout(a: *Assembler, chunks: []SourceFile.Chunk) bool {
    var layout_changed = false;
    for (chunks) |chunk| {
        const initial_address = resolveFixedOrgAddress(a, chunk);
        // std.debug.print("{X:0>16}: Placing fixed chunk\n", .{ initial_address });
        if (doChunkLayout(a, chunk, initial_address)) {
            layout_changed = true;
        }
    }
    return layout_changed;
}

fn resolveFixedOrgAddress(a: *Assembler, chunk: SourceFile.Chunk) u32 {
    var file = a.getSource(chunk.file);
    const operations = file.instructions.items(.operation);

    var initial_address: u32 = 0;

    var iter = chunk.instructions;
    while (iter.next()) |insn_handle| {
        if (operations[insn_handle] == .org) {
            initial_address = file.instructions.items(.address)[insn_handle];
            if (file.instructions.items(.params)[insn_handle]) |expr_handle| {
                var constant = resolveExpressionConstant(a, file, chunk.file, initial_address, expr_handle);
                initial_address = std.math.cast(u32, constant.asInt() catch 0) orelse 0;
            }
            break;
        }
    }

    return initial_address;
}

pub fn doAutoOrgLayout(a: *Assembler, chunks: []SourceFile.Chunk) bool {
    var layout_changed = false;
    for (chunks) |chunk| {
        const initial_address = resolveAutoOrgAddress(a, chunk);
        // std.debug.print("{X:0>16}: Placing auto chunk\n", .{ initial_address });
        if (doChunkLayout(a, chunk, initial_address)) {
            layout_changed = true;
        }
    }
    return layout_changed;
}

fn resolveAutoOrgAddress(a: *Assembler, chunk: SourceFile.Chunk) u32 {
    // TODO add special logic for .kentry sections:
    //      Public labels must be at the start of a chunk (error otherwise).
    //      Chunks with a public label are allocated to pages where that label can be called from user code.
    //      Chunks without a public label are allocated to fully protected pages (i.e. same as kcode) or in areas of kentry pages where they don't overlap the entry addresses.
    //      So there's really 2 different kinds of pages for each named .kentry section
    // TODO add a kentry output mode to write a "header" file that exports the addresses of 

    const chunk_section_handle = chunk.section orelse return 0;

    var file = a.getSource(chunk.file);
    const insn_addresses = file.instructions.items(.address);
    const insn_lengths = file.instructions.items(.length);

    const first_address = insn_addresses[chunk.instructions.begin];
    const end_address = insn_addresses[chunk.instructions.end - 1] + insn_lengths[chunk.instructions.end - 1];
    const chunk_size = @max(1, end_address - first_address);

    const pages = a.pages.items(.page);
    const page_sections = a.pages.items(.section);
    const page_usages = a.pages.items(.usage);

    if (chunk_size < PageData.page_size) {
        // See if there's an existing semi-full chunk that we can reuse
        // TODO it might make sense to add an acceleration structure for this inside Section
        //      e.g. list of non-full pages for that section, sorted by size of largest free span.
        //      But need to ensure that the upkeep cost doesn't exceed the gains here.
        for (0.., page_sections) |page_data_handle, section_handle| {
            if (section_handle == chunk_section_handle) {
                // std.debug.print("Checking page data handle {}\n", .{ page_data_handle });
                var used = page_usages[page_data_handle];
                var unused = used;
                unused.toggleAll();

                var begin: usize = 0;
                while (unused.findFirstSet()) |unused_range_begin| {
                    used.setRangeValue(.{
                        .start = begin,
                        .end = unused_range_begin,
                    }, false);

                    const unused_range_end = @intCast(bus.Page, used.findFirstSet() orelse PageData.page_size);
                    const range_size = unused_range_end - unused_range_begin;
                    if (range_size >= chunk_size) {
                        const page = pages[page_data_handle];
                        // std.debug.print("Using existing page\n", .{ });
                        return (@as(u32, page) << @bitSizeOf(bus.PageOffset)) | @intCast(bus.PageOffset, unused_range_begin);
                    }

                    unused.setRangeValue(.{
                        .start = begin,
                        .end = unused_range_end,
                    }, false);
                    begin = unused_range_end;
                }
            }
        }
    }

    // Start on a brand new page (with free pages following if necessary)
    const full_pages_needed = chunk_size / PageData.page_size;
    const final_page_bytes_needed = chunk_size - (full_pages_needed * PageData.page_size);

    // std.debug.print("Need {} full pages and {} extra bytes\n", .{ full_pages_needed, final_page_bytes_needed });

    // TODO allow defining this range per named section using a directive (also use the first page there for fixed org chunks that can't resolve their .org address)
    const search_range_begin: usize = 1;
    const search_range_end: usize = PageData.num_pages;

    var initial_page = search_range_begin;
    for (search_range_begin..search_range_end) |page_usize| {
        const page = @intCast(bus.Page, page_usize);
        if (page_usize - initial_page >= full_pages_needed) {
            if (final_page_bytes_needed == 0) {
                break;
            } else if (a.page_lookup.get(page)) |page_data_handle| {
                if (page_sections[page_data_handle] == chunk_section_handle) {
                    const final_page_free_bytes = page_usages[page_data_handle].findFirstSet() orelse PageData.page_size;
                    if (final_page_free_bytes >= final_page_bytes_needed) {
                        break;
                    }
                }
            } else {
                // final page is completely unused, so we're good to go
                break;
            }
        }

        if (a.page_lookup.get(page)) |_| {
            // page is already in use; reset counter
            initial_page = page_usize + 1;
        }
    }

    return @intCast(u32, initial_page) << @bitSizeOf(bus.PageOffset);
}

fn doChunkLayout(a: *Assembler, chunk: SourceFile.Chunk, initial_address: u32) bool {
    var file = a.getSource(chunk.file);
    const operations = file.instructions.items(.operation);
    const params = file.instructions.items(.params);
    const insn_flags = file.instructions.items(.flags);
    const insn_addresses = file.instructions.items(.address);
    const insn_lengths = file.instructions.items(.length);

    var layout_changed = false;

    var address = initial_address;
    var iter = chunk.instructions;
    while (iter.next()) |insn_handle| {
        const old_insn_address = insn_addresses[insn_handle];
        var new_insn_address = address;
        switch (operations[insn_handle]) {
            .none, .org, .keep, .def, .undef, .section, .code, .kcode, .entry, .kentry, .data, .kdata, .@"const", .kconst, .stack => {
                // TODO need to look forwards to see if an .align, etc. is coming up that would require changing address now
            },
            .push => {
                // TODO synthesize push instruction
            },
            .pop => {
                // TODO synthesize pop instruction
            },
            .insn => {
                const length = resolveInstructionEncoding(a, file, chunk.file, address, insn_handle, &operations[insn_handle], params[insn_handle]);
                insn_lengths[insn_handle] = length;
                address += length;
                layout_changed = true;
            },
            .bound_insn => |encoding| {
                if (insn_flags[insn_handle].contains(.encoding_depends_on_layout)) {
                    const old_encoding = encoding;
                    operations[insn_handle] = .{ .insn = .{
                        .mnemonic = old_encoding.mnemonic,
                        .suffix = old_encoding.suffix,
                    }};
                    const length = resolveInstructionEncoding(a, file, chunk.file, address, insn_handle, &operations[insn_handle], params[insn_handle]);
                    address += length;
                    switch (operations[insn_handle]) {
                        .bound_insn => |new_encoding| if (!ie.eql(old_encoding.*, new_encoding.*)) {
                            insn_lengths[insn_handle] = length;
                            layout_changed = true;
                        },
                        else => {},
                    }
                } else {
                    address += insn_lengths[insn_handle];
                }
            },
            .@"align" => {
                // TODO handle alignment
            },
            .db => {

            },
            .dw => {
                // TODO handle alignment
            },
            .dd => {
                // TODO handle alignment
            },
        }

        if (new_insn_address != old_insn_address) {
            insn_addresses[insn_handle] = new_insn_address;
            layout_changed = true;
        }
    }

    if (chunk.section) |section_handle| {
        const initial_page = initial_address >> @bitSizeOf(bus.PageOffset);
        const final_page = (address - 1) >> @bitSizeOf(bus.PageOffset);

        for (initial_page .. final_page + 1) |page| {
            const page_data_handle = a.findOrCreatePage(@intCast(bus.Page, page), section_handle);
            var page_usage = a.pages.items(.usage);
            if (page == initial_page or page == final_page) {
                const range = std.bit_set.Range{
                    .start = if (page == initial_page) @truncate(bus.PageOffset, initial_address) else 0,
                    .end = if (page == final_page) @truncate(bus.PageOffset, address) else PageData.page_size,
                };
                page_usage[page_data_handle].setRangeValue(range, true);
                // std.debug.print("{X:0>13}: ({}) Marking page used: {} - {}\n", .{ page, page_data_handle, range.start, range.end });
            } else {
                // std.debug.print("{X:0>13}: ({}) Marking page fully used\n", .{ page, page_data_handle });
                page_usage[page_data_handle] = PageData.UsageBitmap.initFull();
            }
        }
    }

    return layout_changed;
}

fn resolveExpressionConstant(a: *Assembler, file: *SourceFile, file_handle: SourceFile.Handle, ip: u32, expr_handle: Expression.Handle) *const Constant {
    var expr_infos = file.expressions.items(.info);
    var expr_tokens = file.expressions.items(.token);
    var expr_resolved_constants = file.expressions.items(.resolved_constant);
    var expr_resolved_types = file.expressions.items(.resolved_type);

    if (expr_resolved_constants[expr_handle]) |constant| {
        return constant;
    }

    const info = expr_infos[expr_handle];
    switch (info) {
        .literal_symbol_def, .directive_symbol_def,
        .literal_int, .literal_str,
        => unreachable, // These should be guaranteed to already have resolved_constant set

        .list, .arrow_list, .literal_reg => {
            // These don't have a meaningful value
            // but in the case of errors, we might end up trying to look for it anyway,
            // so we'll set it to zero:
            expr_resolved_constants[expr_handle] = &Constant.builtin.zero;
        },

        .literal_symbol_ref => {
            const token_handle = expr_tokens[expr_handle];
            const raw_symbol = file.tokens.get(token_handle).location(file.source);
            const symbol_constant = Constant.initSymbolLiteral(a.gpa, &a.constant_temp, raw_symbol);
            resolveSymbolRefExprConstant(a, file, file_handle, ip, expr_handle, token_handle, symbol_constant, expr_resolved_types, expr_resolved_constants);
        },
        .directive_symbol_ref => |inner_expr| {
            const token_handle = expr_tokens[inner_expr];
            const symbol_constant = expr_resolved_constants[inner_expr].?;
            resolveSymbolRefExprConstant(a, file, file_handle, ip, expr_handle, token_handle, symbol_constant.*, expr_resolved_types, expr_resolved_constants);
        },

        .negate => |inner_expr| {
            const inner_constant = resolveExpressionConstant(a, file, file_handle, ip, inner_expr);
            const value = std.math.negateCast(inner_constant.asInt() catch 0) catch 0;
            const new_constant = Constant.initInt(value, null);
            expr_resolved_constants[expr_handle] = new_constant.intern(a.arena, a.gpa, &a.constants);
        },
        .plus, .minus, .multiply, .shl, .shr => |bin| {
            const left = resolveExpressionConstant(a, file, file_handle, ip, bin.left);
            const right = resolveExpressionConstant(a, file, file_handle, ip, bin.right);
            const lv = left.asInt() catch 0;
            const rv = right.asInt() catch 0;
            const value = switch (info) {
                .plus => lv +% rv,
                .minus => lv -% rv,
                .multiply => lv *% rv,
                .shl => lv << @truncate(u6, @bitCast(u64, rv)),
                .shr => lv >> @truncate(u6, @bitCast(u64, rv)),
                else => unreachable,
            };
            const new_constant = Constant.initInt(value, null);
            expr_resolved_constants[expr_handle] = new_constant.intern(a.arena, a.gpa, &a.constants);
        },
    }

    return expr_resolved_constants[expr_handle].?;
}

fn resolveSymbolRefExprConstant(
    a: *Assembler,
    file: *SourceFile,
    file_handle: SourceFile.Handle,
    ip: u32,
    expr_handle: Expression.Handle,
    symbol_token_handle: lex.Token.Handle,
    symbol_constant: Constant,
    expr_resolved_types: []const ie.ExpressionType,
    expr_resolved_constants: []?*const Constant,
) void {
    if (a.lookupSymbol(file, file_handle, symbol_token_handle, symbol_constant.asString())) |target| {
        switch (target) {
            .expression => |target_expr_handle| {
                expr_resolved_constants[expr_handle] = resolveExpressionConstant(a, file, file_handle, ip, target_expr_handle);
            },
            .instruction => |target_insn_ref| {
                const target_file = a.getSource(target_insn_ref.file);
                var value: i64 = target_file.instructions.items(.address)[target_insn_ref.instruction];
                switch (expr_resolved_types[expr_handle]) {
                    .raw_base_offset, .data_address, .insn_address, .stack_address => |info| {
                        if (std.meta.eql(info.base, .{ .sr = .ip })) {
                            value -= ip;
                        }
                    },
                    else => {},
                }
                const constant = Constant.initInt(value, null);
                expr_resolved_constants[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
            },
            .not_found => {
                expr_resolved_constants[expr_handle] = &Constant.builtin.zero;
            },
        }
    } else unreachable;
}

fn resolveInstructionEncoding(a: *Assembler, file: *SourceFile, file_handle: SourceFile.Handle, ip: u32, insn_handle: Instruction.Handle, op: *Instruction.Operation, params: ?Expression.Handle) u7 {
    const expr_infos = file.expressions.items(.info);
    const expr_resolved_types = file.expressions.items(.resolved_type);

    a.params_temp.clearRetainingCapacity();
    if (params) |expr_handle| {
        buildInstructionParameters(a, file, file_handle, ip, expr_handle, false, expr_infos, expr_resolved_types) catch return 0;
    }

    const what = op.insn;
    var encoding_iter = a.edb.getMatchingEncodings(.{
        .mnemonic = what.mnemonic,
        .suffix = what.suffix,
        .params = a.params_temp.items,
    });

    a.params_temp.clearRetainingCapacity();

    var best_length: ?u7 = null;
    var best_encoding: ?*const ie.InstructionEncoding = null;
    while (encoding_iter.nextPointer()) |enc| {
        const length = ie.getInstructionLength(enc.*);
        if (best_length) |cur_length| {
            if (length < cur_length) {
                best_encoding = enc;
                best_length = length;
            }
        } else {
            best_encoding = enc;
            best_length = length;
        }
    }
    if (best_encoding) |enc| {
        op.* = .{ .bound_insn = enc };
        return best_length.?;
    }

    const insn_token = file.instructions.items(.token)[insn_handle];
    a.recordError(file_handle, insn_token, "No instruction encodings match instruction");
    // TODO suggest valid encodings
    a.invalid_program = true;
    return 0;
}

fn buildInstructionParameters(
    a: *Assembler,
    file: *SourceFile,
    file_handle: SourceFile.Handle,
    ip: u32,
    params: Expression.Handle,
    is_arrow: bool,
    expr_infos: []const Expression.Info,
    expr_resolved_types: []const ie.ExpressionType,
) !void {
    // TODO want a way to have is_arrow on the first parameter?
    switch (expr_infos[params]) {
        .list => |bin| {
            try buildInstructionParameters(a, file, file_handle, ip, bin.left, is_arrow, expr_infos, expr_resolved_types);
            try buildInstructionParameters(a, file, file_handle, ip, bin.right, false, expr_infos, expr_resolved_types);
            return;
        },
        .arrow_list => |bin| {
            try buildInstructionParameters(a, file, file_handle, ip, bin.left, is_arrow, expr_infos, expr_resolved_types);
            try buildInstructionParameters(a, file, file_handle, ip, bin.right, true, expr_infos, expr_resolved_types);
            return;
        },
        else => {}
    }

    const expr_type = expr_resolved_types[params];
    if (expr_type == .poison) {
        return error.Poisoned;
    }

    const constant = resolveExpressionConstant(a, file, file_handle, ip, params);

    a.params_temp.append(a.gpa, .{
        .arrow = is_arrow,
        .expr_type = expr_type,
        .constant = constant.asInt() catch 0,
    }) catch @panic("OOM");
}