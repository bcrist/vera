const std = @import("std");
const bits = @import("bits");
const ie = @import("isa_encoding");
const bus = @import("bus_types");
const lex = @import("lex.zig");
const symbols = @import("symbols.zig");
const Assembler = @import("Assembler.zig");
const SourceFile = @import("SourceFile.zig");
const Section = @import("Section.zig");
const Constant = @import("Constant.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const PageData = @import("PageData.zig");
const Error = @import("Error.zig");
const InsnEncodingError = @import("InsnEncodingError.zig");

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
    var initial_address: u32 = 0;
    if (chunk.section) |section_handle| {
        const range = a.getSection(section_handle).getRange();
        initial_address = range.first;

        var file = a.getSource(chunk.file);
        const s = file.slices();
        const operations = s.insn.items(.operation);
        var iter = chunk.instructions;
        while (iter.next()) |insn_handle| {
            if (operations[insn_handle] == .org) {
                initial_address = s.insn.items(.address)[insn_handle];
                if (s.insn.items(.params)[insn_handle]) |expr_handle| {
                    if (resolveExpressionConstant(a, s, initial_address, expr_handle)) |constant| {
                        initial_address = constant.asInt(u32) catch {
                            a.recordExpressionLayoutError(chunk.file, expr_handle, expr_handle, ".org address too large", .{});
                            break;
                        };
                    }
                }
                break;
            }
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
    const chunk_section_handle = chunk.section orelse return 0;
    const section = a.getSection(chunk_section_handle);

    const address_range = chunk.getAddressRange(a);
    const chunk_size = @max(1, address_range.len);

    const desired_access = section.kind.accessPolicies(chunk_size);

    var alignment = getAlignmentForChunk(a, chunk);
    if (desired_access.execute) |access| switch (access) {
        .kernel_entry_256 => if (alignment.modulo < 256) {
            alignment.modulo = 256;
            alignment.offset = 0;
        },
        .kernel_entry_4096 => if (alignment.modulo < 4096) {
            alignment.modulo = 4096;
            alignment.offset = 0;
        },
        else => {},
    };

    const page_data_slice = a.pages.slice();
    const pages = page_data_slice.items(.page);
    const page_sections = page_data_slice.items(.section);
    const page_access = page_data_slice.items(.access);
    const page_usages = page_data_slice.items(.usage);

    if (chunk_size < PageData.page_size) {
        // See if there's an existing semi-full chunk that we can reuse
        // TODO it might make sense to add an acceleration structure for this inside Section
        //      e.g. list of non-full pages for that section, sorted by size of largest free span (or maybe last used?).
        //      But need to ensure that the upkeep cost doesn't exceed the gains here.
        for (0.., page_sections) |page_data_handle, section_handle| {
            if (section_handle != chunk_section_handle) {
                continue;
            }
            if (!std.meta.eql(page_access[page_data_handle], desired_access)) {
                continue;
            }

            const page = pages[page_data_handle];

            // std.debug.print("Checking page data handle {}\n", .{ page_data_handle });
            var used = page_usages[page_data_handle];
            var unused = used;
            unused.toggleAll();

            var begin: usize = 0;
            while (unused.findFirstSet()) |raw_unused_range_begin| {
                const address = bits.concat2(@intCast(bus.PageOffset, raw_unused_range_begin), page);
                const aligned_address = applyAlignment(address, alignment.modulo, alignment.offset);

                if (@intCast(bus.Page, aligned_address >> @bitSizeOf(bus.PageOffset)) != page) break;
                const unused_range_begin = @truncate(bus.PageOffset, aligned_address);

                used.setRangeValue(.{
                    .start = begin,
                    .end = unused_range_begin,
                }, false);

                const unused_range_end = used.findFirstSet() orelse PageData.page_size;
                const range_size = unused_range_end - unused_range_begin;
                if (range_size >= chunk_size) {
                    // std.debug.print("Using existing page\n", .{ });
                    return aligned_address;
                }

                unused.setRangeValue(.{
                    .start = begin,
                    .end = unused_range_end,
                }, false);

                begin = unused_range_end;
            }
        }
    }

    // Start on a brand new page (with free pages following if necessary)
    const full_pages_needed = chunk_size / PageData.page_size;
    const final_page_bytes_needed = chunk_size - (full_pages_needed * PageData.page_size);

    // std.debug.print("Need {} full pages and {} extra bytes\n", .{ full_pages_needed, final_page_bytes_needed });

    const allowed_range = a.getSection(chunk_section_handle).getRange();

    const search_range_begin: usize = allowed_range.first >> @bitSizeOf(bus.PageOffset);
    const search_range_end: usize = (allowed_range.first + allowed_range.len) >> @bitSizeOf(bus.PageOffset);

    var initial_page = search_range_begin;
    for (search_range_begin..search_range_end) |page_usize| {
        const page = @intCast(bus.Page, page_usize);
        if (page_usize - initial_page >= full_pages_needed) {
            if (final_page_bytes_needed == 0) {
                break;
            } else if (a.page_lookup.get(page)) |page_data_handle| {
                // Currently we never accept a partially filled last page for .entry_kernel sections since the
                // access policies might be wrong.  Theoretically this may result in slightly more fragmentation in
                // corner cases, but it's not worth the extra complexity to handle it IMO.
                if (section.kind != .entry_kernel and page_sections[page_data_handle] == chunk_section_handle) {
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

fn getAlignmentForChunk(a: *Assembler, chunk: SourceFile.Chunk) Assembler.Alignment {
    var file = a.getSource(chunk.file);
    const s = file.slices();
    const insn_operations = s.insn.items(.operation);
    const insn_params = s.insn.items(.params);

    var alignment = Assembler.Alignment{
        .modulo = 1,
        .offset = 0,
    };

    var iter = chunk.instructions;
    while (iter.next()) |insn_handle| {
        switch (insn_operations[insn_handle]) {
            .none, .org, .keep, .def, .undef, .local, .range,
            .section, .boot, .code, .kcode, .entry, .kentry,
            .data, .kdata, .@"const", .kconst, .stack,
            => {},
            .@"align" => if (getResolvedAlignment(s, insn_params[insn_handle])) |resolved_align| {
                alignment = resolved_align;
            },
            .db, .push, .pop, .insn, .bound_insn => break,
            .dw, .dd => {
                if ((alignment.modulo & 1) != 0) {
                    alignment.modulo *= 2;
                }
                if ((alignment.offset & 1) != 0) {
                    alignment.offset = (alignment.offset + 1) % alignment.modulo;
                }
                break;
            },
        }
    }
    return alignment;
}

fn doChunkLayout(a: *Assembler, chunk: SourceFile.Chunk, initial_address: u32) bool {
    var file = a.getSource(chunk.file);
    const s = file.slices();
    const insn_operations = s.insn.items(.operation);
    const insn_addresses = s.insn.items(.address);
    const insn_params = s.insn.items(.params);
    const insn_lengths = s.insn.items(.length);
    const insn_flags = s.insn.items(.flags);

    var layout_changed = false;

    var address = initial_address;
    var iter = chunk.instructions;
    var check_for_alignment_holes = false;
    while (iter.next()) |insn_handle| {
        const old_insn_address = insn_addresses[insn_handle];
        var new_insn_address = address;
        switch (insn_operations[insn_handle]) {
            .none, .org, .keep, .def, .undef, .local, .range,
            .section, .boot, .code, .kcode, .entry, .kentry,
            .data, .kdata, .@"const", .kconst, .stack,
            .@"align" => {
                // Look forwards to see if another .align, etc. is coming up
                var iter2 = chunk.instructions;
                iter2.begin = insn_handle;
                var first = true;
                while (iter2.next()) |insn2| {
                    if (first) {
                        first = false;
                    }
                    switch (insn_operations[insn2]) {
                        .none, .org, .keep, .def, .undef, .local, .range,
                        .section, .boot, .code, .kcode, .entry, .kentry,
                        .data, .kdata, .@"const", .kconst, .stack => {},

                        .@"align" => {
                            if (insn_params[insn2]) |align_expr| {
                                address = resolveAndApplyAlignment(a, s, address, align_expr, first, check_for_alignment_holes, insn2, chunk.section);
                            }
                        },

                        .dw, .dd => {
                            address = applyAlignment(address, 2, 0);
                            break;
                        },

                        .insn, .bound_insn, .push, .pop, .db => break,
                    }
                }
            },
            .insn => {
                const length = resolveInstructionEncoding(a, s, address, insn_handle, &insn_operations[insn_handle], insn_params[insn_handle]);
                insn_lengths[insn_handle] = length;
                address += length;
                layout_changed = true;
                check_for_alignment_holes = true;
            },
            .bound_insn => |encoding| {
                if (insn_flags[insn_handle].contains(.depends_on_layout)) {
                    const old_encoding = encoding;
                    insn_operations[insn_handle] = .{ .insn = .{
                        .mnemonic = old_encoding.mnemonic,
                        .suffix = old_encoding.suffix,
                    }};
                    const length = resolveInstructionEncoding(a, s, address, insn_handle, &insn_operations[insn_handle], insn_params[insn_handle]);
                    address += length;
                    switch (insn_operations[insn_handle]) {
                        .bound_insn => |new_encoding| if (!old_encoding.eqlIncludingOpcodes(new_encoding.*)) {
                            insn_lengths[insn_handle] = length;
                            layout_changed = true;
                        },
                        else => {
                            layout_changed = true;
                        },
                    }
                } else {
                    address += insn_lengths[insn_handle];
                }
                check_for_alignment_holes = true;
            },
            .db => {
                address += resolveDataDirectiveLength(a, s, address, insn_handle, 1);
                check_for_alignment_holes = true;
            },
            .dw => {
                address = checkAndApplyAlignment(a, s, address, 2, 0, check_for_alignment_holes, insn_handle, chunk.section);
                address += resolveDataDirectiveLength(a, s, address, insn_handle, 2);
                check_for_alignment_holes = true;
            },
            .dd => {
                address = checkAndApplyAlignment(a, s, address, 2, 0, check_for_alignment_holes, insn_handle, chunk.section);
                address += resolveDataDirectiveLength(a, s, address, insn_handle, 4);
                check_for_alignment_holes = true;
            },
            .push => {
                address += resolvePushPopDirectiveLength(a, s, insn_handle, .push);
                check_for_alignment_holes = true;
            },
            .pop => {
                address += resolvePushPopDirectiveLength(a, s, insn_handle, .pop);
                check_for_alignment_holes = true;
            },
        }

        if (new_insn_address != old_insn_address) {
            insn_addresses[insn_handle] = new_insn_address;
            layout_changed = true;
        }
    }

    if (chunk.section) |section_handle| {
        const chunk_bytes = address - initial_address;
        const initial_page = initial_address >> @bitSizeOf(bus.PageOffset);
        const final_page = if (address == initial_address) initial_page else (address - 1) >> @bitSizeOf(bus.PageOffset);

        const section = a.getSection(section_handle);
        var access = section.kind.accessPolicies(chunk_bytes);

        for (initial_page .. final_page + 1) |page| {
            const page_data_handle = a.findOrCreatePage(@intCast(bus.Page, page), access, section_handle);
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
            if (page == initial_page and section.kind == .entry_kernel) {
                // only the first page of an entry_kernel chunk should be (partially) callable
                access = Section.Kind.code_kernel.accessPolicies(chunk_bytes);
            }
        }
    }

    return layout_changed;
}

pub fn getResolvedAlignment(s: SourceFile.Slices, maybe_align_expr: ?Expression.Handle) ?Assembler.Alignment {
    const align_expr = maybe_align_expr orelse return null;

    const expr_constants = s.expr.items(.resolved_constant);

    var alignment = Assembler.Alignment{
        .modulo = 0,
        .offset = 0,
    };
    switch (s.expr.items(.info)[align_expr]) {
        .list => |bin| {
            if (expr_constants[bin.left]) |constant| {
                alignment.modulo = constant.asInt(u32) catch return null;
            }
            if (expr_constants[bin.right]) |constant| {
                alignment.offset = constant.asInt(u32) catch return null;
            }
        },
        else => {
            if (expr_constants[align_expr]) |constant| {
                alignment.modulo = constant.asInt(u32) catch return null;
            }
        }
    }

    if (alignment.modulo <= 1 or alignment.offset >= alignment.modulo or @popCount(alignment.modulo) != 1) {
        return null;
    }

    return alignment;
}

fn resolveAndApplyAlignment(
    a: *Assembler,
    s: SourceFile.Slices,
    address: u32,
    align_expr: Expression.Handle,
    report_errors: bool,
    check_for_alignment_holes: bool,
    insn_handle: Instruction.Handle,
    section_handle: ?Section.Handle
) u32 {
    var alignment_expr: Expression.Handle = align_expr;
    var alignment: u32 = 0;
    var offset: u32 = 0;
    switch (s.expr.items(.info)[align_expr]) {
        .list => |bin| {
            alignment_expr = bin.left;
            if (resolveExpressionConstant(a, s, address, bin.left)) |constant| {
                alignment = constant.asInt(u32) catch {
                    if (report_errors) {
                        const token = s.expr.items(.token)[bin.left];
                        const err_flags = Error.FlagSet.initOne(.remove_on_layout_reset);
                        a.recordError(s.file.handle, token, "Overflow (alignment must fit in u32)", err_flags);
                    }
                    return address;
                };
            }
            if (resolveExpressionConstant(a, s, address, bin.right)) |constant| {
                offset = constant.asInt(u32) catch {
                    if (report_errors) {
                        const token = s.expr.items(.token)[bin.right];
                        const err_flags = Error.FlagSet.initOne(.remove_on_layout_reset);
                        a.recordError(s.file.handle, token, "Overflow (offset must fit in u32)", err_flags);
                    }
                    return address;
                };
                if (offset >= alignment or offset < 0) {
                    if (report_errors) {
                        const token = s.expr.items(.token)[bin.right];
                        const err_flags = Error.FlagSet.initOne(.remove_on_layout_reset);
                        a.recordError(s.file.handle, token, "Invalid offset (must be less than alignment)", err_flags);
                    }
                    return address;
                }
            }
        },
        else => {
            if (resolveExpressionConstant(a, s, address, align_expr)) |constant| {
                alignment = constant.asInt(u32) catch {
                    if (report_errors) {
                        const token = s.expr.items(.token)[align_expr];
                        const err_flags = Error.FlagSet.initOne(.remove_on_layout_reset);
                        a.recordError(s.file.handle, token, "Overflow (alignment too must fit in u32)", err_flags);
                    }
                    return address;
                };
            }
        }
    }

    if (alignment <= 1) {
        return address;
    }

    if (@popCount(alignment) != 1) {
        if (report_errors) {
            const token = s.expr.items(.token)[alignment_expr];
            const err_flags = Error.FlagSet.initOne(.remove_on_layout_reset);
            a.recordError(s.file.handle, token, "Invalid alignment (must be power of 2)", err_flags);
        }
        return address;
    }

    if (report_errors) {
        return checkAndApplyAlignment(a, s, address, alignment, @intCast(u32, offset), check_for_alignment_holes, insn_handle, section_handle);
    } else {
        return applyAlignment(address, alignment, @intCast(u32, offset));
    }
}

fn applyAlignment(address: u32, alignment: u32, offset: u32) u32 {
    var new_address_usize = std.mem.alignBackward(address, alignment) + offset;
    if (new_address_usize < address) {
        new_address_usize += alignment;
    }

    return std.math.cast(u32, new_address_usize) orelse address;
}

fn checkAndApplyAlignment(
    a: *Assembler,
    s: SourceFile.Slices,
    address: u32,
    alignment: u32,
    offset: u32,
    check_for_alignment_holes: bool,
    insn_handle: Instruction.Handle,
    section_handle: ?Section.Handle
) u32 {
    var new_address_usize = std.mem.alignBackward(address, alignment) + offset;
    if (new_address_usize < address) {
        new_address_usize += alignment;
    }

    const new_address = std.math.cast(u32, new_address_usize) orelse {
        const token = s.insn.items(.token)[insn_handle];
        const err_flags = Error.FlagSet.initOne(.remove_on_layout_reset);
        a.recordError(s.file.handle, token, "Aligned address overflow", err_flags);
        return address;
    };

    if (new_address != address and check_for_alignment_holes) {
        if (section_handle) |section| switch (a.getSection(section).kind) {
            .info,
            .data_user,
            .data_kernel,
            .constant_user,
            .constant_kernel,
            => {},

            .boot,
            .code_user,
            .code_kernel,
            .entry_user,
            .entry_kernel,
            => {
                const token = s.insn.items(.token)[insn_handle];
                const err_flags = Error.FlagSet.initOne(.remove_on_layout_reset);
                a.recordError(s.file.handle, token, "Alignment not satisfied within instruction stream (try using NOP, NOPE, or an unconditional branch before this point)", err_flags);
            },
        };
    }

    return new_address;
}

fn resolvePushPopDirectiveLength(a: *Assembler, s: SourceFile.Slices, insn_handle: Instruction.Handle, comptime op: Instruction.OperationType) u32 {
    const insn_flags = s.insn.items(.flags);
    const insn_lengths = s.insn.items(.length);

    const flagset = insn_flags[insn_handle];

    if (flagset.contains(.length_computed)) {
        return insn_lengths[insn_handle];
    }

    var stack_size = symbols.computePushOrPopSizeFast(a, s, s.insn.items(.params)[insn_handle]);
    s.insn.items(.operation)[insn_handle] = @unionInit(Instruction.Operation, @tagName(op), stack_size);

    const insn = ie.Instruction{
        .mnemonic = switch (op) {
            .push => .FRAME,
            .pop => .UNFRAME,
            else => unreachable,
        },
        .suffix = .none,
        .params = &.{
            .{
                .expr_type = .{ .constant = {} },
                .constant = stack_size,
            },
        },
    };
    const maybe_encoding = findBestInstructionEncoding(a, insn);

    var length: u32 = 0;
    if (maybe_encoding) |encoding| {
        length = encoding.getInstructionLength();
    } else {
        var flags = Error.FlagSet.initEmpty();
        if (flagset.contains(.depends_on_layout)) {
            flags.insert(.remove_on_layout_reset);
        }
        a.recordError(s.file.handle, s.insn.items(.token)[insn_handle], "Stack frame too large", flags);
    }

    insn_lengths[insn_handle] = length;

    if (!flagset.contains(.depends_on_layout)) {
        insn_flags[insn_handle].insert(.length_computed);
    }

    return length;
}

fn resolveDataDirectiveLength(a: *Assembler, s: SourceFile.Slices, ip: u32, insn_handle: Instruction.Handle, granularity_bytes: u8) u32 {
    const insn_flags = s.insn.items(.flags);
    const insn_lengths = s.insn.items(.length);

    const flagset = insn_flags[insn_handle];

    if (flagset.contains(.length_computed)) {
        return insn_lengths[insn_handle];
    }

    var length: u32 = 0;

    if (s.insn.items(.params)[insn_handle]) |params_expr_handle| {
        const expr_infos = s.expr.items(.info);
        var expr_handle = params_expr_handle;
        while (true) switch (expr_infos[expr_handle]) {
            .list => |bin| {
                length += resolveDataExpressionLength(a, s, ip, bin.left, granularity_bytes);
                expr_handle = bin.right;
            },
            else => {
                length += resolveDataExpressionLength(a, s, ip, expr_handle, granularity_bytes);
                break;
            },
        };
    }

    insn_lengths[insn_handle] = length;

    if (!flagset.contains(.depends_on_layout)) {
        insn_flags[insn_handle].insert(.length_computed);
    }

    return length;
}
fn resolveDataExpressionLength(a: *Assembler, s: SourceFile.Slices, ip: u32, expr_handle: Expression.Handle, granularity_bytes: u8) u32 {
    if (resolveExpressionConstant(a, s, ip, expr_handle)) |constant| {
        return @intCast(u32, std.mem.alignForward(constant.getBitCount(), granularity_bytes * 8) / 8);
    } else return 0;
}

pub fn resolveExpressionConstantOrDefault(a: *Assembler, s: SourceFile.Slices, ip: u32, expr_handle: Expression.Handle, default: anytype) Constant {
    const resolved = resolveExpressionConstant(a, s, ip, expr_handle);
    if (resolved) |constant| return constant.*;

    return switch (@typeInfo(@TypeOf(default))) {
        .Int => Constant.initInt(default),
        .ComptimeInt => Constant.initInt(@as(i64, default)),
        .Pointer => |ptr_info| if(ptr_info.size == .Slice) Constant.initString(default) else default.*,
        .Struct => default,
        else => @compileError("Unsupported default value"),
    };
}

pub fn resolveExpressionConstant(a: *Assembler, s: SourceFile.Slices, ip: u32, expr_handle: Expression.Handle) ?*const Constant {
    var expr_resolved_constants = s.expr.items(.resolved_constant);
    if (expr_resolved_constants[expr_handle]) |constant| {
        return constant;
    }

    var expr_infos = s.expr.items(.info);
    const info = expr_infos[expr_handle];
    switch (info) {
        .local_label_def => unreachable,

        .literal_symbol_def, .directive_symbol_def,
        .literal_int, .literal_str, .reg_to_index,
        => return null,

        .list, .arrow_list, .arrow_prefix, .literal_reg,
        .index_to_reg8, .index_to_reg16, .index_to_reg32,
        => return null,

        .literal_current_address => {
            var value = ip;
            switch (s.expr.items(.resolved_type)[expr_handle]) {
                .raw_base_offset, .data_address, .insn_address, .stack_address => |bo| {
                    if (std.meta.eql(bo.base, .{ .sr = .IP })) {
                        value = 0;
                    }
                },
                else => {},
            }
            const constant = Constant.initInt(value);
            expr_resolved_constants[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
        },

        .literal_symbol_ref => {
            const token_handle = s.expr.items(.token)[expr_handle];
            const raw_symbol = s.file.tokens.get(token_handle).location(s.file.source);
            const symbol_constant = Constant.initSymbolLiteral(a.gpa, &a.constant_temp, raw_symbol);
            resolveSymbolRefExprConstant(a, s, ip, expr_handle, token_handle, symbol_constant);
        },
        .directive_symbol_ref => |inner_expr| {
            const token_handle = s.expr.items(.token)[inner_expr];
            const symbol_constant = expr_resolved_constants[inner_expr].?;
            resolveSymbolRefExprConstant(a, s, ip, expr_handle, token_handle, symbol_constant.*);
        },

        .negate => |inner_expr| {
            const right = resolveExpressionConstant(a, s, ip, inner_expr) orelse return null;
            const zero = Constant.initInt(@as(u1, 0));
            const result = zero.binaryOp(a.gpa, &a.constant_temp, right.*, .subtract);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .complement => |inner_expr| {
            const inner = resolveExpressionConstant(a, s, ip, inner_expr) orelse return null;
            const result = inner.complement(a.gpa, &a.constant_temp);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .plus => |bin| {
            const left = resolveExpressionConstantOrDefault(a, s, ip, bin.left, 0);
            const right = resolveExpressionConstantOrDefault(a, s, ip, bin.right, 0);
            const result = left.binaryOp(a.gpa, &a.constant_temp, right, .add);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .minus => |bin| {
            const left = resolveExpressionConstantOrDefault(a, s, ip, bin.left, 0);
            const right = resolveExpressionConstantOrDefault(a, s, ip, bin.right, 0);
            const result = left.binaryOp(a.gpa, &a.constant_temp, right, .subtract);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .multiply, .shl, .shr, => |bin| {
            const left = resolveExpressionConstant(a, s, ip, bin.left) orelse return null;
            const right = resolveExpressionConstant(a, s, ip, bin.right) orelse return null;

            const lv = left.asInt(i64) catch {
                a.recordExpressionLayoutError(s.file.handle, expr_handle, bin.left, "Overflow (constant too large)", .{});
                return null;
            };
            const rv = right.asInt(i64) catch {
                a.recordExpressionLayoutError(s.file.handle, expr_handle, bin.right, "Overflow (constant too large)", .{});
                return null;
            };

            if (info == .shl or info == .shr) {
                _ = std.math.cast(u6, rv) orelse {
                    a.recordExpressionLayoutError(s.file.handle, expr_handle, expr_handle, "Overflow (constant too large)", .{});
                };
            }

            const value = switch (info) {
                .multiply => std.math.mul(i64, lv, rv),
                .shl => std.math.shlExact(i64, lv, @intCast(u6, rv)),
                .shr => lv >> @intCast(u6, rv),
                else => unreachable,
            } catch {
                a.recordExpressionLayoutError(s.file.handle, expr_handle, expr_handle, "Overflow (constant too large)", .{});
                return null;
            };
            const new_constant = Constant.initInt(value);
            expr_resolved_constants[expr_handle] = new_constant.intern(a.arena, a.gpa, &a.constants);
        },
        .concat => |bin| {
            const left = resolveExpressionConstant(a, s, ip, bin.left) orelse return null;
            const right = resolveExpressionConstant(a, s, ip, bin.right) orelse return null;
            const result = left.concat(a.gpa, &a.constant_temp, right.*);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .bitwise_or => |bin| {
            const left = resolveExpressionConstant(a, s, ip, bin.left) orelse return null;
            const right = resolveExpressionConstant(a, s, ip, bin.right) orelse return null;
            const result = left.binaryOp(a.gpa, &a.constant_temp, right.*, .bitwise_or);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .bitwise_xor => |bin| {
            const left = resolveExpressionConstant(a, s, ip, bin.left) orelse return null;
            const right = resolveExpressionConstant(a, s, ip, bin.right) orelse return null;
            const result = left.binaryOp(a.gpa, &a.constant_temp, right.*, .bitwise_xor);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .bitwise_and => |bin| {
            const left = resolveExpressionConstant(a, s, ip, bin.left) orelse return null;
            const right = resolveExpressionConstant(a, s, ip, bin.right) orelse return null;
            const result = left.binaryOp(a.gpa, &a.constant_temp, right.*, .bitwise_and);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .concat_repeat => |bin| {
            const left = resolveExpressionConstant(a, s, ip, bin.left) orelse return null;
            const right = resolveExpressionConstant(a, s, ip, bin.right) orelse return null;
            const times = right.asInt(u63) catch {
                a.recordExpressionLayoutError(s.file.handle, expr_handle, bin.right, "Overflow (repeat count must fit in u63)", .{});
                return null;
            };
            const result = left.repeat(a.gpa, &a.constant_temp, times);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .length_cast => |bin| {
            const left = resolveExpressionConstant(a, s, ip, bin.left) orelse return null;
            const right = resolveExpressionConstant(a, s, ip, bin.right) orelse return null;
            const width = right.asInt(u63) catch {
                a.recordExpressionLayoutError(s.file.handle, expr_handle, bin.right, "Overflow (constant width must fit in u63)", .{});
                return null;
            };
            const result = left.cloneWithLength(a.gpa, &a.constant_temp, width) catch {
                a.recordExpressionLayoutError(s.file.handle, expr_handle, bin.right, "Truncation would change constant value (consider using .trunc instead)", .{});
                return null;
            };
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .truncate => |bin| {
            const left = resolveExpressionConstant(a, s, ip, bin.left) orelse return null;
            const right = resolveExpressionConstant(a, s, ip, bin.right) orelse return null;
            const width = right.asInt(u63) catch {
                a.recordExpressionLayoutError(s.file.handle, expr_handle, bin.right, "Overflow (constant width must fit in u63)", .{});
                return null;
            };
            const result = left.truncate(width);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .sign_extend => |bin| {
            const left = resolveExpressionConstant(a, s, ip, bin.left) orelse return null;
            const right = resolveExpressionConstant(a, s, ip, bin.right) orelse return null;
            const width = right.asInt(u63) catch {
                a.recordExpressionLayoutError(s.file.handle, expr_handle, bin.right, "Overflow (constant width must fit in u63)", .{});
                return null;
            };
            const result = left.extend(a.gpa, &a.constant_temp, width, .signed) catch {
                a.recordExpressionLayoutError(s.file.handle, expr_handle, bin.right, "Constant is already wider than requested (consider using .trunc instead)", .{});
                return null;
            };
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .zero_extend => |bin| {
            const left = resolveExpressionConstant(a, s, ip, bin.left) orelse return null;
            const right = resolveExpressionConstant(a, s, ip, bin.right) orelse return null;
            const width = right.asInt(u63) catch {
                a.recordExpressionLayoutError(s.file.handle, expr_handle, bin.right, "Overflow (constant width must fit in u63)", .{});
                return null;
            };
            const result = left.extend(a.gpa, &a.constant_temp, width, .unsigned) catch {
                a.recordExpressionLayoutError(s.file.handle, expr_handle, bin.right, "Constant is already wider than requested (consider using .trunc instead)", .{});
                return null;
            };
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },

        .signed_cast => |inner_expr| {
            const constant = resolveExpressionConstant(a, s, ip, inner_expr) orelse return null;
            const result = constant.cloneWithSignedness(a.gpa, &a.constant_temp, .signed);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .unsigned_cast => |inner_expr| {
            const constant = resolveExpressionConstant(a, s, ip, inner_expr) orelse return null;
            const result = constant.cloneWithSignedness(a.gpa, &a.constant_temp, .unsigned);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .remove_signedness_cast, .data_address_cast, .insn_address_cast, .stack_address_cast, .remove_address_cast,
         => |inner_expr| {
            expr_resolved_constants[expr_handle] = resolveExpressionConstant(a, s, ip, inner_expr);
        },
        .absolute_address_cast => |inner_expr| {
            const inner = resolveExpressionConstant(a, s, ip, inner_expr) orelse return null;
            const relative = inner.asInt(i64) catch {
                a.recordExpressionLayoutError(s.file.handle, expr_handle, inner_expr, "Relative address too large", .{});
                return null;
            };
            const absolute = std.math.add(i64, relative, ip) catch {
                a.recordExpressionLayoutError(s.file.handle, expr_handle, inner_expr, "Absolute address overflows u32", .{});
                return null;
            };
            const absolute_u32 = std.math.cast(u32, absolute) orelse {
                a.recordExpressionLayoutError(s.file.handle, expr_handle, inner_expr, "Absolute address overflows u32", .{});
                return null;
            };
            const result = Constant.initInt(absolute_u32);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
    }

    return expr_resolved_constants[expr_handle];
}

fn resolveSymbolRefExprConstant(a: *Assembler, s: SourceFile.Slices, ip: u32, expr_handle: Expression.Handle, symbol_token_handle: lex.Token.Handle, symbol_constant: Constant) void {
    switch (symbols.lookupSymbol(a, s, symbol_token_handle, symbol_constant.asString())) {
        .expression => |target_expr_handle| {
            s.expr.items(.resolved_constant)[expr_handle] = resolveExpressionConstant(a, s, ip, target_expr_handle);
        },
        .instruction => |target_insn_ref| {
            const target_file = a.getSource(target_insn_ref.file);
            var value: i64 = target_file.instructions.items(.address)[target_insn_ref.instruction];
            switch (s.expr.items(.resolved_type)[expr_handle]) {
                .raw_base_offset, .data_address, .insn_address, .stack_address => |info| {
                    if (std.meta.eql(info.base, .{ .sr = .IP })) {
                        value -= ip;
                    }
                },
                else => {},
            }
            const constant = Constant.initInt(value);
            s.expr.items(.resolved_constant)[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
        },
        .stack => |target_stack_ref| {
            var value: i64 = s.insn.items(.address)[target_stack_ref.instruction];
            value += target_stack_ref.additional_sp_offset;
            const constant = Constant.initInt(value);
            s.expr.items(.resolved_constant)[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
        },
        .not_found => {
            return;
        },
    }
}

fn resolveInstructionEncoding(a: *Assembler, s: SourceFile.Slices, ip: u32, insn_handle: Instruction.Handle, op: *Instruction.Operation, params: ?Expression.Handle) u3 {
    const insn = a.buildInstruction(s, ip, op.insn.mnemonic, op.insn.suffix, params, true) orelse return 0;
    const best_encoding = findBestInstructionEncoding(a, insn);
    a.params_temp.clearRetainingCapacity();

    if (best_encoding) |enc| {
        op.* = .{ .bound_insn = enc };
        return enc.getInstructionLength();
    } else {
        var err_flags = InsnEncodingError.FlagSet.initOne(.remove_on_layout_reset);
        a.recordInsnEncodingError(s.file.handle, insn_handle, err_flags);
        return 0;
    }
}

pub fn findBestInstructionEncoding(a: *Assembler, insn: ie.Instruction) ?*const ie.InstructionEncoding {
    var encoding_iter = a.edb.getMatchingEncodings(insn);

    var best_length: ?u3 = null;
    var best_encoding: ?*const ie.InstructionEncoding = null;
    while (encoding_iter.nextPointer()) |enc| {
        const length = enc.getInstructionLength();
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

    return best_encoding;
}

pub fn populatePageChunks(a: *Assembler, chunks: []const SourceFile.Chunk) void {
    var page_chunks = a.pages.items(.chunks);
    var page_sections = a.pages.items(.section);
    for (chunks) |chunk| {
        if (chunk.section) |section| {
            const addresses = chunk.getAddressRange(a);
            const first_page = addresses.first >> @bitSizeOf(bus.PageOffset);
            var last_page = first_page;
            if (addresses.len > 0) {
                last_page = addresses.last() >> @bitSizeOf(bus.PageOffset);
            }

            a.sections.values()[section].has_chunks = true;

            a.chunks.append(a.gpa, .{
                .chunk = chunk,
                .address = addresses.first,
            }) catch @panic("OOM");

            for (first_page .. last_page + 1) |page| {
                const page_data_handle = a.page_lookup.get(@intCast(bus.Page, page)) orelse unreachable;
                page_chunks[page_data_handle].append(a.gpa, chunk) catch @panic("OOM");
                if (page_sections[page_data_handle] != section) {
                    const token = a.getSource(chunk.file).instructions.items(.token)[chunk.instructions.begin];
                    a.recordErrorFmt(chunk.file, token, "Chunk starting here was allocated to page 0x{X:0>5}, but that page is in use by another section", .{ page }, .{});
                }
            }
        }
    }
}

pub fn findOverlappingChunks(a: *Assembler, chunks: []const SourceFile.Chunk) void {
    // This is just the obvious O(n^2) implementation, but since we've already
    // grouped chunks by page, it shouldn't explode too terribly even for large programs,
    // though there is an antagonistic case where you intentionally put a bunch of .orgs
    // with the same address.  Therefore we'll stop checking after a total of 10
    // overlapping pairs are found.
    if (a.overlapping_chunks.count() >= 10) return;
    for (chunks, 0..) |chunk, i| {
        if (chunk.section == null) continue;

        const chunk_range = chunk.getAddressRange(a);
        const check_against = chunks[i + 1 ..];
        for (check_against) |other_chunk| {
            if (chunk.section == null) continue;

            const other_range = other_chunk.getAddressRange(a);
            if (chunk_range.first > other_range.last() or chunk_range.last() < other_range.first) continue;
            a.overlapping_chunks.put(a.gpa, SourceFile.ChunkPair.init(chunk, other_chunk), {}) catch @panic("OOM");
            if (a.overlapping_chunks.count() >= 10) return;
        }
    }
}

pub fn encodePageData(a: *Assembler, file: *SourceFile) void {
    const s = file.slices();
    const insn_params = s.insn.items(.params);
    const insn_addresses = s.insn.items(.address);
    const insn_lengths = s.insn.items(.length);

    const page_datas = a.pages.items(.data);

    for (0.., s.insn.items(.operation)) |insn_handle_usize, op| {
        const insn_handle = @intCast(Instruction.Handle, insn_handle_usize);
        switch (op) {
            .none, .insn, .org, .@"align", .keep, .def, .undef, .local, .range,
            .section, .boot, .code, .kcode, .entry, .kentry, .data, .kdata, .@"const", .kconst, .stack,
            => {},

            .bound_insn => |encoding| {
                const address = insn_addresses[insn_handle];
                const params = insn_params[insn_handle];
                const insn = a.buildInstruction(s, address, encoding.mnemonic, encoding.suffix, params, false).?;
                encodeInstruction(a, s, insn_handle, address, insn_lengths[insn_handle], insn, encoding, page_datas);
            },

            .db, .dw, .dd => {
                const granularity_bytes: u8 = switch (op) {
                    .db => 1,
                    .dw => 2,
                    .dd => 4,
                    else => unreachable,
                };
                const address = insn_addresses[insn_handle];
                const length: usize = insn_lengths[insn_handle];
                const params = insn_params[insn_handle] orelse continue;
                var page = @truncate(bus.Page, address >> @bitSizeOf(bus.PageOffset));
                const initial_offset = @truncate(bus.PageOffset, address);
                var page_data_handle = a.page_lookup.get(page) orelse continue;
                var buffer = page_datas[page_data_handle][initial_offset..];
                var written: usize = 0;

                while (length - written > buffer.len) {
                    encodeDataDirective(s, params, granularity_bytes, written, buffer);
                    written += buffer.len;
                    page += 1;
                    page_data_handle = a.page_lookup.get(page) orelse break;
                    buffer = &page_datas[page_data_handle];
                }
                encodeDataDirective(s, params, granularity_bytes, written, buffer);
            },
            .push, .pop => |stack_size| {
                const insn = ie.Instruction{
                    .mnemonic = switch (op) {
                        .push => .FRAME,
                        .pop => .UNFRAME,
                        else => unreachable,
                    },
                    .suffix = .none,
                    .params = &.{
                        .{
                            .expr_type = .{ .constant = {} },
                            .constant = stack_size,
                        },
                    },
                };
                if (findBestInstructionEncoding(a, insn)) |encoding| {
                    encodeInstruction(a, s, insn_handle, insn_addresses[insn_handle], insn_lengths[insn_handle], insn, encoding, page_datas);
                }
            },
        }
    }
}

fn encodeInstruction(
    a: *Assembler,
    s: SourceFile.Slices,
    insn_handle: Instruction.Handle,
    address: u32,
    length: u32,
    insn: ie.Instruction,
    encoding: *const ie.InstructionEncoding,
    page_datas: [][PageData.page_size]u8
) void {
    const page = @truncate(bus.Page, address >> @bitSizeOf(bus.PageOffset));
    const offset = @truncate(bus.PageOffset, address);

    const page_data_handle = a.page_lookup.get(page) orelse return;
    var buffer = page_datas[page_data_handle][offset..];

    if (length > buffer.len) {
        if (@truncate(u1, address) == 1) {
            a.recordError(s.file.handle, s.insn.items(.token)[insn_handle], "Instruction crosses page boundary and is not word aligned", .{});
        }

        var temp = [_]u8{0} ** 8;
        const temp_insn = insn.write(encoding.*, &temp);

        std.mem.copy(u8, buffer, temp_insn[0..buffer.len]);

        const next_page = page + 1;
        const next_page_data_handle = a.page_lookup.get(next_page) orelse unreachable;
        const next_page_buf = &page_datas[next_page_data_handle];
        std.mem.copy(u8, next_page_buf, temp_insn[buffer.len..]);
    } else {
        _ = insn.write(encoding.*, buffer);
    }
}

fn encodeDataDirective(s: SourceFile.Slices, params_expr_handle: Expression.Handle, granularity_bytes: u8, skip_bytes: usize, out: []u8) void {
    const expr_infos = s.expr.items(.info);
    const expr_resolved_constants = s.expr.items(.resolved_constant);
    var bytes_to_skip = skip_bytes;
    var buf = out;
    var expr_handle = params_expr_handle;
    while (true) switch (expr_infos[expr_handle]) {
        .list => |bin| {
            if (expr_resolved_constants[bin.left]) |constant| {
                const bytes = encodeDataExpression(constant, granularity_bytes, bytes_to_skip, buf);
                if (bytes > bytes_to_skip) {
                    const bytes_written = bytes - bytes_to_skip;
                    bytes_to_skip = 0;
                    buf = buf[bytes_written..];
                    if (buf.len == 0) return;
                } else {
                    bytes_to_skip -= bytes;
                }
            }
            expr_handle = bin.right;
        },
        else => {
            if (expr_resolved_constants[expr_handle]) |constant| {
                _ = encodeDataExpression(constant, granularity_bytes, bytes_to_skip, buf);
            }
            break;
        },
    };
}
fn encodeDataExpression(expr_constant: *const Constant, granularity_bytes: u8, skip_bytes: usize, out: []u8) usize {
    var constant_bytes = std.mem.alignForward(expr_constant.getBitCount(), granularity_bytes * 8) / 8;
    if (skip_bytes >= constant_bytes) {
        return constant_bytes;
    }

    var iter = expr_constant.byteIterator(null);
    iter.skip(skip_bytes);
    constant_bytes -= skip_bytes;

    var buf = out;
    if (buf.len > constant_bytes) {
        buf.len = constant_bytes;
    }

    for (buf) |*b| {
        b.* = iter.next();
    }

    return skip_bytes + buf.len;
}