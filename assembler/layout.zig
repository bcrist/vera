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
    var file = a.getSource(chunk.file);
    const s = file.slices();
    const operations = s.insn.items(.operation);

    var initial_address: u32 = 0;

    var iter = chunk.instructions;
    while (iter.next()) |insn_handle| {
        if (operations[insn_handle] == .org) {
            initial_address = s.insn.items(.address)[insn_handle];
            if (s.insn.items(.params)[insn_handle]) |expr_handle| {
                if (resolveExpressionConstant(a, s, initial_address, expr_handle)) |constant| {
                    const address_i64 = constant.asInt() catch {
                        a.recordExpressionLayoutError(chunk.file, expr_handle, ".org address too large", .{});
                        break;
                    };

                    initial_address = std.math.cast(u32, address_i64) orelse {
                        a.recordExpressionLayoutError(chunk.file, expr_handle, ".org address too large", .{});
                        break;
                    };
                }
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
    // TODO add a kentry output mode to write a "header" file that exports the addresses of kentry labels

    // TODO check for an initial .align directive in the chunk and try to satisfy it

    const chunk_section_handle = chunk.section orelse return 0;

    const address_range = chunk.getAddressRange(a);
    const chunk_size = @max(1, address_range.end - address_range.begin);

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
            .none, .org, .keep, .def, .undef,
            .section, .code, .kcode, .entry, .kentry,
            .data, .kdata, .@"const", .kconst, .stack => {
                // Look forwards to see if an .align, etc. is coming up
                var iter2 = chunk.instructions;
                iter2.begin = insn_handle + 1;
                while (iter2.next()) |insn2| {
                    switch (insn_operations[insn2]) {
                        .none, .org, .keep, .def, .undef,
                        .section, .code, .kcode, .entry, .kentry,
                        .data, .kdata, .@"const", .kconst, .stack => {},

                        .@"align" => {
                            if (insn_params[insn2]) |align_expr| {
                                address = resolveAlignment(a, s, address, align_expr, false, check_for_alignment_holes, insn2, chunk.section);
                            }
                            break;
                        },
                        .dw, .dd => {
                            address = applyAlignment(a, s, address, 2, 0, false, check_for_alignment_holes, insn2, chunk.section);
                            break;
                        },
                        else => break,
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
                if (insn_flags[insn_handle].contains(.encoding_depends_on_layout)) {
                    const old_encoding = encoding;
                    insn_operations[insn_handle] = .{ .insn = .{
                        .mnemonic = old_encoding.mnemonic,
                        .suffix = old_encoding.suffix,
                    }};
                    const length = resolveInstructionEncoding(a, s, address, insn_handle, &insn_operations[insn_handle], insn_params[insn_handle]);
                    address += length;
                    switch (insn_operations[insn_handle]) {
                        .bound_insn => |new_encoding| if (!ie.eql(old_encoding.*, new_encoding.*)) {
                            insn_lengths[insn_handle] = length;
                            layout_changed = true;
                        },
                        else => {},
                    }
                } else {
                    address += insn_lengths[insn_handle];
                }
                check_for_alignment_holes = true;
            },
            .@"align" => {
                if (insn_params[insn_handle]) |align_expr| {
                    address = resolveAlignment(a, s, address, align_expr, true, check_for_alignment_holes, insn_handle, chunk.section);
                }
            },
            .db => {
                if (insn_params[insn_handle]) |expr_handle| {
                    address += resolveDataDirectiveLength(a, s, address, insn_handle, expr_handle, 1);
                }
                check_for_alignment_holes = true;
            },
            .dw => {
                address = applyAlignment(a, s, address, 2, 0, true, check_for_alignment_holes, insn_handle, chunk.section);
                if (insn_params[insn_handle]) |expr_handle| {
                    address += resolveDataDirectiveLength(a, s, address, insn_handle, expr_handle, 2);
                }
                check_for_alignment_holes = true;
            },
            .dd => {
                address = applyAlignment(a, s, address, 2, 0, true, check_for_alignment_holes, insn_handle, chunk.section);
                if (insn_params[insn_handle]) |expr_handle| {
                    address += resolveDataDirectiveLength(a, s, address, insn_handle, expr_handle, 4);
                }
                check_for_alignment_holes = true;
            },
            .push => {
                // TODO synthesize push instruction
                check_for_alignment_holes = true;
            },
            .pop => {
                // TODO synthesize pop instruction
                check_for_alignment_holes = true;
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

fn resolveAlignment(
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
    var alignment: i64 = 0;
    var offset: i64 = 0;
    switch (s.expr.items(.info)[align_expr]) {
        .list => |bin| {
            alignment_expr = bin.left;
            if (resolveExpressionConstant(a, s, address, bin.left)) |constant| {
                alignment = constant.asInt() catch {
                    if (report_errors) {
                        const token = s.expr.items(.token)[bin.left];
                        const err_flags = Error.FlagSet.initOne(.remove_on_layout_reset);
                        a.recordError(s.file.handle, token, "Overflow (alignment too large)", err_flags);
                    }
                    return address;
                };
            }
            if (resolveExpressionConstant(a, s, address, bin.right)) |constant| {
                offset = constant.asInt() catch {
                    if (report_errors) {
                        const token = s.expr.items(.token)[bin.right];
                        const err_flags = Error.FlagSet.initOne(.remove_on_layout_reset);
                        a.recordError(s.file.handle, token, "Overflow (offset too large)", err_flags);
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
                alignment = constant.asInt() catch {
                    if (report_errors) {
                        const token = s.expr.items(.token)[align_expr];
                        const err_flags = Error.FlagSet.initOne(.remove_on_layout_reset);
                        a.recordError(s.file.handle, token, "Overflow (alignment too large)", err_flags);
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

    const alignment_u32 = std.math.cast(u32, alignment) orelse {
        if (report_errors) {
            const token = s.expr.items(.token)[alignment_expr];
            const err_flags = Error.FlagSet.initOne(.remove_on_layout_reset);
            a.recordError(s.file.handle, token, "Overflow (alignment must fit in u32)", err_flags);
        }
        return address;
    };

    return applyAlignment(a, s, address, alignment_u32, @intCast(u32, offset), report_errors, check_for_alignment_holes, insn_handle, section_handle);
}

fn applyAlignment(
    a: *Assembler,
    s: SourceFile.Slices,
    address: u32,
    alignment: u32,
    offset: u32,
    report_errors: bool,
    check_for_alignment_holes: bool,
    insn_handle: Instruction.Handle,
    section_handle: ?Section.Handle
) u32 {
    var new_address_usize = std.mem.alignBackward(address, alignment) + offset;
    if (new_address_usize < address) {
        new_address_usize += alignment;
    }

    const new_address = std.math.cast(u32, new_address_usize) orelse {
        if (report_errors) {
            const token = s.insn.items(.token)[insn_handle];
            const err_flags = Error.FlagSet.initOne(.remove_on_layout_reset);
            a.recordError(s.file.handle, token, "Aligned address overflow", err_flags);
        }
        return address;
    };

    if (new_address != address and check_for_alignment_holes) {
        if (section_handle) |section| switch (a.getSection(section).kind) {
            .info,
            .data_user,
            .data_kernel,
            .constant_user,
            .constant_kernel,
            .stack,
            => {},

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

fn resolveDataDirectiveLength(a: *Assembler, s: SourceFile.Slices, ip: u32, insn_handle: Instruction.Handle, params_expr_handle: Expression.Handle, granularity_bytes: u8) u32 {
    var length: u32 = 0;

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

    s.insn.items(.length)[insn_handle] = length;

    return length;
}
fn resolveDataExpressionLength(a: *Assembler, s: SourceFile.Slices, ip: u32, expr_handle: Expression.Handle, granularity_bytes: u8) u32 {
    if (resolveExpressionConstant(a, s, ip, expr_handle)) |constant| {
        return @intCast(u32, std.mem.alignForward(constant.bit_count, granularity_bytes * 8) / 8);
    } else return 0;
}

pub fn resolveExpressionConstant(a: *Assembler, s: SourceFile.Slices, ip: u32, expr_handle: Expression.Handle) ?*const Constant {
    var expr_resolved_constants = s.expr.items(.resolved_constant);
    if (expr_resolved_constants[expr_handle]) |constant| {
        return constant;
    }

    var expr_infos = s.expr.items(.info);
    const info = expr_infos[expr_handle];
    switch (info) {
        .literal_symbol_def, .directive_symbol_def,
        .literal_int, .literal_str, .reg_to_index,
        => unreachable,

        .list, .arrow_list, .literal_reg,
        .index_to_reg8, .index_to_reg16, .index_to_reg32,
        => return null,

        .literal_current_address => {
            var value = ip;
            switch (s.expr.items(.resolved_type)[expr_handle]) {
                .raw_base_offset, .data_address, .insn_address, .stack_address => |bo| {
                    if (std.meta.eql(bo.base, .{ .sr = .ip })) {
                        value = 0;
                    }
                },
                else => {},
            }
            const constant = Constant.initInt(value, null);
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
            const inner_constant = resolveExpressionConstant(a, s, ip, inner_expr) orelse return null;
            const value = inner_constant.asIntNegated() catch {
                a.recordExpressionLayoutError(s.file.handle, expr_handle, "Overflow (constant too large)", .{});
                return null;
            };
            const new_constant = Constant.initInt(value, null);
            expr_resolved_constants[expr_handle] = new_constant.intern(a.arena, a.gpa, &a.constants);
        },
        .complement => |inner_expr| {
            const inner = resolveExpressionConstant(a, s, ip, inner_expr) orelse return null;
            const result = inner.complement(a.gpa, &a.constant_temp);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .plus, .minus, .multiply, .shl, .shr, => |bin| {
            const left = resolveExpressionConstant(a, s, ip, bin.left) orelse {
                const right = resolveExpressionConstant(a, s, ip, bin.right) orelse return null;
                const value = right.asIntNegated() catch {
                    a.recordExpressionLayoutError(s.file.handle, expr_handle, "Overflow (constant too large)", .{});
                    return null;
                };
                const new_constant = Constant.initInt(value, null);
                expr_resolved_constants[expr_handle] = new_constant.intern(a.arena, a.gpa, &a.constants);
                return expr_resolved_constants[expr_handle];
            };
            const right = resolveExpressionConstant(a, s, ip, bin.right) orelse {
                expr_resolved_constants[expr_handle] = left;
                return expr_resolved_constants[expr_handle];
            };

            const lv = left.asInt() catch {
                a.recordExpressionLayoutError(s.file.handle, bin.left, "Overflow (constant too large)", .{});
                return null;
            };
            const rv = right.asInt() catch {
                a.recordExpressionLayoutError(s.file.handle, bin.right, "Overflow (constant too large)", .{});
                return null;
            };

            if (info == .shl or info == .shr) {
                _ = std.math.cast(u6, rv) orelse {
                    a.recordExpressionLayoutError(s.file.handle, expr_handle, "Overflow (constant too large)", .{});
                };
            }

            const value = switch (info) {
                .plus => std.math.add(i64, lv, rv),
                .minus => std.math.sub(i64, lv, rv),
                .multiply => std.math.mul(i64, lv, rv),
                .shl => std.math.shlExact(i64, lv, @intCast(u6, rv)),
                .shr => lv >> @intCast(u6, rv),
                else => unreachable,
            } catch {
                a.recordExpressionLayoutError(s.file.handle, expr_handle, "Overflow (constant too large)", .{});
                return null;
            };
            const new_constant = Constant.initInt(value, null);
            expr_resolved_constants[expr_handle] = new_constant.intern(a.arena, a.gpa, &a.constants);
        },
        .concat, .bitwise_or, .bitwise_xor, .bitwise_and => |bin| {
            const left = resolveExpressionConstant(a, s, ip, bin.left) orelse return null;
            const right = resolveExpressionConstant(a, s, ip, bin.right) orelse return null;
            const result = switch (info) {
                .concat => left.concat(a.gpa, &a.constant_temp, right.*),
                .bitwise_or => left.bitwiseOr(a.gpa, &a.constant_temp, right.*),
                .bitwise_xor => left.bitwiseXor(a.gpa, &a.constant_temp, right.*),
                .bitwise_and => left.bitwiseAnd(a.gpa, &a.constant_temp, right.*),
                else => unreachable,
            };
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .concat_repeat => |bin| {
            const left = resolveExpressionConstant(a, s, ip, bin.left) orelse return null;
            const right = resolveExpressionConstant(a, s, ip, bin.right) orelse return null;
            const times = right.asInt() catch {
                a.recordExpressionLayoutError(s.file.handle, bin.right, "Overflow (constant too large)", .{});
                return null;
            };
            const result = left.repeat(a.gpa, &a.constant_temp, times);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .length_cast, .truncate, .sign_extend => |bin| {
            const left = resolveExpressionConstant(a, s, ip, bin.left) orelse return null;
            const right = resolveExpressionConstant(a, s, ip, bin.right) orelse return null;
            const width = right.asInt() catch {
                a.recordExpressionLayoutError(s.file.handle, bin.right, "Overflow (constant too large)", .{});
                return null;
            };
            if (width < 0) {
                a.recordExpressionLayoutError(s.file.handle, bin.right, "Width must not be negative", .{});
                return null;
            }
            const result = left.cloneWithLength(a.gpa, &a.constant_temp, @intCast(u64, width), .signed);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .zero_extend => |bin| {
            const left = resolveExpressionConstant(a, s, ip, bin.left) orelse return null;
            const right = resolveExpressionConstant(a, s, ip, bin.right) orelse return null;
            const width = right.asInt() catch {
                a.recordExpressionLayoutError(s.file.handle, bin.right, "Overflow (constant too large)", .{});
                return null;
            };
            if (width < 0) {
                a.recordExpressionLayoutError(s.file.handle, bin.right, "Width must not be negative", .{});
                return null;
            }
            const result = left.cloneWithLength(a.gpa, &a.constant_temp, @intCast(u64, width), .unsigned);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .signed_cast, .unsigned_cast, .maybe_signed_cast,
        .data_address_cast, .insn_address_cast, .stack_address_cast, .remove_address_cast,
         => |inner_expr| {
            expr_resolved_constants[expr_handle] = resolveExpressionConstant(a, s, ip, inner_expr);
        },
        .absolute_address_cast => |inner_expr| {
            const inner = resolveExpressionConstant(a, s, ip, inner_expr) orelse return null;
            const relative = inner.asInt() catch {
                a.recordExpressionLayoutError(s.file.handle, inner_expr, "Relative address too large", .{});
                return null;
            };
            const absolute = std.math.add(i64, relative, ip) catch {
                a.recordExpressionLayoutError(s.file.handle, inner_expr, "Absolute address overflows u32", .{});
                return null;
            };
            const absolute_u32 = std.math.cast(u32, absolute) orelse {
                a.recordExpressionLayoutError(s.file.handle, inner_expr, "Absolute address overflows u32", .{});
                return null;
            };
            const result = Constant.initInt(absolute_u32, null);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
    }

    return expr_resolved_constants[expr_handle];
}

fn resolveSymbolRefExprConstant(a: *Assembler, s: SourceFile.Slices, ip: u32, expr_handle: Expression.Handle, symbol_token_handle: lex.Token.Handle, symbol_constant: Constant) void {
    if (a.lookupSymbol(s, symbol_token_handle, symbol_constant.asString())) |target| {
        switch (target) {
            .expression => |target_expr_handle| {
                s.expr.items(.resolved_constant)[expr_handle] = resolveExpressionConstant(a, s, ip, target_expr_handle);
            },
            .instruction => |target_insn_ref| {
                const target_file = a.getSource(target_insn_ref.file);
                var value: i64 = target_file.instructions.items(.address)[target_insn_ref.instruction];
                switch (s.expr.items(.resolved_type)[expr_handle]) {
                    .raw_base_offset, .data_address, .insn_address, .stack_address => |info| {
                        if (std.meta.eql(info.base, .{ .sr = .ip })) {
                            value -= ip;
                        }
                    },
                    else => {},
                }
                const constant = Constant.initInt(value, null);
                s.expr.items(.resolved_constant)[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
            },
            .not_found => {
                return;
            },
        }
    } else unreachable;
}

fn resolveInstructionEncoding(a: *Assembler, s: SourceFile.Slices, ip: u32, insn_handle: Instruction.Handle, op: *Instruction.Operation, params: ?Expression.Handle) u3 {
    const insn = a.buildInstruction(s, ip, op.insn.mnemonic, op.insn.suffix, params, true) orelse return 0;
    var encoding_iter = a.edb.getMatchingEncodings(insn);
    a.params_temp.clearRetainingCapacity();

    var best_length: ?u3 = null;
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

    var err_flags = InsnEncodingError.FlagSet{};
    if (s.insn.items(.flags)[insn_handle].contains(.encoding_depends_on_layout)) {
        err_flags.insert(.remove_on_layout_reset);
    }
    a.recordInsnEncodingError(s.file.handle, insn_handle, err_flags);
    return 0;
}

pub fn populatePageChunks(a: *Assembler, chunks: []const SourceFile.Chunk) void {
    var page_chunks = a.pages.items(.chunks);
    var page_sections = a.pages.items(.section);
    for (chunks) |chunk| {
        const addresses = chunk.getAddressRange(a);
        const first_page = addresses.begin >> @bitSizeOf(bus.PageOffset);
        const last_page = (addresses.end - 1) >> @bitSizeOf(bus.PageOffset);

        for (first_page .. last_page + 1) |page| {
            const page_data_handle = a.page_lookup.get(@intCast(bus.Page, page)) orelse unreachable;
            page_chunks[page_data_handle].append(a.gpa, chunk) catch @panic("OOM");
            if (chunk.section) |section| {
                if (page_sections[page_data_handle] != section) {
                    const token = a.getSource(chunk.file).instructions.items(.token)[chunk.instructions.begin];
                    a.errors.append(a.gpa, Error.init(a.gpa, chunk.file, token,
                        "Chunk starting here was allocated to page 0x{X:0>5}, but that page is in use by another section", .{ page }, .{})
                    ) catch @panic("OOM");
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
        const chunk_range = chunk.getAddressRange(a);
        const check_against = chunks[i + 1 ..];
        for (check_against) |other_chunk| {
            const other_range = other_chunk.getAddressRange(a);
            if (chunk_range.begin >= other_range.end or chunk_range.end <= other_range.begin) continue;
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

    for (0.., s.insn.items(.operation)) |insn_handle, op| {
        switch (op) {
            .none, .insn, .org, .@"align", .keep, .def, .undef,
            .section, .code, .kcode, .entry, .kentry, .data, .kdata, .@"const", .kconst, .stack,
            => {},

            .bound_insn => |encoding| {
                const address = insn_addresses[insn_handle];
                const length = insn_lengths[insn_handle];
                const params = insn_params[insn_handle];
                const page = @truncate(bus.Page, address >> @bitSizeOf(bus.PageOffset));
                const offset = @truncate(bus.PageOffset, address);
                const page_data_handle = a.page_lookup.get(page) orelse continue;
                var buffer = page_datas[page_data_handle][offset..];

                const insn = a.buildInstruction(s, address, encoding.mnemonic, encoding.suffix, params, false).?;

                if (length > buffer.len) {
                    if (@truncate(u1, address) == 1) {
                        a.recordError(file.handle, file.instructions.items(.token)[insn_handle], "Instruction crosses page boundary and is not word aligned", .{});
                    }
                    var temp = [_]u8{0} ** 8;
                    const temp_insn = ie.encodeInstruction(insn, encoding.*, &temp);
                    std.mem.copy(u8, buffer, temp_insn[0..buffer.len]);

                    const next_page = page + 1;
                    _ = next_page;
                    const next_page_data_handle = a.page_lookup.get(page) orelse unreachable;
                    const next_page_buf = &page_datas[next_page_data_handle];
                    std.mem.copy(u8, next_page_buf, temp_insn[buffer.len..]);
                } else {
                    _ = ie.encodeInstruction(insn, encoding.*, buffer);
                }
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
            .push => {
                // TODO stack sections
            },
            .pop => {
                // TODO stack sections
            },
        }
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
    var constant_bytes = std.mem.alignForward(expr_constant.bit_count, granularity_bytes * 8) / 8;
    if (skip_bytes >= constant_bytes) {
        return constant_bytes;
    }

    var iter = expr_constant.byteIterator(.signed);
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