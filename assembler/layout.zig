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

    var errors = a.errors.items;
    var i = errors.len;
    while (i > 0) {
        i -= 1;
        if (errors[i].flags.contains(.remove_on_layout_reset)) {
            _ = a.errors.swapRemove(i);
        }
    }

    var insn_encoding_errors = a.insn_encoding_errors.items;
    i = insn_encoding_errors.len;
    while (i > 0) {
        i -= 1;
        if (insn_encoding_errors[i].flags.contains(.remove_on_layout_reset)) {
            _ = a.insn_encoding_errors.swapRemove(i);
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
                if (resolveExpressionConstant(a, file, chunk.file, initial_address, expr_handle)) |constant| {
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
                // TODO .db
            },
            .dw => {
                // TODO handle alignment
                // TODO .dw
            },
            .dd => {
                // TODO handle alignment
                // TODO .dd
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

pub fn resolveExpressionConstant(a: *Assembler, file: *SourceFile, file_handle: SourceFile.Handle, ip: u32, expr_handle: Expression.Handle) ?*const Constant {
    var expr_resolved_constants = file.expressions.items(.resolved_constant);
    if (expr_resolved_constants[expr_handle]) |constant| {
        return constant;
    }

    var expr_infos = file.expressions.items(.info);
    const expr_tokens = file.expressions.items(.token);
    const expr_resolved_types = file.expressions.items(.resolved_type);

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
            switch (expr_resolved_types[expr_handle]) {
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
            const inner_constant = resolveExpressionConstant(a, file, file_handle, ip, inner_expr) orelse return null;
            const value = inner_constant.asIntNegated() catch {
                a.recordExpressionLayoutError(file_handle, expr_handle, "Overflow (constant too large)", .{});
                return null;
            };
            const new_constant = Constant.initInt(value, null);
            expr_resolved_constants[expr_handle] = new_constant.intern(a.arena, a.gpa, &a.constants);
        },
        .complement => |inner_expr| {
            const inner = resolveExpressionConstant(a, file, file_handle, ip, inner_expr) orelse return null;
            const result = inner.complement(a.gpa, &a.constant_temp);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .plus, .minus, .multiply, .shl, .shr, => |bin| {
            const left = resolveExpressionConstant(a, file, file_handle, ip, bin.left) orelse return null;
            const right = resolveExpressionConstant(a, file, file_handle, ip, bin.right) orelse return null;
            const lv = left.asInt() catch {
                a.recordExpressionLayoutError(file_handle, bin.left, "Overflow (constant too large)", .{});
                return null;
            };
            const rv = right.asInt() catch {
                a.recordExpressionLayoutError(file_handle, bin.right, "Overflow (constant too large)", .{});
                return null;
            };

            if (info == .shl or info == .shr) {
                _ = std.math.cast(u6, rv) orelse {
                    a.recordExpressionLayoutError(file_handle, expr_handle, "Overflow (constant too large)", .{});
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
                a.recordExpressionLayoutError(file_handle, expr_handle, "Overflow (constant too large)", .{});
                return null;
            };
            const new_constant = Constant.initInt(value, null);
            expr_resolved_constants[expr_handle] = new_constant.intern(a.arena, a.gpa, &a.constants);
        },
        .concat, .bitwise_or, .bitwise_xor, .bitwise_and => |bin| {
            const left = resolveExpressionConstant(a, file, file_handle, ip, bin.left) orelse return null;
            const right = resolveExpressionConstant(a, file, file_handle, ip, bin.right) orelse return null;
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
            const left = resolveExpressionConstant(a, file, file_handle, ip, bin.left) orelse return null;
            const right = resolveExpressionConstant(a, file, file_handle, ip, bin.right) orelse return null;
            const times = right.asInt() catch {
                a.recordExpressionLayoutError(file_handle, bin.right, "Overflow (constant too large)", .{});
                return null;
            };
            const result = left.repeat(a.gpa, &a.constant_temp, times);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .length_cast, .truncate, .sign_extend => |bin| {
            const left = resolveExpressionConstant(a, file, file_handle, ip, bin.left) orelse return null;
            const right = resolveExpressionConstant(a, file, file_handle, ip, bin.right) orelse return null;
            const width = right.asInt() catch {
                a.recordExpressionLayoutError(file_handle, bin.right, "Overflow (constant too large)", .{});
                return null;
            };
            if (width < 0) {
                a.recordExpressionLayoutError(file_handle, bin.right, "Width must not be negative", .{});
                return null;
            }
            const result = left.cloneWithLength(a.gpa, &a.constant_temp, @intCast(u64, width), .signed);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .zero_extend => |bin| {
            const left = resolveExpressionConstant(a, file, file_handle, ip, bin.left) orelse return null;
            const right = resolveExpressionConstant(a, file, file_handle, ip, bin.right) orelse return null;
            const width = right.asInt() catch {
                a.recordExpressionLayoutError(file_handle, bin.right, "Overflow (constant too large)", .{});
                return null;
            };
            if (width < 0) {
                a.recordExpressionLayoutError(file_handle, bin.right, "Width must not be negative", .{});
                return null;
            }
            const result = left.cloneWithLength(a.gpa, &a.constant_temp, @intCast(u64, width), .unsigned);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .signed_cast, .unsigned_cast, .maybe_signed_cast,
        .data_address_cast, .insn_address_cast, .stack_address_cast, .remove_address_cast,
         => |inner_expr| {
            expr_resolved_constants[expr_handle] = resolveExpressionConstant(a, file, file_handle, ip, inner_expr);
        },
        .absolute_address_cast => |inner_expr| {
            const inner = resolveExpressionConstant(a, file, file_handle, ip, inner_expr) orelse return null;
            const relative = inner.asInt() catch {
                a.recordExpressionLayoutError(file_handle, inner_expr, "Relative address too large", .{});
                return null;
            };
            const absolute = std.math.add(i64, relative, ip) catch {
                a.recordExpressionLayoutError(file_handle, inner_expr, "Absolute address overflows u32", .{});
                return null;
            };
            const absolute_u32 = std.math.cast(u32, absolute) orelse {
                a.recordExpressionLayoutError(file_handle, inner_expr, "Absolute address overflows u32", .{});
                return null;
            };
            const result = Constant.initInt(absolute_u32, null);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
    }

    return expr_resolved_constants[expr_handle];
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
                unreachable;
                //expr_resolved_constants[expr_handle] = &Constant.builtin.zero;
            },
        }
    } else unreachable;
}

fn resolveInstructionEncoding(a: *Assembler, file: *SourceFile, file_handle: SourceFile.Handle, ip: u32, insn_handle: Instruction.Handle, op: *Instruction.Operation, params: ?Expression.Handle) u3 {
    const insn = a.buildInstruction(file, file_handle, ip, op.insn.mnemonic, op.insn.suffix, params, true) orelse return 0;
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
    if (file.instructions.items(.flags)[insn_handle].contains(.encoding_depends_on_layout)) {
        err_flags.insert(.remove_on_layout_reset);
    }
    a.recordInsnEncodingError(file_handle, insn_handle, err_flags);
    return 0;
}

pub fn encodePageData(a: *Assembler, file: *SourceFile, file_handle: SourceFile.Handle) void {
    const insn_operations = file.instructions.items(.operation);
    const insn_params = file.instructions.items(.params);
    const insn_addresses = file.instructions.items(.address);
    const insn_lengths = file.instructions.items(.length);

    var page_datas = a.pages.items(.data);

    for (0.., insn_operations) |insn_handle, op| {
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
                const page_data_handle = a.page_lookup.get(page) orelse unreachable;
                var buffer = page_datas[page_data_handle][offset..];

                const insn = a.buildInstruction(file, file_handle, address, encoding.mnemonic, encoding.suffix, params, false).?;

                if (length > buffer.len) {
                    if (@truncate(u1, address) == 1) {
                        a.recordError(file_handle, file.instructions.items(.token)[insn_handle], "Instruction crosses page boundary and is not word aligned", .{});
                    }
                    var temp = [_]u8{0} ** 8;
                    const temp_insn = ie.encodeInstruction(insn, encoding.*, &temp);
                    std.mem.copy(u8, buffer, temp_insn[0..buffer.len]);

                    const next_page = page + 1;
                    _ = next_page;
                    const next_page_data_handle = a.page_lookup.get(page) orelse unreachable;
                    const next_page_buf = page_datas[next_page_data_handle];
                    std.mem.copy(u8, next_page_buf, temp_insn[buffer.len..]);
                } else {
                    _ = ie.encodeInstruction(insn, encoding.*, buffer);
                }
            },

            .db => {
                // TODO .db
            },
            .dw => {
                // TODO .dw
            },
            .dd => {
                // TODO .dd
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
