pub fn do_fixed_org_layout(a: *Assembler, chunks: []Source_File.Chunk) bool {
    var layout_changed = false;
    for (chunks) |chunk| {
        const initial_address = resolve_fixed_org_address(a, chunk);
        // std.debug.print("{X:0>16}: Placing fixed chunk\n", .{ initial_address });
        if (do_chunk_layout(a, chunk, initial_address)) {
            layout_changed = true;
        }
    }
    return layout_changed;
}

fn resolve_fixed_org_address(a: *Assembler, chunk: Source_File.Chunk) u32 {
    var initial_address: u32 = 0;
    if (chunk.section) |section_handle| {
        const range = a.get_section(section_handle).address_range();
        initial_address = range.first;

        var file = a.get_source(chunk.file);
        const s = file.slices();
        const operations = s.insn.items(.operation);
        var iter = chunk.instructions;
        while (iter.next()) |insn_handle| {
            if (operations[insn_handle] == .org) {
                initial_address = s.insn.items(.address)[insn_handle];
                if (s.insn.items(.params)[insn_handle]) |expr_handle| {
                    if (resolve_expression_constant(a, s, initial_address, expr_handle)) |constant| {
                        initial_address = constant.as_int(u32) catch {
                            a.record_expr_layout_error(chunk.file, expr_handle, expr_handle, ".org address too large", .{});
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

pub fn do_auto_org_layout(a: *Assembler, chunks: []Source_File.Chunk) bool {
    var layout_changed = false;
    for (chunks) |chunk| {
        const initial_address = resolve_auto_org_address(a, chunk);
        // std.debug.print("{X:0>16}: Placing auto chunk\n", .{ initial_address });
        if (do_chunk_layout(a, chunk, initial_address)) {
            layout_changed = true;
        }
    }
    return layout_changed;
}

fn resolve_auto_org_address(a: *Assembler, chunk: Source_File.Chunk) u32 {
    const chunk_section_handle = chunk.section orelse return 0;
    const section = a.get_section(chunk_section_handle);

    const address_range = chunk.address_range(a);
    const chunk_size = @max(1, address_range.len);

    const desired_access = section.kind.access_policies(chunk_size);

    var alignment = get_alignment_for_chunk(a, chunk);
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

    if (chunk_size < Page_Data.page_size) {
        // See if there's an existing semi-full chunk that we can reuse
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
                const address = bits.concat(.{
                    arch.addr.Page.Offset.init(@intCast(raw_unused_range_begin)).raw(),
                    page.raw(),
                });
                const aligned_address = apply_alignment(address, alignment.modulo, alignment.offset);

                if (arch.addr.Page.init(@intCast(aligned_address >> @bitSizeOf(arch.addr.Page.Offset))).raw() != page.raw()) break;
                const unused_range_begin = arch.addr.Page.Offset.init(@truncate(aligned_address));

                used.setRangeValue(.{
                    .start = begin,
                    .end = unused_range_begin.raw(),
                }, false);

                const unused_range_end = used.findFirstSet() orelse Page_Data.page_size;
                const range_size = unused_range_end - unused_range_begin.raw();
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
    const full_pages_needed = chunk_size / Page_Data.page_size;
    const final_page_bytes_needed = chunk_size - (full_pages_needed * Page_Data.page_size);

    // std.debug.print("Need {} full pages and {} extra bytes\n", .{ full_pages_needed, final_page_bytes_needed });

    const allowed_range = a.get_section(chunk_section_handle).address_range();

    const search_range_begin: usize = allowed_range.first >> @bitSizeOf(arch.addr.Page.Offset);
    const search_range_end: usize = (allowed_range.first + allowed_range.len) >> @bitSizeOf(arch.addr.Page.Offset);

    var initial_page = search_range_begin;
    for (search_range_begin..search_range_end) |page_usize| {
        const page = arch.addr.Page.init(@intCast(page_usize));
        if (page_usize - initial_page >= full_pages_needed) {
            if (final_page_bytes_needed == 0) {
                break;
            } else if (a.page_lookup.get(page)) |page_data_handle| {
                // Currently we never accept a partially filled last page for .entry_kernel sections since the
                // access policies might be wrong.  Theoretically this may result in slightly more fragmentation in
                // corner cases, but it's not worth the extra complexity to handle it IMO.
                if (section.kind != .entry_kernel and page_sections[page_data_handle] == chunk_section_handle) {
                    const final_page_free_bytes = page_usages[page_data_handle].findFirstSet() orelse Page_Data.page_size;
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

    return @as(u32, @intCast(initial_page)) << @bitSizeOf(arch.addr.Page.Offset);
}

fn get_alignment_for_chunk(a: *Assembler, chunk: Source_File.Chunk) Assembler.Alignment {
    var file = a.get_source(chunk.file);
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
            .none, .nil, .org, .keep, .def, .undef, .local, .range,
            .section, .boot, .code, .kcode, .entry, .kentry,
            .data, .kdata, .@"const", .kconst, .stack,
            => {},
            .@"align" => if (get_resolved_alignment(s, insn_params[insn_handle])) |resolved_align| {
                alignment = resolved_align;
            },
            .db, .zb, .push, .pop, .insn, .bound_insn => break,
            .dh, .zh => {
                if ((alignment.modulo & 1) != 0) {
                    alignment.modulo *= 2;
                }
                if ((alignment.offset & 1) != 0) {
                    alignment.offset = (alignment.offset + 1) % alignment.modulo;
                }
                break;
            },
            .dw, .zw => {
                if ((alignment.modulo & 3) != 0) {
                    alignment.modulo *= 4;
                }
                if ((alignment.offset & 3) != 0) {
                    alignment.offset = (alignment.offset - (alignment.offset & 3) + 4) % alignment.modulo;
                }
                break;
            },
        }
    }
    return alignment;
}

fn do_chunk_layout(a: *Assembler, chunk: Source_File.Chunk, initial_address: u32) bool {
    var file = a.get_source(chunk.file);
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
        const new_insn_address = address;
        switch (insn_operations[insn_handle]) {
            .none, .nil, .org, .keep, .def, .undef, .local, .range,
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
                        .none, .nil, .org, .keep, .def, .undef, .local, .range,
                        .section, .boot, .code, .kcode, .entry, .kentry,
                        .data, .kdata, .@"const", .kconst, .stack => {},

                        .@"align" => {
                            if (insn_params[insn2]) |align_expr| {
                                address = resolve_and_apply_alignment(a, s, address, align_expr, first, check_for_alignment_holes, insn2, chunk.section);
                            }
                        },

                        .dh, .zh => {
                            address = apply_alignment(address, 2, 0);
                            break;
                        },

                        .dw, .zw => {
                            address = apply_alignment(address, 4, 0);
                            break;
                        },

                        .insn, .bound_insn, .push, .pop, .db, .zb => break,
                    }
                }
            },
            .insn => {
                const length = resolve_insn_form(a, s, address, insn_handle, &insn_operations[insn_handle], insn_params[insn_handle]);
                insn_lengths[insn_handle] = length;
                address += length;
                layout_changed = true;
                check_for_alignment_holes = true;
            },
            .bound_insn => |id| {
                if (insn_flags[insn_handle].contains(.depends_on_layout)) {
                    const old_form = iedb.get(id);
                    insn_operations[insn_handle] = .{ .insn = old_form.signature.mnemonic };
                    const length = resolve_insn_form(a, s, address, insn_handle, &insn_operations[insn_handle], insn_params[insn_handle]);
                    address += length;
                    switch (insn_operations[insn_handle]) {
                        .bound_insn => |new_id| if (id != new_id) {
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
                address += resolve_data_directive_length(a, s, address, insn_handle, 1);
                check_for_alignment_holes = true;
            },
            .zb => {
                address += resolve_zeroed_data_directive_length(a, s, address, insn_handle, 1);
                check_for_alignment_holes = true;
            },
            .dh => {
                address = check_and_apply_alignment(a, s, address, 2, 0, check_for_alignment_holes, insn_handle, chunk.section);
                address += resolve_data_directive_length(a, s, address, insn_handle, 2);
                check_for_alignment_holes = true;
            },
            .zh => {
                address = check_and_apply_alignment(a, s, address, 2, 0, check_for_alignment_holes, insn_handle, chunk.section);
                address += resolve_zeroed_data_directive_length(a, s, address, insn_handle, 2);
                check_for_alignment_holes = true;
            },
            .dw => {
                address = check_and_apply_alignment(a, s, address, 2, 0, check_for_alignment_holes, insn_handle, chunk.section);
                address += resolve_data_directive_length(a, s, address, insn_handle, 4);
                check_for_alignment_holes = true;
            },
            .zw => {
                address = check_and_apply_alignment(a, s, address, 2, 0, check_for_alignment_holes, insn_handle, chunk.section);
                address += resolve_zeroed_data_directive_length(a, s, address, insn_handle, 4);
                check_for_alignment_holes = true;
            },
            .push => {
                address += resolve_push_pop_directive_length(a, s, insn_handle, .push);
                check_for_alignment_holes = true;
            },
            .pop => {
                address += resolve_push_pop_directive_length(a, s, insn_handle, .pop);
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
        const initial_page = initial_address >> @bitSizeOf(arch.addr.Page.Offset);
        const final_page = if (address == initial_address) initial_page else (address - 1) >> @bitSizeOf(arch.addr.Page.Offset);

        const section = a.get_section(section_handle);
        var access = section.kind.access_policies(chunk_bytes);

        for (initial_page .. final_page + 1) |page| {
            const page_data_handle = a.find_or_create_page(arch.addr.Page.init(@intCast(page)), access, section_handle);
            var page_usage = a.pages.items(.usage);
            if (page == initial_page or page == final_page) {
                const range = std.bit_set.Range{
                    .start = if (page == initial_page) arch.addr.Page.Offset.init(@truncate(initial_address)).raw() else 0,
                    .end = if (page == final_page) arch.addr.Page.Offset.init(@truncate(address)).raw() else Page_Data.page_size,
                };
                page_usage[page_data_handle].setRangeValue(range, true);
                // std.debug.print("{X:0>13}: ({}) Marking page used: {} - {}\n", .{ page, page_data_handle, range.start, range.end });
            } else {
                // std.debug.print("{X:0>13}: ({}) Marking page fully used\n", .{ page, page_data_handle });
                page_usage[page_data_handle] = Page_Data.Usage_Bitmap.initFull();
            }
            if (page == initial_page and section.kind == .entry_kernel) {
                // only the first page of an entry_kernel chunk should be (partially) callable
                access = Section.Kind.code_kernel.access_policies(chunk_bytes);
            }
        }
    }

    return layout_changed;
}

pub fn get_resolved_alignment(s: Source_File.Slices, maybe_align_expr: ?Expression.Handle) ?Assembler.Alignment {
    const align_expr = maybe_align_expr orelse return null;

    const expr_constants = s.expr.items(.resolved_constant);

    var alignment = Assembler.Alignment{
        .modulo = 0,
        .offset = 0,
    };
    switch (s.expr.items(.info)[align_expr]) {
        .list => |bin| {
            if (expr_constants[bin.left]) |constant| {
                alignment.modulo = constant.as_int(u32) catch return null;
            }
            if (expr_constants[bin.right]) |constant| {
                alignment.offset = constant.as_int(u32) catch return null;
            }
        },
        else => {
            if (expr_constants[align_expr]) |constant| {
                alignment.modulo = constant.as_int(u32) catch return null;
            }
        }
    }

    if (alignment.modulo <= 1 or alignment.offset >= alignment.modulo or @popCount(alignment.modulo) != 1) {
        return null;
    }

    return alignment;
}

fn resolve_and_apply_alignment(
    a: *Assembler,
    s: Source_File.Slices,
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
            if (resolve_expression_constant(a, s, address, bin.left)) |constant| {
                alignment = constant.as_int(u32) catch {
                    if (report_errors) {
                        const err_flags = Error.Flag_Set.initOne(.remove_on_layout_reset);
                        a.record_expr_error(s.file.handle, bin.left, "Overflow (alignment must fit in u32)", err_flags);
                    }
                    return address;
                };
            }
            if (resolve_expression_constant(a, s, address, bin.right)) |constant| {
                offset = constant.as_int(u32) catch {
                    if (report_errors) {
                        const err_flags = Error.Flag_Set.initOne(.remove_on_layout_reset);
                        a.record_expr_error(s.file.handle, bin.right, "Overflow (offset must fit in u32)", err_flags);
                    }
                    return address;
                };
                if (offset >= alignment or offset < 0) {
                    if (report_errors) {
                        const err_flags = Error.Flag_Set.initOne(.remove_on_layout_reset);
                        a.record_expr_error(s.file.handle, bin.right, "Invalid offset (must be less than alignment)", err_flags);
                    }
                    return address;
                }
            }
        },
        else => {
            if (resolve_expression_constant(a, s, address, align_expr)) |constant| {
                alignment = constant.as_int(u32) catch {
                    if (report_errors) {
                        const err_flags = Error.Flag_Set.initOne(.remove_on_layout_reset);
                        a.record_expr_error(s.file.handle, align_expr, "Overflow (alignment too must fit in u32)", err_flags);
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
            const err_flags = Error.Flag_Set.initOne(.remove_on_layout_reset);
            a.record_expr_error(s.file.handle, alignment_expr, "Invalid alignment (must be power of 2)", err_flags);
        }
        return address;
    }

    if (report_errors) {
        return check_and_apply_alignment(a, s, address, alignment, @intCast(offset), check_for_alignment_holes, insn_handle, section_handle);
    } else {
        return apply_alignment(address, alignment, @intCast(offset));
    }
}

fn apply_alignment(address: u32, alignment: u32, offset: u32) u32 {
    var new_address_usize = std.mem.alignBackward(u32, address, alignment) + offset;
    if (new_address_usize < address) {
        new_address_usize += alignment;
    }

    return std.math.cast(u32, new_address_usize) orelse address;
}

fn check_and_apply_alignment(
    a: *Assembler,
    s: Source_File.Slices,
    address: u32,
    alignment: u32,
    offset: u32,
    check_for_alignment_holes: bool,
    insn_handle: Instruction.Handle,
    section_handle: ?Section.Handle
) u32 {
    var new_address_usize = std.mem.alignBackward(u32, address, alignment) + offset;
    if (new_address_usize < address) {
        new_address_usize += alignment;
    }

    const new_address = std.math.cast(u32, new_address_usize) orelse {
        const err_flags = Error.Flag_Set.initOne(.remove_on_layout_reset);
        a.record_insn_error(s.file.handle, insn_handle, "Aligned address overflow", err_flags);
        return address;
    };

    if (new_address != address and check_for_alignment_holes) {
        if (section_handle) |section| switch (a.get_section(section).kind) {
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
                const err_flags = Error.Flag_Set.initOne(.remove_on_layout_reset);
                a.record_insn_error(s.file.handle, insn_handle, "Alignment not satisfied within instruction stream (try using NOP, NOPE, or an unconditional branch before this point)", err_flags);
            },
        };
    }

    return new_address;
}

fn resolve_push_pop_directive_length(a: *Assembler, s: Source_File.Slices, insn_handle: Instruction.Handle, comptime op: Instruction.Operation_Type) u32 {
    const insn_flags = s.insn.items(.flags);
    const insn_lengths = s.insn.items(.length);

    const flagset = insn_flags[insn_handle];

    if (flagset.contains(.length_computed)) {
        return insn_lengths[insn_handle];
    }

    const stack_size = symbols.compute_push_or_pop_size(a, s, s.insn.items(.params)[insn_handle]);
    s.insn.items(.operation)[insn_handle] = @unionInit(Instruction.Operation, @tagName(op), stack_size);

    const insn = isa.Instruction{
        .mnemonic = switch (op) {
            .push => .frame,
            .pop => .unframe,
            else => unreachable,
        },
        .params = &.{
            .{
                .signature = Expression.Type.constant().param_signature(),
                .base_register = .init(0),
                .offset_register = .init(0),
                .constant = stack_size,
            },
        },
    };
    const maybe_form = find_best_insn_form(a, insn);

    var length: u32 = 0;
    if (maybe_form) |form| {
        length = form.len();
    } else {
        var flags = Error.Flag_Set.initEmpty();
        if (flagset.contains(.depends_on_layout)) {
            flags.insert(.remove_on_layout_reset);
        }
        a.record_insn_error(s.file.handle, insn_handle, "Stack frame too large", flags);
    }

    insn_lengths[insn_handle] = length;

    if (!flagset.contains(.depends_on_layout)) {
        insn_flags[insn_handle].insert(.length_computed);
    }

    return length;
}

fn resolve_zeroed_data_directive_length(a: *Assembler, s: Source_File.Slices, ip: u32, insn_handle: Instruction.Handle, granularity_bytes: u8) u32 {
    const insn_flags = s.insn.items(.flags);
    const insn_lengths = s.insn.items(.length);

    const flagset = insn_flags[insn_handle];

    if (flagset.contains(.length_computed)) {
        return insn_lengths[insn_handle];
    }

    var length: u32 = granularity_bytes;

    if (s.insn.items(.params)[insn_handle]) |params_expr_handle| {
        length = if (resolve_expression_constant(a, s, ip, params_expr_handle)) |constant| 
            std.math.mul(u32, constant.as_int(u32) catch 1, granularity_bytes) catch granularity_bytes else 1;
    }

    insn_lengths[insn_handle] = length;

    if (!flagset.contains(.depends_on_layout)) {
        insn_flags[insn_handle].insert(.length_computed);
    }

    return length;
}

fn resolve_data_directive_length(a: *Assembler, s: Source_File.Slices, ip: u32, insn_handle: Instruction.Handle, granularity_bytes: u8) u32 {
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
                length += resolve_data_expression_length(a, s, ip, bin.left, granularity_bytes);
                expr_handle = bin.right;
            },
            else => {
                length += resolve_data_expression_length(a, s, ip, expr_handle, granularity_bytes);
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

fn resolve_data_expression_length(a: *Assembler, s: Source_File.Slices, ip: u32, expr_handle: Expression.Handle, granularity_bytes: u8) u32 {
    if (resolve_expression_constant(a, s, ip, expr_handle)) |constant| {
        return std.mem.alignForward(u32, @intCast(constant.bits()), granularity_bytes * 8) / 8;
    } else return 0;
}

pub fn resolve_expression_constant_or_default(a: *Assembler, s: Source_File.Slices, ip: u32, expr_handle: Expression.Handle, default: anytype) Constant {
    const resolved = resolve_expression_constant(a, s, ip, expr_handle);
    if (resolved) |constant| return constant.*;

    return switch (@typeInfo(@TypeOf(default))) {
        .int => Constant.init_int(default),
        .comptime_int => Constant.init_int(@as(i64, default)),
        .pointer => |ptr_info| if(ptr_info.size == .Slice) Constant.init_string(default) else default.*,
        .@"struct" => default,
        else => @compileError("Unsupported default value"),
    };
}

pub fn resolve_expression_constant(a: *Assembler, s: Source_File.Slices, ip: u32, expr_handle: Expression.Handle) ?*const Constant {
    var expr_resolved_constants = s.expr.items(.resolved_constant);
    if (expr_resolved_constants[expr_handle]) |constant| {
        return constant;
    }

    const expr_infos = s.expr.items(.info);
    const info = expr_infos[expr_handle];
    switch (info) {
        .local_label_def => unreachable,

        .literal_symbol_def, .directive_symbol_def,
        .literal_int, .literal_str, .reg_to_index,
        => return null,

        .list, .literal_reg,
        .index_to_reg,
        => return null,

        .literal_current_address => {
            var value = ip;
            switch (s.expr.items(.resolved_type)[expr_handle]) {
                .raw, .data, .insn => |bot| if (bot.base == .sr and bot.base.sr == .ip) { value = 0; },
                .stack => |bot| if (bot.base == .sr and bot.base.sr == .sp) { value = 0; },
                else => {},
            }
            const constant = Constant.init_int(value);
            expr_resolved_constants[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
        },

        .literal_symbol_ref => {
            const token_handle = s.expr.items(.token)[expr_handle];
            const raw_symbol = s.file.tokens.get(token_handle).span(s.file.source);
            const symbol_constant = Constant.init_symbol_literal(a.gpa, &a.constant_temp, raw_symbol);
            resolve_symbol_ref_expr_constant(a, s, ip, expr_handle, token_handle, symbol_constant);
        },
        .directive_symbol_ref => |inner_expr| {
            const token_handle = s.expr.items(.token)[inner_expr];
            const symbol_constant = expr_resolved_constants[inner_expr].?;
            resolve_symbol_ref_expr_constant(a, s, ip, expr_handle, token_handle, symbol_constant.*);
        },

        .negate => |inner_expr| {
            const right = resolve_expression_constant(a, s, ip, inner_expr) orelse return null;
            const zero = Constant.init_int(@as(u1, 0));
            const result = zero.binary_op(a.gpa, &a.constant_temp, right.*, .subtract);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .complement => |inner_expr| {
            const inner = resolve_expression_constant(a, s, ip, inner_expr) orelse return null;
            const result = inner.complement(a.gpa, &a.constant_temp);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .plus => |bin| {
            const left = resolve_expression_constant_or_default(a, s, ip, bin.left, 0);
            const right = resolve_expression_constant_or_default(a, s, ip, bin.right, 0);
            const result = left.binary_op(a.gpa, &a.constant_temp, right, .add);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .minus => |bin| {
            const left = resolve_expression_constant_or_default(a, s, ip, bin.left, 0);
            const right = resolve_expression_constant_or_default(a, s, ip, bin.right, 0);
            const result = left.binary_op(a.gpa, &a.constant_temp, right, .subtract);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .multiply, .shl, .shr, => |bin| {
            const left = resolve_expression_constant(a, s, ip, bin.left) orelse return null;
            const right = resolve_expression_constant(a, s, ip, bin.right) orelse return null;

            const lv = left.as_int(i64) catch {
                a.record_expr_layout_error(s.file.handle, expr_handle, bin.left, "Overflow (constant too large)", .{});
                return null;
            };
            const rv = right.as_int(i64) catch {
                a.record_expr_layout_error(s.file.handle, expr_handle, bin.right, "Overflow (constant too large)", .{});
                return null;
            };

            if (info == .shl or info == .shr) {
                _ = std.math.cast(u6, rv) orelse {
                    a.record_expr_layout_error(s.file.handle, expr_handle, expr_handle, "Overflow (constant too large)", .{});
                };
            }

            const value = switch (info) {
                .multiply => std.math.mul(i64, lv, rv),
                .shl => std.math.shlExact(i64, lv, @intCast(rv)),
                .shr => lv >> @intCast(rv),
                else => unreachable,
            } catch {
                a.record_expr_layout_error(s.file.handle, expr_handle, expr_handle, "Overflow (constant too large)", .{});
                return null;
            };
            const new_constant = Constant.init_int(value);
            expr_resolved_constants[expr_handle] = new_constant.intern(a.arena, a.gpa, &a.constants);
        },
        .concat => |bin| {
            const left = resolve_expression_constant(a, s, ip, bin.left) orelse return null;
            const right = resolve_expression_constant(a, s, ip, bin.right) orelse return null;
            const result = left.concat(a.gpa, &a.constant_temp, right.*);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .bitwise_or => |bin| {
            const left = resolve_expression_constant(a, s, ip, bin.left) orelse return null;
            const right = resolve_expression_constant(a, s, ip, bin.right) orelse return null;
            const result = left.binary_op(a.gpa, &a.constant_temp, right.*, .bitwise_or);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .bitwise_xor => |bin| {
            const left = resolve_expression_constant(a, s, ip, bin.left) orelse return null;
            const right = resolve_expression_constant(a, s, ip, bin.right) orelse return null;
            const result = left.binary_op(a.gpa, &a.constant_temp, right.*, .bitwise_xor);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .bitwise_and => |bin| {
            const left = resolve_expression_constant(a, s, ip, bin.left) orelse return null;
            const right = resolve_expression_constant(a, s, ip, bin.right) orelse return null;
            const result = left.binary_op(a.gpa, &a.constant_temp, right.*, .bitwise_and);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .concat_repeat => |bin| {
            const left = resolve_expression_constant(a, s, ip, bin.left) orelse return null;
            const right = resolve_expression_constant(a, s, ip, bin.right) orelse return null;
            const times = right.as_int(u63) catch {
                a.record_expr_layout_error(s.file.handle, expr_handle, bin.right, "Overflow (repeat count must fit in u63)", .{});
                return null;
            };
            const result = left.repeat(a.gpa, &a.constant_temp, times);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .length_cast => |bin| {
            const left = resolve_expression_constant(a, s, ip, bin.left) orelse return null;
            const right = resolve_expression_constant(a, s, ip, bin.right) orelse return null;
            const width = right.as_int(u63) catch {
                a.record_expr_layout_error(s.file.handle, expr_handle, bin.right, "Overflow (constant width must fit in u63)", .{});
                return null;
            };
            const result = left.clone_with_length(a.gpa, &a.constant_temp, width) catch {
                a.record_expr_layout_error(s.file.handle, expr_handle, bin.right, "Truncation would change constant value (consider using .trunc instead)", .{});
                return null;
            };
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .truncate => |bin| {
            const left = resolve_expression_constant(a, s, ip, bin.left) orelse return null;
            const right = resolve_expression_constant(a, s, ip, bin.right) orelse return null;
            const width = right.as_int(u63) catch {
                a.record_expr_layout_error(s.file.handle, expr_handle, bin.right, "Overflow (constant width must fit in u63)", .{});
                return null;
            };
            const result = left.truncate(width);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .sign_extend => |bin| {
            const left = resolve_expression_constant(a, s, ip, bin.left) orelse return null;
            const right = resolve_expression_constant(a, s, ip, bin.right) orelse return null;
            const width = right.as_int(u63) catch {
                a.record_expr_layout_error(s.file.handle, expr_handle, bin.right, "Overflow (constant width must fit in u63)", .{});
                return null;
            };
            const result = left.extend(a.gpa, &a.constant_temp, width, .signed) catch {
                a.record_expr_layout_error(s.file.handle, expr_handle, bin.right, "Constant is already wider than requested (consider using .trunc instead)", .{});
                return null;
            };
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .zero_extend => |bin| {
            const left = resolve_expression_constant(a, s, ip, bin.left) orelse return null;
            const right = resolve_expression_constant(a, s, ip, bin.right) orelse return null;
            const width = right.as_int(u63) catch {
                a.record_expr_layout_error(s.file.handle, expr_handle, bin.right, "Overflow (constant width must fit in u63)", .{});
                return null;
            };
            const result = left.extend(a.gpa, &a.constant_temp, width, .unsigned) catch {
                a.record_expr_layout_error(s.file.handle, expr_handle, bin.right, "Constant is already wider than requested (consider using .trunc instead)", .{});
                return null;
            };
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .lf_cast => |inner_expr| {
            const constant = resolve_expression_constant(a, s, ip, inner_expr) orelse return null;
            a.constant_temp.clearRetainingCapacity();
            var remaining = constant.as_string();
            var bytes_removed: u63 = 0;
            while (std.mem.indexOfAny(u8, remaining, "\r\n")) |idx| {
                a.constant_temp.appendSlice(a.gpa, remaining[0..idx]) catch @panic("OOM");
                a.constant_temp.appendSlice(a.gpa, "\n") catch @panic("OOM");
                var end_of_line = idx + 1;
                switch (remaining[idx]) {
                    '\r' => {
                        if (idx + 1 < remaining.len and remaining[idx + 1] == '\n') {
                            end_of_line += 1;
                            bytes_removed += 1;
                        }
                    },
                    '\n' => {},
                    else => unreachable,
                }
                remaining = remaining[end_of_line..];
            }
            a.constant_temp.appendSlice(a.gpa, remaining) catch @panic("OOM");
            const result = Constant.init_string_bits(a.constant_temp.items, constant.bits() - bytes_removed * 8, constant.signedness()) catch unreachable;
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .crlf_cast => |inner_expr| {
            const constant = resolve_expression_constant(a, s, ip, inner_expr) orelse return null;
            a.constant_temp.clearRetainingCapacity();
            var remaining = constant.as_string();
            var bytes_added: u63 = 0;
            while (std.mem.indexOfAny(u8, remaining, "\r\n")) |idx| {
                a.constant_temp.appendSlice(a.gpa, remaining[0..idx]) catch @panic("OOM");
                a.constant_temp.appendSlice(a.gpa, "\r\n") catch @panic("OOM");
                var end_of_line = idx + 1;
                switch (remaining[idx]) {
                    '\r' => {
                        if (idx + 1 < remaining.len and remaining[idx + 1] == '\n') {
                            end_of_line += 1;
                        } else {
                            bytes_added += 1;
                        }
                    },
                    '\n' => {
                        bytes_added += 1;
                    },
                    else => unreachable,
                }
                remaining = remaining[end_of_line..];
            }
            a.constant_temp.appendSlice(a.gpa, remaining) catch @panic("OOM");
            const result = Constant.init_string_bits(a.constant_temp.items, constant.bits() + bytes_added * 8, constant.signedness()) catch unreachable;
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .signed_cast => |inner_expr| {
            const constant = resolve_expression_constant(a, s, ip, inner_expr) orelse return null;
            const result = constant.clone_with_signedness(a.gpa, &a.constant_temp, .signed);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .unsigned_cast => |inner_expr| {
            const constant = resolve_expression_constant(a, s, ip, inner_expr) orelse return null;
            const result = constant.clone_with_signedness(a.gpa, &a.constant_temp, .unsigned);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
        .remove_signedness_cast, .data_address_cast, .insn_address_cast, .stack_address_cast, .remove_address_cast,
         => |inner_expr| {
            expr_resolved_constants[expr_handle] = resolve_expression_constant(a, s, ip, inner_expr);
        },
        .absolute_address_cast => |inner_expr| {
            const inner = resolve_expression_constant(a, s, ip, inner_expr) orelse return null;
            const relative = inner.as_int(i64) catch {
                a.record_expr_layout_error(s.file.handle, expr_handle, inner_expr, "Relative address too large", .{});
                return null;
            };
            const absolute = std.math.add(i64, relative, ip) catch {
                a.record_expr_layout_error(s.file.handle, expr_handle, inner_expr, "Absolute address overflows u32", .{});
                return null;
            };
            const absolute_u32 = std.math.cast(u32, absolute) orelse {
                a.record_expr_layout_error(s.file.handle, expr_handle, inner_expr, "Absolute address overflows u32", .{});
                return null;
            };
            const result = Constant.init_int(absolute_u32);
            expr_resolved_constants[expr_handle] = result.intern(a.arena, a.gpa, &a.constants);
        },
    }

    return expr_resolved_constants[expr_handle];
}

fn resolve_symbol_ref_expr_constant(a: *Assembler, s: Source_File.Slices, ip: u32, expr_handle: Expression.Handle, symbol_token_handle: isa.lex.Token.Handle, symbol_constant: Constant) void {
    switch (symbols.lookup_symbol(a, s, symbol_token_handle, symbol_constant.as_string(), false)) {
        .expression => |target_expr_handle| {
            s.expr.items(.resolved_constant)[expr_handle] = resolve_expression_constant(a, s, ip, target_expr_handle);
        },
        .instruction => |target_insn_ref| {
            const target_file = a.get_source(target_insn_ref.file);
            var value: i64 = target_file.instructions.items(.address)[target_insn_ref.instruction];
            if (s.expr.items(.resolved_type)[expr_handle].base_offset_type()) |bot| {
                if (bot.base == .sr and bot.base.sr == .ip) {
                    value -= ip;
                }
            }
            const constant = Constant.init_int(value);
            s.expr.items(.resolved_constant)[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
        },
        .stack => |target_stack_ref| {
            var value: i64 = s.insn.items(.address)[target_stack_ref.instruction];
            value += target_stack_ref.additional_sp_offset;
            const constant = Constant.init_int(value);
            s.expr.items(.resolved_constant)[expr_handle] = constant.intern(a.arena, a.gpa, &a.constants);
        },
        .not_found => {
            return;
        },
    }
}

fn resolve_insn_form(a: *Assembler, s: Source_File.Slices, ip: u32, insn_handle: Instruction.Handle, op: *Instruction.Operation, params: ?Expression.Handle) isa.Instruction.Encoded.Length_Bytes {
    const insn = a.build_instruction(s, ip, op.insn, params, true) orelse return 0;
    const best_form = find_best_insn_form(a, insn);
    a.params_temp.clearRetainingCapacity();

    if (best_form) |form| {
        op.* = .{ .bound_insn = form.id.? };
        return form.len();
    } else {
        const err_flags = Error.Flag_Set.initOne(.remove_on_layout_reset);
        a.record_insn_encoding_error(s.file.handle, insn_handle, err_flags);
        return 0;
    }
}

pub fn find_best_insn_form(a: *Assembler, insn: isa.Instruction) ?isa.Instruction.Form {
    var iter = a.edb.find_matches(insn);

    // TODO use priority system

    var best_length: ?isa.Instruction.Encoded.Length_Bytes = null;
    var best_form: ?isa.Instruction.Form = null;
    while (iter.next()) |form| {
        const length = form.len();
        if (best_length) |cur_length| {
            if (length < cur_length) {
                best_form = form;
                best_length = length;
            }
        } else {
            best_form = form;
            best_length = length;
        }
    }

    return best_form;
}

pub fn populate_page_chunks(a: *Assembler, chunks: []const Source_File.Chunk) void {
    var page_chunks = a.pages.items(.chunks);
    const page_sections = a.pages.items(.section);
    for (chunks) |chunk| {
        if (chunk.section) |section| {
            const addresses = chunk.address_range(a);
            const first_page = addresses.first >> @bitSizeOf(arch.addr.Page.Offset);
            var last_page = first_page;
            if (addresses.len > 0) {
                last_page = addresses.last() >> @bitSizeOf(arch.addr.Page.Offset);
            }

            a.sections.values()[section].has_chunks = true;

            a.chunks.append(a.gpa, .{
                .chunk = chunk,
                .address = addresses.first,
            }) catch @panic("OOM");

            for (first_page .. last_page + 1) |page| {
                const page_data_handle = a.page_lookup.get(arch.addr.Page.init(@intCast(page))) orelse unreachable;
                page_chunks[page_data_handle].append(a.gpa, chunk) catch @panic("OOM");
                if (page_sections[page_data_handle] != section) {
                    a.record_insn_error_fmt(chunk.file, chunk.instructions.begin, "Chunk starting here was allocated to page 0x{X:0>5}, but that page is in use by another section", .{ page }, .{});
                }
            }
        }
    }
}

pub fn find_overlapping_chunks(a: *Assembler, chunks: []const Source_File.Chunk) void {
    // This is just the obvious O(n^2) implementation, but since we've already
    // grouped chunks by page, it shouldn't explode too terribly even for large programs,
    // though there is an antagonistic case where you intentionally put a bunch of .orgs
    // with the same address.  Therefore we'll stop checking after a total of 10
    // overlapping pairs are found.
    if (a.overlapping_chunks.count() >= 10) return;
    for (chunks, 0..) |chunk, i| {
        if (chunk.section == null) continue;

        const chunk_range = chunk.address_range(a);
        if (chunk_range.len == 0) continue;

        const check_against = chunks[i + 1 ..];
        for (check_against) |other_chunk| {
            if (chunk.section == null) continue;

            const other_range = other_chunk.address_range(a);
            if (other_range.len == 0) continue;

            if (chunk_range.first > other_range.last() or chunk_range.last() < other_range.first) continue;

            a.overlapping_chunks.put(a.gpa, Source_File.Chunk_Pair.init(chunk, other_chunk), {}) catch @panic("OOM");
            if (a.overlapping_chunks.count() >= 10) return;
        }
    }
}

pub fn encode_page_data(a: *Assembler, file: *Source_File) void {
    const s = file.slices();
    const insn_params = s.insn.items(.params);
    const insn_addresses = s.insn.items(.address);
    const insn_lengths = s.insn.items(.length);

    const page_datas = a.pages.items(.data);

    for (0.., s.insn.items(.operation)) |insn_handle_usize, op| {
        const insn_handle: Instruction.Handle = @intCast(insn_handle_usize);
        switch (op) {
            .none, .nil, .insn, .org, .@"align", .keep, .def, .undef, .local, .range,
            .section, .boot, .code, .kcode, .entry, .kentry, .data, .kdata, .@"const", .kconst, .stack,
            => {},

            .bound_insn => |id| {
                const address = insn_addresses[insn_handle];
                const params = insn_params[insn_handle];
                const form = iedb.get(id);
                const insn = a.build_instruction(s, address, form.signature.mnemonic, params, false).?;
                encode_instruction(a, s, insn_handle, address, insn_lengths[insn_handle], insn, form, page_datas);
            },

            .db, .dh, .dw => {
                const granularity_bytes: u8 = switch (op) {
                    .db => 1,
                    .dh => 2,
                    .dw => 4,
                    else => unreachable,
                };
                const address = insn_addresses[insn_handle];
                const length: usize = insn_lengths[insn_handle];
                const params = insn_params[insn_handle] orelse continue;
                var page = arch.addr.Page.init(@truncate(address >> @bitSizeOf(arch.addr.Page.Offset)));
                const initial_offset = arch.addr.Page.Offset.init(@truncate(address));
                var page_data_handle = a.page_lookup.get(page) orelse continue;
                var buffer = page_datas[page_data_handle][initial_offset.raw()..];
                var written: usize = 0;

                while (length - written > buffer.len) {
                    encode_data_directive(s, params, granularity_bytes, written, buffer);
                    written += buffer.len;
                    page = arch.addr.Page.init(page.raw() + 1);
                    page_data_handle = a.page_lookup.get(page) orelse break;
                    buffer = &page_datas[page_data_handle];
                }
                encode_data_directive(s, params, granularity_bytes, written, buffer);
            },
            .zb, .zh, .zw => {
                const address = insn_addresses[insn_handle];
                var remaining: usize = insn_lengths[insn_handle];
                var page = arch.addr.Page.init(@truncate(address >> @bitSizeOf(arch.addr.Page.Offset)));
                const initial_offset = arch.addr.Page.Offset.init(@truncate(address));
                var page_data_handle = a.page_lookup.get(page) orelse continue;
                var buffer = page_datas[page_data_handle][initial_offset.raw()..];

                while (remaining > 0) {
                    if (buffer.len > remaining) {
                        @memset(buffer[0..remaining], 0);
                        break;
                    }
                    @memset(buffer, 0);
                    remaining -= buffer.len;
                    page = arch.addr.Page.init(page.raw() + 1);
                    page_data_handle = a.page_lookup.get(page) orelse break;
                    buffer = &page_datas[page_data_handle];
                }
            },
            .push, .pop => |stack_size| {
                const insn = isa.Instruction{
                    .mnemonic = switch (op) {
                        .push => .frame,
                        .pop => .unframe,
                        else => unreachable,
                    },
                    .params = &.{
                        .{
                            .signature = Expression.Type.constant().param_signature(),
                            .base_register = .init(0),
                            .offset_register = .init(0),
                            .constant = stack_size,
                        },
                    },
                };
                if (find_best_insn_form(a, insn)) |form| {
                    encode_instruction(a, s, insn_handle, insn_addresses[insn_handle], insn_lengths[insn_handle], insn, form, page_datas);
                }
            },
        }
    }
}

fn encode_instruction(
    a: *Assembler,
    s: Source_File.Slices,
    insn_handle: Instruction.Handle,
    address: u32,
    length: u32,
    insn: isa.Instruction,
    form: isa.Instruction.Form,
    page_datas: [][Page_Data.page_size]u8
) void {
    const page = arch.addr.Page.init(@truncate(address >> @bitSizeOf(arch.addr.Page.Offset)));
    const offset = arch.addr.Page.Offset.init(@truncate(address));

    const page_data_handle = a.page_lookup.get(page) orelse return;
    const buffer = page_datas[page_data_handle][offset.raw()..];

    const temp_insn = form.encode(insn, 0);

    if (length > buffer.len) {
        if (@as(u1, @truncate(address)) == 1) {
            a.record_insn_error(s.file.handle, insn_handle, "Instruction crosses page boundary and is not word aligned", .{});
        }

        @memcpy(buffer, temp_insn.as_bytes().ptr);

        const next_page = arch.addr.Page.init(page.raw() + 1);
        const next_page_data_handle = a.page_lookup.get(next_page) orelse unreachable;
        const next_page_buf = &page_datas[next_page_data_handle];
        @memcpy(next_page_buf.ptr, temp_insn.as_bytes()[buffer.len..]);
    } else {
        @memcpy(buffer.ptr, temp_insn.as_bytes());
    }
}

fn encode_data_directive(s: Source_File.Slices, params_expr_handle: Expression.Handle, granularity_bytes: u8, skip_bytes: usize, out: []u8) void {
    const expr_infos = s.expr.items(.info);
    const expr_resolved_constants = s.expr.items(.resolved_constant);
    var bytes_to_skip = skip_bytes;
    var buf = out;
    var expr_handle = params_expr_handle;
    while (true) switch (expr_infos[expr_handle]) {
        .list => |bin| {
            if (expr_resolved_constants[bin.left]) |constant| {
                const bytes = encode_data_expression(constant, granularity_bytes, bytes_to_skip, buf);
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
                _ = encode_data_expression(constant, granularity_bytes, bytes_to_skip, buf);
            }
            break;
        },
    };
}
fn encode_data_expression(expr_constant: *const Constant, granularity_bytes: u8, skip_bytes: usize, out: []u8) usize {
    var constant_bytes = std.mem.alignForward(usize, expr_constant.bits(), granularity_bytes * 8) / 8;
    if (skip_bytes >= constant_bytes) {
        return constant_bytes;
    }

    var iter = expr_constant.byte_iterator(null);
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

const symbols = @import("symbols.zig");
const Assembler = @import("Assembler.zig");
const Source_File = @import("Source_File.zig");
const Section = @import("Section.zig");
const Constant = @import("Constant.zig");
const Instruction = @import("Instruction.zig");
const Expression = @import("Expression.zig");
const Page_Data = @import("Page_Data.zig");
const Error = @import("Error.zig");
const iedb = @import("iedb");
const isa = @import("isa");
const arch = @import("arch");
const bits = @import("bits");
const std = @import("std");