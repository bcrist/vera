pub fn dump(a: *Assembler, writer: *std.io.Writer) !void {
    try writer.writeAll("Files:\n");
    for (a.files.items) |file| {
        const block_slice = file.blocks.slice();
        try writer.print("   {s}\n", .{ file.name });
        try writer.print("      Blocks:\n", .{});
        for (0..,
            block_slice.items(.first_insn),
            block_slice.items(.end_insn),
            block_slice.items(.block_type),
            block_slice.items(.section),
            block_slice.items(.keep),
        ) |handle, begin, end, maybe_type, maybe_section, keep| {
            try writer.print("         #{}: #{} - #{}", .{ handle, begin, end });
            if (maybe_type) |block_type| {
                try writer.print(" .{s}", .{ @tagName(block_type) });
            }
            if (maybe_section) |section| {
                try writer.print(" section#{}", .{ section });
            }
            if (keep) {
                try writer.writeAll(" .keep");
            }
            try writer.writeAll("\n");
        }
        try writer.print("      Instructions:\n", .{});
        for (0..,
            file.instructions.items(.label),
            file.instructions.items(.operation),
            file.instructions.items(.params),
            file.instructions.items(.address),
            file.instructions.items(.length),
            file.instructions.items(.flags),
        ) |handle, label_handle, operation, params_handle, address, length, flags| {
            try writer.print("         #{}:", .{ handle });
            if (address != 0) {
                try writer.print(" {X:0>8}", .{ address });
            }
            try writer.print(" (+{})", .{ length });

            if (label_handle) |label| {
                try writer.print(" #{}:", .{ label });
            }
            switch (operation) {
                .insn => |mnemonic| {
                    try writer.print(" {f}", .{ mnemonic });
                },
                .bound_insn => |id| {
                    try writer.print(" bound_insn #{d}", .{ id });
                },
                else => {
                    try writer.print(" {s}", .{ @tagName(operation) });
                },
            }
            if (params_handle) |params| {
                try writer.print(" #{}", .{ params });
            }

            var flags_iter = flags.iterator();
            while (flags_iter.next()) |f| {
                try writer.print(" .{s}", .{ @tagName(f) });
            }

            switch (operation) {
                .bound_insn => |id| {
                    try writer.writeByte('\t');
                    const form = iedb.get(id);
                    try form.format(writer);
                },
                else => {},
            }
            try writer.writeAll("\n");
        }
        try writer.print("      Expressions:\n", .{});
        for (0..,
            file.expressions.items(.resolved_type),
            file.expressions.items(.resolved_constant),
            file.expressions.items(.info),
            file.expressions.items(.flags),
        ) |handle, expr_type, maybe_constant, info, flags| {
            try writer.print("         #{:0>5}:", .{ handle });
            try writer.print(" {s}", .{ @tagName(info) });
             switch (info) {
                .list,
                .plus, .minus, .multiply, .shl, .shr, .concat, .concat_repeat,
                .bitwise_or, .bitwise_xor, .bitwise_and, .length_cast,
                .truncate, .sign_extend, .zero_extend,
                 => |binary| {
                    try writer.print(" #{}, #{}", .{ binary.left, binary.right });
                },

                .directive_symbol_def, .directive_symbol_ref,
                .signed_cast, .unsigned_cast, .remove_signedness_cast,
                .negate, .complement, .absolute_address_cast, .local_label_def,
                .data_address_cast, .insn_address_cast, .stack_address_cast, .remove_address_cast,
                .reg_to_index, .index_to_reg, .crlf_cast, .lf_cast
                => |unary| {
                    try writer.print(" #{}", .{ unary });
                },

                .literal_int, .literal_str, .literal_reg, .literal_symbol_def, .literal_symbol_ref, .literal_current_address,
                => {},
            }

            if (expr_type != .unknown and expr_type != .poison and expr_type != .symbol_def) {
                try writer.writeByte(' ');
                try print_expression_type(expr_type, writer);
            }
            if (maybe_constant) |constant| {
                try writer.writeByte(' ');
                try print_constant(writer, constant);
            }

            var flags_iter = flags.iterator();
            while (flags_iter.next()) |f| {
                try writer.print(" .{s}", .{ @tagName(f) });
            }

            try writer.writeAll("\n");
        }
    }

    try writer.writeAll("Public Labels:\n");
    var symbol_iter = a.public_labels.iterator();
    while (symbol_iter.next()) |entry| {
        const symbol = entry.key_ptr.*;
        const file_handle = entry.value_ptr.file;
        const insn_handle = entry.value_ptr.instruction;
        const file_name = a.get_source(file_handle).name;
        try writer.print("   {s} #{} <- {s}\n", .{ file_name, insn_handle, symbol });
    }

    try writer.writeAll("Sections:\n");
    for (0.., a.sections.entries.items(.value)) |section_handle, section| {
        try writer.print("   #{}: {s} : {s}\n", .{ section_handle, section.name, @tagName(section.kind) });
    }

    try writer.writeAll("Pages:\n");
    for (a.pages.items(.page), a.pages.items(.section), a.pages.items(.usage), a.pages.items(.data), a.pages.items(.chunks)) |page, section_handle, usage, data, chunks| {
        try writer.print("   {X:0>5}: #{}\n", .{ page.raw(), section_handle });
        for (0..8) |row| {
            try writer.writeAll("      usage:");
            for (0..8) |col| {
                try writer.writeAll(" ");
                const chunk_index = row * 8 + (7 - col);
                const usage_chunk = std.mem.toBytes(usage.masks[chunk_index]);
                for (0..8) |byte| {
                    try writer.print("{X:0>2}", .{ usage_chunk[7 - byte] });
                }
            }
            try writer.writeAll("\n");
        }
        for (usage.masks, 0..) |usage_chunk, chunk_index| {
            if (usage_chunk != 0) {
                const offset = chunk_index * 64;
                const data_chunk = data[offset .. offset + 64];
                try writer.print("      data:{X:0>3}-{X:0>3}: ", .{ offset + 63, offset });
                for (0..64) |i| {
                    try writer.print("{X:0>2}", .{ data_chunk[63 - i] });
                }
                try writer.print(" {f}\n", .{ fmt_slice_replace_non_ascii(data_chunk) });
            }
        }
        for (chunks.items) |chunk| {
            try writer.print("      chunk: file #{}: #{}-#{}\n", .{ chunk.file, chunk.instructions.begin, chunk.instructions.end });
        }
    }

    try a.print_errors(writer);
}

fn print_expression_type(expr_type: Expression.Type, writer: *std.io.Writer) !void {
    try isa.fmt.print_parameter_signature(expr_type.param_signature(), .{
        .base_register = expr_type.param_base_register(),
        .offset_register = expr_type.param_offset_register(),
    }, writer);
}

fn print_constant(writer: *std.io.Writer, constant: *const Constant) !void {
    try writer.print("{:>4}b: ", .{ constant.bit_count });
    if (constant.as_int(i64) catch null) |int_value| {
        const uint_value = constant.as_int(u64) catch unreachable;
        try writer.print("0x{X} {} ", .{ uint_value, int_value });
    }
    try writer.print("'{f}'", .{ std.ascii.hexEscape(constant.as_string(), .upper) });
}

fn fmt_slice_replace_non_ascii(bytes: []const u8) std.fmt.Alt([]const u8, format_slice_replace_non_ascii_impl) {
    return .{ .data = bytes };
}

fn format_slice_replace_non_ascii_impl(bytes: []const u8, writer: *std.io.Writer) !void {
    for (bytes) |c| {
        if (std.ascii.isPrint(c)) {
            try writer.writeByte(c);
        } else if (c == 0) {
            try writer.writeByte('.');
        } else {
            try writer.writeByte('?');
        }
    }
}

const Expression = @import("Expression.zig");
const Assembler = @import("Assembler.zig");
const Constant = @import("Constant.zig");
const iedb = @import("iedb");
const isa = @import("isa");
const std = @import("std");
