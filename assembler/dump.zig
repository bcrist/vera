const std = @import("std");
const ie = @import("isa_encoding");
const Assembler = @import("Assembler.zig");
const Constant = @import("Constant.zig");
const ExpressionType = ie.Parameter.ExpressionType;

pub fn dump(self: *Assembler, writer: anytype) !void {
    try writer.writeAll("Files:\n");
    for (self.files.items) |file| {
        try writer.print("   {s}\n", .{ file.name });
        try writer.print("      Blocks:\n", .{});
        for (0..,
            file.blocks.items(.first_insn),
            file.blocks.items(.end_insn),
            file.blocks.items(.section),
            file.blocks.items(.keep),
        ) |handle, begin, end, maybe_section, keep| {
            try writer.print("         #{}: #{} - #{}", .{ handle, begin, end });
            if (maybe_section) |section| {
                try writer.print(" section#{}", .{ section });
            }
            if (keep) {
                try writer.writeAll(" keep");
            }
            try writer.writeAll("\n");
        }
        try writer.print("      Instructions:\n", .{});
        for (0..,
            file.instructions.items(.label),
            file.instructions.items(.token),
            file.instructions.items(.operation),
            file.instructions.items(.params),
            file.instructions.items(.address),
            file.instructions.items(.flags),
        ) |handle, label_handle, token_handle, operation, params_handle, address, flags| {
            try writer.print("         #{}:", .{ handle });
            if (address != 0) {
                try writer.print(" {X:0>8}", .{ address });
            }
            if (label_handle) |label| {
                try writer.print(" #{}:", .{ label });
            }
            switch (operation) {
                .insn => |insn| {
                    try writer.print(" {s}", .{ @tagName(insn.mnemonic) });
                    if (insn.suffix != .none) {
                        try writer.print(".{s}", .{ @tagName(insn.suffix) });
                    }
                },
                .bound_insn => {
                    try writer.writeAll(" bound_insn");
                },
                else => {
                    try writer.print(" {s}", .{ @tagName(operation) });
                },
            }
            if (params_handle) |params| {
                try writer.print(" #{}", .{ params });
            }
            const token = file.tokens.get(token_handle);
            try writer.print(" '{s}'", .{ token.location(file.source) });

            var flags_iter = flags.iterator();
            while (flags_iter.next()) |f| {
                try writer.print(" .{s}", .{ @tagName(f) });
            }

            switch (operation) {
                .bound_insn => |encoding| {
                    try writer.writeByte('\t');
                    try encoding.print(writer);
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
                .list, .arrow_list,
                .plus, .minus, .multiply, .shl, .shr, .concat, .concat_repeat,
                .bitwise_or, .bitwise_xor, .bitwise_and, .length_cast,
                .truncate, .sign_extend, .zero_extend,
                 => |binary| {
                    try writer.print(" #{}, #{}", .{ binary.left, binary.right });
                },

                .directive_symbol_def, .directive_symbol_ref,
                .signed_cast, .unsigned_cast, .remove_signedness_cast,
                .negate, .complement, .absolute_address_cast,
                .data_address_cast, .insn_address_cast, .stack_address_cast, .remove_address_cast,
                .reg_to_index, .index_to_reg8, .index_to_reg16, .index_to_reg32,
                => |unary| {
                    try writer.print(" #{}", .{ unary });
                },

                .literal_int, .literal_str, .literal_reg, .literal_symbol_def, .literal_symbol_ref, .literal_current_address,
                => {},
            }

            if (expr_type != .unknown) {
                try writer.writeByte(' ');
                try expr_type.print(writer);
            }
            if (maybe_constant) |constant| {
                try writer.writeByte(' ');
                try printConstant(writer, constant);
            }

            var flags_iter = flags.iterator();
            while (flags_iter.next()) |f| {
                try writer.print(" .{s}", .{ @tagName(f) });
            }

            try writer.writeAll("\n");
        }
    }

    try writer.writeAll("Symbols:\n");
    var symbol_iter = self.symbols.iterator();
    while (symbol_iter.next()) |entry| {
        const symbol = entry.key_ptr.*;
        const file_handle = entry.value_ptr.file;
        const insn_handle = entry.value_ptr.instruction;
        const file_name = self.getSource(file_handle).name;
        try writer.print("   {s} #{} <- {s}\n", .{ file_name, insn_handle, symbol });
    }

    try writer.writeAll("Sections:\n");
    for (0.., self.sections.entries.items(.value)) |section_handle, section| {
        try writer.print("   #{}: {s} : {s}\n", .{ section_handle, section.name, @tagName(section.kind) });
    }

    try writer.writeAll("Pages:\n");
    for (self.pages.items(.page), self.pages.items(.section), self.pages.items(.usage), self.pages.items(.data), self.pages.items(.chunks)) |page, section_handle, usage, data, chunks| {
        try writer.print("   {X:0>5}: #{}\n", .{ page, section_handle });
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
                try writer.print(" {}\n", .{ fmtSliceReplaceNonAscii(data_chunk) });
            }
        }
        for (chunks.items) |chunk| {
            try writer.print("      chunk: file #{}: #{}-#{}\n", .{ chunk.file, chunk.instructions.begin, chunk.instructions.end });
        }
    }

    try self.printErrors(writer);
}

fn printConstant(writer: anytype, constant: *const Constant) !void {
    try writer.print("{:>4}b: ", .{ constant.bit_count });
    if (constant.asInt(i64) catch null) |int_value| {
        var uint_value = constant.asInt(u64) catch unreachable;
        try writer.print("0x{X} {} ", .{ uint_value, int_value });
    }
    try writer.print("'{s}'", .{ std.fmt.fmtSliceEscapeUpper(constant.asString()) });
}

fn fmtSliceReplaceNonAscii(bytes: []const u8) std.fmt.Formatter(formatSliceReplaceNonAsciiImpl) {
    return .{ .data = bytes };
}

fn formatSliceReplaceNonAsciiImpl(
    bytes: []const u8,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;

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
