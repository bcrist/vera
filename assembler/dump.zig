const std = @import("std");
const ie = @import("instruction_encoding");
const Assembler = @import("Assembler.zig");

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

            try writer.writeAll("\n");
            switch (operation) {
                .bound_insn => |encoding| {
                    try encoding.print(writer, "            ");
                },
                else => {},
            }
        }
        try writer.print("      Expressions:\n", .{});
        for (0..,
            file.expressions.items(.token),
            file.expressions.items(.resolved_type),
            file.expressions.items(.resolved_constant),
            file.expressions.items(.info),
            file.expressions.items(.flags),
        ) |handle, token_handle, expr_type, maybe_constant, info, flags| {
            try writer.print("         #{:0>5}:", .{ handle });
            if (maybe_constant) |constant| {
                try writer.print(" {X:0>16}", .{ @ptrToInt(constant) });
            }
            if (expr_type != .unknown) {
                try dumpType(writer, expr_type, " ");
            }

            try writer.print("   {s}", .{ @tagName(info) });
             switch (info) {
                .list, .arrow_list,
                .plus, .minus, .multiply, .shl, .shr, .concat, .concat_repeat,
                .bitwise_or, .bitwise_xor, .bitwise_and, .length_cast,
                .truncate, .sign_extend, .zero_extend,
                 => |binary| {
                    try writer.print(" #{}, #{}", .{ binary.left, binary.right });
                },

                .directive_symbol_def, .directive_symbol_ref,
                .signed_cast, .unsigned_cast, .maybe_signed_cast,
                .negate, .complement, .absolute_address_cast,
                .data_address_cast, .insn_address_cast, .stack_address_cast, .remove_address_cast,
                .reg_to_index, .index_to_reg8, .index_to_reg16, .index_to_reg32,
                => |unary| {
                    try writer.print(" #{}", .{ unary });
                },

                .literal_int, .literal_str, .literal_reg, .literal_symbol_def, .literal_symbol_ref, .literal_current_address,
                => {},
            }
            const token = file.tokens.get(token_handle);
            try writer.print(" '{s}'", .{ token.location(file.source) });

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

    try writer.writeAll("Constants:\n");
    var constant_iter = self.constants.keyIterator();
    while (constant_iter.next()) |k| {
        const ptr = k.*;
        try writer.print("   {X:0>16}:{:>4}b: ", .{ @ptrToInt(ptr), ptr.bit_count });
        if (ptr.asInt()) |int_value| {
            var uint_value = @bitCast(u64, int_value);
            if (ptr.bit_count < 64) {
                uint_value &= (@as(u64, 1) << @intCast(u6, ptr.bit_count)) - 1;
            }
            try writer.print("0x{X} {} ", .{ uint_value, int_value });
        } else |_| {}
        try writer.print("'{s}'\n", .{ std.fmt.fmtSliceEscapeUpper(ptr.asString()) });
    }

    // TODO pages

    try writer.writeAll("Errors:\n");
    for (self.errors.items) |err| {
        try err.print(self.*, writer);
    }

    if (self.invalid_program) {
        try writer.writeAll("Program is invalid!\n");
    }
}

fn dumpType(writer: anytype, expr_type: ie.ExpressionType, prefix: []const u8) !void {
    try writer.print("{s}{s}", .{ prefix, @tagName(expr_type) });
    switch (expr_type) {
        .unknown, .poison, .symbol_def, .constant => {},
        .reg8, .reg16, .reg32 => |reg| {
            try writer.print(" #{}", .{ reg.index });
            if (reg.signedness) |sign| {
                try writer.print(" .{s}", .{ @tagName(sign) });
            }
        },
        .sr => |sr| {
            try writer.print(" {s}", .{ @tagName(sr) });
        },
        .raw_base_offset, .data_address, .insn_address, .stack_address => |info| {
            try dumpInnerType(writer, info.base, " ");
            try dumpInnerType(writer, info.offset, " + ");
        },
    }
}

fn dumpInnerType(writer: anytype, inner_type: ie.BaseOffsetType.InnerType, prefix: []const u8) !void {
try writer.print("{s}{s}", .{ prefix, @tagName(inner_type) });
    switch (inner_type) {
        .none => {},
        .constant => {},
        .reg8, .reg16, .reg32 => |reg| {
            try writer.print(" #{}", .{ reg.index });
            if (reg.signedness) |sign| {
                try writer.print(" .{s}", .{ @tagName(sign) });
            }
        },
        .sr => |sr| {
            try writer.print(" {s}", .{ @tagName(sr) });
        },
    }
}
