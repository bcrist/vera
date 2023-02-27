const std = @import("std");
const allocators = @import("allocators.zig");
const sx = @import("sx");
const misc = @import("misc");
const uc_layout = @import("microcode_layout");
const instruction_encoding = @import("instruction_encoding");

const Opcode = misc.Opcode;
const InstructionEncoding = instruction_encoding.InstructionEncoding;

const gpa = &allocators.global_gpa;
const perm_arena = &allocators.global_arena;

var instructions: [65536]?*InstructionEncoding = .{ null } ** 65536;
var descriptions: [65536]?[]const u8 = .{ null } ** 65536;

fn getOrCreateInstruction(in: InstructionEncoding) *InstructionEncoding {
    const first_opcode = in.opcodes.min;
    const last_opcode = in.opcodes.max;

    if (first_opcode > 0) {
        var prev = instructions[first_opcode - uc_layout.getOpcodeGranularity(first_opcode)];
        if (prev) |p| {
            if (instruction_encoding.eql(in, p.*)) {
                p.opcodes.max = last_opcode;
                return p;
            }
        }
    }

    var alloc = perm_arena.allocator();
    var i = alloc.create(InstructionEncoding) catch @panic("Out of memory!");
    i.* = in;
    return i;
}

pub fn recordInstruction(insn: InstructionEncoding, desc: ?[]const u8) void {
    if (insn.mnemonic == ._reserved) {
        return;
    }

    var ptr = getOrCreateInstruction(insn);

    var iter = uc_layout.opcodeIterator(insn.opcodes.min, insn.opcodes.max);
    while (iter.next()) |cur_opcode| {
        if (instructions[cur_opcode]) |existing| {
            std.debug.print("Opcode {X} has already been assigned to {s}\n", .{
                cur_opcode,
                @tagName(existing.mnemonic),
            });
        }
        instructions[cur_opcode] = ptr;
        descriptions[cur_opcode] = desc;
    }
}

pub fn getInstructionByOpcode(opcode: Opcode) ?*InstructionEncoding {
    return instructions[opcode];
}

pub fn writeInstructionData(inner: anytype) !void {
    var writer = sx.Writer(@TypeOf(inner)).init(allocators.temp_arena.allocator(), inner);
    try writer.openExpanded();
    for (instructions, descriptions, 0..) |maybe_insn, maybe_desc, opcode| {
        if (maybe_insn) |i| {
            if (i.opcodes.min == opcode) {
                try writer.open();
                try writer.printValue("{X:0>4}", .{ i.opcodes.min });
                try writer.printValue("{X:0>4}", .{ i.opcodes.max });
                try writer.tag(i.mnemonic);
                if (i.suffix != .none) {
                    try writer.tag(i.suffix);
                }
                writer.setCompact(false);
                if (maybe_desc) |desc| {
                    try writer.expression("desc");
                    try writer.string(desc);
                    _ = try writer.close();
                }

                if (i.opcode_base != i.opcodes.min) {
                    var param_uses_opcode = false;
                    for (i.params) |param| {
                        if (param.base_src == .opcode or param.offset_src == .opcode) {
                            param_uses_opcode = true;
                            break;
                        }
                    }
                    if (param_uses_opcode) {
                        try writer.expression("opcode-base");
                        try writer.printValue("{X:0>4}", .{ i.opcode_base });
                        _ = try writer.close();
                    }
                }

                for (i.params) |param| {
                    try writer.expression("param");
                    if (param.arrow) {
                        try writer.string("->");
                    }
                    try writer.tag(param.type.base);

                    if (param.type.offset != .none) {
                        try writer.tag(param.type.offset);
                    }

                    if (param.base_src != .implicit) {
                        try writer.expression("base-src");
                        try writer.tag(param.base_src);
                        _ = try writer.close();
                    }

                    if (param.offset_src != .implicit) {
                        try writer.expression("offset-src");
                        try writer.tag(param.offset_src);
                        _ = try writer.close();
                    }

                    if (param.constant_reverse) {
                        try writer.expression("rev");
                        _ = try writer.close();
                    }

                    if (param.min_reg != 0 or param.max_reg != 15) {
                        try writer.expression("reg");
                        try writer.int(param.min_reg, 10);
                        try writer.int(param.max_reg, 10);
                        _ = try writer.close();
                    }

                    for (param.constant_ranges) |range| {
                        try writer.expression("range");
                        try writer.int(range.min, 10);
                        try writer.int(range.max, 10);
                        _ = try writer.close();
                    }

                    for (param.alt_constant_ranges) |range| {
                        try writer.expression("alt-range");
                        try writer.int(range.min, 10);
                        try writer.int(range.max, 10);
                        _ = try writer.close();
                    }

                    if (param.constant_align != 1) {
                        try writer.expression("align");
                        try writer.int(param.constant_align, 10);
                        _ = try writer.close();
                    }

                    _ = try writer.close();
                }

                _ = try writer.close();
            }
        }
    }
    try writer.done();
}
