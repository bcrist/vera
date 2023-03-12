const std = @import("std");
const allocators = @import("allocators.zig");
const TempAllocator = @import("temp_allocator");
const ControlSignals = @import("ControlSignals");
const uc = @import("microcode");
const sx = @import("sx");
const bits = @import("bits");
const instruction_encoding = @import("instruction_encoding");
const misc = @import("misc");
const deep_hash_map = @import("deep_hash_map");
const panic = @import("instruction_builder.zig").panic;

const gpa = &allocators.global_gpa;
const perm_arena = &allocators.global_arena;

const Opcode = misc.Opcode;
const InstructionEncoding = instruction_encoding.InstructionEncoding;

var cycle_dedup = deep_hash_map.DeepAutoHashMap(*ControlSignals, *ControlSignals).init(gpa.allocator());
var unconditional_continuations = deep_hash_map.DeepAutoHashMap(*ControlSignals, uc.Address).init(gpa.allocator());
pub var microcode: [misc.microcode_length]?*ControlSignals = [_]?*ControlSignals { null } ** misc.microcode_length;
var used_signals: [misc.microcode_length]std.EnumSet(ControlSignals.SignalName) = .{ std.EnumSet(ControlSignals.SignalName).initEmpty() } ** misc.microcode_length;

var instructions: [misc.opcode_count]?*InstructionEncoding = .{ null } ** misc.opcode_count;
var descriptions: [misc.opcode_count]?[]const u8 = .{ null } ** misc.opcode_count;

var next_unconditional_continuation: uc.Continuation = 0x1FF;

pub fn getOrCreateUnconditionalContinuation(cycle: *ControlSignals, signals_used_in_cycle: std.EnumSet(ControlSignals.SignalName)) uc.Address {
    if (unconditional_continuations.get(cycle)) |ua| {
        used_signals[ua].setUnion(signals_used_in_cycle);
        return ua;
    } else {
        const min_n = @enumToInt(uc.Vectors.last) + 1;

        var n = next_unconditional_continuation;
        var ua = uc.getAddressForContinuation(n, .{});

        while (n >= min_n and microcode[ua] != null) {
            n -= 1;
            ua = uc.getAddressForContinuation(n, .{});
        }

        // occasionally useful for debugging changes that cause unexpected increases in continuation use:
        // if (@import("instruction_builder.zig").insn) |i| {
        //     std.debug.print("Allocating continuation {X:0>3}:\n", .{ n });
        //     @import("instruction_builder.zig").printCyclePath(i.initial_uc_address, i.encoding);
        // }

        if (n < min_n) {
            // TODO use conditional slots?
            std.debug.panic("No more continuations left!", .{});
        }
        next_unconditional_continuation = n - 1;

        const deduped = putMicrocodeCycle(ua, cycle, signals_used_in_cycle);
        unconditional_continuations.put(deduped, ua) catch @panic("Out of memory!");
        return ua;
    }
}

pub fn getContinuationsLeft() usize {
    const min_n = @enumToInt(uc.Vectors.last) + 1;
    return next_unconditional_continuation - min_n + 1;
}

pub fn getMicrocodeCycle(ua: uc.Address) ?*ControlSignals {
    return microcode[ua];
}

pub fn getUsedSignalsForAddress(ua: uc.Address) std.EnumSet(ControlSignals.SignalName) {
    return used_signals[ua];
}

pub fn putMicrocodeCycle(ua: uc.Address, cycle: *ControlSignals, signals_used_in_cycle: std.EnumSet(ControlSignals.SignalName)) *ControlSignals {
    var deduped = cycle;
    if (cycle_dedup.get(cycle)) |c| {
        deduped = c;
    } else {
        deduped = perm_arena.allocator().create(ControlSignals) catch @panic("Out of memory!");
        deduped.* = cycle.*;
        cycle_dedup.put(deduped, deduped) catch @panic("Out of memory!");
    }

    putMicrocodeCycleNoDedup(ua, deduped, signals_used_in_cycle);
    return deduped;
}

// provided cycle should be from perm_arena
pub fn putMicrocodeCycleNoDedup(ua: uc.Address, cycle: *ControlSignals, signals_used_in_cycle: std.EnumSet(ControlSignals.SignalName)) void {
    if (microcode[ua] != null) {
        if (uc.getOpcodeForAddress(ua)) |opcode| {
            if (getInstructionByOpcode(opcode)) |insn| {
                panic("Microcode address {X} (opcode {X}) is already in use by {}", .{ ua, opcode, insn.mnemonic });
            } else {
                panic("Microcode address {X} (opcode {X}) is already in use", .{ ua, opcode });
            }
        } else {
            panic("Microcode address {X} is already in use", .{ ua });
        }
    } else {
        microcode[ua] = cycle;
        used_signals[ua].setUnion(signals_used_in_cycle);
    }
}

fn getOrCreateInstruction(in: InstructionEncoding) *InstructionEncoding {
    const first_opcode = in.opcodes.min;
    const last_opcode = in.opcodes.max;

    if (first_opcode > 0) {
        var prev = instructions[first_opcode - uc.getOpcodeGranularity(first_opcode)];
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

    var iter = uc.opcodeIterator(insn.opcodes.min, insn.opcodes.max);
    while (iter.next()) |cur_opcode| {
        if (instructions[cur_opcode]) |existing| {
            std.debug.print("Opcode {X} has already been assigned to {s}\n", .{
                cur_opcode,
                @tagName(existing.mnemonic),
            });
        }
        for (0..uc.getOpcodeGranularity(cur_opcode)) |offset| {
            instructions[cur_opcode + offset] = ptr;
            descriptions[cur_opcode + offset] = desc;
        }
    }
}

pub fn getInstructionByOpcode(opcode: Opcode) ?*InstructionEncoding {
    return instructions[opcode];
}

pub fn analyzeCustom(temp_arena: *TempAllocator, writer: anytype) !void {
    temp_arena.reset();
    for (microcode, 0..) |maybe_cycle, ua_usize| {
        if (maybe_cycle) |cycle| {
            switch (cycle.kr_rsel) {
                .oa => {
                    const ua = @intCast(uc.Address, ua_usize);
                    if (uc.getOpcodeForAddress(ua)) |opcode| {
                        try writer.print("kr_rsel is {} for opcode {X:0>4}\n", .{ cycle.kr_rsel, opcode });
                    } else if (uc.getContinuationNumberForAddress(ua)) |cont| {
                        try writer.print("kr_rsel is {} for continuation {X:0>3}\n", .{ cycle.kr_rsel, cont });
                    } else {
                        try writer.print("kr_rsel is {} for address {X:0>4}\n", .{ cycle.kr_rsel, ua });
                    }
                },
                else => {},
            }
        }
    }
}

pub fn analyzeControlSignalUsage(temp_arena: *TempAllocator, comptime signals: []const ControlSignals.SignalName, writer: anytype) !void {
    temp_arena.reset();
    var temp = std.ArrayList(u8).init(temp_arena.allocator());
    var data = std.StringHashMap(u16).init(temp_arena.allocator());

    for (microcode, 0..) |maybe_cycle, ua| {
        if (maybe_cycle) |cycle| {
            temp.clearRetainingCapacity();
            const signals_used_in_cycle = used_signals[ua];
            inline for (signals) |signal| {
                if (signals_used_in_cycle.contains(signal)) {
                    const v = @field(cycle, @tagName(signal));
                    var w = temp.writer();
                    switch (@typeInfo(@TypeOf(v))) {
                        .Enum  => try w.print("{s: <20} ", .{ @tagName(v) }),
                        .Bool  => try w.print("{: <20} ", .{ v }),
                        .Union => try w.print("{X: <20} ", .{ v.raw() }),
                        .Int   => try w.print("0x{X: <18} ", .{ v }),
                        else   => try w.print("{s: <20} ", .{ "?????" }),
                    }
                } else {
                    try temp.appendSlice("---                  ");
                }
            }

            var result = try data.getOrPut(temp.items);
            if (result.found_existing) {
                result.value_ptr.* += 1;
            } else {
                result.key_ptr.* = try temp_arena.allocator().dupe(u8, temp.items);
                result.value_ptr.* = 1;
            }
        }
    }

    try writer.writeAll("-------- ");
    inline for (signals) |signal| {
        try writer.print(" {s: <20}", .{ @tagName(signal) });
    }
    try writer.writeAll("\n");
    var iter = data.iterator();
    while (iter.next()) |entry| {
        try writer.print("{: >8}: {s}\n", .{ entry.value_ptr.*, entry.key_ptr.* });
    }
    try writer.writeAll("\n");
}

pub fn writeOpcodeTableSmall(writer: anytype) !void {
    try writer.writeAll("  ");
    for (0..0x10) |col| {
        try writer.writeByte(' ');
        try writer.writeByte('x');
        try writer.writeByte("0123456789ABCDEF"[col]);
        try writer.writeByte(' ');
    }

    for (0..0x10) |row| {
        for (0..2) |subrow| {
            try writer.writeByte('\n');
            try writer.writeByte(switch (subrow) {
                0 => "0123456789ABCDEF"[row],
                else => ' ',
            });
            try writer.writeByte(' ');

            for (0..0x10) |col| {
                for (0..4) |subcol| {
                    var num_occupied: usize = 0;
                    for (0..4) |subsubcol| {
                        for (0..8) |subsubrow| {
                            const opcode = bits.concat(.{
                                @intCast(u2, subsubcol),
                                @intCast(u3, subsubrow),
                                @intCast(u2, subcol),
                                @intCast(u1, subrow),
                                @intCast(u4, col),
                                @intCast(u4, row),
                            });
                            if (instructions[opcode]) |_| {
                                num_occupied += 1;
                            }
                        }
                    }

                    const ch: u8 = switch (num_occupied) {
                        else => '#',
                        24...31 => 'X',
                        16...23 => '=',
                        8...15 => ':',
                        1...7 => '.',
                        0 => ' ',
                    };

                    try writer.writeByte(ch);
                }
            }
        }
    }
    try writer.writeAll("\n\n");
}

pub fn writeOpcodeTableLarge(writer: anytype) !void {
    try writer.writeAll("   ");
    for (0..0x100) |col| {
        try writer.writeByte("0123456789ABCDEF"[col >> 4]);
    }
    try writer.writeAll("\n   ");
    for (0..0x100) |col| {
        try writer.writeByte("0123456789ABCDEF"[@truncate(u4, col)]);
    }

    for (0..0x10) |row| {
        for (0..0x10) |subrow| {
            try writer.writeByte('\n');
            try writer.writeByte("0123456789ABCDEF"[row]);
            try writer.writeByte("0123456789ABCDEF"[subrow]);
            try writer.writeByte(' ');
            for (0..0x10) |col| {
                for (0..0x10) |subcol| {
                    const opcode = bits.concat(.{
                        @intCast(u4, subcol),
                        @intCast(u4, subrow),
                        @intCast(u4, col),
                        @intCast(u4, row),
                    });

                    if (instructions[opcode]) |_| {
                        try writer.writeByte('#');
                    } else if (subrow == 0xF or subcol == 0xF) {
                        try writer.writeByte('.');
                    } else {
                        try writer.writeByte(' ');
                    }
                }
            }
        }
    }
    try writer.writeAll("\n\n");
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
