const std = @import("std");
const allocators = @import("allocators.zig");
const TempAllocator = @import("temp_allocator");
const ControlSignals = @import("ControlSignals");
const uc = @import("microcode");
const bits = @import("bits");
const sx = @import("sx");
const isa = @import("isa_types");
const ie = @import("isa_encoding");
const misc = @import("misc");
const deep_hash_map = @import("deep_hash_map");
const panic = @import("instruction_builder.zig").panic;

const gpa = &allocators.global_gpa;
const perm_arena = &allocators.global_arena;

const Opcode = isa.Opcode;
const InstructionEncoding = ie.InstructionEncoding;

var cycle_dedup = deep_hash_map.DeepAutoHashMap(*ControlSignals, *ControlSignals).init(gpa.allocator());

var unconditional_continuations = deep_hash_map.DeepAutoHashMap(*ControlSignals, uc.Continuation).init(gpa.allocator());
var conditional_continuations = deep_hash_map.DeepRecursiveAutoHashMap(ConditionalContinuationKey, uc.Continuation).init(gpa.allocator());

pub var microcode: [misc.microcode_length]?*ControlSignals = [_]?*ControlSignals { null } ** misc.microcode_length;
var used_signals: [misc.microcode_length]std.EnumSet(ControlSignals.SignalName) = .{ std.EnumSet(ControlSignals.SignalName).initEmpty() } ** misc.microcode_length;

var instructions: [misc.opcode_count]?*InstructionEncoding = .{ null } ** misc.opcode_count;
var descriptions: [misc.opcode_count]?[]const u8 = .{ null } ** misc.opcode_count;

var aliases = std.ArrayList(InstructionEncoding.WithDescription).init(gpa.allocator());

var next_unconditional_continuation: uc.Continuation = 0x1FF;
var next_conditional_continuation: uc.Continuation = 0x3FF;

pub fn getOrCreateUnconditionalContinuation(requested_continuation: ?uc.Continuation, cycle: *ControlSignals, signals_used_in_cycle: std.EnumSet(ControlSignals.SignalName)) uc.Continuation {
    if (requested_continuation == null) {
        if (unconditional_continuations.get(cycle)) |continuation| {
            used_signals[uc.getAddressForContinuation(continuation, .{})].setUnion(signals_used_in_cycle);
            return continuation;
        }
    }

    const selected_continuation = requested_continuation orelse blk: {
        const min_continuation = @enumToInt(uc.Vectors.last) + 1;

        var continuation = next_unconditional_continuation;

        while (continuation >= min_continuation and microcode[uc.getAddressForContinuation(continuation, .{})] != null) {
            continuation -= 1;
        }

        if (continuation < min_continuation) {
            continuation = 0x200;

            while (continuation <= next_conditional_continuation and microcode[uc.getAddressForContinuation(continuation, .{})] != null) {
                continuation += 1;
            }

            if (continuation > next_conditional_continuation) {
                std.debug.panic("No more continuations left!", .{});
            }
        }
        next_unconditional_continuation = continuation - 1;
        break :blk continuation;
    };

    if (microcode[uc.getAddressForContinuation(selected_continuation, .{})] != null) {
        panic("Unconditional continuation {X} is already in use", .{ selected_continuation });
    }

    // occasionally useful for debugging changes that cause unexpected increases in continuation use:
    // if (@import("instruction_builder.zig").insn) |i| {
    //     if (i.initial_uc_address) |ua| {
    //         @import("instruction_builder.zig").printCyclePath(ua, i.encoding);
    //     }
    // }
    // std.debug.print("Allocating unconditional continuation {X:0>3}\n", .{ selected_continuation });

    const deduped = putMicrocodeCycle(uc.getAddressForContinuation(selected_continuation, .{}), cycle, signals_used_in_cycle);
    unconditional_continuations.put(deduped, selected_continuation) catch @panic("Out of memory!");

    if (selected_continuation >= 0x200) {
        // if we run out of unconditional continuations and use a conditional one instead,
        // make sure we write it to the addresses for all flag permutations:
        var permutations = uc.flagPermutationIterator(uc.conditional_continuation_checked_flags);
        _ = permutations.next(); // we already wrote the no-flags case
        while (permutations.next()) |flags| {
            putMicrocodeCycleNoDedup(uc.getAddressForContinuation(selected_continuation, flags), deduped, signals_used_in_cycle);
        }
    }

    return selected_continuation;
}

pub const ConditionalCycleData = struct {
    flags: uc.FlagSet,
    cs: *ControlSignals,
    used_signals: std.EnumSet(ControlSignals.SignalName),
};
const ConditionalContinuationKey = struct {
    cycles: []const ConditionalCycleData,
    unqueried_flags: uc.FlagSet,
};

pub fn getOrCreateConditionalContinuation(requested_continuation: ?uc.Continuation, cycle_data: []const ConditionalCycleData, unqueried_flags: uc.FlagSet) uc.Continuation {
    if (requested_continuation == null) {
        if (conditional_continuations.get(.{
            .cycles = cycle_data,
            .unqueried_flags = unqueried_flags,
        })) |found_continuation| {
            // TODO merge used signals data
            return found_continuation;
        }
    }

    var selected_continuation = requested_continuation orelse blk: {
        const min_continuation = 0x200;

        var continuation = next_conditional_continuation;

        while (continuation >= min_continuation and microcode[uc.getAddressForContinuation(continuation, .{})] != null) {
            continuation -= 1;
        }

        if (continuation < min_continuation) {
            std.debug.panic("No more conditional continuations left!", .{});
        }
        next_conditional_continuation = continuation - 1;
        break :blk continuation;
    };

    if (microcode[uc.getAddressForContinuation(selected_continuation, .{})] != null) {
        panic("Conditional continuation {X} is already in use", .{ selected_continuation });
    }

    // occasionally useful for debugging changes that cause unexpected increases in continuation use:
    // if (@import("instruction_builder.zig").insn) |i| {
    //     if (i.initial_uc_address) |ua| {
    //         @import("instruction_builder.zig").printCyclePath(ua, i.encoding);
    //     }
    // }
    // std.debug.print("Allocating conditional continuation {X:0>3}:\n", .{ selected_continuation });

    const copied_cycle_data = perm_arena.allocator().dupe(ConditionalCycleData, cycle_data) catch @panic("Out of memory!");
    for (copied_cycle_data) |*cycle| {
        cycle.cs = dedupMicrocodeCycle(cycle.cs);
        var permutations = uc.flagPermutationIterator(unqueried_flags);
        while (permutations.next()) |unqueried_flag_permutation| {
            var combined_flags = cycle.flags;
            combined_flags.setUnion(unqueried_flag_permutation);
            const ua = uc.getAddressForContinuation(selected_continuation, combined_flags);
            putMicrocodeCycleNoDedup(ua, cycle.cs, cycle.used_signals);
        }
    }

    conditional_continuations.put(.{
        .cycles = copied_cycle_data,
        .unqueried_flags = unqueried_flags,
    }, selected_continuation) catch @panic("Out of memory!");

    return selected_continuation;
}

pub fn getUnconditionalContinuationsLeft() usize {
    const min_n = @enumToInt(uc.Vectors.last) + 1;
    return next_unconditional_continuation - min_n + 1;
}

pub fn getConditionalContinuationsLeft() usize {
    return next_conditional_continuation - 0x200 + 1;
}

pub fn getMicrocodeCycle(ua: uc.Address) ?*ControlSignals {
    return microcode[ua];
}

pub fn getUsedSignalsForAddress(ua: uc.Address) std.EnumSet(ControlSignals.SignalName) {
    return used_signals[ua];
}

pub fn dedupMicrocodeCycle(cycle: *ControlSignals) *ControlSignals {
    if (cycle_dedup.get(cycle)) |c| {
        return c;
    } else {
        var deduped = perm_arena.allocator().create(ControlSignals) catch @panic("Out of memory!");
        deduped.* = cycle.*;
        cycle_dedup.put(deduped, deduped) catch @panic("Out of memory!");
        return deduped;
    }
}

pub fn putMicrocodeCycleOpcodePermutations(opcode: Opcode, queried_flag_permutation: uc.FlagSet, unqueried_flags: uc.FlagSet, cycle: *ControlSignals, signals_used_in_cycle: std.EnumSet(ControlSignals.SignalName)) *ControlSignals {
    const deduped = dedupMicrocodeCycle(cycle);
    var permutations = uc.flagPermutationIterator(unqueried_flags);
    while (permutations.next()) |unqueried_flag_permutation| {
        var combined_flags = queried_flag_permutation;
        combined_flags.setUnion(unqueried_flag_permutation);
        const ua = uc.getAddressForOpcode(opcode, combined_flags);
        putMicrocodeCycleNoDedup(ua, deduped, signals_used_in_cycle);
    }
    return deduped;
}

pub fn putMicrocodeCycle(ua: uc.Address, cycle: *ControlSignals, signals_used_in_cycle: std.EnumSet(ControlSignals.SignalName)) *ControlSignals {
    const deduped = dedupMicrocodeCycle(cycle);
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
            if (in.eqlExceptOpcodes(p.*)) {
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
            panic("Opcode {X} has already been assigned to {s}\n", .{
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

pub fn recordAlias(insn: InstructionEncoding, desc: ?[]const u8) void {
    var alias = aliases.addOne() catch @panic("Out of memory");
    alias.encoding = insn;
    alias.desc = desc orelse "";
}

pub fn validateAliases() !void {
    var stderr = std.io.getStdErr().writer();
    for (aliases.items) |alias| {
        var opcode_iter = uc.opcodeIterator(alias.encoding.opcodes.min, alias.encoding.opcodes.max);
        while (opcode_iter.next()) |opcode| {
            if (getInstructionByOpcode(opcode)) |encoding| {
                const original_len = encoding.getInstructionLength();
                const alias_len = alias.encoding.getInstructionLength();
                if (original_len != alias_len and !(encoding.mnemonic == .NOPE and alias.encoding.mnemonic == .B and alias.encoding.suffix == .none)) {
                    try stderr.print("Alias for opcode {X:0>4} has length {} but expected lenth is {}\n", .{ opcode, alias_len, original_len });
                }
            } else {
                try stderr.print("Alias defined for opcode {X:0>4} but there is no non-alias definition for it!\n", .{ opcode });
            }
        }
    }
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

pub fn writeInstructionEncodings(inner_writer: anytype) !void {
    const InnerWriter = @TypeOf(inner_writer);
    var writer = sx.Writer(InnerWriter).init(allocators.temp_arena.allocator(), inner_writer);
    try writer.openExpanded();

    for (instructions, descriptions, 0..) |maybe_insn, maybe_desc, opcode| {
        if (maybe_insn) |i| {
            if (i.opcodes.min == opcode) {
                try ie.data.writeInstructionEncoding(InnerWriter, &writer, i.*, maybe_desc, false);
            }
        }
    }
    for (aliases.items) |alias| {
        try ie.data.writeInstructionEncoding(InnerWriter, &writer, alias.encoding, alias.desc, false);
    }

    try writer.done();
}
