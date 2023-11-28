

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


//const Instruction_Encoding = isa.Instruction_Encoding;
const isa = arch.isa;
const Control_Signals = hw.Control_Signals;
const hw = arch.hw;
const arch = @import("lib_arch");
const deep_hash_map = @import("deep_hash_map");
const bits = @import("bits");
const std = @import("std");
