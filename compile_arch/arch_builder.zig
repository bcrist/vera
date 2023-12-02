

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



//const Instruction_Encoding = isa.Instruction_Encoding;
const isa = arch.isa;
const Control_Signals = hw.Control_Signals;
const hw = arch.hw;
const arch = @import("lib_arch");
const deep_hash_map = @import("deep_hash_map");
const bits = @import("bits");
const std = @import("std");
