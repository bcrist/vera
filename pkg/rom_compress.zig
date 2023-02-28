const std = @import("std");
const bits = @import("bits");

const Range = struct {
    offset: u32,
    count: u32,

    fn write(self: Range, dest: *std.ArrayList(u8)) std.mem.Allocator.Error!u32 {
        var offset = self.offset;
        var count = self.count;

        while (offset > 0xFFFF) {
            _ = try write(Range{
                .offset = 0xFFFF,
                .count = 0,
            }, dest);
            offset -= 0xFFFF;
        }

        if (offset < 128 and count == 1) {
            try dest.append(bits.concat(.{
                @as(u1, 0),
                @intCast(u7, offset),
            }));

        } else if (offset < 2048 and (count == 256 or count <= 64 and @popCount(count) == 1)) {
            const encoded_count: u3 = switch (count) {
                256 => 0,
                1 => 1,
                2 => 2,
                4 => 3,
                8 => 4,
                16 => 5,
                32 => 6,
                64 => 7,
                else => unreachable,
            };
            try dest.append(bits.concat(.{
                @as(u2, 1),
                encoded_count,
                @truncate(u3, offset),
            }));
            try dest.append(@intCast(u8, offset >> 3));

        } else {
            if (count > std.math.maxInt(u6)) {
                count = std.math.maxInt(u6);
            }

            try dest.append(bits.concat(.{
                @as(u2, 3),
                @intCast(u6, count),
            }));
            try dest.append(@truncate(u8, offset));
            try dest.append(@intCast(u8, offset >> 8));
        }

        return count;
    }
};

pub fn Entry(comptime A: type, comptime D: type) type {
    return struct {
        const Addr = A;
        const Data = D;
        const Self = @This();

        addr: A,
        data: D,

        pub fn init(addr: A, data: D) Self {
            return .{ .addr = addr, .data = data };
        }

        pub fn lessThan(_: ?void, e0: Self, e1: Self) bool {
            if (e0.data != e1.data) {
                return e0.data < e1.data;
            }

            return e0.addr < e1.addr;
        }
    };
}

pub fn compress(comptime E: type, result_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator, entries: []E, ) ![]u8 {
    // partition entries into groups having the same data value
    std.debug.assert(entries.len > 0);
    std.sort.sort(E, entries, @as(?void, null), E.lessThan);
    var d_partitions = std.ArrayList([]E).init(temp_allocator);
    defer d_partitions.deinit();
    {
        var partition = try d_partitions.addOne();
        partition.* = entries[0..1];
        var d = partition.*[0].data;
        for (entries[1..]) |entry| {
            if (d == entry.data) {
                partition.len += 1;
            } else {
                var new_partition = partition.*;
                new_partition.len += 1;
                new_partition = new_partition[new_partition.len-1..];
                partition = try d_partitions.addOne();
                partition.* = new_partition;
                d = new_partition[0].data;
            }
        }
    }

    var result_buffer = std.ArrayList(u8).init(temp_allocator);
    defer result_buffer.deinit();

    var addr_ranges = std.ArrayList(Range).init(temp_allocator);
    defer addr_ranges.deinit();

    var d: E.Data = 0;
    for (d_partitions.items) |partition| {
        var d_delta = partition[0].data - d;
        d = partition[0].data;

        var a: E.Addr = 0;

        addr_ranges.clearRetainingCapacity();
        for (partition) |entry| {
            var a_delta = entry.addr - a;
            a = entry.addr +% 1;

            if (a_delta == 0 and addr_ranges.items.len > 0) {
                var last_addr_range = &addr_ranges.items[addr_ranges.items.len - 1];
                if (last_addr_range.count < std.math.maxInt(u6) or (last_addr_range.count == std.math.maxInt(u6) and last_addr_range.offset < 2048)) {
                    last_addr_range.count += 1;
                    continue;
                }
            }

            if (a_delta > 0xFFFF) {
                var first_new_addr_range = try addr_ranges.addOne();
                var new_addr_range = first_new_addr_range;
                while (a_delta > 0xFFFF) {
                    a_delta -= 0xFFFF;
                    new_addr_range.count = 0;
                    new_addr_range = try addr_ranges.addOne();
                    new_addr_range.offset = 0xFFFF;
                }
                first_new_addr_range.offset = a_delta;
                new_addr_range.count = 1;
            } else {
                var new_addr_range = try addr_ranges.addOne();
                new_addr_range.offset = a_delta;
                new_addr_range.count = 1;
            }
        }

        var addr_ranges_to_write = addr_ranges.items;
        var range_to_write = Range {
            .offset = d_delta,
            .count = @intCast(u32, addr_ranges_to_write.len),
        };
        
        while (addr_ranges_to_write.len > 0) {
            var written_count = try Range.write(range_to_write, &result_buffer);
            for (addr_ranges_to_write[0..written_count]) |addr_range| {
                const written_addr_count = try Range.write(addr_range, &result_buffer);
                std.debug.assert(written_addr_count == addr_range.count);
            }
            range_to_write.offset = 0;
            range_to_write.count -= written_count;
            addr_ranges_to_write = addr_ranges_to_write[written_count..];
        }
    }

    return try result_allocator.dupe(u8, result_buffer.items);
}
