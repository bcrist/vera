const std = @import("std");
const builtin = @import("builtin");
const native_os = builtin.target.os.tag;

pub fn Encoder(comptime A: type) type {
    return struct {
        const Self = @This();
        const Addr = A;

        data: std.ArrayList(u8),
        data_rec_count: usize,
        pretty: bool = false,

        pub fn init(allocator: std.mem.Allocator, header_data: []const u8) !Self {
            switch (@bitSizeOf(Addr)) {
                16, 24, 32 => {},
                else => unreachable,
            }

            var self = Self {
                .data = std.ArrayList(u8).init(allocator),
                .data_rec_count = 0,
            };
            try self._encode16('0', 0, header_data);
            return self;
        }

        pub fn deinit(self: *Self) void {
            self.data.deinit();
        }

        fn _byte(self: *Self, d: u8) !u8 {
            try self.data.append("0123456789ABCDEF"[d >> 4]);
            try self.data.append("0123456789ABCDEF"[@truncate(u4, d)]);
            return d;
        }

        fn _encodeData(self: *Self, data: []const u8, initial_checksum: u8) !void {
            var checksum = initial_checksum;

            if (self.pretty) {
                try self.data.append(' ');
            }

            for (data) |d| {
                checksum +%= try self._byte(d);
            }

            if (self.pretty) {
                try self.data.append(' ');
            }

            _ = try self._byte(checksum ^ 0xFF);

            if (native_os == .windows) {
                try self.data.append('\r');
            }
            try self.data.append('\n');
        }

        fn _encode16(self: *Self, record_type: u8, address: u16, data: []const u8) !void {
            try self.data.append('S');
            try self.data.append(record_type);

            if (self.pretty) {
                try self.data.append(' ');
            }

            var checksum = try self._byte(@intCast(u8, data.len + 3));

            if (self.pretty) {
                try self.data.append(' ');
            }

            checksum +%= try self._byte(@intCast(u8, address >> 8));
            checksum +%= try self._byte(@truncate(u8, address));

            try self._encodeData(data, checksum);
        }

        fn _encode24(self: *Self, record_type: u8, address: u24, data: []const u8) !void {
            try self.data.append('S');
            try self.data.append(record_type);

            if (self.pretty) {
                try self.data.append(' ');
            }

            var checksum = try self._byte(@intCast(u8, data.len + 4));

            if (self.pretty) {
                try self.data.append(' ');
            }

            checksum +%= try self._byte(@intCast(u8, address >> 16));
            checksum +%= try self._byte(@truncate(u8, address >> 8));
            checksum +%= try self._byte(@truncate(u8, address));

            try self._encodeData(data, checksum);
        }

        fn _encode32(self: *Self, record_type: u8, address: u32, data: []const u8) !void {
            try self.data.append('S');
            try self.data.append(record_type);

            if (self.pretty) {
                try self.data.append(' ');
            }

            var checksum = try self._byte(@intCast(u8, data.len + 5));

            if (self.pretty) {
                try self.data.append(' ');
            }

            checksum +%= try self._byte(@intCast(u8, address >> 24));
            checksum +%= try self._byte(@truncate(u8, address >> 16));
            checksum +%= try self._byte(@truncate(u8, address >> 8));
            checksum +%= try self._byte(@truncate(u8, address));

            try self._encodeData(data, checksum);
        }

        pub fn encode(self: *Self, address: Addr, data: []const u8) !void {
            var start = address;
            var remaining = data;

            while (remaining.len > 32) {
                switch(@bitSizeOf(Addr)) {
                    16 => try self._encode16('1', @bitCast(u16, start), remaining[0..32]),
                    24 => try self._encode24('2', @bitCast(u24, start), remaining[0..32]),
                    32 => try self._encode32('3', @bitCast(u32, start), remaining[0..32]),
                    else => unreachable,
                }
                self.data_rec_count += 1;
                start += 32;
                remaining = remaining[32..];
            }

            switch(@bitSizeOf(Addr)) {
                16 => try self._encode16('1', @bitCast(u16, start), remaining),
                24 => try self._encode24('2', @bitCast(u24, start), remaining),
                32 => try self._encode32('3', @bitCast(u32, start), remaining),
                else => unreachable,
            }
            self.data_rec_count += 1;
        }

        pub fn finish(self: *Self, termination_address: Addr) !void {
            if (self.data_rec_count <= 0xFFFF) {
                try self._encode16('5', @intCast(u16, self.data_rec_count), &[_]u8{});
            } else if (self.data_rec_count <= 0xFFFFFF) {
                try self._encode24('6', @intCast(u24, self.data_rec_count), &[_]u8{});
            }

            switch(@bitSizeOf(Addr)) {
                16 => try self._encode16('9', @bitCast(u16, termination_address), &[_]u8{}),
                24 => try self._encode24('8', @bitCast(u24, termination_address), &[_]u8{}),
                32 => try self._encode32('7', @bitCast(u32, termination_address), &[_]u8{}),
                else => unreachable,
            }
        }
    };
}
