const std = @import("std");
const builtin = @import("builtin");
const native_os = builtin.target.os.tag;

pub fn writer(comptime Address: type, inner_writer: anytype, header_data: []const u8, pretty: bool) !Writer(Address, @TypeOf(inner_writer)) {
    return Writer(Address, @TypeOf(inner_writer)).init(inner_writer, header_data, pretty);
}

pub fn Writer(comptime Address: type, comptime InnerWriter: type) type {
    switch (@typeInfo(Address).Int.bits) {
        16, 24, 32 => {},
        else => @compileError("Invalid address type; must be u32, u24, or u16"),
    }

    return struct {
        inner: InnerWriter,
        data_rec_count: usize,
        pretty: bool,

        const Self = @This();

        pub fn init(inner_writer: InnerWriter, header_data: []const u8, pretty: bool) !Self {
            var self = Self{
                .inner = inner_writer,
                .data_rec_count = 0,
                .pretty = pretty,
            };
            try self.writeRecord('0', 0, header_data);
            return self;
        }

        fn writeByte(self: *Self, d: u8) !void {
            try self.inner.writeByte("0123456789ABCDEF"[d >> 4]);
            try self.inner.writeByte("0123456789ABCDEF"[@truncate(u4, d)]);
        }

        fn writeRecord(self: *Self, record_type: u8, address: anytype, data: []const u8) !void {
            const A = @TypeOf(address);
            std.debug.assert(@bitSizeOf(A) <= 32);

            try self.inner.writeByte('S');
            try self.inner.writeByte(record_type);

            if (self.pretty) {
                try self.inner.writeByte(' ');
            }

            var checksum = @intCast(u8, data.len + 3);
            try self.writeByte(checksum);

            if (self.pretty) {
                try self.inner.writeByte(' ');
            }

            if (comptime @bitSizeOf(A) > 24) {
                const address_part = @truncate(u8, address >> 24);
                try self.writeByte(address_part);
                checksum +%= address_part;
            }
            if (comptime @bitSizeOf(A) > 16) {
                const address_part = @truncate(u8, address >> 16);
                try self.writeByte(address_part);
                checksum +%= address_part;
            }

            const address_high = @truncate(u8, address >> 8);
            try self.writeByte(address_high);
            checksum +%= address_high;

            const address_low = @truncate(u8, address);
            try self.writeByte(address_low);
            checksum +%= address_low;

            if (self.pretty) {
                try self.inner.writeByte(' ');
            }

            for (data) |d| {
                try self.writeByte(d);
                checksum +%= d;
            }

            if (self.pretty) {
                try self.inner.writeByte(' ');
            }

            try self.writeByte(checksum ^ 0xFF);

            if (native_os == .windows) {
                try self.inner.writeByte('\r');
            }
            try self.inner.writeByte('\n');

            self.data_rec_count += 1;
        }

        pub fn write(self: *Self, address: Address, data: []const u8) !void {
            var start = address;
            var remaining = data;

            const data_record_type = switch(@bitSizeOf(Address)) {
                16 => '1',
                24 => '2',
                32 => '3',
                else => unreachable,
            };

            while (remaining.len > 32) {
                try self.writeRecord(data_record_type, start, remaining[0..32]);
                start += 32;
                remaining = remaining[32..];
            }

            try self.writeRecord(data_record_type, start, remaining);
        }

        pub fn finish(self: *Self, termination_address: Address) !void {
            if (self.data_rec_count <= 0xFFFF) {
                try self.writeRecord('5', @intCast(u16, self.data_rec_count), "");
            } else if (self.data_rec_count <= 0xFFFFFF) {
                try self.writeRecord('6', @intCast(u24, self.data_rec_count), "");
            }

            const termination_record_type = switch (@bitSizeOf(Address)) {
                16 => '9',
                24 => '8',
                32 => '7',
                else => unreachable,
            };
            try self.writeRecord(termination_record_type, termination_address, "");
        }
    };
}
