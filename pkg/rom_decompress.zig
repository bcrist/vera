const std = @import("std");
const bits = @import("bits");

const Range = struct {
    offset: u16,
    count: u8,
    bytes: u8,

    fn read(data: []const u8) Range {
        var r: Range = undefined;

        var initial = data[0];
        if ((initial & 1) == 0) {
            r.bytes = 1;
            r.count = 1;
            r.offset = @truncate(u7, initial >> 1);
        } else if ((initial & 2) == 0) {
            var encoded_count = @truncate(u3, initial >> 2);
            r.bytes = 2;
            r.count = if (encoded_count == 0) 0 else @as(u7, 1) << (encoded_count - 1);
            r.offset = bits.concat(.{
                @intCast(u3, initial >> 5),
                data[1],
            });
        } else { // (initial & 3) == 3
            r.bytes = 3;
            r.count = @truncate(u6, initial >> 2);
            r.offset = bits.concat(.{
                data[1],
                data[2],
            });
        }

        // const print = @import("std").debug.print;

        // for (data[0..r.bytes]) |b| {
        //     print("{X:0>2} ", .{ b });
        // }
        // print(" = Range{} offset={} count={}\n", .{ r.bytes, r.offset, r.count });

        return r;
    }
};

const RangeIterator = struct {
    data: []const u8 = &[_]u8{},
    ranges_remaining: u8 = 0,

    fn next(self: *RangeIterator) ?Range {
        if (self.ranges_remaining == 0) return null;
        var r = Range.read(self.data);
        self.data = self.data[r.bytes..];
        self.ranges_remaining -= 1;
        return r;
    }
};


pub fn decompress(input: []const u8, context: anytype) void {
    var data = input;

    var d: u32 = 0;
    var a: u32 = 0;

    while (data.len > 0) {
        var rec = Range.read(data);
        data = data[rec.bytes..];

        d += rec.offset;
        if (rec.offset > 0) {
            a = 0;
        }

        if (rec.count > 0) {
            context.data(d);

            var iter = RangeIterator {
                .data = data,
                .ranges_remaining = rec.count,
            };
            while (iter.next()) |range| {
                a +%= range.offset;
                var count = range.count;
                while (count > 0) : (count -= 1) {
                    context.address(a);
                    a += 1;
                }
            }

            data = iter.data;
        }
    }
}


test "rom_compress/decompress idempotence" {
    const compress = @import("rom_compress");
    const Entry = compress.Entry(u32, u16);

    const n = 65536;
    var raw_test_data = [_]u16 {0} ** n;
    var test_data = [_]Entry { .{ .addr = undefined, .data = undefined } } ** n;

    var rng = std.rand.Xoroshiro128.init(1234);
    var rnd = rng.random();
    var i: usize = 0;
    while (i < test_data.len) : (i += 1) {
        const v = if (i < 32768) @as(u16, rnd.int(u4)) else rnd.int(u16);
        raw_test_data[i] = v;
        test_data[i] = .{
            .addr = @intCast(u32, i),
            .data = v,
        };
    }

    var compressed = try compress.compress(Entry, std.testing.allocator, std.testing.allocator, &test_data);
    defer std.testing.allocator.free(compressed);

    const Ctx = struct {
        test_data: []u16,
        d: u16 = undefined,

        const Self = @This();
        fn data(self: *Self, d: u32) void {
            //std.debug.print("Data: {}\n", .{ d });
            self.d = @intCast(u16, d);
        }

        fn address(self: *Self, a: u32) void {
            //std.debug.print("Address: {}\n", .{ a });
            if (self.d != self.test_data[a]) {
                //std.debug.print("Test data: {any}\n", .{ self.test_data });
                std.debug.print("Address: {}  Found: {}  Expected:  {}\n", .{ a, self.d, self.test_data[a] });
                @panic("rom compress/decompress error!");
            }
        }
    };

    var ctx = Ctx { .test_data = &raw_test_data };
    decompress(compressed, &ctx);
}
