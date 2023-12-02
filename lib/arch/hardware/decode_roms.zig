pub const num_roms = (@bitSizeOf(hw.decode.Result) + 7) / 8;

pub const Compressed_Rom_Data = [num_roms][]const u8;

const Rom_Entry = rom_compress.Entry(hw.decode.Address.Raw, u8);

pub fn write_compressed_roms(result_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator, data: []const hw.decode.Result) !Compressed_Rom_Data {
    var result: Compressed_Rom_Data = undefined;
    var entries = try std.ArrayList(Rom_Entry).initCapacity(temp_allocator, data.len);
    defer entries.deinit();
    for (0..num_roms) |rom| {
        entries.clearRetainingCapacity();
        for (0.., data) |addr_usize, result_data| {
            const addr: hw.decode.Address.Raw = @intCast(addr_usize);
            const u8_data: u8 = @truncate(result_data.raw() >> @intCast(8 * rom));
            try entries.append(Rom_Entry.init(addr, u8_data));
        }
        result[rom] = try rom_compress.compress(Rom_Entry, result_allocator, temp_allocator, entries.items);
    }
    return result;
}

fn read_compressed_rom(comptime n: u8, compressed_data: []const u8, results: []hw.decode.Result) void {
    const Decompress_Context = struct {
        results: []hw.decode.Result,
        d: u8 = undefined,

        const Self = @This();

        pub fn data(self: *Self, d: u32) void {
            self.d = @intCast(d);
        }

        pub fn address(self: *Self, a: u32) void {
            const buf = std.mem.asBytes(&self.results[a]);
            buf[n] = self.d;
        }
    };
    var ctx: Decompress_Context = .{ .results = results };
    rom_decompress.decompress(compressed_data, &ctx);
}

pub fn read_compressed_roms(roms: Compressed_Rom_Data, results: []hw.decode.Result) void {
    inline for (0..num_roms) |n| {
        read_compressed_rom(n, roms[n], results);
    }
}

fn convert_microcode_to_srec(result_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator, comptime n: u8, results: []const hw.decode.Result) ![]u8 {
    var rom_name = std.fmt.comptimePrint("Decode Rom {}", .{n});

    var temp = std.ArrayList(u8).init(temp_allocator);
    defer temp.deinit();

    var encoded_data = std.ArrayList(u8).init(temp_allocator);
    defer encoded_data.deinit();

    var writer = try srec.writer(u24, encoded_data.writer(), .{
        .header_data = rom_name,
        .pretty = true,
    });

    for (results) |result| {
        const data: u8 = @truncate(result.raw() >> @intCast(n * 8));
        try temp.append(data);
    }

    try writer.write(0, temp.items);
    try writer.finish(0);

    return result_allocator.dupe(u8, encoded_data.items);
}

pub fn write_srec_roms(result_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator, results: []const hw.decode.Result) !Compressed_Rom_Data {
    var result: Compressed_Rom_Data = undefined;
    inline for (0..num_roms) |n| {
        result[n] = try convert_microcode_to_srec(result_allocator, temp_allocator, n, results);
    }
    return result;
}

const Control_Signals = hw.Control_Signals;
const hw = arch.hw;
const arch = @import("lib_arch");
const rom_compress = @import("rom_compress");
const rom_decompress = @import("rom_decompress");
const srec = @import("srec");
const std = @import("std");
