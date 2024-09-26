pub const Rom = [Address.count]Result;

pub const Address = arch.IR;

pub const Result = packed struct (u24) {
    entry: microcode.Slot,
    cv: CV_Mode,
    wio: arch.Write_Index_Offset,
    krio: arch.K.Read_Index_Offset,

    pub inline fn init(raw_value: Raw) Result {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Result) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = std.meta.Int(.unsigned, @bitSizeOf(Result));
};

pub const CV_Mode = enum(u2) {
    zero,
    one,
    c,
    v,

    pub inline fn init(raw_value: Raw) CV_Mode {
        return @enumFromInt(raw_value);
    }

    pub inline fn raw(self: CV_Mode) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum;

    pub const Raw = std.meta.Tag(CV_Mode);
};

pub fn write_compressed_rom(result_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator, rom_data: *const Rom) ![]const u8 {
    const Rom_Entry = rom_compress.Entry(Address.Raw, Result.Raw);
    var entries = try std.ArrayList(Rom_Entry).initCapacity(temp_allocator, rom_data.len);
    defer entries.deinit();
    for (0.., rom_data) |addr_usize, result_data| {
        // Note we're byte swapping the addresses here so that 8 bit opcodes appear as contiguous regions to the compression algorithm.
        // It requires also byte swapping when decoding to get the correct IR data out.
        const addr = Address.init(@intCast(addr_usize)).to_byte_swapped();
        const data: Result.Raw = result_data.raw();
        try entries.append(Rom_Entry.init(addr, data));
    }
    return try rom_compress.compress(Rom_Entry, result_allocator, temp_allocator, entries.items);
}

pub fn read_compressed_rom(comptime n: u8, compressed_data: []const u8, results: *Rom) void {
    const Decompress_Context = struct {
        results: *Rom,
        d: Result = undefined,

        const Self = @This();

        pub fn data(self: *Self, d: u32) void {
            self.d = Result.init(@intCast(d));
        }

        pub fn address(self: *Self, a: u32) void {
            const addr = Address.from_byte_swapped(@intCast(a)).raw();
            const buf = std.mem.asBytes(&self.results[addr]);
            buf[n] = self.d;
        }
    };
    var ctx: Decompress_Context = .{ .results = results };
    rom_decompress.decompress(compressed_data, &ctx);
}

pub fn write_srec_rom(result_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator, rom_data: *const Rom) ![]u8 {
    var temp = std.ArrayList(u8).init(temp_allocator);
    defer temp.deinit();

    var encoded_data = std.ArrayList(u8).init(temp_allocator);
    defer encoded_data.deinit();

    var writer = try srec.writer(u24, encoded_data.writer(), .{
        .header_data = "Vera Instruction Decode ROM",
        .pretty = true,
    });

    for (rom_data) |result| {
        const data = std.mem.toBytes(result.raw());
        try temp.appendSlice(&data);
    }

    try writer.write(0, temp.items);
    try writer.finish(0);

    return result_allocator.dupe(u8, encoded_data.items);
}

pub fn write_ihex_rom(result_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator, rom_data: *const Rom) ![]u8 {
    var temp = std.ArrayList(u8).init(temp_allocator);
    defer temp.deinit();

    var encoded_data = std.ArrayList(u8).init(temp_allocator);
    defer encoded_data.deinit();

    var writer = ihex.writer(u32, encoded_data.writer(), .{
        .pretty = true,
    });

    for (rom_data) |result| {
        const data = std.mem.toBytes(result.raw());
        try temp.appendSlice(&data);
    }

    try writer.write(0, temp.items);
    try writer.finish(0);

    return result_allocator.dupe(u8, encoded_data.items);
}

pub fn write_csv(result_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator, rom_data: *const Rom) ![]u8 {
    var out = std.ArrayList(u8).init(temp_allocator);
    defer out.deinit();

    var w = out.writer();

    try w.writeAll("IR,entry,cv,wio,krio\n");

    for (0.., rom_data) |raw_addr, data| {
        if (data.entry != .invalid_instruction) {
            const addr = Address.init(@intCast(raw_addr));
            try w.print("{},{},{},{},{}\n", .{ addr, data.entry, data.cv, data.wio, data.krio });
        }
    }

    return result_allocator.dupe(u8, out.items);
}

const arch = @import("../arch.zig");
const fmt = @import("fmt.zig");
const microcode = @import("microcode.zig");
const rom_compress = @import("rom_compress");
const rom_decompress = @import("rom_decompress");
const srec = @import("srec");
const ihex = @import("ihex");
const std = @import("std");
