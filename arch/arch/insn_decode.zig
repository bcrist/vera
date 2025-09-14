pub const Rom = [Address.count]Result;

pub const Address = reg.IR;

pub const Result = packed struct (u24) {
    entry: microcode.Slot,
    cv: CV_Mode,
    wio: reg.gpr.Write_Index_Offset,
    krio: bus.K.Read_Index_Offset,

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

    pub const format = fmt.format_enum_dec;

    pub const Raw = meta.Backing(CV_Mode);
};

pub fn write_compressed_rom(temp_allocator: std.mem.Allocator, w: *std.io.Writer, rom_data: *const Rom) !void {
    const Rom_Entry = rom_compress.Entry(Address.Raw, Result.Raw);
    var entries = try std.ArrayList(Rom_Entry).initCapacity(temp_allocator, rom_data.len);
    defer entries.deinit(temp_allocator);
    for (0.., rom_data) |addr_usize, result_data| {
        // Note we're byte swapping the addresses here so that 8 bit opcodes appear as contiguous regions to the compression algorithm.
        // It requires also byte swapping when decoding to get the correct IR data out.
        const addr = Address.init(@intCast(addr_usize)).to_byte_swapped();
        const data: Result.Raw = result_data.raw();
        entries.appendAssumeCapacity(Rom_Entry.init(addr, data));
    }
    return try rom_compress.compress(Rom_Entry, temp_allocator, w, entries.items);
}

pub fn read_compressed_rom(compressed_data: []const u8, results: *Rom) void {
    const Decompress_Context = struct {
        results: *Rom,
        d: Result = undefined,

        const Self = @This();

        pub fn data(self: *Self, d: u32) void {
            self.d = Result.init(@intCast(d));
        }

        pub fn address(self: *Self, a: u32) void {
            const addr = Address.from_byte_swapped(@intCast(a)).raw();
            self.results[addr] = self.d;
        }
    };
    var ctx: Decompress_Context = .{ .results = results };
    rom_decompress.decompress(compressed_data, &ctx);
}

pub fn write_srec_rom(temp_allocator: std.mem.Allocator, w: *std.io.Writer, rom_data: *const Rom) !void {
    var temp: std.ArrayList(u8) = .empty;
    defer temp.deinit(temp_allocator);

    var writer = try srec.writer(u24, w, .{
        .header_data = "Vera Instruction Decode ROM",
        .pretty = true,
    });

    for (rom_data) |result| {
        const data = std.mem.toBytes(result.raw());
        try temp.appendSlice(temp_allocator, &data);
    }

    try writer.write(0, temp.items);
    try writer.finish(0);
}

pub fn write_ihex_rom(temp_allocator: std.mem.Allocator, w: *std.io.Writer, rom_data: *const Rom) !void {
    var temp: std.ArrayList(u8) = .empty;
    defer temp.deinit(temp_allocator);

    var writer = ihex.writer(u32, w, .{
        .pretty = true,
    });

    for (rom_data) |result| {
        const data = std.mem.toBytes(result.raw());
        try temp.appendSlice(temp_allocator, &data);
    }

    try writer.write(0, temp.items);
    try writer.finish(0);
}

pub fn write_csv(w: *std.io.Writer, rom_data: *const Rom) !void {
    try w.writeAll("IR,entry,cv,wio,krio\n");

    for (0.., rom_data) |raw_addr, data| {
        if (data.entry != .invalid_instruction) {
            const addr = Address.init(@intCast(raw_addr));
            try w.print("{f},{f},{f},{f},{f}\n", .{ addr, data.entry, data.cv, data.wio, data.krio });
        }
    }
}

const bus = @import("bus.zig");
const reg = @import("reg.zig");
const fmt = @import("fmt.zig");
const microcode = @import("microcode.zig");
const rom_compress = @import("rom_compress");
const rom_decompress = @import("rom_decompress");
const srec = @import("srec");
const ihex = @import("ihex");
const meta = @import("meta");
const std = @import("std");
