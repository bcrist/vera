pub const Rom = [Address.count]Control_Signals;

pub const Address = packed struct (u16) {
    flags: Flags,
    slot: Slot,

    pub inline fn init(raw_value: Raw) Address {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Address) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = u16;
    pub const count = std.math.maxInt(Raw) + 1;
    pub const count_per_slot = std.math.maxInt(Flags.Raw) + 1;
};

pub const Slot = enum (u12) {
    reset = 0,
    page_fault = 1,
    access_fault = 2,
    page_align_fault = 3,
    align_fault = 4,
    overflow_fault = 5,
    instruction_protection_fault = 6,
    invalid_instruction_fault = 7,
    double_fault = 8,
    interrupt = 9,
    invalid_instruction = 0xFFF,
    _,
    pub inline fn init(raw_value: Raw) Slot {
        return @enumFromInt(raw_value);
    }
    pub inline fn raw(self: Slot) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum_hex;

    pub const Raw = std.meta.Tag(Slot); 
    pub const first = init(Slot.interrupt.raw() + 1);
    pub const last = init(Slot.invalid_instruction.raw() - 1);
    pub const count = std.math.maxInt(Raw) + 1;

    pub const Source = enum (u2) {
        hold = 0,
        insn_decoder = 1,
        continuation = 2,
        seq_literal = 3,

        pub inline fn init(raw_value: Source.Raw) Source {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Source) Source.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum;

        pub const Raw = std.meta.Tag(Source);
    };
};

pub const Flags = packed struct (u4) {
    cv: bool,
    n: bool,
    z: bool,
    k: bool,

    pub inline fn init(raw_value: Raw) Flags {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Flags) Raw {
        return @bitCast(self);
    }

    pub fn format(self: Flags, comptime f: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = f;
        _ = options;

        try writer.writeByte(if (self.k) 'K' else '.');
        try writer.writeByte(if (self.z) 'Z' else '.');
        try writer.writeByte(if (self.n) 'N' else '.');
        try writer.writeByte(if (self.cv) 'X' else '.');
    }

    pub inline fn zero(self: Flags) bool { return self.z; }
    pub inline fn negative(self: Flags) bool { return self.n; }
    pub inline fn positive(self: Flags) bool { return !self.z and !self.n; }
    pub inline fn kernel(self: Flags) bool { return self.k; }

    pub const Raw = u4;
    pub const count = std.math.maxInt(Raw) + 1;
};

pub const Flags_With_Carry = packed struct (u4) {
    c: bool,
    n: bool,
    z: bool,
    k: bool,

    pub inline fn init(raw_value: Raw) Flags_With_Carry {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Flags_With_Carry) Raw {
        return @bitCast(self);
    }

    pub fn format(self: Flags, comptime f: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = f;
        _ = options;

        try writer.writeByte(if (self.k) 'K' else '.');
        try writer.writeByte(if (self.z) 'Z' else '.');
        try writer.writeByte(if (self.n) 'N' else '.');
        try writer.writeByte(if (self.c) 'C' else '.');
    }

    pub inline fn zero(self: Flags_With_Carry) bool { return self.z; }
    pub inline fn negative(self: Flags_With_Carry) bool { return self.n; }
    pub inline fn positive(self: Flags_With_Carry) bool { return !self.z and !self.n; }
    pub inline fn carry(self: Flags_With_Carry) bool { return self.c; }
    pub inline fn kernel(self: Flags_With_Carry) bool { return self.k; }
    pub inline fn unsigned_less_than(self: Flags_With_Carry) bool { return !self.c and !self.z; } // checking z here isn't actually necessary, but makes the intention clear
    pub inline fn unsigned_greater_than(self: Flags_With_Carry) bool { return self.c and !self.z; }

    pub const Raw = u4;
};

pub const Flags_With_Overflow = packed struct (u4) {
    v: bool,
    n: bool,
    z: bool,
    k: bool,

    pub inline fn init(raw_value: Raw) Flags_With_Carry {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Flags_With_Carry) Raw {
        return @bitCast(self);
    }

    pub fn format(self: Flags, comptime f: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = f;
        _ = options;

        try writer.writeByte(if (self.k) 'K' else '.');
        try writer.writeByte(if (self.z) 'Z' else '.');
        try writer.writeByte(if (self.n) 'N' else '.');
        try writer.writeByte(if (self.v) 'V' else '.');
    }

    pub inline fn zero(self: Flags_With_Carry) bool { return self.z; }
    pub inline fn negative(self: Flags_With_Carry) bool { return self.n; }
    pub inline fn positive(self: Flags_With_Carry) bool { return !self.z and !self.n; }
    pub inline fn overflow(self: Flags_With_Carry) bool { return self.v; }
    pub inline fn kernel(self: Flags_With_Carry) bool { return self.k; }
    pub inline fn signed_less_than(self: Flags_With_Carry) bool { return !self.z and self.n != self.v; }
    pub inline fn signed_greater_than(self: Flags_With_Carry) bool { return !self.z and self.n == self.v; }

    pub const Raw = u4;
};

pub const Setup_Microcode_Entry = packed struct (u24) {
    vao: arch.addr.Virtual.Microcode_Offset,
    vari: arch.addr.Virtual.Base_SR_Index,
    sr1ri: arch.SR1_Index,
    sr2ri: arch.SR2_Index,
    jsrc: arch.J.Source,
    ksrc: arch.K.Source,

    pub inline fn init(raw_value: Raw) Setup_Microcode_Entry {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Setup_Microcode_Entry) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = u24;
};

pub const Compute_Microcode_Entry = packed struct (u24) {
    mode: arch.Compute_Mode,
    unit: arch.Compute_Unit,
    width: arch.D.Width,
    dir: arch.D.Direction,
    vaspace: arch.addr.Space,
    atop: arch.addr.translation.Op,
    sr2wsrc: arch.SR_Write_Source,
    sr2wi: arch.SR2_Index,
    special: arch.Special_Op,

    pub inline fn init(raw_value: Raw) Compute_Microcode_Entry {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Compute_Microcode_Entry) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = u24;
};

pub const Transact_Microcode_Entry = packed struct (u32) {
    next: Slot,
    sr1wi: arch.SR1_Index,
    sr1wsrc: arch.SR_Write_Source,
    seqop: arch.Sequencer_Op,
    statop: arch.Status.Op,
    tiw: bool,
    gprw: bool,
    drw: bool,
    irw: bool,
    lsrc: arch.L.Source,
    allowint: bool,
    power: arch.Power_Mode,

    pub inline fn init(raw_value: Raw) Transact_Microcode_Entry {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Transact_Microcode_Entry) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = u32;
};

pub fn write_compressed_rom(comptime Entry: type, result_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator, microcode: *const [Address.count]?Control_Signals) ![]const u8 {
    const Rom_Entry = rom_compress.Entry(Address.Raw, Entry.Raw);
    var entries = try std.ArrayList(Rom_Entry).initCapacity(temp_allocator, microcode.len);
    defer entries.deinit();
    for (microcode, 0..) |optional_cs, ua| {
        if (optional_cs) |cs| {
            const addr: Address.Raw = @intCast(ua);
            var entry: Entry = undefined;
            inline for (@typeInfo(Entry).Struct.fields) |field_info| {
                if (field_info.name[0] == '_') continue;
                @field(entry, field_info.name) = @field(cs, field_info.name);
            }
            try entries.append(Rom_Entry.init(addr, entry.raw()));
        }
    }
    return try rom_compress.compress(Rom_Entry, result_allocator, temp_allocator, entries.items);
}

pub fn read_compressed_rom(comptime Entry: type, compressed_data: []const u8, microcode: *Rom) void {
    const Decompress_Context = struct {
        microcode: *Rom,
        d: Entry = undefined,

        const Self = @This();

        pub fn data(self: *Self, d: u32) void {
            self.d = Entry.init(@intCast(d));
        }

        pub fn address(self: *Self, a: u32) void {
            inline for (@typeInfo(Entry).Struct.fields) |field_info| {
                if (field_info.name[0] == '_') continue;
                @field(self.microcode[a], field_info.name) = @field(self.d, field_info.name);
            }
        }
    };
    var ctx: Decompress_Context = .{ .microcode = microcode };
    rom_decompress.decompress(compressed_data, &ctx);
}

pub fn write_srec_rom(comptime Entry: type, result_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator, microcode: *const [Address.count]?Control_Signals) ![]u8 {
    var temp = std.ArrayList(u8).init(temp_allocator);
    defer temp.deinit();

    var encoded_data = std.ArrayList(u8).init(temp_allocator);
    defer encoded_data.deinit();

    const rom_name = comptime name: {
        var rom_name: []const u8 = @typeName(Entry);
        if (std.mem.lastIndexOf(u8, rom_name, ".")) |index| {
            rom_name = rom_name[index + 1 ..];
        }
        break :name rom_name;
    };

    var writer = try srec.writer(u24, encoded_data.writer(), .{
        .header_data = "Vera Microcode ROM: " ++ rom_name,
        .pretty = true,
    });

    var start: u24 = undefined;
    for (microcode, 0..) |optional_cs, addr| {
        if (optional_cs) |cs| {
            var entry: Entry = undefined;
            inline for (@typeInfo(Entry).Struct.fields) |field_info| {
                if (field_info.name[0] == '_') continue;
                @field(entry, field_info.name) = @field(cs, field_info.name);
            }

            if (temp.items.len == 0) {
                start = @intCast(addr * @sizeOf(Entry));
            }

            const bytes = std.mem.toBytes(entry.raw());
            try temp.appendSlice(&bytes);
        } else if (temp.items.len > 0) {
            try writer.write(start, temp.items);
            temp.clearRetainingCapacity();
        }
    }

    if (temp.items.len > 0) {
        try writer.write(start, temp.items);
    }

    try writer.finish(0);

    return result_allocator.dupe(u8, encoded_data.items);
}

pub fn write_csv(result_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator, microcode: *const [Address.count]?Control_Signals, fn_names: ?*const [Slot.count][]const u8) ![]u8 {
    var out = std.ArrayList(u8).init(temp_allocator);
    defer out.deinit();

    var w = out.writer();

    if (fn_names) |_| {
        try w.writeAll("UCA,Slot,fn,Flags");
    } else {
        try w.writeAll("UCA,Slot,Flags");
    }

    for (std.enums.values(arch.Control_Signal)) |signal| {
        try w.writeByte(',');
        try w.writeAll(@tagName(signal));
    }
    try w.writeByte('\n');

    for (0..Slot.count) |slot| {
        const addr0: Address = .{
            .slot = Slot.init(@intCast(slot)),
            .flags = Flags.init(0),
        };

        const unconditional = for (1..Flags.count) |raw_flags| {
            const addr: Address = .{
                .slot = Slot.init(@intCast(slot)),
                .flags = Flags.init(@intCast(raw_flags)),
            };
            const cs0 = microcode[addr0.raw()];
            const cs = microcode[addr.raw()];
            if (cs0 == null and cs == null) continue;

            if (!(cs0 orelse break false).eql(cs orelse break false)) break false;
        } else true;

        if (unconditional) {
            if (microcode[addr0.raw()]) |cs| {
                if (fn_names) |names| {
                    try w.print("{},{},{s},****", .{ addr0, addr0.slot, names[slot] });
                } else {
                    try w.print("{},{},****", .{ addr0, addr0.slot });
                }
                try write_csv_signals(w, cs);
                try w.writeByte('\n');
            }
        } else for (0..Flags.count) |raw_flags| {
            const addr: Address = .{
                .slot = Slot.init(@intCast(slot)),
                .flags = Flags.init(@intCast(raw_flags)),
            };
            if (fn_names) |names| {
                try w.print("{},{},{s},{}", .{ addr, addr.slot, names[slot], addr.flags });
            } else {
                try w.print("{},{},{}", .{ addr, addr.slot, addr.flags });
            }
            if (microcode[addr.raw()]) |cs| {
                try write_csv_signals(w, cs);
            } else {
                for (std.enums.values(arch.Control_Signal)) |_| {
                    try w.writeByte(',');
                }
            }
            try w.writeByte('\n');
        }
    }

    return result_allocator.dupe(u8, out.items);
}

fn write_csv_signals(w: anytype, cs: Control_Signals) !void {
    inline for (comptime std.enums.values(arch.Control_Signal)) |signal| {
        const value = @field(cs, @tagName(signal));
        switch (signal) {
            .gprw => try w.writeAll(if (value) ",GPRW" else ","),
            .irw => try w.writeAll(if (value) ",IRW" else ","),
            .drw => try w.writeAll(if (value) ",DRW" else ","),
            .tiw => try w.writeAll(if (value) ",TIW" else ","),
            .allowint => try w.writeAll(if (value) ",ALLOWINT" else ","),
            .power => try w.writeAll(switch (value) {
                .run => ",",
                .sleep => ",SLEEP",
            }),
            else => try w.print(",{}", .{ value }),
        }
    }
}

const Control_Signals = @import("Control_Signals.zig");
const fmt = @import("fmt.zig");
const arch = @import("../arch.zig");
const rom_compress = @import("rom_compress");
const rom_decompress = @import("rom_decompress");
const srec = @import("srec");
const std = @import("std");
