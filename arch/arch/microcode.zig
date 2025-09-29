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

    pub const Raw = meta.Backing(Address);
    pub const count = std.math.maxInt(Raw) + 1;
    pub const count_per_slot = std.math.maxInt(Flags.Raw) + 1;
};

pub const Slot = enum (u12) {
    reset = 0,
    double_fault = 1,
    page_fault = 2,
    access_fault = 3,
    pipe_fault = 4,
    page_align_fault = 5,
    align_fault = 6,
    overflow_fault = 7,
    rs_underflow_fault = 8,
    rs_overflow_fault = 9,
    invalid_instruction_fault = 10,
    instruction_protection_fault = 11,
    interrupt = 12,
    invalid_instruction = 0xFFF,
    _,
    pub inline fn init(raw_value: Raw) Slot {
        return @enumFromInt(raw_value);
    }
    pub inline fn raw(self: Slot) Raw {
        return @intFromEnum(self);
    }

    pub const format = fmt.format_enum_hex;

    pub const Raw = meta.Backing(Slot); 
    pub const first = init(Slot.interrupt.raw() + 1);
    pub const last = init(Slot.invalid_instruction.raw() - 1);
    pub const count = std.math.maxInt(Raw) + 1;

    pub const Source = enum (u2) {
        fucs = 0,
        insn_decoder = 1,
        continuation = 2,
        seq_literal = 3,

        pub inline fn init(raw_value: Source.Raw) Source {
            return @enumFromInt(raw_value);
        }

        pub inline fn raw(self: Source) Source.Raw {
            return @intFromEnum(self);
        }

        pub const format = fmt.format_enum_dec;

        pub const Raw = meta.Backing(Source);
    };

    pub const Sequencer_Literal = enum(u4) {
        reset = 0,
        double_fault = 1,
        page_fault = 2,
        access_fault = 3,
        pipe_fault = 4,
        page_align_fault = 5,
        align_fault = 6,
        overflow_fault = 7,
        rs_underflow_fault = 8,
        rs_overflow_fault = 9,
        invalid_instruction_fault = 10,
        instruction_protection_fault = 11,
        interrupt = 12,
        _,

        pub inline fn init(raw_value: Sequencer_Literal.Raw) Sequencer_Literal {
            return @enumFromInt(raw_value);
        }
        pub inline fn raw(self: Sequencer_Literal) Sequencer_Literal.Raw {
            return @intFromEnum(self);
        }

        pub fn from_slot(s: Slot) Sequencer_Literal {
            return .init(@intCast(@intFromEnum(s)));
        }
        pub fn slot(self: Sequencer_Literal) Slot {
            return .init(self.raw());
        }

        pub const format = fmt.format_enum_hex;

        pub const Raw = meta.Backing(Sequencer_Literal); 
        pub const count = std.math.maxInt(Sequencer_Literal.Raw) + 1;
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

    pub fn format(self: Flags, writer: *std.io.Writer) !void {
        try writer.writeByte(if (self.k) 'K' else '.');
        try writer.writeByte(if (self.z) 'Z' else '.');
        try writer.writeByte(if (self.n) 'N' else '.');
        try writer.writeByte(if (self.cv) 'X' else '.');
    }

    pub inline fn zero(self: Flags) bool { return self.z; }
    pub inline fn negative(self: Flags) bool { return self.n; }
    pub inline fn positive(self: Flags) bool { return !self.z and !self.n; }
    pub inline fn kernel(self: Flags) bool { return self.k; }

    pub const Raw = meta.Backing(Flags);
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

    pub fn format(self: Flags, writer: *std.io.Writer) !void {
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

    pub const Raw = meta.Backing(Flags_With_Carry);
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

    pub fn format(self: Flags, writer: *std.io.Writer) !void {
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

    pub const Raw = meta.Backing(Flags_With_Overflow);
};

pub const Setup_Microcode_Entry = packed struct (u32) {
    vao: addr.Virtual.Offset,
    vari: addr.Virtual.Base,
    sr1ri: reg.sr1.Index,
    sr2ri: reg.sr2.Index,
    jsrc: bus.J.Source,
    ksrc: bus.K.Source,
    special: misc.Special_Op,
    _unused: u4 = 0,

    pub inline fn init(raw_value: Raw) Setup_Microcode_Entry {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Setup_Microcode_Entry) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = meta.Backing(Setup_Microcode_Entry);
};

pub const Compute_Microcode_Entry = packed struct (u24) {
    mode: compute.Mode,
    unit: compute.Unit,
    width: bus.D.Width,
    dsrc: bus.D.Source,
    space: addr.Space,
    at_op: addr.translation.Op,
    sr2wsrc: reg.sr.Write_Source,
    sr2wi: reg.sr2.Index,
    _unused: u2 = 0,

    pub inline fn init(raw_value: Raw) Compute_Microcode_Entry {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Compute_Microcode_Entry) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = meta.Backing(Compute_Microcode_Entry);
};

pub const Transact_Microcode_Entry = packed struct (u24) {
    sr1wi: reg.sr1.Index,
    sr1wsrc: reg.sr.Write_Source,
    flag_op: reg.Flags.Op,
    tiw: misc.Generic_Write_Enable,
    gprw: misc.Generic_Write_Enable,
    drw: misc.Generic_Write_Enable,
    irw: misc.Generic_Write_Enable,
    lsrc: bus.L.Source,
    seq_op: misc.Sequencer_Op,
    _unused: u6 = 0,

    pub inline fn init(raw_value: Raw) Transact_Microcode_Entry {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Transact_Microcode_Entry) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = meta.Backing(Transact_Microcode_Entry);
};

pub const Decode_Microcode_Entry = packed struct (u16) {
    next: Slot,
    allow_int: misc.Interrupt_Enable,
    power: misc.Power_Mode,
    _unused: u2 = 0,

    pub inline fn init(raw_value: Raw) Decode_Microcode_Entry {
        return @bitCast(raw_value);
    }

    pub inline fn raw(self: Decode_Microcode_Entry) Raw {
        return @bitCast(self);
    }

    pub const format = fmt.format_raw_hex;

    pub const Raw = meta.Backing(Decode_Microcode_Entry);
};

pub fn write_compressed_rom(comptime Entry: type, temp_allocator: std.mem.Allocator, w: *std.io.Writer, microcode: *const [Address.count]?Control_Signals) !void {
    const Rom_Entry = rom_compress.Entry(Address.Raw, Entry.Raw);
    var entries = try std.ArrayList(Rom_Entry).initCapacity(temp_allocator, microcode.len);
    defer entries.deinit(temp_allocator);
    for (microcode, 0..) |optional_cs, ua| {
        if (optional_cs) |cs| {
            const address: Address.Raw = @intCast(ua);
            var entry: Entry = undefined;
            inline for (@typeInfo(Entry).@"struct".fields) |field_info| {
                if (field_info.name[0] == '_') continue;
                @field(entry, field_info.name) = @field(cs, field_info.name);
            }
            entries.appendAssumeCapacity(Rom_Entry.init(address, entry.raw()));
        }
    }
    return try rom_compress.compress(Rom_Entry, temp_allocator, w, entries.items);
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
            inline for (@typeInfo(Entry).@"struct".fields) |field_info| {
                if (field_info.name[0] == '_') continue;
                @field(self.microcode[a], field_info.name) = @field(self.d, field_info.name);
            }
        }
    };
    var ctx: Decompress_Context = .{ .microcode = microcode };
    rom_decompress.decompress(compressed_data, &ctx);
}

pub fn  write_srec_rom(comptime Entry: type, temp_allocator: std.mem.Allocator, w: *std.io.Writer, microcode: *const [Address.count]?Control_Signals) !void {
    var temp: std.ArrayList(u8) = .empty;
    defer temp.deinit(temp_allocator);

    const rom_name = comptime name: {
        var rom_name: []const u8 = @typeName(Entry);
        if (std.mem.lastIndexOf(u8, rom_name, ".")) |index| {
            rom_name = rom_name[index + 1 ..];
        }
        break :name rom_name;
    };

    var writer = try srec.writer(u24, w, .{
        .header_data = "Vera Microcode ROM: " ++ rom_name,
        .pretty = true,
    });

    var start: u24 = undefined;
    for (microcode, 0..) |optional_cs, address| {
        if (optional_cs) |cs| {
            var entry: Entry = undefined;
            inline for (@typeInfo(Entry).@"struct".fields) |field_info| {
                if (field_info.name[0] == '_') continue;
                @field(entry, field_info.name) = @field(cs, field_info.name);
            }

            if (temp.items.len == 0) {
                start = @intCast(address * @sizeOf(Entry));
            }

            const bytes = std.mem.toBytes(entry.raw());
            try temp.appendSlice(temp_allocator, &bytes);
        } else if (temp.items.len > 0) {
            try writer.write(start, temp.items);
            temp.clearRetainingCapacity();
        }
    }

    if (temp.items.len > 0) {
        try writer.write(start, temp.items);
    }

    try writer.finish(0);
}

pub fn write_ihex_rom(comptime Entry: type, temp_allocator: std.mem.Allocator, w: *std.io.Writer, microcode: *const [Address.count]?Control_Signals) !void {
    var temp: std.ArrayList(u8) = .empty;
    defer temp.deinit(temp_allocator);

    var writer = ihex.writer(u32, w, .{
        .pretty = true,
    });

    var start: u32 = undefined;
    for (microcode, 0..) |optional_cs, address| {
        if (optional_cs) |cs| {
            var entry: Entry = undefined;
            inline for (@typeInfo(Entry).@"struct".fields) |field_info| {
                if (field_info.name[0] == '_') continue;
                @field(entry, field_info.name) = @field(cs, field_info.name);
            }

            if (temp.items.len == 0) {
                start = @intCast(address * @sizeOf(Entry));
            }

            const bytes = std.mem.toBytes(entry.raw());
            try temp.appendSlice(temp_allocator, &bytes);
        } else if (temp.items.len > 0) {
            try writer.write(start, temp.items);
            temp.clearRetainingCapacity();
        }
    }

    if (temp.items.len > 0) {
        try writer.write(start, temp.items);
    }

    try writer.finish(0);
}

pub fn write_csv(temp_allocator: std.mem.Allocator, w: *std.io.Writer, microcode: *const [Address.count]?Control_Signals, fn_names: ?*const [Slot.count][]const u8) !void {
    if (fn_names) |_| {
        try w.writeAll("UCA,Slot,fn,Flags");
    } else {
        try w.writeAll("UCA,Slot,Flags");
    }

    for (std.enums.values(Control_Signal)) |signal| {
        try w.writeByte(',');
        try w.writeAll(@tagName(signal));
    }
    try w.writeByte('\n');

    const Conditional_Info = struct {
        flags: Flags.Raw,
        wildcards: Flags.Raw,
        cs: Control_Signals,
    };

    var conditional_temp: std.ArrayList(Conditional_Info) = .empty;
    defer conditional_temp.deinit(temp_allocator);

    for (0..Slot.count) |slot| {
        const addr0: Address = .{
            .slot = Slot.init(@intCast(slot)),
            .flags = Flags.init(0),
        };

        const unconditional = for (1..Flags.count) |raw_flags| {
            const addr1: Address = .{
                .slot = Slot.init(@intCast(slot)),
                .flags = Flags.init(@intCast(raw_flags)),
            };
            const cs0 = microcode[addr0.raw()];
            const cs = microcode[addr1.raw()];
            if (cs0 == null and cs == null) continue;

            // The microcode won't work properly if a slot has cycle data for some combinations of flags but not others.
            // the Processor should always ensure that isn't the case, so we'll rely on it here to simplify the logic:
            std.debug.assert(cs0 != null and cs != null);

            if (!(cs0.?).eql(cs.?)) break false;
        } else true;

        if (unconditional) {
            if (microcode[addr0.raw()]) |cs| {
                if (fn_names) |names| {
                    try w.print("{f},{f},{s},****", .{ addr0, addr0.slot, names[slot] });
                } else {
                    try w.print("{f},{f},****", .{ addr0, addr0.slot });
                }
                try write_csv_signals(w, cs);
                try w.writeByte('\n');
            }
        } else {
            conditional_temp.clearRetainingCapacity();

            for (0..Flags.count) |raw_flags| {
                const address: Address = .{
                    .slot = Slot.init(@intCast(slot)),
                    .flags = Flags.init(@intCast(raw_flags)),
                };
                try conditional_temp.append(temp_allocator, .{
                    .flags = @intCast(raw_flags),
                    .wildcards = 0,
                    .cs = microcode[address.raw()].?,
                });
            }

            var check_for_merges = true;
            while (check_for_merges) {
                check_for_merges = false;
                var i = conditional_temp.items.len - 1;
                while (i > 0) : (i -= 1) {
                    const q = conditional_temp.items[i];
                    for (conditional_temp.items[0..i]) |*info| {
                        if (q.wildcards == info.wildcards and @popCount(info.flags ^ q.flags) == 1 and q.cs.eql(info.cs)) {
                            info.wildcards |= info.flags ^ q.flags;
                            info.flags = info.flags & ~info.wildcards;
                            _ = conditional_temp.swapRemove(i);
                            check_for_merges = true;
                            break;
                        }
                    }
                }
            }

            for (conditional_temp.items) |info| {
                var flags_ascii: [@bitSizeOf(Flags)]u8 = undefined;
                var wildcards_ascii: [@bitSizeOf(Flags)]u8 = undefined;
                _ = try std.fmt.bufPrint(&flags_ascii, "{f}", .{ Flags.init(info.flags) });
                _ = try std.fmt.bufPrint(&wildcards_ascii, "{f}", .{ Flags.init(info.wildcards) });
                for (&flags_ascii, wildcards_ascii) |*f, wild| {
                    if (wild != '.') f.* = '*';
                }

                const address: Address = .{
                    .slot = Slot.init(@intCast(slot)),
                    .flags = Flags.init(info.flags),
                };
                if (fn_names) |names| {
                    try w.print("{f},{f},{s},{s}", .{ address, address.slot, names[slot], &flags_ascii });
                } else {
                    try w.print("{f},{f},{s}", .{ address, address.slot, &flags_ascii });
                }
                if (microcode[address.raw()]) |cs| {
                    try write_csv_signals(w, cs);
                } else {
                    for (std.enums.values(Control_Signal)) |_| {
                        try w.writeByte(',');
                    }
                }
                try w.writeByte('\n');
            }
        }
    }
}

fn write_csv_signals(w: *std.io.Writer, cs: Control_Signals) !void {
    inline for (comptime std.enums.values(Control_Signal)) |signal| {
        const value = @field(cs, @tagName(signal));
        switch (signal) {
            .gprw => try w.writeAll(if (value == .write) ",GPRW" else ","),
            .irw => try w.writeAll(if (value == .write) ",IRW" else ","),
            .drw => try w.writeAll(if (value == .write) ",DRW" else ","),
            .tiw => try w.writeAll(if (value == .write) ",TIW" else ","),
            .allow_int => try w.writeAll(if (value == .allow) ",ALLOW_INT" else ","),
            .power => try w.writeAll(switch (value) {
                .run => ",",
                .sleep => ",SLEEP",
            }),
            else => try w.print(",{f}", .{ value }),
        }
    }
}

const Control_Signal = std.meta.FieldEnum(Control_Signals);

const Control_Signals = @import("Control_Signals.zig");
const compute = @import("compute.zig");
const bus = @import("bus.zig");
const reg = @import("reg.zig");
const misc = @import("misc.zig");
const addr = @import("addr.zig");
const fmt = @import("fmt.zig");
const rom_compress = @import("rom_compress");
const rom_decompress = @import("rom_decompress");
const srec = @import("srec");
const ihex = @import("ihex");
const meta = @import("meta");
const std = @import("std");
