// pub const all_compressed_data = [_][]const u8{
//     @embedFile("../../../roms/microcode_0"),
//     @embedFile("../../../roms/microcode_1"),
//     @embedFile("../../../roms/microcode_2"),
//     @embedFile("../../../roms/microcode_3"),
//     @embedFile("../../../roms/microcode_4"),
//     @embedFile("../../../roms/microcode_5"),
// };

pub const num_roms = 12;

pub const Compressed_Rom_Data = [num_roms][]const u8;

const Rom_Entry = rom_compress.Entry(hw.microcode.Address.Raw, u8);

const Rom0 = packed struct (u8) {
    c_ij: hw.IJ,
    c_ik: hw.IK,
};
const Rom1 = packed struct (u8) {
    ij_op: Control_Signals.Operand_Index_Op,
    ik_op: Control_Signals.Operand_Index_Op,
    seq_op: Control_Signals.Sequencer_Op,
};
const Rom2 = packed struct (u8) {
    iw_op: Control_Signals.Operand_Index_Op,
    c_iw: hw.IW,
    id_mode: Control_Signals.ID_Mode,
};
const Rom3 = packed struct (u8) {
    sr1_ri: Control_Signals.SR1_Index,
    sr2_ri: Control_Signals.SR2_Index,
    unit: Control_Signals.Compute_Unit,
};
const Rom4 = packed struct (u8) {
    allow_int: bool,
    bus_dir: Control_Signals.Bus_Direction,
    jl_src: Control_Signals.JL_Source,
    _reserved: u3 = 0,
};
const Rom5 = packed struct (u8) {
    literal: Control_Signals.Literal,
};
const Rom6 = packed struct (u8) {
    jh_src: Control_Signals.JH_Source,
    k_src: Control_Signals.K_Source,
    offset_src: Control_Signals.Address_Offset_Source,
};
const Rom7 = packed struct (u8) {
    base_ri: Control_Signals.Any_SR_Index,
    special: Control_Signals.Special_Op,
    bus_width: Control_Signals.Bus_Width,
};
const Rom8 = packed struct (u8) {
    mode: Control_Signals.Compute_Mode,
    at_op: Control_Signals.Address_Translator_Op,
};
const Rom9 = packed struct (u8) {
    addr_space: Control_Signals.Address_Space,
    ll_src: Control_Signals.LL_Source,
    lh_src: Control_Signals.LH_Source,
};
const Rom10 = packed struct (u8) {
    reg_write: Control_Signals.Register_Write_Mode,
    sr1_wi: Control_Signals.SR1_Index,
    sr2_wi: Control_Signals.SR2_Index,
};
const Rom11 = packed struct (u8) {
    sr1_wsrc: Control_Signals.SR1_Write_Source,
    sr2_wsrc: Control_Signals.SR2_Write_Source,
    stat_op: Control_Signals.Status_Op,
    _reserved: u1 = 0,
};

fn apply_to_cs(comptime Rom_Data: type, data: Rom_Data, cs: *Control_Signals) void {
    inline for (@typeInfo(Rom_Data).Struct.fields) |field_info| {
        if (field_info.name[0] == '_') continue;
        @field(cs.*, field_info.name) = @field(data, field_info.name);
    }
}
fn from_cs(comptime Rom_Data: type, cs: Control_Signals) Rom_Data {
    var data: Rom_Data = undefined;
    inline for (@typeInfo(Rom_Data).Struct.fields) |field_info| {
        if (field_info.name[0] == '_') continue;
        @field(data, field_info.name) = @field(cs, field_info.name);
    }
    return data;
}

fn convert_microcode_to_rom_entries(comptime Rom_Data: type, microcode: []const ?Control_Signals, entries: *std.ArrayList(Rom_Entry)) !void {
    entries.clearRetainingCapacity();
    for (microcode, 0..) |optional_cs, ua| {
        if (optional_cs) |cs| {
            const addr: hw.microcode.Address.Raw = @intCast(ua);
            const data: u8 = @bitCast(from_cs(Rom_Data, cs));
            try entries.append(Rom_Entry.init(addr, data));
        }
    }
}

pub fn write_compressed_roms(result_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator, microcode: []const ?Control_Signals) !Compressed_Rom_Data {
    var result: Compressed_Rom_Data = undefined;
    var entries = try std.ArrayList(Rom_Entry).initCapacity(temp_allocator, microcode.len);
    defer entries.deinit();
    inline for (0..num_roms) |n| {
        const Rom_Data = @field(@This(), std.fmt.comptimePrint("Rom{}", .{ n }));
        try convert_microcode_to_rom_entries(Rom_Data, microcode, &entries);
        result[n] = try rom_compress.compress(Rom_Entry, result_allocator, temp_allocator, entries.items);
    }
    return result;
}

fn read_compressed_rom(comptime Rom_Data: type, compressed_data: []const u8, microcode: []Control_Signals) void {
    const Decompress_Context = struct {
        microcode: []Control_Signals,
        d: Rom_Data = undefined,

        const Self = @This();

        pub fn data(self: *Self, d: u32) void {
            const raw: u8 = @intCast(d);
            self.d = @bitCast(raw);
        }

        pub fn address(self: *Self, a: u32) void {
            apply_to_cs(Rom_Data, self.d, &self.microcode[a]);
        }
    };
    var ctx: Decompress_Context = .{ .microcode = microcode };
    rom_decompress.decompress(compressed_data, &ctx);
}

pub fn read_compressed_roms(roms: Compressed_Rom_Data, microcode: []Control_Signals) void {
    @memset(microcode, std.mem.zeroInit(Control_Signals, .{ .mode = Control_Signals.Compute_Mode.init(0) }));
    inline for (0..num_roms) |n| {
        const Rom_Data = @field(@This(), std.fmt.comptimePrint("Rom{}", .{ n }));
        read_compressed_rom(Rom_Data, roms[n], microcode);
    }
}

fn convert_microcode_to_srec(result_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator, comptime Rom_Data: type, microcode: []const ?Control_Signals) ![]u8 {
    var rom_name: []const u8 = @typeName(Rom_Data);
    if (std.mem.lastIndexOf(u8, rom_name, ".")) |index| {
        rom_name = rom_name[index + 1 ..];
    }

    var temp = std.ArrayList(u8).init(temp_allocator);
    defer temp.deinit();

    var encoded_data = std.ArrayList(u8).init(temp_allocator);
    defer encoded_data.deinit();

    var writer = try srec.writer(u24, encoded_data.writer(), .{
        .header_data = rom_name,
        .pretty = true,
    });

    var start: u24 = undefined;
    for (microcode, 0..) |optional_cs, addr| {
        if (optional_cs) |cs| {
            const data: u8 = @bitCast(from_cs(Rom_Data, cs));

            if (temp.items.len == 0) {
                start = @intCast(addr);
            }

            try temp.append(data);
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

pub fn write_srec_roms(result_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator, microcode: []const ?Control_Signals) !Compressed_Rom_Data {
    var result: Compressed_Rom_Data = undefined;
    inline for (0..num_roms) |n| {
        const Rom_Data = @field(@This(), std.fmt.comptimePrint("Rom{}", .{ n }));
        result[n] = try convert_microcode_to_srec(result_allocator, temp_allocator, Rom_Data, microcode);
    }
    return result;
}

const Control_Signals = hw.Control_Signals;
const hw = @import("../hardware.zig");
const arch = @import("lib_arch");
const rom_compress = @import("rom_compress");
const rom_decompress = @import("rom_decompress");
const srec = @import("srec");
const std = @import("std");
