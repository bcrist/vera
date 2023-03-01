const std = @import("std");
const rom_compress = @import("rom_compress");
const rom_decompress = @import("rom_decompress");
const srec = @import("srec");
const ControlSignals = @import("ControlSignals");
const uc = @import("microcode");
const misc = @import("misc");

pub const Roms = [6][]const u8;

const RomEntry = rom_compress.Entry(u16, u16);

const Rom0Data = packed struct {
    literal: ControlSignals.Literal, // 6
    jr_rsel: ControlSignals.RegFileIndexingSource, // 2
    kr_rsel: ControlSignals.RegFileIndexingSource, // 2
    jr_rx: bool, // 1
    kr_rx: bool, // 1
    jl_src: ControlSignals.JLSource, // 2
    dl_op: ControlSignals.DataLatchOp, // 2

    fn init(cs: ControlSignals) Rom0Data {
        return .{
            .literal = cs.literal,
            .jr_rsel = cs.jr_rsel,
            .kr_rsel = cs.kr_rsel,
            .jr_rx = cs.jr_rx,
            .kr_rx = cs.kr_rx,
            .jl_src = cs.jl_src,
            .dl_op = cs.dl_op,
        };
    }
    fn apply(self: Rom0Data, cs: *ControlSignals) void {
        cs.literal = self.literal;
        cs.jr_rsel = self.jr_rsel;
        cs.kr_rsel = self.kr_rsel;
        cs.jr_rx = self.jr_rx;
        cs.kr_rx = self.kr_rx;
        cs.jl_src = self.jl_src;
        cs.dl_op = self.dl_op;
    }
};

const Rom1Data = packed struct {
    jh_src: ControlSignals.JHSource, // 3
    k_src: ControlSignals.KSource, // 3
    sr1_ri: ControlSignals.SR1Index, // 3
    sr2_ri: ControlSignals.SR2Index, // 3
    ll_src: ControlSignals.LLSource, // 4

    fn init(cs: ControlSignals) Rom1Data {
        return .{
            .jh_src = cs.jh_src,
            .k_src = cs.k_src,
            .sr1_ri = cs.sr1_ri,
            .sr2_ri = cs.sr2_ri,
            .ll_src = cs.ll_src,
        };
    }
    fn apply(self: Rom1Data, cs: *ControlSignals) void {
        cs.jh_src = self.jh_src;
        cs.k_src = self.k_src;
        cs.sr1_ri = self.sr1_ri;
        cs.sr2_ri = self.sr2_ri;
        cs.ll_src = self.ll_src;
    }
};

const Rom2Data = packed struct {
    offset: ControlSignals.AddressOffset, // 2
    alu_mode: u4, // 4
    bus_mode: ControlSignals.BusMode, // 2
    bus_byte: ControlSignals.BusWidth, // 1
    bus_rw: ControlSignals.BusDirection, // 1
    at_op: ControlSignals.AddressTranslatorOp, // 2
    special: ControlSignals.SpecialOp, // 3
    _: u1 = 0,

    fn init(cs: ControlSignals) Rom2Data {
        return .{
            .offset = cs.offset,
            .alu_mode = cs.alu_mode.raw(),
            .bus_mode = cs.bus_mode,
            .bus_byte = cs.bus_byte,
            .bus_rw = cs.bus_rw,
            .at_op = cs.at_op,
            .special = cs.special,
        };
    }
    fn apply(self: Rom2Data, cs: *ControlSignals) void {
        cs.offset = self.offset;
        cs.alu_mode = .{ .unknown = self.alu_mode };
        cs.bus_mode = self.bus_mode;
        cs.bus_byte = self.bus_byte;
        cs.bus_rw = self.bus_rw;
        cs.at_op = self.at_op;
        cs.special = self.special;
    }
};

const Rom3Data = packed struct {
    ob_oa_op: ControlSignals.OperandRegOp, // 2
    lh_src: ControlSignals.LHSource, // 4
    jkr_wsel: ControlSignals.RegFileIndexingSource, // 2
    jkr_wmode: ControlSignals.RegFileWriteMode, // 2
    sr1_wi: ControlSignals.SR1Index, // 3
    sr2_wi: ControlSignals.SR2Index, // 3

    fn init(cs: ControlSignals) Rom3Data {
        return .{
            .ob_oa_op = cs.ob_oa_op,
            .lh_src = cs.lh_src,
            .jkr_wsel = cs.jkr_wsel,
            .jkr_wmode = cs.jkr_wmode,
            .sr1_wi = cs.sr1_wi,
            .sr2_wi = cs.sr2_wi,
        };
    }
    fn apply(self: Rom3Data, cs: *ControlSignals) void {
        cs.ob_oa_op = self.ob_oa_op;
        cs.lh_src = self.lh_src;
        cs.jkr_wsel = self.jkr_wsel;
        cs.jkr_wmode = self.jkr_wmode;
        cs.sr1_wi = self.sr1_wi;
        cs.sr2_wi = self.sr2_wi;
    }
};

const Rom4Data = packed struct {
    sr1_wsrc: ControlSignals.SR1WriteDataSource, // 2
    sr2_wsrc: ControlSignals.SR2WriteDataSource, // 2
    stat_op: ControlSignals.STAT_Op, // 4
    allow_int: bool, // 1
    seq_op: ControlSignals.Sequencer_Op, // 2
    _: u5 = 0, // 5

    fn init(cs: ControlSignals) Rom4Data {
        return .{
            .sr1_wsrc = cs.sr1_wsrc,
            .sr2_wsrc = cs.sr2_wsrc,
            .stat_op = cs.stat_op,
            .allow_int = cs.allow_int,
            .seq_op = cs.seq_op,
        };
    }
    fn apply(self: Rom4Data, cs: *ControlSignals) void {
        cs.sr1_wsrc = self.sr1_wsrc;
        cs.sr2_wsrc = self.sr2_wsrc;
        cs.stat_op = self.stat_op;
        cs.allow_int = self.allow_int;
        cs.seq_op = self.seq_op;
    }
};

const Rom5Data = packed struct {
    next_uop: uc.Continuation, // 10
    base: ControlSignals.AnySRIndex, // 4
    _: u2 = 0, // 2

    fn init(cs: ControlSignals) Rom5Data {
        return .{
            .next_uop = cs.next_uop,
            .base = cs.base,
        };
    }
    fn apply(self: Rom5Data, cs: *ControlSignals) void {
        cs.next_uop = self.next_uop;
        cs.base = self.base;
    }
};

fn convertMicrocodeToRomEntries(comptime RomData: type, microcode: *const [misc.microcode_length]?*ControlSignals, entries: *std.ArrayList(RomEntry)) !void {
    entries.clearRetainingCapacity();
    for (microcode, 0..) |optional_cs, ua| {
        if (optional_cs) |cs| {
            const addr = @intCast(u16, ua);
            const data = @bitCast(u16, RomData.init(cs.*));
            try entries.append(RomEntry.init(addr, data));
        }
    }
}

pub fn writeCompressedRoms(result_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator, microcode: *const [misc.microcode_length]?*ControlSignals) !Roms {
    var result: Roms = undefined;
    var entries = try std.ArrayList(RomEntry).initCapacity(temp_allocator, misc.microcode_length);
    defer entries.deinit();
    inline for ([_]type{ Rom0Data, Rom1Data, Rom2Data, Rom3Data, Rom4Data, Rom5Data }, 0..) |RomData, n| {
        try convertMicrocodeToRomEntries(RomData, microcode, &entries);
        result[n] = try rom_compress.compress(RomEntry, result_allocator, temp_allocator, entries.items);
    }
    return result;
}

fn readCompressedRom(comptime RomData: type, compressed_data: []const u8, cs: []ControlSignals) void {
    const Ctx = struct {
        cs: []ControlSignals,
        d: RomData = undefined,

        const Self = @This();

        pub fn data(self: *Self, d: u32) void {
            self.d = @bitCast(RomData, @intCast(u16, d));
        }

        pub fn address(self: *Self, a: u32) void {
            self.d.apply(&self.cs[a]);
        }
    };
    var ctx = Ctx{ .cs = cs };
    rom_decompress.decompress(compressed_data, &ctx);
}

pub fn readCompressedRoms(roms: Roms, microcode: []ControlSignals) void {
    std.debug.assert(microcode.len >= misc.microcode_length);
    inline for ([_]type{ Rom0Data, Rom1Data, Rom2Data, Rom3Data, Rom4Data, Rom5Data }, 0..) |RomData, n| {
        readCompressedRom(RomData, roms[n], microcode);
    }
}

fn convertMicrocodeToSRec(comptime RomData: type, microcode: *const [misc.microcode_length]?*ControlSignals, result_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator) ![]u8 {
    var rom_name: []const u8 = @typeName(RomData);
    if (std.mem.lastIndexOf(u8, rom_name, ".")) |index| {
        rom_name = rom_name[index + 1 ..];
    }

    var encoder = try srec.Encoder(u24).init(temp_allocator, rom_name);
    defer encoder.deinit();

    encoder.pretty = true;

    var start: u24 = undefined;
    var temp = std.ArrayList(u8).init(temp_allocator);
    defer temp.deinit();

    for (microcode, 0..) |optional_cs, ua| {
        if (optional_cs) |cs| {
            const data = @bitCast(u16, RomData.init(cs.*));

            if (temp.items.len == 0) {
                start = @intCast(u24, ua) * 2;
            }

            try temp.append(@truncate(u8, data));
            try temp.append(@intCast(u8, data >> 8));
        } else if (temp.items.len > 0) {
            try encoder.encode(start, temp.items);
            temp.clearRetainingCapacity();
        }
    }

    if (temp.items.len > 0) {
        try encoder.encode(start, temp.items);
    }

    try encoder.finish(0);

    return result_allocator.dupe(u8, encoder.data.items);
}

pub fn writeSRecRoms(result_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator, microcode: *const [misc.microcode_length]?*ControlSignals) !Roms {
    var result: Roms = undefined;
    inline for ([_]type{ Rom0Data, Rom1Data, Rom2Data, Rom3Data, Rom4Data, Rom5Data }, 0..) |RomData, n| {
        result[n] = try convertMicrocodeToSRec(RomData, microcode, result_allocator, temp_allocator);
    }
    return result;
}
