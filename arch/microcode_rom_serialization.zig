const std = @import("std");
const rom_compress = @import("rom_compress");
const rom_decompress = @import("rom_decompress");
const srec = @import("srec");
const ctrl = @import("control_signals");
const uc_layout = @import("microcode_layout");
const misc = @import("misc");

const Control_Signals = ctrl.Control_Signals;
pub const Roms = [6][]const u8;

const RomEntry = rom_compress.Entry(u16, u16);

const Rom0Data = packed struct {
    LITERAL: ctrl.Literal, // 6
    JR_RSEL: ctrl.Reg_File_Indexing_Source, // 2
    KR_RSEL: ctrl.Reg_File_Indexing_Source, // 2
    JR_RX: bool, // 1
    KR_RX: bool, // 1
    JL_SRC: ctrl.JL_Source, // 2
    DL_OP: ctrl.Data_Latch_Op, // 2

    fn init(cs: Control_Signals) Rom0Data {
        return .{
            .LITERAL = cs.LITERAL,
            .JR_RSEL = cs.JR_RSEL,
            .KR_RSEL = cs.KR_RSEL,
            .JR_RX = cs.JR_RX,
            .KR_RX = cs.KR_RX,
            .JL_SRC = cs.JL_SRC,
            .DL_OP = cs.DL_OP,
        };
    }
    fn apply(self: Rom0Data, cs: *Control_Signals) void {
        cs.LITERAL = self.LITERAL;
        cs.JR_RSEL = self.JR_RSEL;
        cs.KR_RSEL = self.KR_RSEL;
        cs.JR_RX = self.JR_RX;
        cs.KR_RX = self.KR_RX;
        cs.JL_SRC = self.JL_SRC;
        cs.DL_OP = self.DL_OP;
    }
};

const Rom1Data = packed struct {
    JH_SRC: ctrl.JH_Source, // 3
    K_SRC: ctrl.K_Source, // 3
    SR1_RI: ctrl.SR1_Index, // 3
    SR2_RI: ctrl.SR2_Index, // 3
    LL_SRC: ctrl.LL_Source, // 4

    fn init(cs: Control_Signals) Rom1Data {
        return .{
            .JH_SRC = cs.JH_SRC,
            .K_SRC = cs.K_SRC,
            .SR1_RI = cs.SR1_RI,
            .SR2_RI = cs.SR2_RI,
            .LL_SRC = cs.LL_SRC,
        };
    }
    fn apply(self: Rom1Data, cs: *Control_Signals) void {
        cs.JH_SRC = self.JH_SRC;
        cs.K_SRC = self.K_SRC;
        cs.SR1_RI = self.SR1_RI;
        cs.SR2_RI = self.SR2_RI;
        cs.LL_SRC = self.LL_SRC;
    }
};

const Rom2Data = packed struct {
    OFFSET: ctrl.Address_Offset, // 2
    ALU_MODE: u4, // 4
    BUS_MODE: ctrl.Bus_Mode, // 2
    BUS_BYTE: ctrl.Bus_Width, // 1
    BUS_RW: ctrl.Bus_Direction, // 1
    AT_OP: ctrl.AT_Op, // 2
    SPECIAL: ctrl.Special_Op, // 3
    _: u1 = 0,

    fn init(cs: Control_Signals) Rom2Data {
        return .{
            .OFFSET = cs.OFFSET,
            .ALU_MODE = cs.ALU_MODE.raw(),
            .BUS_MODE = cs.BUS_MODE,
            .BUS_BYTE = cs.BUS_BYTE,
            .BUS_RW = cs.BUS_RW,
            .AT_OP = cs.AT_OP,
            .SPECIAL = cs.SPECIAL,
        };
    }
    fn apply(self: Rom2Data, cs: *Control_Signals) void {
        cs.OFFSET = self.OFFSET;
        cs.ALU_MODE = .{ .unknown = self.ALU_MODE };
        cs.BUS_MODE = self.BUS_MODE;
        cs.BUS_BYTE = self.BUS_BYTE;
        cs.BUS_RW = self.BUS_RW;
        cs.AT_OP = self.AT_OP;
        cs.SPECIAL = self.SPECIAL;
    }
};

const Rom3Data = packed struct {
    OB_OA_OP: ctrl.Operand_Reg_Op, // 2
    LH_SRC: ctrl.LH_Source, // 4
    JKR_WSEL: ctrl.Reg_File_Indexing_Source, // 2
    JKR_WMODE: ctrl.Reg_File_Write_Mode, // 2
    SR1_WI: ctrl.SR1_Index, // 3
    SR2_WI: ctrl.SR2_Index, // 3

    fn init(cs: Control_Signals) Rom3Data {
        return .{
            .OB_OA_OP = cs.OB_OA_OP,
            .LH_SRC = cs.LH_SRC,
            .JKR_WSEL = cs.JKR_WSEL,
            .JKR_WMODE = cs.JKR_WMODE,
            .SR1_WI = cs.SR1_WI,
            .SR2_WI = cs.SR2_WI,
        };
    }
    fn apply(self: Rom3Data, cs: *Control_Signals) void {
        cs.OB_OA_OP = self.OB_OA_OP;
        cs.LH_SRC = self.LH_SRC;
        cs.JKR_WSEL = self.JKR_WSEL;
        cs.JKR_WMODE = self.JKR_WMODE;
        cs.SR1_WI = self.SR1_WI;
        cs.SR2_WI = self.SR2_WI;
    }
};

const Rom4Data = packed struct {
    SR1_WSRC: ctrl.SR1_Write_Data_Source, // 2
    SR2_WSRC: ctrl.SR2_Write_Data_Source, // 2
    STAT_OP: ctrl.STAT_Op, // 4
    ALLOW_INT: bool, // 1
    SEQ_OP: ctrl.Sequencer_Op, // 2
    _: u5 = 0, // 5

    fn init(cs: Control_Signals) Rom4Data {
        return .{
            .SR1_WSRC = cs.SR1_WSRC,
            .SR2_WSRC = cs.SR2_WSRC,
            .STAT_OP = cs.STAT_OP,
            .ALLOW_INT = cs.ALLOW_INT,
            .SEQ_OP = cs.SEQ_OP,
        };
    }
    fn apply(self: Rom4Data, cs: *Control_Signals) void {
        cs.SR1_WSRC = self.SR1_WSRC;
        cs.SR2_WSRC = self.SR2_WSRC;
        cs.STAT_OP = self.STAT_OP;
        cs.ALLOW_INT = self.ALLOW_INT;
        cs.SEQ_OP = self.SEQ_OP;
    }
};

const Rom5Data = packed struct {
    NEXT_UOP: uc_layout.UC_Continuation, // 10
    BASE: ctrl.Any_SR_Index, // 4
    _: u2 = 0, // 2

    fn init(cs: Control_Signals) Rom5Data {
        return .{
            .NEXT_UOP = cs.NEXT_UOP,
            .BASE = cs.BASE,
        };
    }
    fn apply(self: Rom5Data, cs: *Control_Signals) void {
        cs.NEXT_UOP = self.NEXT_UOP;
        cs.BASE = self.BASE;
    }
};

fn convertMicrocodeToRomEntries(comptime RomData: type, microcode: *const [misc.microcode_length]?*Control_Signals, entries: *std.ArrayList(RomEntry)) !void {
    entries.clearRetainingCapacity();
    for (microcode, 0..) |optional_cs, ua| {
        if (optional_cs) |cs| {
            const addr = @intCast(u16, ua);
            const data = @bitCast(u16, RomData.init(cs.*));
            try entries.append(RomEntry.init(addr, data));
        }
    }
}

pub fn writeCompressedRoms(result_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator, microcode: *const [misc.microcode_length]?*Control_Signals) !Roms {
    var result: Roms = undefined;
    var entries = try std.ArrayList(RomEntry).initCapacity(temp_allocator, misc.microcode_length);
    defer entries.deinit();
    inline for ([_]type{ Rom0Data, Rom1Data, Rom2Data, Rom3Data, Rom4Data, Rom5Data }, 0..) |RomData, n| {
        try convertMicrocodeToRomEntries(RomData, microcode, &entries);
        result[n] = try rom_compress.compress(RomEntry, result_allocator, temp_allocator, entries.items);
    }
    return result;
}

fn readCompressedRom(comptime RomData: type, compressed_data: []const u8, cs: []Control_Signals) void {
    const Ctx = struct {
        cs: []Control_Signals,
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

pub fn readCompressedRoms(roms: Roms, microcode: []Control_Signals) void {
    std.debug.assert(microcode.len >= misc.microcode_length);
    inline for ([_]type{ Rom0Data, Rom1Data, Rom2Data, Rom3Data, Rom4Data, Rom5Data }, 0..) |RomData, n| {
        readCompressedRom(RomData, roms[n], microcode);
    }
}

fn convertMicrocodeToSRec(comptime RomData: type, microcode: *const [misc.microcode_length]?*Control_Signals, result_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator) ![]u8 {
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

pub fn writeSRecRoms(result_allocator: std.mem.Allocator, temp_allocator: std.mem.Allocator, microcode: *const [misc.microcode_length]?*Control_Signals) !Roms {
    var result: Roms = undefined;
    inline for ([_]type{ Rom0Data, Rom1Data, Rom2Data, Rom3Data, Rom4Data, Rom5Data }, 0..) |RomData, n| {
        result[n] = try convertMicrocodeToSRec(RomData, microcode, result_allocator, temp_allocator);
    }
    return result;
}
