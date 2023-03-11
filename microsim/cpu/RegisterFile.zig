const std = @import("std");
const bits = @import("bits");
const ControlSignals = @import("ControlSignals");
const misc = @import("misc");
const bus = @import("bus_types");

const RegistersetNumber = misc.RegistersetNumber;
const RegisterIndex = misc.RegisterIndex;

const RegisterFile = @This();

gpr_even: [512]u16,
gpr_odd: [512]u16,
sr1: [512]u32,
sr2: [512]u32,

pub fn reset(self: *RegisterFile) void {
    self.gpr_even = .{0} ** 512;
    self.gpr_odd = .{0} ** 512;
    self.sr1 = .{0} ** 512;
    self.sr2 = .{0} ** 512;
}

pub fn randomize(self: *RegisterFile, rnd: std.rand.Random) void {
    rnd.bytes(std.mem.sliceAsBytes(self.gpr_even[0..]));
    rnd.bytes(std.mem.sliceAsBytes(self.gpr_odd[0..]));
    rnd.bytes(std.mem.sliceAsBytes(self.sr1[0..]));
    rnd.bytes(std.mem.sliceAsBytes(self.sr2[0..]));
}

fn gprAddress(rsn: RegistersetNumber, index: RegisterIndex) u9 {
    return bits.concat2(@truncate(u3, index >> 1), rsn);
}
fn sr1Address(rsn: RegistersetNumber, index: ControlSignals.SR1Index) u9 {
    std.debug.assert(@bitSizeOf(ControlSignals.SR1Index) == 3);
    return bits.concat2(@enumToInt(index), rsn);
}
fn sr2Address(rsn: RegistersetNumber, index: ControlSignals.SR2Index) u9 {
    std.debug.assert(@bitSizeOf(ControlSignals.SR2Index) == 3);
    return bits.concat2(@enumToInt(index), rsn);
}

pub fn readEven(self: *const RegisterFile, rsn: RegistersetNumber, index: RegisterIndex) u16 {
    return self.gpr_even[gprAddress(rsn, index)];
}
pub fn writeEven(self: *RegisterFile, rsn: RegistersetNumber, index: RegisterIndex, value: u16) void {
    self.gpr_even[gprAddress(rsn, index)] = value;
}

pub fn readOdd(self: *const RegisterFile, rsn: RegistersetNumber, index: RegisterIndex) u16 {
    return self.gpr_odd[gprAddress(rsn, index)];
}
pub fn writeOdd(self: *RegisterFile, rsn: RegistersetNumber, index: RegisterIndex, value: u16) void {
    self.gpr_odd[gprAddress(rsn, index)] = value;
}

pub fn readSR1(self: *const RegisterFile, rsn: RegistersetNumber, index: ControlSignals.SR1Index) u32 {
    return self.sr1[sr1Address(rsn, index)];
}
pub fn writeSR1(self: *RegisterFile, rsn: RegistersetNumber, index: ControlSignals.SR1Index, value: u32) void {
    self.sr1[sr1Address(rsn, index)] = value;
}

pub fn readSR2(self: *const RegisterFile, rsn: RegistersetNumber, index: ControlSignals.SR2Index) u32 {
    return self.sr2[sr2Address(rsn, index)];
}
pub fn writeSR2(self: *RegisterFile, rsn: RegistersetNumber, index: ControlSignals.SR2Index, value: u32) void {
    self.sr2[sr2Address(rsn, index)] = value;
}

pub fn readGPR(self: *const RegisterFile, rsn: RegistersetNumber, index: RegisterIndex) u16 {
    if ((index & 1) == 1) {
        return self.readOdd(rsn, index);
    } else {
        return self.readEven(rsn, index);
    }
}
pub fn writeGPR(self: *RegisterFile, rsn: RegistersetNumber, index: RegisterIndex, value: u16) void {
    if ((index & 1) == 1) {
        return self.writeOdd(rsn, index, value);
    } else {
        return self.writeEven(rsn, index, value);
    }
}
pub fn readSignedGPR(self: *const RegisterFile, rsn: RegistersetNumber, index: RegisterIndex) i16 {
    if ((index & 1) == 1) {
        return @bitCast(i16, self.readOdd(rsn, index));
    } else {
        return @bitCast(i16, self.readEven(rsn, index));
    }
}
pub fn writeSignedGPR(self: *RegisterFile, rsn: RegistersetNumber, index: RegisterIndex, value: i16) void {
    if ((index & 1) == 1) {
        return self.writeOdd(rsn, index, @bitCast(u16, value));
    } else {
        return self.writeEven(rsn, index, @bitCast(u16, value));
    }
}

pub fn readGPR32(self: *const RegisterFile, rsn: RegistersetNumber, index: RegisterIndex) u32 {
    return bits.concat(.{
        self.readGPR(rsn, index),
        self.readGPR(rsn, index ^ 1),
    });
}
pub fn writeGPR32(self: *RegisterFile, rsn: RegistersetNumber, index: RegisterIndex, value: u32) void {
    self.writeGPR(rsn, index, @truncate(u16, value));
    self.writeGPR(rsn, index ^ 1, @intCast(u16, value >> 16));
}
pub fn readSignedGPR32(self: *const RegisterFile, rsn: RegistersetNumber, index: RegisterIndex) i32 {
    return @bitCast(i32, bits.concat(.{
        self.readGPR(rsn, index),
        self.readGPR(rsn, index ^ 1),
    }));
}
pub fn writeSignedGPR32(self: *RegisterFile, rsn: RegistersetNumber, index: RegisterIndex, value: i32) void {
    const unsigned = @bitCast(u32, value);
    self.writeGPR(rsn, index, @truncate(u16, unsigned));
    self.writeGPR(rsn, index ^ 1, @intCast(u16, unsigned >> 16));
}

pub fn readSR(self: *const RegisterFile, rsn: RegistersetNumber, index: ControlSignals.AnySRIndex) u32 {
    if (ControlSignals.addressBaseToSR1(index)) |sr1| {
        return self.readSR1(rsn, sr1);
    } else if (ControlSignals.addressBaseToSR2(index)) |sr2| {
        return self.readSR2(rsn, sr2);
    } else {
        unreachable;
    }
}
pub fn writeSR(self: *const RegisterFile, rsn: RegistersetNumber, index: ControlSignals.AnySRIndex, value: u32) void {
    if (ControlSignals.addressBaseToSR1(index)) |sr1| {
        self.writeSR1(rsn, sr1, value);
    } else if (ControlSignals.addressBaseToSR2(index)) |sr2| {
        self.writeSR2(rsn, sr2, value);
    } else {
        unreachable;
    }
}

pub const SetupInputs = struct {
    rsn: RegistersetNumber,
    oa: misc.OperandA,
    ob: misc.OperandB,

    cs_jl_src: ControlSignals.JLSource,
    cs_jh_src: ControlSignals.JHSource,
    cs_k_src: ControlSignals.KSource,
    cs_jr_rsel: ControlSignals.RegFileIndexingSource,
    cs_kr_rsel: ControlSignals.RegFileIndexingSource,
    cs_jr_rx: bool,
    cs_kr_rx: bool,
    cs_sr1_ri: ControlSignals.SR1Index,
    cs_sr2_ri: ControlSignals.SR2Index,
    cs_base: ControlSignals.AnySRIndex,
    cs_literal: ControlSignals.Literal,
};

pub const SetupOutputs = struct {
    j: bus.JParts,
    k: bus.K,
    sr1: bus.JParts,
    sr2: bus.JParts,
    address_base: bus.VirtualAddress,
};

pub fn setup(self: *const RegisterFile, in: SetupInputs) SetupOutputs {
    const jr_index: RegisterIndex = switch (in.cs_jr_rsel) {
        .zero => 0,
        .literal => @truncate(RegisterIndex, in.cs_literal),
        .oa => in.oa,
        .ob => in.ob,
    };
    const kr_index: RegisterIndex = switch (in.cs_kr_rsel) {
        .zero => 0,
        .literal => @truncate(RegisterIndex, in.cs_literal),
        .oa => in.oa,
        .ob => in.ob,
    };

    var jr_swap = @truncate(u1, jr_index) == 1;
    if (in.cs_jr_rx) {
        jr_swap = !jr_swap;
    }

    var kr_swap = @truncate(u1, kr_index) == 1;
    if (in.cs_kr_rx) {
        kr_swap = !kr_swap;
    }

    const jr = if (jr_swap) blk: {
        break :blk bus.JParts{
            .low = self.readOdd(in.rsn, jr_index),
            .high = self.readEven(in.rsn, jr_index),
        };
    } else blk: {
        break :blk bus.JParts{
            .low = self.readEven(in.rsn, jr_index),
            .high = self.readOdd(in.rsn, jr_index),
        };
    };

    const kr = if (kr_swap) blk: {
        break :blk self.readOdd(in.rsn, kr_index);
    } else blk: {
        break :blk self.readEven(in.rsn, kr_index);
    };

    const sr1 = @bitCast(bus.JParts, self.readSR1(in.rsn, in.cs_sr1_ri));
    const sr2 = @bitCast(bus.JParts, self.readSR2(in.rsn, in.cs_sr2_ri));

    return .{
        .j = .{
            .low = switch (in.cs_jl_src) {
                .zero => 0,
                .jrl => jr.low,
                .sr1l => sr1.low,
                .sr2l => sr2.low,
            },
            .high = switch (in.cs_jh_src) {
                .zero => 0,
                .neg_one => 0xFFFF,
                .sx_jl => bits.sx(u16, switch (in.cs_jl_src) {
                    .zero => @as(u1, 0),
                    .jrl => @intCast(u1, jr.low >> 15),
                    .sr1l => @intCast(u1, sr1.low >> 15),
                    .sr2l => @intCast(u1, sr2.low >> 15),
                }),
                .jrh => jr.high,
                .sr1h => sr1.high,
                .sr2h => sr2.high,
            },
        },
        .k = switch (in.cs_k_src) {
            .zero             => 0,
            .kr               => kr,
            .sr1l             => sr1.low,
            .sr2l             => sr2.low,
            .ob_oa_zx         => bits.zx(u16, bits.concat2(in.oa, in.ob)),
            .literal          => bits.zx(u16, in.cs_literal),
            .literal_minus_64 => bits._1x(u16, in.cs_literal),
            .literal_special  => misc.decodeSpecialKLiteral(in.cs_literal),
        },
        .sr1 = @bitCast(bus.JParts, sr1),
        .sr2 = @bitCast(bus.JParts, sr2),
        .address_base = self.readSR(in.rsn, in.cs_base),
    };
}

pub const TransactInputs = struct {
    rsn: RegistersetNumber,
    setup_rsn: RegistersetNumber,
    oa: misc.OperandA,
    ob: misc.OperandB,
    inhibit_writes: bool,
    l: bus.LParts,
    sr1: bus.JParts,
    sr2: bus.JParts,
    virtual_address: bus.VirtualAddressParts,

    cs_jkr_wsel: ControlSignals.RegFileIndexingSource,
    cs_jkr_wmode: ControlSignals.RegFileWriteMode,
    cs_sr1_wsrc: ControlSignals.SR1WriteDataSource,
    cs_sr2_wsrc: ControlSignals.SR2WriteDataSource,
    cs_sr1_wi: ControlSignals.SR1Index,
    cs_sr2_wi: ControlSignals.SR2Index,
    cs_literal: ControlSignals.Literal,
};

pub fn transact(self: *RegisterFile, in: TransactInputs) void {
    if (in.inhibit_writes) {
        return;
    }

    const jkr_index: RegisterIndex = switch (in.cs_jkr_wsel) {
        .zero => 0,
        .literal => @truncate(RegisterIndex, in.cs_literal),
        .oa => in.oa,
        .ob => in.ob,
    };

    const odd_register = (jkr_index & 1) == 1;
    switch (in.cs_jkr_wmode) {
        .no_write => {},
        .write_16 => {
            if (odd_register) {
                self.writeOdd(in.rsn, jkr_index, in.l.low);
            } else {
                self.writeEven(in.rsn, jkr_index, in.l.low);
            }
        },
        .write_16_xor1 => {
            if (odd_register) {
                self.writeEven(in.rsn, jkr_index, in.l.low);
            } else {
                self.writeOdd(in.rsn, jkr_index, in.l.low);
            }
        },
        .write_32 => {
            if (odd_register) {
                self.writeOdd(in.rsn, jkr_index, in.l.low);
                self.writeEven(in.rsn, jkr_index, in.l.high);
            } else {
                self.writeEven(in.rsn, jkr_index, in.l.low);
                self.writeOdd(in.rsn, jkr_index, in.l.high);
            }
        },
    }

    switch (in.cs_sr1_wsrc) {
        .no_write => {},
        .rsn_sr1 => {
            const val = bits.concat2(@truncate(u16, @bitCast(u32, in.sr1)), @as(u16, in.setup_rsn));
            self.writeSR1(in.rsn, in.cs_sr1_wi, val);
        },
        .l_bus => self.writeSR1(in.rsn, in.cs_sr1_wi, @bitCast(u32, in.l)),
        .virtual_address => self.writeSR1(in.rsn, in.cs_sr1_wi, @bitCast(u32, in.virtual_address)),
    }

    switch (in.cs_sr2_wsrc) {
        .no_write => {},
        .sr2 => self.writeSR2(in.rsn, in.cs_sr2_wi, @bitCast(u32, in.sr2)),
        .l_bus => self.writeSR2(in.rsn, in.cs_sr2_wi, @bitCast(u32, in.l)),
        .virtual_address => self.writeSR2(in.rsn, in.cs_sr2_wi, @bitCast(u32, in.virtual_address)),
    }
}
