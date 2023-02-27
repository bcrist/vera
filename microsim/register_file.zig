const std = @import("std");
const sim = @import("simulator");
const bits = @import("bits");
const ctrl = @import("control_signals");
const misc = @import("misc");

const RSN = misc.RSN;
const Register_Index = misc.Register_Index;
const Split_J_Bus = sim.Split_J_Bus;
const Split_L_Bus = sim.Split_L_Bus;
const OB_OA = sim.OB_OA;
const Context = ctrl.Context;
const VirtualAddress = sim.VirtualAddress;

pub const State = struct {
    gpr_even: [512]u16,
    gpr_odd: [512]u16,
    sr1: [512]u32,
    sr2: [512]u32,

    pub fn reset(self: *State) void {
        self.gpr_even = .{0} ** 512;
        self.gpr_odd = .{0} ** 512;
        self.sr1 = .{0} ** 512;
        self.sr2 = .{0} ** 512;
    }

    pub fn randomize(self: *State, rnd: std.rand.Random) void {
        rnd.bytes(std.mem.sliceAsBytes(self.gpr_even[0..]));
        rnd.bytes(std.mem.sliceAsBytes(self.gpr_odd[0..]));
        rnd.bytes(std.mem.sliceAsBytes(self.sr1[0..]));
        rnd.bytes(std.mem.sliceAsBytes(self.sr2[0..]));
    }

    fn gprAddress(index: Register_Index, rsn: RSN) u9 {
        return bits.concat2(@truncate(u3, index >> 1), rsn);
    }
    fn sr1Address(index: ctrl.SR1_Index, rsn: RSN) u9 {
        std.debug.assert(@bitSizeOf(ctrl.SR1_Index) == 3);
        return bits.concat2(@enumToInt(index), rsn);
    }
    fn sr2Address(index: ctrl.SR2_Index, rsn: RSN) u9 {
        std.debug.assert(@bitSizeOf(ctrl.SR2_Index) == 3);
        return bits.concat2(@enumToInt(index), rsn);
    }

    pub fn readEven(self: *const State, index: Register_Index, rsn: RSN) u16 {
        return self.gpr_even[gprAddress(index, rsn)];
    }
    pub fn writeEven(self: *State, index: Register_Index, rsn: RSN, value: u16) void {
        self.gpr_even[gprAddress(index, rsn)] = value;
    }

    pub fn readOdd(self: *const State, index: Register_Index, rsn: RSN) u16 {
        return self.gpr_odd[gprAddress(index, rsn)];
    }
    pub fn writeOdd(self: *State, index: Register_Index, rsn: RSN, value: u16) void {
        self.gpr_odd[gprAddress(index, rsn)] = value;
    }

    pub fn readSR1(self: *const State, index: ctrl.SR1_Index, rsn: RSN) u32 {
        return self.sr1[sr1Address(index, rsn)];
    }
    pub fn writeSR1(self: *State, index: ctrl.SR1_Index, rsn: RSN, value: u32) void {
        self.sr1[sr1Address(index, rsn)] = value;
    }

    pub fn readSR2(self: *const State, index: ctrl.SR2_Index, rsn: RSN) u32 {
        return self.sr2[sr2Address(index, rsn)];
    }
    pub fn writeSR2(self: *State, index: ctrl.SR2_Index, rsn: RSN, value: u32) void {
        self.sr2[sr2Address(index, rsn)] = value;
    }

    pub fn readSR(self: *const State, index: ctrl.Any_SR_Index, rsn: RSN) u32 {
        if (ctrl.addressBaseToSR1(index)) |sr1| {
            return self.readSR1(sr1, rsn);
        } else if (ctrl.addressBaseToSR2(index)) |sr2| {
            return self.readSR2(sr2, rsn);
        } else {
            unreachable;
        }
    }
    pub fn writeSR(self: *const State, index: ctrl.Any_SR_Index, rsn: RSN, value: u32) void {
        if (ctrl.addressBaseToSR1(index)) |sr1| {
            self.writeSR1(sr1, rsn, value);
        } else if (ctrl.addressBaseToSR2(index)) |sr2| {
            self.writeSR2(sr2, rsn, value);
        } else {
            unreachable;
        }
    }
};

pub const SetupInputs = struct {
    rsn: RSN,
    oboa: OB_OA,
    JL_SRC: ctrl.JL_Source,
    JH_SRC: ctrl.JH_Source,
    K_SRC: ctrl.K_Source,
    JR_RSEL: ctrl.Reg_File_Indexing_Source,
    KR_RSEL: ctrl.Reg_File_Indexing_Source,
    JR_RX: bool,
    KR_RX: bool,
    SR1_RI: ctrl.SR1_Index,
    SR2_RI: ctrl.SR2_Index,
    BASE: ctrl.Any_SR_Index,
    LITERAL: ctrl.Literal,
};

pub const SetupOutputs = struct {
    j: Split_J_Bus,
    k: misc.K_Bus,
    sr1: Split_J_Bus,
    sr2: Split_J_Bus,
    address_base: misc.Virtual_Address,
};

pub const TransactInputs = struct {
    rsn: RSN,
    read_rsn: RSN,
    oboa: OB_OA,
    inhibit_writes: bool,
    l: Split_L_Bus,
    sr1: Split_J_Bus,
    sr2: Split_J_Bus,
    virtual_address: VirtualAddress,
    JKR_WSEL: ctrl.Reg_File_Indexing_Source,
    JKR_WMODE: ctrl.Reg_File_Write_Mode,
    SR1_WSRC: ctrl.SR1_Write_Data_Source,
    SR2_WSRC: ctrl.SR2_Write_Data_Source,
    SR1_WI: ctrl.SR1_Index,
    SR2_WI: ctrl.SR2_Index,
    LITERAL: ctrl.Literal,
};

pub const RegisterView = struct {
    state: *State,
    rsn: RSN,

    pub fn init(state: *State, rsn: RSN) RegisterView {
        return .{
            .state = state,
            .rsn = rsn,
        };
    }

    pub fn readGPR(self: *const RegisterView, index: Register_Index) u16 {
        if ((index & 1) == 1) {
            return self.state.readOdd(index, self.rsn);
        } else {
            return self.state.readEven(index, self.rsn);
        }
    }
    pub fn writeGPR(self: *RegisterView, index: Register_Index, value: u16) void {
        if ((index & 1) == 1) {
            return self.state.writeOdd(index, self.rsn, value);
        } else {
            return self.state.writeEven(index, self.rsn, value);
        }
    }
    pub fn readSignedGPR(self: *const RegisterView, index: Register_Index) i16 {
        if ((index & 1) == 1) {
            return @bitCast(i16, self.state.readOdd(index, self.rsn));
        } else {
            return @bitCast(i16, self.state.readEven(index, self.rsn));
        }
    }
    pub fn writeSignedGPR(self: *RegisterView, index: Register_Index, value: i16) void {
        if ((index & 1) == 1) {
            return self.state.writeOdd(index, self.rsn, @bitCast(u16, value));
        } else {
            return self.state.writeEven(index, self.rsn, @bitCast(u16, value));
        }
    }

    pub fn readGPR32(self: *const RegisterView, index: Register_Index) u32 {
        return bits.concat(.{
            self.readGPR(index),
            self.readGPR(index ^ 1),
        });
    }
    pub fn writeGPR32(self: *RegisterView, index: Register_Index, value: u32) void {
        self.writeGPR(index, @truncate(u16, value));
        self.writeGPR(index ^ 1, @intCast(u16, value >> 16));
    }
    pub fn readSignedGPR32(self: *const RegisterView, index: Register_Index) i32 {
        return @bitCast(i32, bits.concat(.{
            self.readGPR(index),
            self.readGPR(index ^ 1),
        }));
    }
    pub fn writeSignedGPR32(self: *RegisterView, index: Register_Index, value: i32) void {
        const unsigned = @bitCast(u32, value);
        self.writeGPR(index, @truncate(u16, unsigned));
        self.writeGPR(index ^ 1, @intCast(u16, unsigned >> 16));
    }

    pub fn readSR(self: *const RegisterView, index: ctrl.Any_SR_Index) u32 {
        return self.state.readSR(index, self.rsn);
    }
    pub fn writeSR(self: *RegisterView, index: ctrl.Any_SR_Index, value: u32) void {
        return self.state.writeSR(index, self.rsn, value);
    }

    pub fn readSR1(self: *const RegisterView, index: ctrl.SR1_Index) u32 {
        return self.state.readSR1(index, self.rsn);
    }
    pub fn writeSR1(self: *RegisterView, index: ctrl.SR1_Index, value: u32) void {
        return self.state.writeSR1(index, self.rsn, value);
    }

    pub fn readSR2(self: *const RegisterView, index: ctrl.SR2_Index) u32 {
        return self.state.readSR2(index, self.rsn);
    }
    pub fn writeSR2(self: *RegisterView, index: ctrl.SR2_Index, value: u32) void {
        return self.state.writeSR2(index, self.rsn, value);
    }

    pub fn printRegs(self: *const RegisterView, writer: anytype) !void {
        try writer.print(" R1: {X:0>4}   R0: {X:0>4}      Z: {X:0>8}     Z: {X:0>8}\n", .{
            self.readGPR(1), self.readGPR(0), self.readSR1(.zero), self.readSR2(.zero),
        });
        try writer.print(" R3: {X:0>4}   R2: {X:0>4}     RP: {X:0>8}    IP: {X:0>8}\n", .{
            self.readGPR(3), self.readGPR(2), self.readSR1(.RP), self.readSR2(.IP),
        });
        try writer.print(" R5: {X:0>4}   R4: {X:0>4}     SP: {X:0>8}   NIP: {X:0>8}\n", .{
            self.readGPR(5), self.readGPR(4), self.readSR1(.SP), self.readSR2(.next_IP),
        });
        try writer.print(" R7: {X:0>4}   R6: {X:0>4}     BP: {X:0>8}   ASN: {X:0>8}\n", .{
            self.readGPR(7), self.readGPR(6), self.readSR1(.BP), self.readSR2(.ASN),
        });
        try writer.print(" R9: {X:0>4}   R8: {X:0>4}   UADL: {X:0>8}   KXP: {X:0>8}\n", .{
            self.readGPR(9), self.readGPR(8), self.readSR1(.fault_UA_DL), self.readSR2(.KXP),
        });
        try writer.print("R11: {X:0>4}  R10: {X:0>4}  RSTAT: {X:0>8}   UXP: {X:0>8}\n", .{
            self.readGPR(11), self.readGPR(10), self.readSR1(.fault_RSN_STAT), self.readSR2(.UXP),
        });
        try writer.print("R13: {X:0>4}  R12: {X:0>4}  ROBOA: {X:0>8}   RSR: {X:0>8}\n", .{
            self.readGPR(13), self.readGPR(12), self.readSR1(.int_RSN_fault_OB_OA), self.readSR2(.RS_reserved),
        });
        try writer.print("R15: {X:0>4}  R14: {X:0>4}   TMP1: {X:0>8}  TMP2: {X:0>8}\n", .{
            self.readGPR(15), self.readGPR(14), self.readSR1(.temp_1), self.readSR2(.temp_2),
        });
    }
};

pub fn setup(state: *const State, in: SetupInputs) SetupOutputs {
    const jr_index: Register_Index = switch (in.JR_RSEL) {
        .zero => 0,
        .LITERAL => @truncate(Register_Index, in.LITERAL),
        .OA => in.oboa.oa,
        .OB => in.oboa.ob,
    };
    const kr_index: Register_Index = switch (in.KR_RSEL) {
        .zero => 0,
        .LITERAL => @truncate(Register_Index, in.LITERAL),
        .OA => in.oboa.oa,
        .OB => in.oboa.ob,
    };

    var jr_swap = @truncate(u1, jr_index) == 1;
    if (in.JR_RX) {
        jr_swap = !jr_swap;
    }

    var kr_swap = @truncate(u1, kr_index) == 1;
    if (in.KR_RX) {
        kr_swap = !kr_swap;
    }

    const jr = if (jr_swap) blk: {
        break :blk Split_J_Bus{
            .low = state.readOdd(jr_index, in.rsn),
            .high = state.readEven(jr_index, in.rsn),
        };
    } else blk: {
        break :blk Split_J_Bus{
            .low = state.readEven(jr_index, in.rsn),
            .high = state.readOdd(jr_index, in.rsn),
        };
    };

    const kr = if (kr_swap) blk: {
        break :blk state.readOdd(kr_index, in.rsn);
    } else blk: {
        break :blk state.readEven(kr_index, in.rsn);
    };

    const sr1 = @bitCast(Split_J_Bus, state.readSR1(in.SR1_RI, in.rsn));
    const sr2 = @bitCast(Split_J_Bus, state.readSR2(in.SR2_RI, in.rsn));

    return .{
        .j = .{
            .low = switch (in.JL_SRC) {
                .zero => 0,
                .JRL => jr.low,
                .SR1L => sr1.low,
                .SR2L => sr2.low,
            },
            .high = switch (in.JH_SRC) {
                .zero => 0,
                .neg_one => 0xFFFF,
                .sx => bits.sx(u16, switch (in.JL_SRC) {
                    .zero => @as(u1, 0),
                    .JRL => @intCast(u1, jr.low >> 15),
                    .SR1L => @intCast(u1, sr1.low >> 15),
                    .SR2L => @intCast(u1, sr2.low >> 15),
                }),
                .JRL => jr.low,
                .JRH => jr.high,
                .SR1H => sr1.high,
                .SR2H => sr2.high,
            },
        },
        .k = switch (in.K_SRC) {
            .zero             => 0,
            .KR               => kr,
            .SR1L             => sr1.low,
            .SR2L             => sr2.low,
            .OB_OA_zx         => bits.zx(u16, @bitCast(u8, in.oboa)),
            .LITERAL          => bits.zx(u16, in.LITERAL),
            .LITERAL_minus_64 => bits._1x(u16, in.LITERAL),
            .LITERAL_special  => bits.concat(.{
                @as(u6, 0),
                @truncate(u7, @shlExact(@as(u8, 1), @truncate(u3, in.LITERAL))),
                @truncate(u3, in.LITERAL >> 3),
            }),
        },
        .sr1 = @bitCast(Split_J_Bus, sr1),
        .sr2 = @bitCast(Split_J_Bus, sr2),
        .address_base = state.readSR(in.BASE, in.rsn),
    };
}

pub fn transact(state: *State, in: TransactInputs) void {
    if (in.inhibit_writes) {
        return;
    }

    const jkr_index: Register_Index = switch (in.JKR_WSEL) {
        .zero => 0,
        .LITERAL => @truncate(Register_Index, in.LITERAL),
        .OA => in.oboa.oa,
        .OB => in.oboa.ob,
    };

    const odd_register = (jkr_index & 1) == 1;
    switch (in.JKR_WMODE) {
        .no_write => {},
        .write_16 => {
            if (odd_register) {
                state.writeOdd(jkr_index, in.rsn, in.l.low);
            } else {
                state.writeEven(jkr_index, in.rsn, in.l.low);
            }
        },
        .write_16_xor1 => {
            if (odd_register) {
                state.writeEven(jkr_index, in.rsn, in.l.low);
            } else {
                state.writeOdd(jkr_index, in.rsn, in.l.low);
            }
        },
        .write_32 => {
            if (odd_register) {
                state.writeOdd(jkr_index, in.rsn, in.l.low);
                state.writeEven(jkr_index, in.rsn, in.l.high);
            } else {
                state.writeEven(jkr_index, in.rsn, in.l.low);
                state.writeOdd(jkr_index, in.rsn, in.l.high);
            }
        },
    }

    switch (in.SR1_WSRC) {
        .no_write => {},
        .RSN_SR1 => {
            const val = bits.concat2(@truncate(u16, @bitCast(u32, in.sr1)), @as(u16, in.read_rsn));
            state.writeSR1(in.SR1_WI, in.rsn, val);
        },
        .L   => state.writeSR1(in.SR1_WI, in.rsn, @bitCast(u32, in.l)),
        .PN  => state.writeSR1(in.SR1_WI, in.rsn, @bitCast(u32, in.virtual_address)),
    }

    switch (in.SR2_WSRC) {
        .no_write => {},
        .SR2 => state.writeSR2(in.SR2_WI, in.rsn, @bitCast(u32, in.sr2)),
        .L   => state.writeSR2(in.SR2_WI, in.rsn, @bitCast(u32, in.l)),
        .PN  => state.writeSR2(in.SR2_WI, in.rsn, @bitCast(u32, in.virtual_address)),
    }
}
