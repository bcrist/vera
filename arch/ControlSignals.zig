const std = @import("std");
const uc = @import("microcode");
const misc = @import("misc");

const ControlSignals = @This();

literal: Literal,
jr_rsel: RegFileIndexingSource,
kr_rsel: RegFileIndexingSource,
jr_rx: bool,
kr_rx: bool,
jl_src: JLSource,
jh_src: JHSource,
k_src: KSource,
sr1_ri: SR1Index,
sr2_ri: SR2Index,
base: AnySRIndex,
offset: AddressOffset,
compute_mode: ComputeMode,
bus_mode: BusMode,
bus_byte: BusWidth,
bus_rw: BusDirection,
at_op: AddressTranslatorOp,
special: SpecialOp,
ll_src: LLSource,
lh_src: LHSource,
jkr_wsel: RegFileIndexingSource,
jkr_wmode: RegFileWriteMode,
sr1_wi: SR1Index,
sr2_wi: SR2Index,
sr1_wsrc: SR1WriteDataSource,
sr2_wsrc: SR2WriteDataSource,
stat_op: StatusRegOp,
dl_op: DataLatchOp,
ob_oa_op: OperandRegOp,
allow_int: bool,
seq_op: SequencerOp,
next_uop: uc.Continuation,

pub const SignalName = enum {
    literal,
    jr_rsel,
    kr_rsel,
    jr_rx,
    kr_rx,
    jl_src,
    jh_src,
    k_src,
    sr1_ri,
    sr2_ri,
    base,
    offset,
    compute_mode,
    bus_mode,
    bus_byte,
    bus_rw,
    at_op,
    special,
    ll_src,
    lh_src,
    jkr_wsel,
    jkr_wmode,
    sr1_wi,
    sr2_wi,
    sr1_wsrc,
    sr2_wsrc,
    stat_op,
    dl_op,
    ob_oa_op,
    allow_int,
    seq_op,
    next_uop,
};

pub fn init() ControlSignals {
    return .{
        .literal = 0,
        .jr_rsel = .zero,
        .kr_rsel = .zero,
        .jr_rx = false,
        .kr_rx = false,
        .jl_src = .zero,
        .jh_src = .zero,
        .k_src = .zero,
        .sr1_ri = .zero,
        .sr2_ri = .zero,
        .base = .zero,
        .offset = .zero,
        .compute_mode = ComputeMode.unused,
        .bus_mode = .raw,
        .bus_byte = .word,
        .bus_rw = .read,
        .at_op = .none,
        .special = .none,
        .ll_src = .zero,
        .lh_src = .zero,
        .jkr_wsel = .zero,
        .jkr_wmode = .no_write,
        .sr1_wi = .zero,
        .sr2_wi = .zero,
        .sr1_wsrc = .no_write,
        .sr2_wsrc = .no_write,
        .stat_op = .hold,
        .dl_op = .hold,
        .ob_oa_op = .hold,
        .allow_int = false,
        .seq_op = .next_uop,
        .next_uop = 0,
    };
}

pub fn randomize(self: *ControlSignals, rnd: std.rand.Random) void {
    self.literal = rnd.int(Literal);
    self.jr_rsel = rnd.enumValue(RegFileIndexingSource);
    self.kr_rsel = rnd.enumValue(RegFileIndexingSource);
    self.jr_rx = rnd.boolean();
    self.kr_rx = rnd.boolean();
    self.jl_src = rnd.enumValue(JLSource);
    self.jh_src = rnd.enumValue(JHSource);
    self.k_src = rnd.enumValue(KSource);
    self.sr1_ri = rnd.enumValue(SR1Index);
    self.sr2_ri = rnd.enumValue(SR2Index);
    self.base = rnd.enumValue(AnySRIndex);
    self.offset = rnd.enumValue(AddressOffset);
    self.compute_mode = .{ .unknown = rnd.int(u4) };
    self.bus_mode = rnd.enumValue(BusMode);
    self.bus_byte = rnd.enumValue(BusWidth);
    self.bus_rw = rnd.enumValue(BusDirection);
    self.at_op = rnd.enumValue(AddressTranslatorOp);
    self.special = rnd.enumValue(SpecialOp);
    self.ll_src = rnd.enumValue(LLSource);
    switch (self.ll_src) {
        // The simulator normally crashes on access to an address that's not hooked up to anything,
        // and this would be likely to trigger that so we'll just prevent it.  It shouldn't be a problem
        // for the real computer to have this happen during the first few cycles after power-on.
        .d16, .d8_sx => self.ll_src = .zero,
        else => {},
    }
    self.lh_src = rnd.enumValue(LHSource);
    self.jkr_wsel = rnd.enumValue(RegFileIndexingSource);
    self.jkr_wmode = rnd.enumValue(RegFileWriteMode);
    self.sr1_wi = rnd.enumValue(SR1Index);
    self.sr2_wi = rnd.enumValue(SR2Index);
    self.sr1_wsrc = rnd.enumValue(SR1WriteDataSource);
    self.sr2_wsrc = rnd.enumValue(SR2WriteDataSource);
    self.stat_op = rnd.enumValue(StatusRegOp);
    self.dl_op = rnd.enumValue(DataLatchOp);
    self.ob_oa_op = rnd.enumValue(OperandRegOp);
    self.allow_int = rnd.boolean();
    self.seq_op = rnd.enumValue(SequencerOp);
    self.next_uop = rnd.int(uc.Continuation);
}

pub fn print(self: *const ControlSignals, writer: anytype) !void {
    const def = ControlSignals.init();
    if (self.literal != def.literal) try writer.print(" literal={x}", .{ self.literal });
    if (self.jr_rsel != def.jr_rsel) try writer.print(" jr_rsel={s}", .{ @tagName(self.jr_rsel) });
    if (self.jr_rx != def.jr_rx) try writer.print(" jr_rx={}", .{ @boolToInt(self.jr_rx) });
    if (self.kr_rsel != def.kr_rsel) try writer.print(" kr_rsel={s}", .{ @tagName(self.kr_rsel) });
    if (self.kr_rx != def.kr_rx) try writer.print(" kr_rx={}", .{ @boolToInt(self.kr_rx) });
    if (self.sr1_ri != def.sr1_ri) try writer.print(" sr1_ri={s}", .{ @tagName(self.sr1_ri) });
    if (self.sr2_ri != def.sr2_ri) try writer.print(" sr2_ri={s}", .{ @tagName(self.sr2_ri) });
    if (self.jl_src != def.jl_src) try writer.print(" jl_src={s}", .{ @tagName(self.jl_src) });
    if (self.jh_src != def.jh_src) try writer.print(" jh_src={s}", .{ @tagName(self.jh_src) });
    if (self.k_src != def.k_src) try writer.print(" k_src={s}", .{ @tagName(self.k_src) });
    if (self.base != def.base) try writer.print(" base={s}", .{ @tagName(self.base) });
    if (self.offset != def.offset) try writer.print(" offset={s}", .{ @tagName(self.offset) });

    if (!std.meta.eql(self.compute_mode, def.compute_mode)) {
        switch (self.compute_mode) {
            .unknown => |value| {
                try writer.print(" compute_mode=unknown({X})", .{ value });
            },
            else => {
                const what: []const u8 = switch (self.compute_mode) {
                    .arith    => |value| @tagName(value),
                    .logic    => |value| @tagName(value),
                    .mult     => |value| @tagName(value),
                    .shift    => |value| @tagName(value),
                    else => "none",
                };

                try writer.print(" compute_mode={s}.{s}", .{ @tagName(@as(ComputeModeTag, self.compute_mode)), what });
            },
        }
    }
    if (self.bus_mode != def.bus_mode) try writer.print(" bus_mode={s}", .{ @tagName(self.bus_mode) });
    if (self.bus_byte != def.bus_byte) try writer.print(" bus_byte={s}", .{ @tagName(self.bus_byte) });
    if (self.bus_rw != def.bus_rw) try writer.print(" bus_rw={s}", .{ @tagName(self.bus_rw) });
    if (self.at_op != def.at_op) try writer.print(" at_op={s}", .{ @tagName(self.at_op) });
    if (self.ll_src != def.ll_src) try writer.print(" ll_src={s}", .{ @tagName(self.ll_src) });
    if (self.lh_src != def.lh_src) try writer.print(" lh_src={s}", .{ @tagName(self.lh_src) });
    if (self.jkr_wsel != def.jkr_wsel) try writer.print(" jkr_wsel={s}", .{ @tagName(self.jkr_wsel) });
    if (self.jkr_wmode != def.jkr_wmode) try writer.print(" jkr_wmode={s}", .{ @tagName(self.jkr_wmode) });
    if (self.sr1_wsrc != def.sr1_wsrc) try writer.print(" sr1_wsrc={s}", .{ @tagName(self.sr1_wsrc) });
    if (self.sr1_wi != def.sr1_wi) try writer.print(" sr1_wi={s}", .{ @tagName(self.sr1_wi) });
    if (self.sr2_wsrc != def.sr2_wsrc) try writer.print(" sr2_wsrc={s}", .{ @tagName(self.sr2_wsrc) });
    if (self.sr2_wi != def.sr2_wi) try writer.print(" sr2_wi={s}", .{ @tagName(self.sr2_wi) });
    if (self.stat_op != def.stat_op) try writer.print(" stat_op={s}", .{ @tagName(self.stat_op) });
    if (self.dl_op != def.dl_op) try writer.print(" dl_op={s}", .{ @tagName(self.dl_op) });
    if (self.ob_oa_op != def.ob_oa_op) try writer.print(" ob_oa_op={s}", .{ @tagName(self.ob_oa_op) });
    if (self.special != def.special) try writer.print(" special={s}", .{ @tagName(self.special) });
    if (self.allow_int != def.allow_int) try writer.print(" allow_int={}", .{ @boolToInt(self.allow_int) });
    if (self.seq_op != def.seq_op) try writer.print(" seq_op={s}", .{ @tagName(self.seq_op) });
    if (self.next_uop != def.next_uop) try writer.print(" next_uop={x}", .{ self.next_uop });

    try writer.print("\n", .{});
}

pub fn address_offset(self: *ControlSignals) misc.SignedOffsetForLiteral {
    return switch (self.offset) {
        .zero => 0,
        .two => 2,
        .literal => self.literal,
        .literal_minus_64 => @as(misc.SignedOffsetForLiteral, self.literal) - 64,
    };
}

pub const Literal = u6;

// determines which general purpose registers to read/write
pub const RegFileIndexingSource = enum(u2) {
    zero = 0,
    literal = 1,
    oa = 2,
    ob = 3,
};

pub const RegFileWriteMode = enum(u2) {
    no_write = 0,
    write_16 = 1,
    write_16_xor1 = 2,
    write_32 = 3,
};

// what drives the JL bus?
pub const JLSource = enum(u2) {
    zero = 0,
    jrl = 1,
    sr1l = 2,
    sr2l = 3,
};

// what drives the JH bus?
pub const JHSource = enum(u3) {
    zero = 0,
    neg_one = 1,
    sx_jl = 2,
    // 3 unused
    jrl = 4, // TODO unused?
    jrh = 5,
    sr1h = 6,
    sr2h = 7,
};

pub const KSource = enum(u3) {
    zero = 0,
    kr = 1,
    sr1l = 2,
    sr2l = 3, // TODO only used by a few instructions, might be able to replace with sr1l?
    literal = 4,
    literal_minus_64 = 5,
    literal_special = 6, // 3:8 decoder on LITERAL[2:0] -> K[13:7], LITERAL[4:3] -> K[15:14]
    ob_oa_zx = 7,
};

pub const SR1Index = enum(u3) {
    zero = 0,           // 0x0000_0000 (currently not used for anything)
    rp = 1,             // return pointer
    sp = 2,             // stack pointer
    bp = 3,             // stack base pointer
    fault_ua_dl = 4,            // UA (high 16b) and DL (low 16b) copied when entering a fault handler.
    fault_rsn_stat = 5,         // STAT (bits 15:0) copied when entering a fault handler.  previous RSN (bits 21:16) stored when using STRS/LDRS.
    int_rsn_fault_ob_oa = 6,    // OB/OA (bits 7:0) copied when entering a fault handler.  previous RSN (bits 21:16) stored when entering an interrupt handler.
    temp_1 = 7,
};

pub const SR2Index = enum(u3) {
    zero = 0,           // 0x0000_0000 (must never be overwritten)
    ip = 1,             // instruction pointer
    asn = 2,            // address space number
    next_ip = 3,        // when next instruction is loaded before the last cycle of the current instruction, the next IP is kept here.
    kxp = 4,            // kernel context pointer
    uxp = 5,            // user context pointer
    rs_reserved = 6,    // used only by STRS/LDRS and not stored in Registerset_State.  If a fault occurs during one of these instructions, the faulting registerset cannot be STRS/LDRS'd and then resumed.
    temp_2 = 7,
};

pub const AnySRIndex = enum(u4) {
    zero = @enumToInt(SR2Index.zero),
    ip = @enumToInt(SR2Index.ip),
    next_ip = @enumToInt(SR2Index.next_ip),
    asn = @enumToInt(SR2Index.asn),
    kxp = @enumToInt(SR2Index.kxp),
    uxp = @enumToInt(SR2Index.uxp),
    rs_reserved = @enumToInt(SR2Index.rs_reserved),
    temp_2 = @enumToInt(SR2Index.temp_2),

    sr1_zero = @as(u4, @enumToInt(SR1Index.zero)) + 8,
    rp = @as(u4, @enumToInt(SR1Index.rp)) + 8,
    sp = @as(u4, @enumToInt(SR1Index.sp)) + 8,
    bp = @as(u4, @enumToInt(SR1Index.bp)) + 8,
    fault_ua_dl = @as(u4, @enumToInt(SR1Index.fault_ua_dl)) + 8,
    fault_rsn_stat = @as(u4, @enumToInt(SR1Index.fault_rsn_stat)) + 8,
    int_rsn_fault_ob_oa = @as(u4, @enumToInt(SR1Index.int_rsn_fault_ob_oa)) + 8,
    temp_1 = @as(u4, @enumToInt(SR1Index.temp_1)) + 8,
};

pub fn addressBaseToSR1(base: AnySRIndex) ?SR1Index {
    const ord = @enumToInt(base);
    if (ord >= 8) {
        return @intToEnum(SR1Index, ord - 8);
    } else {
        return null;
    }
}

pub fn addressBaseToSR2(base: AnySRIndex) ?SR2Index {
    const ord = @enumToInt(base);
    if (ord >= 8) {
        return null;
    } else {
        return @intToEnum(SR2Index, ord);
    }
}

pub const AddressOffset = enum(u2) {
    zero = 0,
    literal = 1,
    two = 2,
    literal_minus_64 = 3,
};

pub const SR1WriteDataSource = enum(u2) {
    no_write = 0,
    rsn_sr1 = 1, // bits 31:16 from RSN that was used during setup cycle, bits 15:0 from SR1 data that was read during setup cycle.
    l_bus = 2,
    virtual_address = 3,
};

pub const SR2WriteDataSource = enum(u2) {
    no_write = 0,
    sr2 = 1,
    l_bus = 2,
    virtual_address = 3,
};

pub const LLSource = enum(u4) {
    zero = 0,
    logic = 1,
    shift_l = 2,
    arith_l = 3,
    mult_l = 4,
    d16 = 6,
    d8_sx = 7,
    stat = 12,
    pipe = 13,
    last_translation_info_l = 14,
};

pub const LHSource = enum(u4) {
    zero = 0,
    logic = 1,
    shift_h = 2,
    arith_h = 3,
    mult_h = 4,
    d16 = 6,
    sx_ll = 7,
    jh = 8,
    prev_ua = 9,
    last_translation_info_h = 14,
};

pub const DataLatchOp = enum(u2) {
    hold = 0,
    from_d = 1,
    to_d = 2,
};

pub const OperandRegOp = enum(u2) {
    hold = 0,
    from_dl = 1,
    increment_ob = 2,
    clear_ob = 3,
};

pub const BusDirection = enum(u1) {
    read = 0,
    write = 1,
};

pub const BusWidth = enum(u1) {
    word = 0,
    byte = 1,
};

pub const BusMode = enum(u2) {
    raw = 0,
    data = 1,
    stack = 2,
    insn = 3,
};

// what should the address translator do this cycle?
// bus operation is inhibited if not .translate
pub const AddressTranslatorOp = enum(u2) {
    none = 0,
    translate = 1,
    update = 2,
    invalidate = 3,
};

pub const ComputeModeTag = enum {
    unused,
    unknown,
    arith,
    logic,
    mult,
    shift,
};

pub const ComputeMode = union(ComputeModeTag) {
    unused,
    unknown: u4,
    arith: ArithMode,
    logic: LogicMode,
    mult: MultMode,
    shift: ShiftMode,

    pub fn raw(mode: ComputeMode) u4 {
        return switch (mode) {
            .unused => @as(u4, 0),
            .unknown  => |value| value,
            .arith    => |value| @enumToInt(value),
            .logic    => |value| @enumToInt(value),
            .mult     => |value| @enumToInt(value),
            .shift    => |value| @enumToInt(value),
        };
    }
};

pub const ArithMode = enum(u4) {
    jl_plus_k            = @bitCast(u4, ArithModeBits { .subtract = false, .carry_borrow = false, .width = .jl_k }),
    jl_minus_k           = @bitCast(u4, ArithModeBits { .subtract = true,  .carry_borrow = false, .width = .jl_k }),
    jl_plus_k_plus_c     = @bitCast(u4, ArithModeBits { .subtract = false, .carry_borrow = true,  .width = .jl_k }),
    jl_minus_k_minus_c   = @bitCast(u4, ArithModeBits { .subtract = true,  .carry_borrow = true,  .width = .jl_k }),

    j_plus_k_zx          = @bitCast(u4, ArithModeBits { .subtract = false, .carry_borrow = false, .width = .j_k_zx }),
    j_minus_k_zx         = @bitCast(u4, ArithModeBits { .subtract = true,  .carry_borrow = false, .width = .j_k_zx }),
    j_plus_k_zx_plus_c   = @bitCast(u4, ArithModeBits { .subtract = false, .carry_borrow = true,  .width = .j_k_zx }),
    j_minus_k_zx_minus_c = @bitCast(u4, ArithModeBits { .subtract = true,  .carry_borrow = true,  .width = .j_k_zx }),

    j_plus_k_sx          = @bitCast(u4, ArithModeBits { .subtract = false, .carry_borrow = false, .width = .j_k_sx }),
    j_minus_k_sx         = @bitCast(u4, ArithModeBits { .subtract = true,  .carry_borrow = false, .width = .j_k_sx }),
    j_plus_k_sx_plus_c   = @bitCast(u4, ArithModeBits { .subtract = false, .carry_borrow = true,  .width = .j_k_sx }),
    j_minus_k_sx_minus_c = @bitCast(u4, ArithModeBits { .subtract = true,  .carry_borrow = true,  .width = .j_k_sx }),

    j_plus_k_1x          = @bitCast(u4, ArithModeBits { .subtract = false, .carry_borrow = false, .width = .j_k_1x }),
    j_minus_k_1x         = @bitCast(u4, ArithModeBits { .subtract = true,  .carry_borrow = false, .width = .j_k_1x }),
    j_plus_k_1x_plus_c   = @bitCast(u4, ArithModeBits { .subtract = false, .carry_borrow = true,  .width = .j_k_1x }),
    j_minus_k_1x_minus_c = @bitCast(u4, ArithModeBits { .subtract = true,  .carry_borrow = true,  .width = .j_k_1x }),
};
pub const ArithWidthMode = enum(u2) {
    jl_k = 0, // 16 bit
    j_k_zx = 1,
    j_k_sx = 2,
    j_k_1x = 3,
};
pub const ArithModeBits = packed struct {
    subtract: bool,
    carry_borrow: bool,
    width: ArithWidthMode,
};

pub const LogicMode = enum(u4) {
    jl_and_k     = @bitCast(u4, LogicModeBits { .op = .jl_and_k,         .invert_k = false }),
    jl_nand_k    = @bitCast(u4, LogicModeBits { .op = .jl_nand_k,        .invert_k = false }),
    jl_and_not_k = @bitCast(u4, LogicModeBits { .op = .jl_and_k,         .invert_k = true  }),
    jl_or_not_k  = @bitCast(u4, LogicModeBits { .op = .jl_or_k,          .invert_k = true  }),
    jl_nor_k     = @bitCast(u4, LogicModeBits { .op = .jl_nor_k,         .invert_k = false }),
    jl_or_k      = @bitCast(u4, LogicModeBits { .op = .jl_or_k,          .invert_k = false }),
    jl_xor_k     = @bitCast(u4, LogicModeBits { .op = .jl_xor_k,         .invert_k = false }),
    jl_xnor_k    = @bitCast(u4, LogicModeBits { .op = .jl_xor_k,         .invert_k = true  }),
    cb_k         = @bitCast(u4, LogicModeBits { .op = .count_k,          .invert_k = false }), // count set bits
    cz_k         = @bitCast(u4, LogicModeBits { .op = .count_k,          .invert_k = true  }), // count zero bits
    ctb_k        = @bitCast(u4, LogicModeBits { .op = .count_trailing_k, .invert_k = false }), // count trailing (lsb) set bits
    ctz_k        = @bitCast(u4, LogicModeBits { .op = .count_trailing_k, .invert_k = true  }), // count trailing (lsb) zero bits
    clb_k        = @bitCast(u4, LogicModeBits { .op = .count_leading_k,  .invert_k = false }), // count leading (msb) set bits
    clz_k        = @bitCast(u4, LogicModeBits { .op = .count_leading_k,  .invert_k = true  }), // count leading (msb) zero bits
};
pub const LogicModeBits = packed struct {
    op: LogicOp,
    invert_k: bool,
};
pub const LogicOp = enum(u3) {
    jl_or_k = 0,
    jl_nor_k = 1,
    jl_and_k = 2,
    jl_nand_k = 3,
    jl_xor_k = 4,
    count_k = 5,
    count_trailing_k = 6,
    count_leading_k = 7,
};

pub const MultMode = enum(u4) {
    jl_zx_k_zx = @bitCast(u4, MultModeBits { .jl = .unsigned, .k = .unsigned, .swap_output = false }),
    jl_sx_k_zx = @bitCast(u4, MultModeBits { .jl = .signed,   .k = .unsigned, .swap_output = false }),
    jl_zx_k_sx = @bitCast(u4, MultModeBits { .jl = .unsigned, .k = .signed,   .swap_output = false }),
    jl_sx_k_sx = @bitCast(u4, MultModeBits { .jl = .signed,   .k = .signed,   .swap_output = false }),

    jl_zx_k_zx_swap = @bitCast(u4, MultModeBits { .jl = .unsigned, .k = .unsigned, .swap_output = true }),
    jl_sx_k_zx_swap = @bitCast(u4, MultModeBits { .jl = .signed,   .k = .unsigned, .swap_output = true }),
    jl_zx_k_sx_swap = @bitCast(u4, MultModeBits { .jl = .unsigned, .k = .signed,   .swap_output = true }),
    jl_sx_k_sx_swap = @bitCast(u4, MultModeBits { .jl = .signed,   .k = .signed,   .swap_output = true }),
};
pub const MultDataType = enum(u1) {
    unsigned = 0,
    signed = 1,
};
pub const MultModeBits = packed struct {
    jl: MultDataType,
    k: MultDataType,
    swap_output: bool,
    _unused: u1 = 0,
};

pub const ShiftMode = enum(u4) {
    jl_shr_k4 = @bitCast(u4, ShiftModeBits { .left = false, .early_swap16 = false, .late_swap16 = false, .wide = false }),
    jl_shl_k4 = @bitCast(u4, ShiftModeBits { .left = true,  .early_swap16 = false, .late_swap16 = false, .wide = false }),
    jh_shr_k4 = @bitCast(u4, ShiftModeBits { .left = false, .early_swap16 = true,  .late_swap16 = false, .wide = false }),
    jh_shl_k4 = @bitCast(u4, ShiftModeBits { .left = true,  .early_swap16 = true,  .late_swap16 = false, .wide = false }),
    j_shr_k5  = @bitCast(u4, ShiftModeBits { .left = false, .early_swap16 = false, .late_swap16 = false, .wide = true }),
    j_shl_k5  = @bitCast(u4, ShiftModeBits { .left = true,  .early_swap16 = true,  .late_swap16 = true,  .wide = true }),
};
pub const ShiftModeBits = packed struct {
    left: bool,
    early_swap16: bool,
    late_swap16: bool,
    wide: bool,
};

pub const StatusRegOp = enum(u4) {
    hold = 0,
    zn_from_l = 1,
    zn_from_l_c_from_shift = 2,
    zn_from_l_no_set_z = 3,
    zn_from_ll = 5,
    zn_from_ll_c_from_shift = 6,
    zn_from_ll_no_set_z = 7,
    znvc_from_arith = 8,
    znvc_from_arith_no_set_z = 9,
    load_znvc_from_ll = 10,
    load_znvcka_from_ll = 11,
    clear_a = 12,
    set_a = 13,
    clear_s = 14,
    set_s = 15,
};

pub const SequencerOp = enum(u2) {
    next_uop = 0,
    next_instruction = 1,
    next_uop_force_normal = 2,
    fault_return = 3,
};

pub const SpecialOp = enum(u3) {
    none = 0,
    atomic_this = 1, // This cycle is atomic, but the next one is not, unless it is also .atomic_this
    atomic_next = 2, // All upcoming cycles are atomic, until the next cycle that's .atomic_this or .atomic_end
    atomic_end = 3, // The next cycle is not atomic, unless it's .atomic_this.  If this cycle wasn't atomic to begin with, this has no effect.
    block_transfer = 4, // transfers 8 bytes from FLASH or PSRAM into RAM, or vice versa, depending on bus_ctrl.  D_Bus is not used by this.
    load_rsn_from_ll = 5,
    toggle_rsn = 6,
    trigger_fault = 7,
};
