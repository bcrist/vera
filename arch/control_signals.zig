const std = @import("std");
const uc_layout = @import("microcode_layout");
const misc = @import("misc");

pub const Control_Signal = enum {
    LITERAL,
    JR_RSEL,
    KR_RSEL,
    JR_RX,
    KR_RX,
    JL_SRC,
    JH_SRC,
    K_SRC,
    SR1_RI,
    SR2_RI,
    BASE,
    OFFSET,
    ALU_MODE,
    BUS_MODE,
    BUS_BYTE,
    BUS_RW,
    AT_OP,
    SPECIAL,
    LL_SRC,
    LH_SRC,
    JKR_WSEL,
    JKR_WMODE,
    SR1_WI,
    SR2_WI,
    SR1_WSRC,
    SR2_WSRC,
    STAT_OP,
    DL_OP,
    OB_OA_OP,
    ALLOW_INT,
    SEQ_OP,
    NEXT_UOP,
};

pub const Control_Signals = struct {
    LITERAL: Literal,
    JR_RSEL: Reg_File_Indexing_Source,
    KR_RSEL: Reg_File_Indexing_Source,
    JR_RX: bool,
    KR_RX: bool,
    JL_SRC: JL_Source,
    JH_SRC: JH_Source,
    K_SRC: K_Source,
    SR1_RI: SR1_Index,
    SR2_RI: SR2_Index,
    BASE: Any_SR_Index,
    OFFSET: Address_Offset,
    ALU_MODE: ALU_Mode,
    BUS_MODE: Bus_Mode,
    BUS_BYTE: Bus_Width,
    BUS_RW: Bus_Direction,
    AT_OP: AT_Op,
    SPECIAL: Special_Op,
    LL_SRC: LL_Source,
    LH_SRC: LH_Source,
    JKR_WSEL: Reg_File_Indexing_Source,
    JKR_WMODE: Reg_File_Write_Mode,
    SR1_WI: SR1_Index,
    SR2_WI: SR2_Index,
    SR1_WSRC: SR1_Write_Data_Source,
    SR2_WSRC: SR2_Write_Data_Source,
    STAT_OP: STAT_Op,
    DL_OP: Data_Latch_Op,
    OB_OA_OP: Operand_Reg_Op,
    ALLOW_INT: bool,
    SEQ_OP: Sequencer_Op,
    NEXT_UOP: uc_layout.UC_Continuation,

    pub fn init() Control_Signals {
        return .{
            .LITERAL = 0,
            .JR_RSEL = .zero,
            .KR_RSEL = .zero,
            .JR_RX = false,
            .KR_RX = false,
            .JL_SRC = .zero,
            .JH_SRC = .zero,
            .K_SRC = .zero,
            .SR1_RI = .zero,
            .SR2_RI = .zero,
            .BASE = .zero,
            .OFFSET = .zero,
            .ALU_MODE = ALU_Mode.unused,
            .BUS_MODE = .raw,
            .BUS_BYTE = .word,
            .BUS_RW = .read,
            .AT_OP = .hold,
            .SPECIAL = .none,
            .LL_SRC = .zero,
            .LH_SRC = .zero,
            .JKR_WSEL = .zero,
            .JKR_WMODE = .no_write,
            .SR1_WI = .zero,
            .SR2_WI = .zero,
            .SR1_WSRC = .no_write,
            .SR2_WSRC = .no_write,
            .STAT_OP = .hold,
            .DL_OP = .hold,
            .OB_OA_OP = .hold,
            .ALLOW_INT = false,
            .SEQ_OP = .next_uop,
            .NEXT_UOP = 0,
        };
    }

    pub fn randomize(self: *Control_Signals, rnd: std.rand.Random) void {
        self.LITERAL = rnd.int(Literal);
        self.JR_RSEL = rnd.enumValue(Reg_File_Indexing_Source);
        self.KR_RSEL = rnd.enumValue(Reg_File_Indexing_Source);
        self.JR_RX = rnd.boolean();
        self.KR_RX = rnd.boolean();
        self.JL_SRC = rnd.enumValue(JL_Source);
        self.JH_SRC = rnd.enumValue(JH_Source);
        self.K_SRC = rnd.enumValue(K_Source);
        self.SR1_RI = rnd.enumValue(SR1_Index);
        self.SR2_RI = rnd.enumValue(SR2_Index);
        self.BASE = rnd.enumValue(Any_SR_Index);
        self.OFFSET = rnd.enumValue(Address_Offset);
        self.ALU_MODE = .{ .unknown = rnd.int(u4) };
        self.BUS_MODE = rnd.enumValue(Bus_Mode);
        self.BUS_BYTE = rnd.enumValue(Bus_Width);
        self.BUS_RW = rnd.enumValue(Bus_Direction);
        self.AT_OP = rnd.enumValue(AT_Op);
        self.SPECIAL = rnd.enumValue(Special_Op);
        self.LL_SRC = rnd.enumValue(LL_Source);
        switch (self.LL_SRC) {
            // The simulator normally crashes on access to an address that's not hooked up to anything,
            // and this would be likely to trigger that so we'll just prevent it.  It shouldn't be a problem
            // for the real computer to have this happen during the first few cycles after power-on.
            .D16, .D8_sx => self.LL_SRC = .zero,
            else => {},
        }
        self.LH_SRC = rnd.enumValue(LH_Source);
        self.JKR_WSEL = rnd.enumValue(Reg_File_Indexing_Source);
        self.JKR_WMODE = rnd.enumValue(Reg_File_Write_Mode);
        self.SR1_WI = rnd.enumValue(SR1_Index);
        self.SR2_WI = rnd.enumValue(SR2_Index);
        self.SR1_WSRC = rnd.enumValue(SR1_Write_Data_Source);
        self.SR2_WSRC = rnd.enumValue(SR2_Write_Data_Source);
        self.STAT_OP = rnd.enumValue(STAT_Op);
        self.DL_OP = rnd.enumValue(Data_Latch_Op);
        self.OB_OA_OP = rnd.enumValue(Operand_Reg_Op);
        self.ALLOW_INT = rnd.boolean();
        self.SEQ_OP = rnd.enumValue(Sequencer_Op);
        self.NEXT_UOP = rnd.int(uc_layout.UC_Continuation);
    }

    pub fn print(self: *const Control_Signals, writer: anytype) !void {
        const def = Control_Signals.init();
        if (self.LITERAL != def.LITERAL) try writer.print(" LITERAL={x}", .{ self.LITERAL });
        if (self.JR_RSEL != def.JR_RSEL) try writer.print(" JR_RSEL={s}", .{ @tagName(self.JR_RSEL) });
        if (self.JR_RX != def.JR_RX) try writer.print(" JR_RX={}", .{ @boolToInt(self.JR_RX) });
        if (self.KR_RSEL != def.KR_RSEL) try writer.print(" KR_RSEL={s}", .{ @tagName(self.KR_RSEL) });
        if (self.KR_RX != def.KR_RX) try writer.print(" KR_RX={}", .{ @boolToInt(self.KR_RX) });
        if (self.SR1_RI != def.SR1_RI) try writer.print(" SR1_RI={s}", .{ @tagName(self.SR1_RI) });
        if (self.SR2_RI != def.SR2_RI) try writer.print(" SR2_RI={s}", .{ @tagName(self.SR2_RI) });
        if (self.JL_SRC != def.JL_SRC) try writer.print(" JL_SRC={s}", .{ @tagName(self.JL_SRC) });
        if (self.JH_SRC != def.JH_SRC) try writer.print(" JH_SRC={s}", .{ @tagName(self.JH_SRC) });
        if (self.K_SRC != def.K_SRC) try writer.print(" K_SRC={s}", .{ @tagName(self.K_SRC) });
        if (self.BASE != def.BASE) try writer.print(" BASE={s}", .{ @tagName(self.BASE) });
        if (self.OFFSET != def.OFFSET) try writer.print(" OFFSET={s}", .{ @tagName(self.OFFSET) });

        if (!std.meta.eql(self.ALU_MODE, def.ALU_MODE)) {
            switch (self.ALU_MODE) {
                .unknown => |value| {
                    try writer.print(" ALU_MODE=unknown({X})", .{ value });
                },
                else => {
                    const what: []const u8 = switch (self.ALU_MODE) {
                        .arith    => |value| @tagName(value),
                        .logic    => |value| @tagName(value),
                        .mult     => |value| @tagName(value),
                        .shift    => |value| @tagName(value),
                        .bitcount => |value| @tagName(value),
                        else => "none",
                    };

                    const ALU_Mode_Tag = comptime @typeInfo(ALU_Mode).Union.tag_type.?;
                    try writer.print(" ALU_MODE={s}.{s}", .{ @tagName(@as(ALU_Mode_Tag, self.ALU_MODE)), what });
                },
            }
        }
        if (self.BUS_MODE != def.BUS_MODE) try writer.print(" BUS_MODE={s}", .{ @tagName(self.BUS_MODE) });
        if (self.BUS_BYTE != def.BUS_BYTE) try writer.print(" BUS_BYTE={s}", .{ @tagName(self.BUS_BYTE) });
        if (self.BUS_RW != def.BUS_RW) try writer.print(" BUS_RW={s}", .{ @tagName(self.BUS_RW) });
        if (self.AT_OP != def.AT_OP) try writer.print(" AT_OP={s}", .{ @tagName(self.AT_OP) });
        if (self.LL_SRC != def.LL_SRC) try writer.print(" LL_SRC={s}", .{ @tagName(self.LL_SRC) });
        if (self.LH_SRC != def.LH_SRC) try writer.print(" LH_SRC={s}", .{ @tagName(self.LH_SRC) });
        if (self.JKR_WSEL != def.JKR_WSEL) try writer.print(" JKR_WSEL={s}", .{ @tagName(self.JKR_WSEL) });
        if (self.JKR_WMODE != def.JKR_WMODE) try writer.print(" JKR_WMODE={s}", .{ @tagName(self.JKR_WMODE) });
        if (self.SR1_WSRC != def.SR1_WSRC) try writer.print(" SR1_WSRC={s}", .{ @tagName(self.SR1_WSRC) });
        if (self.SR1_WI != def.SR1_WI) try writer.print(" SR1_WI={s}", .{ @tagName(self.SR1_WI) });
        if (self.SR2_WSRC != def.SR2_WSRC) try writer.print(" SR2_WSRC={s}", .{ @tagName(self.SR2_WSRC) });
        if (self.SR2_WI != def.SR2_WI) try writer.print(" SR2_WI={s}", .{ @tagName(self.SR2_WI) });
        if (self.STAT_OP != def.STAT_OP) try writer.print(" STAT_OP={s}", .{ @tagName(self.STAT_OP) });
        if (self.DL_OP != def.DL_OP) try writer.print(" DL_OP={s}", .{ @tagName(self.DL_OP) });
        if (self.OB_OA_OP != def.OB_OA_OP) try writer.print(" OB_OA_OP={s}", .{ @tagName(self.OB_OA_OP) });
        if (self.STAT_OP != def.STAT_OP) try writer.print(" STAT_OP={s}", .{ @tagName(self.STAT_OP) });
        if (self.SPECIAL != def.SPECIAL) try writer.print(" SPECIAL={s}", .{ @tagName(self.SPECIAL) });
        if (self.ALLOW_INT != def.ALLOW_INT) try writer.print(" ALLOW_INT={}", .{ @boolToInt(self.ALLOW_INT) });
        if (self.SEQ_OP != def.SEQ_OP) try writer.print(" SEQ_OP={s}", .{ @tagName(self.SEQ_OP) });
        if (self.NEXT_UOP != def.NEXT_UOP) try writer.print(" NEXT_UOP={x}", .{ self.NEXT_UOP });

        try writer.print("\n", .{});
    }

    pub fn address_offset(self: *Control_Signals) misc.Signed_Offset_For_Literal {
        return switch (self.OFFSET) {
            .zero => 0,
            .two => 2,
            .LITERAL => self.LITERAL,
            .LITERAL_minus_64 => @as(misc.Signed_Offset_For_Literal, self.LITERAL) - 64,
        };
    }
};

pub const Literal = u6;

// what determines which general purpose registers to read/write?
pub const Reg_File_Indexing_Source = enum(u2) {
    zero = 0,
    LITERAL = 1,
    OA = 2,
    OB = 3,
};

pub const Reg_File_Write_Mode = enum(u2) {
    no_write = 0,
    write_16 = 1,
    write_16_xor1 = 2,
    write_32 = 3,
};

// what drives the JL bus?
pub const JL_Source = enum(u2) {
    zero = 0,
    JRL = 1,
    SR1L = 2,
    SR2L = 3,
};

// what drives the JH bus?
pub const JH_Source = enum(u3) {
    zero = 0,
    neg_one = 1,
    sx = 2, // copy high bit of JL 16 times
    // 3 unused
    JRL = 4,
    JRH = 5,
    SR1H = 6,
    SR2H = 7,
};

pub const K_Source = enum(u3) {
    zero = 0,
    KR = 1,
    SR1L = 2,
    SR2L = 3,
    LITERAL = 4,
    LITERAL_minus_64 = 5,
    LITERAL_special = 6, // 3:8 decoder on LITERAL[2:0] -> K[13:7], LITERAL[4:3] -> K[15:14]
    OB_OA_zx = 7,
};

pub const SR1_Index = enum(u3) {
    zero = 0,           // 0x0000_0000 (currently not used for anything)
    RP = 1,             // return pointer
    SP = 2,             // stack pointer
    BP = 3,             // stack base pointer
    fault_UA_DL = 4,            // UA (high 16b) and DL (low 16b) copied when entering a fault handler.
    fault_RSN_STAT = 5,         // STAT (bits 15:0) copied when entering a fault handler.  previous RSN (bits 21:16) stored when using STRS/LDRS.
    int_RSN_fault_OB_OA = 6,    // OB/OA (bits 7:0) copied when entering a fault handler.  previous RSN (bits 21:16) stored when entering an interrupt handler.
    temp_1 = 7,
};

pub const SR2_Index = enum(u3) {
    zero = 0,           // 0x0000_0000 (must never be overwritten)
    IP = 1,             // instruction pointer
    ASN = 2,            // address space number
    next_IP = 3,        // when next instruction is loaded before the last cycle of the current instruction, the next IP is kept here.
    KXP = 4,            // kernel context pointer
    UXP = 5,            // user context pointer
    RS_reserved = 6,    // used only by STRS/LDRS and not stored in Registerset_State.  If a fault occurs during one of these instructions, the faulting registerset cannot be STRS/LDRS'd and then resumed.
    temp_2 = 7,
};

pub const Any_SR_Index = enum(u4) {
    zero = @enumToInt(SR2_Index.zero),
    IP = @enumToInt(SR2_Index.IP),
    next_IP = @enumToInt(SR2_Index.next_IP),
    ASN = @enumToInt(SR2_Index.ASN),
    KXP = @enumToInt(SR2_Index.KXP),
    UXP = @enumToInt(SR2_Index.UXP),
    RS_reserved = @enumToInt(SR2_Index.RS_reserved),
    temp_2 = @enumToInt(SR2_Index.temp_2),

    SR1_zero = @as(u4, @enumToInt(SR1_Index.zero)) + 8,
    RP = @as(u4, @enumToInt(SR1_Index.RP)) + 8,
    SP = @as(u4, @enumToInt(SR1_Index.SP)) + 8,
    BP = @as(u4, @enumToInt(SR1_Index.BP)) + 8,
    fault_UA_DL = @as(u4, @enumToInt(SR1_Index.fault_UA_DL)) + 8,
    fault_RSN_STAT = @as(u4, @enumToInt(SR1_Index.fault_RSN_STAT)) + 8,
    int_RSN_fault_OB_OA = @as(u4, @enumToInt(SR1_Index.int_RSN_fault_OB_OA)) + 8,
    temp_1 = @as(u4, @enumToInt(SR1_Index.temp_1)) + 8,
};

pub fn addressBaseToSR1(base: Any_SR_Index) ?SR1_Index {
    const ord = @enumToInt(base);
    if (ord >= 8) {
        return @intToEnum(SR1_Index, ord - 8);
    } else {
        return null;
    }
}

pub fn addressBaseToSR2(base: Any_SR_Index) ?SR2_Index {
    const ord = @enumToInt(base);
    if (ord >= 8) {
        return null;
    } else {
        return @intToEnum(SR2_Index, ord);
    }
}

pub const Address_Offset = enum(u2) {
    zero = 0,
    LITERAL = 1,
    two = 2,
    LITERAL_minus_64 = 3,
};

pub const SR1_Write_Data_Source = enum(u2) {
    no_write = 0,
    RSN_SR1 = 1, // bits 31:16 from RSN that was used during setup cycle, bits 15:0 from SR1 data that was read during setup cycle.
    L = 2,
    PN = 3,
};

pub const SR2_Write_Data_Source = enum(u2) {
    no_write = 0,
    SR2 = 1,
    L = 2,
    PN = 3,
};

pub const LL_Source = enum(u4) {
    zero = 0,
    logic = 1,
    shift_L = 2,
    arith_L = 3,
    mult_L = 4,
    bitcount = 5,
    D16 = 6,
    D8_sx = 7,
    AT_ME_L = 10,
    AT_OE_L = 11,
    STAT = 12,
    pipe_ID = 13,
    last_mmu_op_L = 14,
};

pub const LH_Source = enum(u4) {
    zero = 0,
    logic = 1,
    shift_H = 2,
    arith_H = 3,
    mult_H = 4,
    D16 = 6,
    sx = 7, // repeat LL[15]
    JH = 8,
    prev_UA = 9,
    AT_ME_H = 10,
    AT_OE_H = 11,
    last_mmu_op_H = 14,
};

pub const Data_Latch_Op = enum(u2) {
    hold = 0,
    from_D = 1,
    to_D = 2,
};

pub const Operand_Reg_Op = enum(u2) {
    hold = 0,
    from_DL = 1,
    increment_OB = 2,
    clear_OB = 3,
};

pub const Bus_Direction = enum(u1) {
    read = 0,
    write = 1,
};

pub const Bus_Width = enum(u1) {
    word = 0,
    byte = 1,
};

pub const Bus_Mode = enum(u2) {
    raw = 0,
    data = 1,
    stack = 2,
    insn = 3,
};

// what should the address translator do this cycle?
// bus operation is inhibited if not .translate
pub const AT_Op = enum(u2) {
    hold = 0, // uses ME/OE from previous cycle
    translate = 1,
    update = 2,
    invalidate = 3,
};

pub const ALU_Mode_Type = enum {
    unused,
    unknown,
    arith,
    logic,
    mult,
    shift,
    bitcount,
};

pub const ALU_Mode = union(ALU_Mode_Type) {
    unused,
    unknown: u4,
    arith: Arith_Mode,
    logic: Logic_Mode,
    mult: Multiplier_Mode,
    shift: Shift_Mode,
    bitcount: Bitcount_Mode,

    pub fn raw(mode: ALU_Mode) u4 {
        return switch (mode) {
            .unused => @as(u4, 0),
            .unknown  => |value| value,
            .arith    => |value| @enumToInt(value),
            .logic    => |value| @enumToInt(value),
            .mult     => |value| @enumToInt(value),
            .shift    => |value| @enumToInt(value),
            .bitcount => |value| @enumToInt(value),
        };
    }
};

pub const Arith_Mode = enum(u4) {
    JL_plus_K            = @bitCast(u4, Arith_Mode_Bits { .subtract = false, .carry_borrow = false, .width = .JL_K }),
    JL_minus_K           = @bitCast(u4, Arith_Mode_Bits { .subtract = true,  .carry_borrow = false, .width = .JL_K }),
    JL_plus_K_plus_C     = @bitCast(u4, Arith_Mode_Bits { .subtract = false, .carry_borrow = true,  .width = .JL_K }),
    JL_minus_K_minus_C   = @bitCast(u4, Arith_Mode_Bits { .subtract = true,  .carry_borrow = true,  .width = .JL_K }),

    J_plus_K_zx          = @bitCast(u4, Arith_Mode_Bits { .subtract = false, .carry_borrow = false, .width = .J_K_zx }),
    J_minus_K_zx         = @bitCast(u4, Arith_Mode_Bits { .subtract = true,  .carry_borrow = false, .width = .J_K_zx }),
    J_plus_K_zx_plus_C   = @bitCast(u4, Arith_Mode_Bits { .subtract = false, .carry_borrow = true,  .width = .J_K_zx }),
    J_minus_K_zx_minus_C = @bitCast(u4, Arith_Mode_Bits { .subtract = true,  .carry_borrow = true,  .width = .J_K_zx }),

    J_plus_K_sx          = @bitCast(u4, Arith_Mode_Bits { .subtract = false, .carry_borrow = false, .width = .J_K_sx }),
    J_minus_K_sx         = @bitCast(u4, Arith_Mode_Bits { .subtract = true,  .carry_borrow = false, .width = .J_K_sx }),
    J_plus_K_sx_plus_C   = @bitCast(u4, Arith_Mode_Bits { .subtract = false, .carry_borrow = true,  .width = .J_K_sx }),
    J_minus_K_sx_minus_C = @bitCast(u4, Arith_Mode_Bits { .subtract = true,  .carry_borrow = true,  .width = .J_K_sx }),

    J_plus_K_1x          = @bitCast(u4, Arith_Mode_Bits { .subtract = false, .carry_borrow = false, .width = .J_K_1x }),
    J_minus_K_1x         = @bitCast(u4, Arith_Mode_Bits { .subtract = true,  .carry_borrow = false, .width = .J_K_1x }),
    J_plus_K_1x_plus_C   = @bitCast(u4, Arith_Mode_Bits { .subtract = false, .carry_borrow = true,  .width = .J_K_1x }),
    J_minus_K_1x_minus_C = @bitCast(u4, Arith_Mode_Bits { .subtract = true,  .carry_borrow = true,  .width = .J_K_1x }),
};
pub const Arith_Width_Mode = enum(u2) {
    JL_K = 0, // 16 bit
    J_K_zx = 1,
    J_K_sx = 2,
    J_K_1x = 3,
};
pub const Arith_Mode_Bits = packed struct {
    subtract: bool,
    carry_borrow: bool,
    width: Arith_Width_Mode,
};

pub const Logic_Mode = enum(u4) {
    JL_and_K     = @bitCast(u4, Logic_Mode_Bits { .xor = false, .invert_JL = false, .invert_K = false, .invert_Y = false }),
    JL_nand_K    = @bitCast(u4, Logic_Mode_Bits { .xor = false, .invert_JL = false, .invert_K = false, .invert_Y = true  }),
    JL_and_not_K = @bitCast(u4, Logic_Mode_Bits { .xor = false, .invert_JL = false, .invert_K = true,  .invert_Y = false }),
    not_JL_or_K  = @bitCast(u4, Logic_Mode_Bits { .xor = false, .invert_JL = false, .invert_K = true,  .invert_Y = true  }),
    not_JL_and_K = @bitCast(u4, Logic_Mode_Bits { .xor = false, .invert_JL = true,  .invert_K = false, .invert_Y = false }),
    JL_or_not_K  = @bitCast(u4, Logic_Mode_Bits { .xor = false, .invert_JL = true,  .invert_K = false, .invert_Y = true  }),
    JL_nor_K     = @bitCast(u4, Logic_Mode_Bits { .xor = false, .invert_JL = true,  .invert_K = true,  .invert_Y = false }),
    JL_or_K      = @bitCast(u4, Logic_Mode_Bits { .xor = false, .invert_JL = true,  .invert_K = true,  .invert_Y = true  }),
    JL_xor_K     = @bitCast(u4, Logic_Mode_Bits { .xor = true,  .invert_JL = false, .invert_K = false, .invert_Y = false }),
    JL_xnor_K    = @bitCast(u4, Logic_Mode_Bits { .xor = true,  .invert_JL = false, .invert_K = false, .invert_Y = true  }),
};
pub const Logic_Mode_Bits = packed struct {
    xor: bool, // alternative is AND
    invert_JL: bool,
    invert_K: bool,
    invert_Y: bool,
};

pub const Bitcount_Mode = enum(u4) {
    cb_JL  = @bitCast(u4, Bitcount_Mode_Bits { .priority = false, .invert_JL = false, .reverse = false }), // count set bits
    cz_JL  = @bitCast(u4, Bitcount_Mode_Bits { .priority = false, .invert_JL = true,  .reverse = false }), // count zero bits
    clb_JL = @bitCast(u4, Bitcount_Mode_Bits { .priority = true,  .invert_JL = false, .reverse = true  }), // count leading (msb) set bits
    ctb_JL = @bitCast(u4, Bitcount_Mode_Bits { .priority = true,  .invert_JL = false, .reverse = false }), // count trailing (lsb) set bits
    clz_JL = @bitCast(u4, Bitcount_Mode_Bits { .priority = true,  .invert_JL = true,  .reverse = true  }), // count leading (msb) zeroes
    ctz_JL = @bitCast(u4, Bitcount_Mode_Bits { .priority = true,  .invert_JL = true,  .reverse = false }), // count trailing (lsb) zeroes
};
pub const Bitcount_Mode_Bits = packed struct {
    priority: bool,
    invert_JL: bool,
    reverse: bool,
    _unused: u1 = 0,
};

pub const Multiplier_Mode = enum(u4) {
    JL_zx_K_zx = @bitCast(u4, Multiplier_Mode_Bits { .JL = .unsigned, .K = .unsigned, .swap_output = false }),
    JL_sx_K_zx = @bitCast(u4, Multiplier_Mode_Bits { .JL = .signed,   .K = .unsigned, .swap_output = false }),
    JL_zx_K_sx = @bitCast(u4, Multiplier_Mode_Bits { .JL = .unsigned, .K = .signed,   .swap_output = false }),
    JL_sx_K_sx = @bitCast(u4, Multiplier_Mode_Bits { .JL = .signed,   .K = .signed,   .swap_output = false }),

    JL_zx_K_zx_swap = @bitCast(u4, Multiplier_Mode_Bits { .JL = .unsigned, .K = .unsigned, .swap_output = true }),
    JL_sx_K_zx_swap = @bitCast(u4, Multiplier_Mode_Bits { .JL = .signed,   .K = .unsigned, .swap_output = true }),
    JL_zx_K_sx_swap = @bitCast(u4, Multiplier_Mode_Bits { .JL = .unsigned, .K = .signed,   .swap_output = true }),
    JL_sx_K_sx_swap = @bitCast(u4, Multiplier_Mode_Bits { .JL = .signed,   .K = .signed,   .swap_output = true }),
};
pub const Multiplier_Data_Type = enum(u1) {
    unsigned = 0,
    signed = 1,
};
pub const Multiplier_Mode_Bits = packed struct {
    JL: Multiplier_Data_Type,
    K: Multiplier_Data_Type,
    swap_output: bool,
    _unused: u1 = 0,
};

pub const Shift_Mode = enum(u4) {
    JL_shr_K4 = @bitCast(u4, Shift_Mode_Bits { .left = false, .early_swap16 = false, .late_swap16 = false, .wide = false }),
    JL_shl_K4 = @bitCast(u4, Shift_Mode_Bits { .left = true,  .early_swap16 = false, .late_swap16 = false, .wide = false }),
    JH_shr_K4 = @bitCast(u4, Shift_Mode_Bits { .left = false, .early_swap16 = true,  .late_swap16 = false, .wide = false }),
    JH_shl_K4 = @bitCast(u4, Shift_Mode_Bits { .left = true,  .early_swap16 = true,  .late_swap16 = false, .wide = false }),
    J_shr_K5  = @bitCast(u4, Shift_Mode_Bits { .left = false, .early_swap16 = false, .late_swap16 = false, .wide = true }),
    J_shl_K5  = @bitCast(u4, Shift_Mode_Bits { .left = true,  .early_swap16 = true,  .late_swap16 = true,  .wide = true }),
};
pub const Shift_Mode_Bits = packed struct {
    left: bool,
    early_swap16: bool,
    late_swap16: bool,
    wide: bool,
};

pub const STAT_Op = enum(u4) {
    hold = 0,
    ZN_from_L = 1,
    ZN_from_L_C_from_shift = 2,
    ZN_from_L_no_set_Z = 3,
    ZN_from_address_translator = 4,
    ZN_from_LL = 5,
    ZN_from_LL_C_from_shift = 6,
    ZN_from_LL_no_set_Z = 7,
    ZNVC_from_arith = 8,
    ZNVC_from_arith_no_set_Z = 9,
    load_ZNVC_from_LL = 10,
    load_ZNVCKA_from_LL = 11,
    clear_A = 12,
    set_A = 13,
    clear_S = 14,
    set_S = 15,
};

pub const Sequencer_Op = enum(u2) {
    next_uop = 0,
    next_instruction = 1,
    next_uop_force_normal = 2,
    fault_return = 3,
};

pub const Special_Op = enum(u3) {
    none = 0,
    atomic_this = 1, // This cycle is atomic, but the next one is not, unless it is also .atomic_this
    atomic_next = 2, // All upcoming cycles are atomic, until the next cycle that's .atomic_this or .atomic_end
    atomic_end = 3, // The next cycle is not atomic, unless it's .atomic_this.  If this cycle wasn't atomic to begin with, this has no effect.
    block_transfer = 4, // transfers 8 bytes from FLASH or PSRAM into RAM, or vice versa, depending on bus_ctrl.  D_Bus is not used by this.
    load_RSN_from_LL = 5,
    toggle_RSN = 6,
    trigger_fault = 7,
};
