const std = @import("std");
const bits = @import("bits");
const ctrl = @import("control_signals");
const misc = @import("misc");
const uc_layout = @import("microcode_layout");
const ib = @import("instruction_builder.zig");
const panic = ib.panic;

var cycle = ctrl.Control_Signals.init();
var assigned_signals: std.EnumSet(ctrl.Control_Signal) = .{};

pub const ALU_Flag_Mode = enum {
    no_flags,
    flags,
};

pub const ALU_Freshness = enum {
    fresh,
    cont,
};

pub const ZX_or_SX = enum {
    zx,
    sx,
};

pub const ZX_SX_or_1X = enum {
    zx,
    sx,
    _1x,
};

pub const OA_or_OB = enum {
    OA,
    OB,
};

pub const OA_or_OB_xor = enum {
    OA,
    OB,
    OAxor1,
    OBxor1,
};

pub const Shift_Dir = enum {
    left,
    right,
};

pub const Swap_Halves = enum {
    normal,
    swap,
};

pub const Bitcount_Mode = enum {
    all,
    leading,
    trailing,
};

pub const Set_Clear_Hold = enum {
    set,
    clear,
    hold,
};

pub fn start() void {
    cycle = ctrl.Control_Signals.init();
    assigned_signals = .{};
}

pub fn finish() ctrl.Control_Signals {
    switch (cycle.AT_OP) {
        .hold => {},
        .translate => {
            validate_address();
            ensure_set(.BUS_BYTE);
        },
        .update, .invalidate => {
            validate_address();
        },
    }

    if (cycle.DL_OP == .from_D and cycle.BUS_RW == .read and cycle.AT_OP != .translate) {
        panic("Expected AT_OP to be .translate", .{});
    }

    if (cycle.DL_OP == .to_D and cycle.BUS_RW != .write and cycle.LL_SRC != .D16 and cycle.LL_SRC != .D8_sx) {
        panic("Expected DL value to be written to the bus or LL", .{});
    }

    switch (cycle.LL_SRC) {
        .zero, .STAT, .AT_ME_L, .AT_OE_L, .pipe_ID, .last_mmu_op_L => {},
        .logic    => validate_ALU_MODE(.logic),
        .shift_L  => validate_ALU_MODE(.shift),
        .arith_L  => validate_ALU_MODE(.arith),
        .mult_L   => validate_ALU_MODE(.mult),
        .bitcount => validate_ALU_MODE(.bitcount),
        .D16      => if (cycle.DL_OP != .to_D) validate_bus_read(null),
        .D8_sx    => if (cycle.DL_OP != .to_D) validate_bus_read(.byte),
    }

    switch (cycle.LH_SRC) {
        .zero, .sx, .JH, .prev_UA, .AT_ME_H, .AT_OE_H, .last_mmu_op_H => {},
        .logic => validate_ALU_MODE(.logic),
        .shift_H => validate_ALU_MODE(.shift),
        .arith_H => validate_ALU_MODE(.arith),
        .mult_H => validate_ALU_MODE(.mult),
        .D16 => if (cycle.DL_OP != .to_D) validate_bus_read(null),
    }

    switch (cycle.STAT_OP) {
        .hold,
        .ZN_from_L,
        .ZN_from_L_no_set_Z,
        .ZN_from_LL,
        .ZN_from_LL_no_set_Z,
        .load_ZNVC_from_LL,
        .load_ZNVCKA_from_LL,
        .clear_A,
        .set_A,
        .clear_S,
        .set_S,
        => {},

        .ZN_from_address_translator => {
            if (cycle.AT_OP == .hold) {
                panic("Expected AT_OP to be set", .{});
            }
        },

        .ZN_from_L_C_from_shift, .ZN_from_LL_C_from_shift => validate_ALU_MODE(.shift),

        .ZNVC_from_arith, .ZNVC_from_arith_no_set_Z => validate_ALU_MODE(.arith),
    }

    if ((cycle.AT_OP == .hold or cycle.BUS_RW == .read)
            and cycle.JKR_WMODE == .no_write
            and cycle.SR1_WSRC == .no_write
            and cycle.SR2_WSRC == .no_write
            and cycle.SPECIAL == .none
            and cycle.STAT_OP == .hold
            and cycle.OB_OA_OP == .hold
            and cycle.DL_OP == .hold
            and cycle.SEQ_OP == .next_uop) {
        panic("Did you forget to store the result somewhere?", .{});
    }

    if (cycle.SEQ_OP == .next_instruction and !cycle.ALLOW_INT) {
        panic("Expected ALLOW_INT when SEQ_OP is .next_instruction", .{});
    }

    switch (cycle.SEQ_OP) {
        .next_uop, .next_uop_force_normal => {},
        else => if (is_set(.NEXT_UOP)) {
            panic("Expected SEQ_OP to be .next_uop", .{});
        },
    }

    return cycle;
}

fn is_set(signal: ctrl.Control_Signal) bool {
    return assigned_signals.contains(signal);
}

fn ensure_set(signal: ctrl.Control_Signal) void {
    if (!is_set(signal)) {
        panic("Expected {s} to be assigned", .{ @tagName(signal) });
    }
}

fn validate_ALU_MODE(expected: ctrl.ALU_Mode_Type) void {
    const mode = cycle.ALU_MODE; // workaround for https://github.com/ziglang/zig/issues/14641
    if (mode != expected) {
        panic("Expected ALU_MODE to be {s}", .{ @tagName(expected) });
    }

    if (!is_set(.JL_SRC)) {
        panic("Expected JL_SRC to be set for ALU operation", .{});
    } else switch (cycle.JL_SRC) {
        .zero => {},
        .JRL => {
            ensure_set(.JR_RSEL);
            ensure_set(.JR_RX);
        },
        .SR1L => {
            ensure_set(.SR1_RI);
        },
        .SR2L => {
            ensure_set(.SR2_RI);
        },
    }

    if (!is_set(.JH_SRC)) {
        panic("Expected JH_SRC to be set for ALU operation", .{});
    } else switch (cycle.JH_SRC) {
        .zero, .neg_one, .sx => {},
        .JRL, .JRH => {
            ensure_set(.JR_RSEL);
            ensure_set(.JR_RX);
        },
        .SR1H => {
            ensure_set(.SR1_RI);
        },
        .SR2H => {
            ensure_set(.SR2_RI);
        },
    }

    if (!is_set(.K_SRC)) {
        panic("Expected K_SRC to be set for ALU operation", .{});
    } else switch (cycle.K_SRC) {
        .zero, .OB_OA_zx => {},
        .LITERAL, .LITERAL_minus_64, .LITERAL_special => ensure_set(.LITERAL),
        .KR => {
            ensure_set(.KR_RSEL);
            ensure_set(.KR_RX);
        },
        .SR1L => {
            ensure_set(.SR1_RI);
        },
        .SR2L => {
            ensure_set(.SR2_RI);
        },
    }
}

fn validate_address() void {
    ensure_set(.BUS_RW);
    ensure_set(.BUS_MODE);
    ensure_set(.BASE);
    ensure_set(.OFFSET);
    ensure_set(.AT_OP);
}

fn validate_bus_read(width: ?ctrl.Bus_Width) void {
    validate_address();
    if (cycle.BUS_RW != .read) {
        panic("Expected BUS_RW to be .read", .{});
    }

    if (cycle.AT_OP != .translate) {
        panic("Expected AT_OP to be .translate", .{});
    }

    if (width) |w| {
        if (w != cycle.BUS_BYTE) {
            panic("Expected BUS_BYTE to be {}", .{ w });
        }
    }
}

pub fn LITERAL(value: ctrl.Literal) void {
    if (is_set(.LITERAL) and cycle.LITERAL != value) {
        panic("Can't assign {} to LITERAL; already has value {}", .{ value, cycle.LITERAL });
    }
    cycle.LITERAL = value;
    assigned_signals.insert(.LITERAL);
}
pub fn JR_RSEL(value: ctrl.Reg_File_Indexing_Source) void {
    if (is_set(.JR_RSEL) and cycle.JR_RSEL != value) {
        panic("Can't assign {} to JR_RSEL; already has value {}", .{ value, cycle.JR_RSEL });
    }
    cycle.JR_RSEL = value;
    assigned_signals.insert(.JR_RSEL);
}

pub fn KR_RSEL(value: ctrl.Reg_File_Indexing_Source) void {
    if (is_set(.KR_RSEL) and cycle.KR_RSEL != value) {
        panic("Can't assign {} to KR_RSEL; already has value {}", .{ value, cycle.KR_RSEL });
    }
    cycle.KR_RSEL = value;
    assigned_signals.insert(.KR_RSEL);
}

pub fn JR_RX(value: bool) void {
    if (is_set(.JR_RX) and cycle.JR_RX != value) {
        panic("Can't assign {} to JR_RX; already has value {}", .{ value, cycle.JR_RX });
    }
    cycle.JR_RX = value;
    assigned_signals.insert(.JR_RX);
}

pub fn KR_RX(value: bool) void {
    if (is_set(.KR_RX) and cycle.KR_RX != value) {
        panic("Can't assign {} to KR_RX; already has value {}", .{ value, cycle.KR_RX });
    }
    cycle.KR_RX = value;
    assigned_signals.insert(.KR_RX);
}

pub fn JL_SRC(value: ctrl.JL_Source) void {
    if (is_set(.JL_SRC) and cycle.JL_SRC != value) {
        panic("Can't assign {} to JL_SRC; already has value {}", .{ value, cycle.JL_SRC });
    }
    cycle.JL_SRC = value;
    assigned_signals.insert(.JL_SRC);
}

pub fn JH_SRC(value: ctrl.JH_Source) void {
    if (is_set(.JH_SRC) and cycle.JH_SRC != value) {
        panic("Can't assign {} to JH_SRC; already has value {}", .{ value, cycle.JH_SRC });
    }
    cycle.JH_SRC = value;
    assigned_signals.insert(.JH_SRC);
}

pub fn K_SRC(value: ctrl.K_Source) void {
    if (is_set(.K_SRC) and cycle.K_SRC != value) {
        panic("Can't assign {} to K_SRC; already has value {}", .{ value, cycle.K_SRC });
    }
    cycle.K_SRC = value;
    assigned_signals.insert(.K_SRC);
}

pub fn SR1_RI(value: ctrl.SR1Index) void {
    if (is_set(.SR1_RI) and cycle.SR1_RI != value) {
        panic("Can't assign {} to SR1_RI; already has value {}", .{ value, cycle.SR1_RI });
    }
    cycle.SR1_RI = value;
    assigned_signals.insert(.SR1_RI);
}

pub fn SR2_RI(value: ctrl.SR2Index) void {
    if (is_set(.SR2_RI) and cycle.SR2_RI != value) {
        panic("Can't assign {} to SR2_RI; already has value {}", .{ value, cycle.SR2_RI });
    }
    cycle.SR2_RI = value;
    assigned_signals.insert(.SR2_RI);
}

pub fn BASE(value: ctrl.AnySRIndex) void {
    if (is_set(.BASE) and cycle.BASE != value) {
        panic("Can't assign {} to BASE; already has value {}", .{ value, cycle.BASE });
    }
    cycle.BASE = value;
    assigned_signals.insert(.BASE);
}

pub fn OFFSET(value: ctrl.Address_Offset) void {
    if (is_set(.OFFSET) and cycle.OFFSET != value) {
        panic("Can't assign {} to OFFSET; already has value {}", .{ value, cycle.OFFSET });
    }
    cycle.OFFSET = value;
    assigned_signals.insert(.OFFSET);
}

pub fn ALU_MODE(value: ctrl.ALU_Mode) void {
    if (value == .unused) {
        panic("ALU_MODE can't be manually set to .unused", .{});
    }
    if (is_set(.ALU_MODE) and !std.meta.eql(cycle.ALU_MODE, value)) {
        panic("Can't assign {} to ALU_MODE; already has value {}", .{ value, cycle.ALU_MODE });
    }
    cycle.ALU_MODE = value;
    assigned_signals.insert(.ALU_MODE);
}

pub fn BUS_MODE(value: ctrl.Bus_Mode) void {
    if (is_set(.BUS_MODE) and cycle.BUS_MODE != value) {
        panic("Can't assign {} to BUS_MODE; already has value {}", .{ value, cycle.BUS_MODE });
    }
    cycle.BUS_MODE = value;
    assigned_signals.insert(.BUS_MODE);
}

pub fn BUS_BYTE(value: ctrl.Bus_Width) void {
    if (is_set(.BUS_BYTE) and cycle.BUS_BYTE != value) {
        panic("Can't assign {} to BUS_BYTE; already has value {}", .{ value, cycle.BUS_BYTE });
    }
    cycle.BUS_BYTE = value;
    assigned_signals.insert(.BUS_BYTE);
}

pub fn BUS_RW(value: ctrl.Bus_Direction) void {
    if (is_set(.BUS_RW) and cycle.BUS_RW != value) {
        panic("Can't assign {} to BUS_RW; already has value {}", .{ value, cycle.BUS_RW });
    }
    cycle.BUS_RW = value;
    assigned_signals.insert(.BUS_RW);
}

pub fn AT_OP(value: ctrl.AT_Op) void {
    if (is_set(.AT_OP) and cycle.AT_OP != value) {
        panic("Can't assign {} to AT_OP; already has value {}", .{ value, cycle.AT_OP });
    }
    cycle.AT_OP = value;
    assigned_signals.insert(.AT_OP);
}

pub fn SPECIAL(value: ctrl.Special_Op) void {
    if (is_set(.SPECIAL) and cycle.SPECIAL != value) {
        panic("Can't assign {} to SPECIAL; already has value {}", .{ value, cycle.SPECIAL });
    }
    cycle.SPECIAL = value;
    assigned_signals.insert(.SPECIAL);
}

pub fn LL_SRC(value: ctrl.LL_Source) void {
    if (is_set(.LL_SRC) and cycle.LL_SRC != value) {
        panic("Can't assign {} to LL_SRC; already has value {}", .{ value, cycle.LL_SRC });
    }
    cycle.LL_SRC = value;
    assigned_signals.insert(.LL_SRC);
}

pub fn LH_SRC(value: ctrl.LH_Source) void {
    if (is_set(.LH_SRC) and cycle.LH_SRC != value) {
        panic("Can't assign {} to LH_SRC; already has value {}", .{ value, cycle.LH_SRC });
    }
    cycle.LH_SRC = value;
    assigned_signals.insert(.LH_SRC);
}

pub fn JKR_WSEL(value: ctrl.Reg_File_Indexing_Source) void {
    if (is_set(.JKR_WSEL) and cycle.JKR_WSEL != value) {
        panic("Can't assign {} to JKR_WSEL; already has value {}", .{ value, cycle.JKR_WSEL });
    }
    cycle.JKR_WSEL = value;
    assigned_signals.insert(.JKR_WSEL);
}

pub fn JKR_WMODE(value: ctrl.Reg_File_Write_Mode) void {
    if (is_set(.JKR_WMODE) and cycle.JKR_WMODE != value) {
        panic("Can't assign {} to JKR_WMODE; already has value {}", .{ value, cycle.JKR_WMODE });
    }
    cycle.JKR_WMODE = value;
    assigned_signals.insert(.JKR_WMODE);
}

pub fn SR1_WI(value: ctrl.SR1Index) void {
    if (is_set(.SR1_WI) and cycle.SR1_WI != value) {
        panic("Can't assign {} to SR1_WI; already has value {}", .{ value, cycle.SR1_WI });
    }
    cycle.SR1_WI = value;
    assigned_signals.insert(.SR1_WI);
}

fn _SR2_WI(value: ctrl.SR2Index) void {
    if (is_set(.SR2_WI) and cycle.SR2_WI != value) {
        panic("Can't assign {} to SR2_WI; already has value {}", .{ value, cycle.SR2_WI });
    }
    cycle.SR2_WI = value;
    assigned_signals.insert(.SR2_WI);
}

pub fn SR2_WI(value: ctrl.SR2Index) void {
    _SR2_WI(value);
    if (value == .ip) {
        if (ib.insn) |i| {
            i.setNextInsnOffset(-1);
        }
    }
}

pub fn SR1_WSRC(value: ctrl.SR1_Write_Data_Source) void {
    if (is_set(.SR1_WSRC) and cycle.SR1_WSRC != value) {
        panic("Can't assign {} to SR1_WSRC; already has value {}", .{ value, cycle.SR1_WSRC });
    }
    cycle.SR1_WSRC = value;
    assigned_signals.insert(.SR1_WSRC);
}

pub fn SR2_WSRC(value: ctrl.SR2_Write_Data_Source) void {
    if (is_set(.SR2_WSRC) and cycle.SR2_WSRC != value) {
        panic("Can't assign {} to SR2_WSRC; already has value {}", .{ value, cycle.SR2_WSRC });
    }
    cycle.SR2_WSRC = value;
    assigned_signals.insert(.SR2_WSRC);
}

pub fn STAT_OP(value: ctrl.STAT_Op) void {
    if (is_set(.STAT_OP) and cycle.STAT_OP != value) {
        panic("Can't assign {} to STAT_OP; already has value {}", .{ value, cycle.STAT_OP });
    }
    cycle.STAT_OP = value;
    assigned_signals.insert(.STAT_OP);
}

pub fn DL_OP(value: ctrl.Data_Latch_Op) void {
    if (is_set(.DL_OP) and cycle.DL_OP != value) {
        panic("Can't assign {} to DL_OP; already has value {}", .{ value, cycle.DL_OP });
    }
    if (value == .from_D) {
        if (ib.insn) |i| {
            i.DL_state = .loaded;
        }
    }
    cycle.DL_OP = value;
    assigned_signals.insert(.DL_OP);
}

pub fn OB_OA_OP(value: ctrl.Operand_Reg_Op) void {
    if (is_set(.OB_OA_OP) and cycle.OB_OA_OP != value) {
        panic("Can't assign {} to OB_OA_OP; already has value {}", .{ value, cycle.OB_OA_OP });
    }
    if (ib.insn) |i| {
        switch (value) {
            .hold => {},
            .from_DL => {
                i.OA_state = .loaded;
                i.OB_state = .loaded;
            },
            .increment_OB, .clear_OB => {
                i.OB_state = .loaded;
            },
        }
    }
    cycle.OB_OA_OP = value;
    assigned_signals.insert(.OB_OA_OP);
}

pub fn ALLOW_INT(value: bool) void {
    if (is_set(.ALLOW_INT) and cycle.ALLOW_INT != value) {
        panic("Can't assign {} to ALLOW_INT; already has value {}", .{ value, cycle.ALLOW_INT });
    }
    cycle.ALLOW_INT = value;
    assigned_signals.insert(.ALLOW_INT);
}

pub fn SEQ_OP(value: ctrl.Sequencer_Op) void {
    if (is_set(.SEQ_OP) and cycle.SEQ_OP != value) {
        panic("Can't assign {} to SEQ_OP; already has value {}", .{ value, cycle.SEQ_OP });
    }
    cycle.SEQ_OP = value;
    assigned_signals.insert(.SEQ_OP);
}

pub fn NEXT_UOP(value: uc_layout.UC_Continuation) void {
    if (is_set(.NEXT_UOP) and cycle.NEXT_UOP != value) {
        panic("Can't assign {} to NEXT_UOP; already has value {}", .{ value, cycle.NEXT_UOP });
    }
    cycle.NEXT_UOP = value;
    assigned_signals.insert(.NEXT_UOP);
}

pub fn address(base: ctrl.AnySRIndex, offset: misc.SignedOffsetForLiteral) void {
    BASE(base);

    if (offset == 0) {
        OFFSET(.zero);
    } else if (offset == 2) {
        OFFSET(.two);
    } else {
        var raw_offset: ctrl.Literal = undefined;
        if (offset < 0) {
            raw_offset = @intCast(ctrl.Literal, @as(i8, offset) + 64);
            OFFSET(.LITERAL_minus_64);
        } else {
            raw_offset = @intCast(ctrl.Literal, offset);
            OFFSET(.LITERAL);
        }
        LITERAL(raw_offset);
    }
}

//////////////////////////////////////////////////////////////////////////////
// Movement & Constants

pub fn zero_to_J() void {
    JL_SRC(.zero);
    JH_SRC(.zero);
}

pub fn zero_to_K() void {
    K_SRC(.zero);
}

pub fn zero_to_LL() void {
    LL_SRC(.zero);
}
pub fn zero_to_LH() void {
    LH_SRC(.zero);
}
pub fn zero_to_L() void {
    LL_SRC(.zero);
    LH_SRC(.zero);
}

pub fn literal_to_K(literal: i17) void {
    if (literal == 0) {
        K_SRC(.zero);
    } else if (literal >= 0 and literal < 64) {
        K_SRC(.LITERAL);
        LITERAL(@intCast(ctrl.Literal, literal));
    } else if (literal < 0 and literal >= -64) {
        K_SRC(.LITERAL_minus_64);
        LITERAL(@intCast(ctrl.Literal, literal + 64));
    } else {
        const raw = @bitCast(u17, literal);
        const encoded_bits = @truncate(u7, raw >> 6);
        const encoded: u3 = @ctz(encoded_bits);
        const high_bits = @truncate(u3, raw >> 13);

        const special = bits.concat(.{
            @as(u6, 0),
            @truncate(u7, @shlExact(@as(u8, 1), encoded)),
            high_bits,
        });

        if (special != raw) {
            panic("Invalid literal for K: {} (expected {b}, found {b})", .{ literal, raw, special });
        }

        K_SRC(.LITERAL_special);
        LITERAL(bits.concat2(encoded, high_bits));
    }
}

pub fn literal_to_LL(literal: i17) void {
    if (literal == 0) {
        zero_to_LL();
    } else {
        zero_to_J();
        literal_to_K(literal);
        ALU_MODE(.{ .logic = .JL_xor_K });
        LL_SRC(.logic);
    }
}
pub fn literal_to_LH(literal: i17) void {
    if (literal == 0) {
        zero_to_LH();
    } else {
        zero_to_J();
        literal_to_K(literal);
        ALU_MODE(.{ .logic = .JL_xor_K });
        LH_SRC(.logic);
    }
}
pub fn literal_to_L(literal: i17) void {
    if (literal == 0) {
        zero_to_L();
    } else {
        zero_to_J();
        literal_to_K(literal);
        ALU_MODE(.{ .arith = .J_plus_K_sx });
        LL_SRC(.arith_L);
        LH_SRC(.arith_H);
    }
}

pub fn OB_OA_to_K() void {
    K_SRC(.OB_OA_zx);
}

pub fn OB_OA_to_LL() void {
    JL_SRC(.zero);
    K_SRC(.OB_OA_zx);
    ALU_MODE(.{ .logic = .JL_xor_K });
    LL_SRC(.logic);
}

pub fn OB_OA_to_LH() void {
    JL_SRC(.zero);
    K_SRC(.OB_OA_zx);
    ALU_MODE(.{ .logic = .JL_xor_K });
    LH_SRC(.logic);
}

pub fn SR1H_to_JH(index: ctrl.SR1Index) void {
    SR1_RI(index);
    JH_SRC(.SR1H);
}

pub fn SR1_to_J(index: ctrl.SR1Index) void {
    SR1_RI(index);
    JL_SRC(.SR1L);
    JH_SRC(.SR1H);
}
pub fn SR1L_to_K(index: ctrl.SR1Index) void {
    SR1_RI(index);
    K_SRC(.SR1L);
}

pub fn SR1_to_L(index: ctrl.SR1Index) void {
    SR1_to_J(index);
    J_to_L();
}

pub fn SR2_to_J(index: ctrl.SR2Index) void {
    SR2_RI(index);
    JL_SRC(.SR2L);
    JH_SRC(.SR2H);
}

pub fn SR2L_to_K(index: ctrl.SR2Index) void {
    SR2_RI(index);
    K_SRC(.SR2L);
}

pub fn SR2_to_L(index: ctrl.SR2Index) void {
    SR2_to_J(index);
    J_to_L();
}

pub fn SR_to_J(which: ctrl.AnySRIndex) void {
    if (ctrl.addressBaseToSR1(which)) |sr1| {
        SR1_to_J(sr1);
    } else if (ctrl.addressBaseToSR2(which)) |sr2| {
        SR2_to_J(sr2);
    }
}

pub fn SRL_to_K(which: ctrl.AnySRIndex) void {
    if (ctrl.addressBaseToSR1(which)) |sr1| {
        SR1L_to_K(sr1);
    } else if (ctrl.addressBaseToSR2(which)) |sr2| {
        SR2L_to_K(sr2);
    }
}

pub fn SR_to_L(which: ctrl.AnySRIndex) void {
    if (ctrl.addressBaseToSR1(which)) |sr1| {
        SR1_to_L(sr1);
    } else if (ctrl.addressBaseToSR2(which)) |sr2| {
        SR2_to_L(sr2);
    }
}

pub fn SRL_to_LL(which: ctrl.AnySRIndex) void {
    if (ctrl.addressBaseToSR1(which)) |sr1| {
        SR1_to_J(sr1);
    } else if (ctrl.addressBaseToSR2(which)) |sr2| {
        SR2_to_J(sr2);
    }
    JL_to_LL();
}

pub fn SRH_to_LL(which: ctrl.AnySRIndex) void {
    if (ctrl.addressBaseToSR1(which)) |sr1| {
        SR1_to_J(sr1);
    } else if (ctrl.addressBaseToSR2(which)) |sr2| {
        SR2_to_J(sr2);
    }
    JH_to_LL();
}

pub fn reg_to_J(register: misc.RegisterIndex, ext: ZX_SX_or_1X) void {
    if (register == 0) {
        JR_RSEL(.zero);
        JR_RX(false);
    } else if (register == 1) {
        JR_RSEL(.zero);
        JR_RX(true);
    } else if (is_set(.LITERAL) and (cycle.LITERAL ^ 1) == register) {
        JR_RSEL(.LITERAL);
        JR_RX(true);
        LITERAL(register ^ 1);
    } else {
        JR_RSEL(.LITERAL);
        JR_RX(false);
        LITERAL(register);
    }
    JL_SRC(.JRL);
    JH_SRC(switch (ext) {
        ._1x => .neg_one,
        .zx => .zero,
        .sx => .sx,
    });
}

pub fn reg_to_K(register: misc.RegisterIndex) void {
    if (register == 0) {
        KR_RSEL(.zero);
        KR_RX(false);
    } else if (register == 1) {
        KR_RSEL(.zero);
        KR_RX(true);
    } else if (is_set(.LITERAL) and (cycle.LITERAL ^ 1) == register) {
        KR_RSEL(.LITERAL);
        KR_RX(true);
        LITERAL(register ^ 1);
    } else {
        KR_RSEL(.LITERAL);
        KR_RX(false);
        LITERAL(register);
    }
    K_SRC(.KR);
}

pub fn reg_to_L(register: misc.RegisterIndex, ext: ZX_SX_or_1X) void {
    reg_to_J(register, ext);
    J_to_L();
}

pub fn reg_to_LL(register: misc.RegisterIndex) void {
    reg_to_J(register, .zx);
    JL_to_LL();
}

pub fn reg32_to_J(register: misc.RegisterIndex) void {
    if (register == 0) {
        JR_RSEL(.zero);
        JR_RX(false);
    } else if (register == 1) {
        JR_RSEL(.zero);
        JR_RX(true);
    } else if (is_set(.LITERAL) and (cycle.LITERAL ^ 1) == register) {
        JR_RSEL(.LITERAL);
        JR_RX(true);
        LITERAL(register ^ 1);
    } else {
        JR_RSEL(.LITERAL);
        JR_RX(false);
        LITERAL(register);
    }
    JL_SRC(.JRL);
    JH_SRC(.JRH);
}

pub fn reg32_to_L(register: misc.RegisterIndex) void {
    reg32_to_J(register);
    J_to_L();
}

pub fn op_reg_to_J(which: OA_or_OB_xor, ext: ZX_SX_or_1X) void {
    JR_RSEL(switch (which) {
        .OA, .OAxor1 => .OA,
        .OB, .OBxor1 => .OB,
    });
    JR_RX(switch (which) {
        .OA, .OB => false,
        .OAxor1, .OBxor1 => true,
    });
    JL_SRC(.JRL);
    JH_SRC(switch (ext) {
        ._1x => .neg_one,
        .zx => .zero,
        .sx => .sx,
    });
}

pub fn op_reg_to_K(which: OA_or_OB_xor) void {
    KR_RSEL(switch (which) {
        .OA, .OAxor1 => .OA,
        .OB, .OBxor1 => .OB,
    });
    KR_RX(switch (which) {
        .OA, .OB => false,
        .OAxor1, .OBxor1 => true,
    });
    K_SRC(.KR);
}

pub fn op_reg_to_L(which: OA_or_OB_xor, ext: ZX_SX_or_1X) void {
    op_reg_to_J(which, ext);
    J_to_L();
}

pub fn op_reg_to_LL(which: OA_or_OB_xor) void {
    op_reg_to_J(which, .zx);
    JL_to_LL();
}

pub fn op_reg32_to_J(which: OA_or_OB_xor) void {
    JR_RSEL(switch (which) {
        .OA, .OAxor1 => .OA,
        .OB, .OBxor1 => .OB,
    });
    JR_RX(switch (which) {
        .OA, .OB => false,
        .OAxor1, .OBxor1 => true,
    });
    JL_SRC(.JRL);
    JH_SRC(.JRH);
}

pub fn op_reg32_to_L(which: OA_or_OB_xor) void {
    op_reg32_to_J(which);
    J_to_L();
}

pub fn JL_to_L() void {
    zero_to_K();
    ALU_MODE(.{ .logic = .JL_xor_K });
    LL_SRC(.logic);
    LH_SRC(.zero);
}

pub fn JL_to_LL() void {
    zero_to_K();
    ALU_MODE(.{ .logic = .JL_xor_K });
    LL_SRC(.logic);
}

pub fn JL_to_LL_and_LH() void {
    zero_to_K();
    ALU_MODE(.{ .logic = .JL_xor_K });
    LL_SRC(.logic);
    LH_SRC(.logic);
}

pub fn JH_to_LH() void {
    LH_SRC(.JH);
}

pub fn JH_to_LL() void {
    zero_to_K();
    ALU_MODE(.{ .shift = .JH_shr_K4 });
    LL_SRC(.shift_L);
}

pub fn JL_to_LH() void {
    zero_to_K();
    ALU_MODE(.{ .shift = .JH_shr_K4 });
    LH_SRC(.shift_H);
}

pub fn J_to_L() void {
    zero_to_K();
    ALU_MODE(.{ .arith = .J_plus_K_zx });
    LL_SRC(.arith_L);
    LH_SRC(.arith_H);
}

pub fn K_to_L(ext: ZX_SX_or_1X) void {
    zero_to_J();
    ALU_MODE(.{ .arith = switch (ext) {
        .zx => .add_J_K_zx,
        .sx => .add_J_K_sx,
        ._1x => .add_J_K_1x,
    } });
    LL_SRC(.arith_L);
    LH_SRC(.arith_H);
}

pub fn STAT_to_LL() void {
    LL_SRC(.STAT);
}

pub fn STAT_to_L() void {
    LL_SRC(.STAT);
    LH_SRC(.zero);
}

pub fn last_mmu_op_to_L() void {
    LL_SRC(.last_mmu_op_L);
    LH_SRC(.last_mmu_op_H);
}

pub fn PN_to_SR1(index: ctrl.SR1Index) void {
    SR1_WI(index);
    SR1_WSRC(.PN);
}

pub fn PN_to_SR2(index: ctrl.SR2Index) void {
    SR2_WI(index);
    SR2_WSRC(.PN);
}

pub fn PN_to_SR(which: ctrl.AnySRIndex) void {
    if (ctrl.addressBaseToSR1(which)) |sr1| {
        PN_to_SR1(sr1);
    } else if (ctrl.addressBaseToSR2(which)) |sr2| {
        PN_to_SR2(sr2);
    }
}

pub fn SR_to_PN(base: ctrl.AnySRIndex, offset: misc.SignedOffsetForLiteral) void {
    address(base, offset);
}

pub fn read_to_D(base: ctrl.AnySRIndex, offset: misc.SignedOffsetForLiteral, width: ctrl.Bus_Width, mode: ctrl.Bus_Mode) void {
    address(base, offset);
    AT_OP(.translate);
    BUS_MODE(mode);
    BUS_BYTE(width);
    BUS_RW(.read);
    if (base == .ip and mode == .insn) {
        if (ib.insn) |i| {
            i.onIPRelativeAccess(offset, switch (width) {
                .byte => 1,
                .word => 2,
            });
        }
    }
}

pub fn IP_read_to_D(offset: misc.SignedOffsetForLiteral, width: ctrl.Bus_Width) void {
    read_to_D(.ip, offset, width, .insn);
}

pub fn write_from_LL(base: ctrl.AnySRIndex, offset: misc.SignedOffsetForLiteral, width: ctrl.Bus_Width, mode: ctrl.Bus_Mode) void {
    address(base, offset);
    AT_OP(.translate);
    BUS_MODE(mode);
    BUS_BYTE(width);
    BUS_RW(.write);
    DL_OP(.hold);
}

pub fn write_from_DL(base: ctrl.AnySRIndex, offset: misc.SignedOffsetForLiteral, width: ctrl.Bus_Width, mode: ctrl.Bus_Mode) void {
    address(base, offset);
    AT_OP(.translate);
    BUS_MODE(mode);
    BUS_BYTE(width);
    BUS_RW(.write);
    DL_OP(.to_D);
}

pub fn LL_to_D() void {
    AT_OP(.hold);
    BUS_MODE(.data);
    BUS_BYTE(.word);
    BUS_RW(.write);
}

pub fn D_to_L(ext: ZX_SX_or_1X) void {
    switch (ext) {
        .zx => {
            LL_SRC(.D16);
            LH_SRC(.zero);
        },
        .sx => {
            if (!is_set(.BUS_BYTE)) {
                panic("BUS_BYTE not set!", .{});
            }
            LL_SRC(switch (cycle.BUS_BYTE) {
                .byte => .D8_sx,
                .word => .D16,
            });
            LH_SRC(.sx);
        },
        ._1x => {
            if (cycle.BUS_BYTE == .byte) {
                panic("._1x cannot be used when reading a byte value!", .{});
            }
            LL_SRC(.D16);
            zero_to_J();
            zero_to_K();
            ALU_MODE(.{ .logic = .JL_xnor_K });
            LH_SRC(.logic);
        },
    }
}

pub fn D_to_LH() void {
    LH_SRC(.D16);
}

pub fn D_to_LL() void {
    LL_SRC(.D16);
}

pub fn D_to_DL() void {
    DL_OP(.from_D);
}

pub fn D_to_OB_OA() void {
    DL_OP(.from_D);
    OB_OA_OP(.from_DL);
}

pub fn DL_to_LL() void {
    AT_OP(.hold);
    DL_OP(.to_D);
    LL_SRC(.D16);
}

pub fn L_to_SR1(index: ctrl.SR1Index) void {
    SR1_WI(index);
    SR1_WSRC(.L);
}

pub fn L_to_SR2(index: ctrl.SR2Index) void {
    SR2_WI(index);
    SR2_WSRC(.L);
}

pub fn L_to_SR(which: ctrl.AnySRIndex) void {
    if (ctrl.addressBaseToSR1(which)) |sr1| {
        L_to_SR1(sr1);
    } else if (ctrl.addressBaseToSR2(which)) |sr2| {
        L_to_SR2(sr2);
    } else {
        unreachable;
    }
}

// pub fn SR1_to_SR1(src_index: ctrl.SR1Index, dest_index: ctrl.SR1Index) void {
//     SR1_RI(src_index);
//     SR1_WI(dest_index);
//     SR1_WSRC(.SR1);
// }

pub fn SR2_to_SR2(src_index: ctrl.SR2Index, dest_index: ctrl.SR2Index) void {
    SR2_RI(src_index);
    SR2_WI(dest_index);
    SR2_WSRC(.SR2);
}

pub fn LL_to_ZNVC() void {
    STAT_OP(.load_ZNVC_from_LL);
}

pub fn LL_to_STAT() void {
    STAT_OP(.load_ZNVCKA_from_LL);
}

pub fn LL_to_RSN() void {
    SPECIAL(.load_RSN_from_LL);
}

pub fn toggle_RSN() void {
    SPECIAL(.toggle_RSN);
}

pub fn RSN_to_SR1H(index: ctrl.SR1Index) void {
    SR1_RI(index);
    SR1_WI(index);
    SR1_WSRC(.RSN_SR1);
}

pub fn reload_ASN() void {
    SR2_RI(.asn);
    SR2_WI(.asn);
    SR2_WSRC(.SR2);
}

pub fn pipe_id_to_LL() void {
    LL_SRC(.pipe_ID);
}

pub fn pipe_id_to_L() void {
    pipe_id_to_LL();
    zero_to_LH();
}

pub fn LL_to_op_reg(which: OA_or_OB_xor) void {
    JKR_WMODE(switch (which) {
        .OA, .OB => .write_16,
        .OAxor1, .OBxor1 => .write_16_xor1,
    });
    JKR_WSEL(switch (which) {
        .OA, .OAxor1 => .OA,
        .OB, .OBxor1 => .OB,
    });
}

pub fn LL_to_reg(register: misc.RegisterIndex) void {
    if (register == 0) {
        JKR_WSEL(.zero);
        JKR_WMODE(.write_16);
    } else if (register == 1) {
        JKR_WSEL(.zero);
        JKR_WMODE(.write_16_xor1);
    } else if (is_set(.LITERAL) and (cycle.LITERAL ^ 1) == register) {
        JKR_WSEL(.LITERAL);
        JKR_WMODE(.write_16_xor1);
        LITERAL(register ^ 1);
    } else {
        JKR_WSEL(.LITERAL);
        JKR_WMODE(.write_16);
        LITERAL(register);
    }
}

pub fn L_to_op_reg32(which: OA_or_OB) void {
    JKR_WMODE(.write_32);
    JKR_WSEL(switch (which) {
        .OA => .OA,
        .OB => .OB,
    });
}

pub fn L_to_reg32(register: misc.RegisterIndex) void {
    JKR_WMODE(.write_32);
    if (register == 0) {
        JKR_WSEL(.zero);
    } else {
        JKR_WSEL(.LITERAL);
        LITERAL(register);
    }
}

pub fn load_next_insn(ip_offset: misc.SignedOffsetForLiteral) void {
    address(.ip, ip_offset);
    AT_OP(.translate);
    BUS_MODE(.insn);
    BUS_BYTE(.word);
    BUS_RW(.read);
    DL_OP(.from_D);
    SR2_WI(.next_ip);
    SR2_WSRC(.PN);
    assume_next_insn_loaded(ip_offset);
}

pub fn assume_next_insn_loaded(ip_offset: misc.SignedOffsetForLiteral) void {
    if (ib.insn) |i| {
        i.DL_state = .next_insn;
        i.setNextInsnOffset(ip_offset);
    }
}

pub fn exec_next_insn() void {
    if (is_set(.SR2_RI) and cycle.SR2_RI != .next_ip) {
        address(.next_ip, 0);
        SR2_WSRC(.PN);
    } else {
        SR2_RI(.next_ip);
        SR2_WSRC(.SR2);
    }
    _SR2_WI(.ip);
    OB_OA_OP(.from_DL);
    ALLOW_INT(true);
    SEQ_OP(.next_instruction);
    if (is_set(.SPECIAL)) {
        switch (cycle.SPECIAL) {
            .none, .atomic_next, .atomic_this => {},
            else => SPECIAL(.atomic_end),
        }
    } else {
        SPECIAL(.atomic_end);
    }

    if (ib.insn) |i| {
        i.OA_state = i.DL_state;
        i.OB_state = i.DL_state;
        i.setNextInsnExecuted();
    }
}

pub fn branch(base: ctrl.AnySRIndex, offset: misc.SignedOffsetForLiteral) void {
    address(base, offset);
    AT_OP(.translate);
    BUS_MODE(.insn);
    BUS_BYTE(.word);
    BUS_RW(.read);
    DL_OP(.from_D);
    if (base != .ip or offset != 0) {
        SR2_WI(.ip);
        SR2_WSRC(.PN);
    }
    OB_OA_OP(.from_DL);
    ALLOW_INT(true);
    SEQ_OP(.next_instruction);
    if (is_set(.SPECIAL)) {
        switch (cycle.SPECIAL) {
            .none, .atomic_next, .atomic_this => {},
            else => SPECIAL(.atomic_end),
        }
    } else {
        SPECIAL(.atomic_end);
    }
}

pub fn load_and_exec_next_insn(ip_offset: misc.SignedOffsetForLiteral) void {
    branch(.ip, ip_offset);
    if (ib.insn) |i| {
        i.DL_state = .next_insn;
        i.OA_state = .next_insn;
        i.OB_state = .next_insn;
        i.setNextInsnOffset(ip_offset);
        i.setNextInsnExecuted();
    }
}

//////////////////////////////////////////////////////////////////////////////
// Arith

pub fn add_to_LL(freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    ALU_MODE(.{ .arith = switch (freshness) {
        .fresh => .JL_plus_K,
        .cont => .JL_plus_K_plus_C,
    } });
    LL_SRC(.arith_L);
    switch (flags) {
        .no_flags => {},
        .flags => STAT_OP(switch (freshness) {
            .fresh => .ZNVC_from_arith,
            .cont => .ZNVC_from_arith_no_set_Z,
        }),
    }
}

pub fn sub_to_LL(freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    ALU_MODE(.{ .arith = switch (freshness) {
        .fresh => .JL_minus_K,
        .cont => .JL_minus_K_minus_C,
    } });
    LL_SRC(.arith_L);
    switch (flags) {
        .no_flags => {},
        .flags => STAT_OP(switch (freshness) {
            .fresh => .ZNVC_from_arith,
            .cont => .ZNVC_from_arith_no_set_Z,
        }),
    }
}

pub fn add_to_L(ext: ZX_SX_or_1X, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    ALU_MODE(.{ .arith = switch (freshness) {
        .fresh => switch (ext) {
            .zx => ctrl.Arith_Mode.J_plus_K_zx,
            .sx => ctrl.Arith_Mode.J_plus_K_sx,
            ._1x => ctrl.Arith_Mode.J_plus_K_1x,
        },
        .cont => switch (ext) {
            .zx => ctrl.Arith_Mode.J_plus_K_zx_plus_C,
            .sx => ctrl.Arith_Mode.J_plus_K_sx_plus_C,
            ._1x => ctrl.Arith_Mode.J_plus_K_1x_plus_C,
        },
    } });
    LL_SRC(.arith_L);
    LH_SRC(.arith_H);
    switch (flags) {
        .no_flags => {},
        .flags => STAT_OP(switch (freshness) {
            .fresh => .ZNVC_from_arith,
            .cont => .ZNVC_from_arith_no_set_Z,
        }),
    }
}

pub fn sub_to_L(ext: ZX_SX_or_1X, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    ALU_MODE(.{ .arith = switch (freshness) {
        .fresh => switch (ext) {
            .zx => ctrl.Arith_Mode.J_minus_K_zx,
            .sx => ctrl.Arith_Mode.J_minus_K_sx,
            ._1x => ctrl.Arith_Mode.J_minus_K_1x,
        },
        .cont => switch (ext) {
            .zx => ctrl.Arith_Mode.J_minus_K_zx_minus_C,
            .sx => ctrl.Arith_Mode.J_minus_K_sx_minus_C,
            ._1x => ctrl.Arith_Mode.J_minus_K_1x_minus_C,
        },
    } });
    LL_SRC(.arith_L);
    LH_SRC(.arith_H);
    switch (flags) {
        .no_flags => {},
        .flags => STAT_OP(switch (freshness) {
            .fresh => .ZNVC_from_arith,
            .cont => .ZNVC_from_arith_no_set_Z,
        }),
    }
}

pub fn SR_plus_literal_to_L(left: ctrl.AnySRIndex, right: i17, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    SR_to_J(left);
    literal_to_K(right);
    add_to_L(if (right < 0) ._1x else .zx, freshness, flags);
}
pub fn SR_minus_literal_to_L(left: ctrl.AnySRIndex, right: i17, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    SR_to_J(left);
    literal_to_K(right);
    sub_to_L(if (right < 0) ._1x else .zx, freshness, flags);
}

pub fn SR_plus_op_reg_to_L(left: ctrl.AnySRIndex, right: OA_or_OB_xor, ext: ZX_SX_or_1X, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    SR_to_J(left);
    op_reg_to_K(right);
    add_to_L(ext, freshness, flags);
}
pub fn SR_minus_op_reg_to_L(left: ctrl.AnySRIndex, right: OA_or_OB_xor, ext: ZX_SX_or_1X, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    SR_to_J(left);
    op_reg_to_K(right);
    sub_to_L(ext, freshness, flags);
}

pub fn SR_plus_SRL_to_L(left: ctrl.AnySRIndex, right: ctrl.AnySRIndex, ext: ZX_SX_or_1X, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    SR_to_J(left);
    SRL_to_K(right);
    add_to_L(ext, freshness, flags);
}
pub fn SR_minus_SRL_to_L(left: ctrl.AnySRIndex, right: ctrl.AnySRIndex, ext: ZX_SX_or_1X, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    SR_to_J(left);
    SRL_to_K(right);
    sub_to_L(ext, freshness, flags);
}

pub fn op_reg32_plus_SRL_to_L(left: OA_or_OB_xor, right: ctrl.AnySRIndex, ext: ZX_SX_or_1X, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    op_reg32_to_J(left);
    SRL_to_K(right);
    add_to_L(ext, freshness, flags);
}
pub fn op_reg32_minus_SRL_to_L(left: OA_or_OB_xor, right: ctrl.AnySRIndex, ext: ZX_SX_or_1X, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    op_reg32_to_J(left);
    SRL_to_K(right);
    sub_to_L(ext, freshness, flags);
}

pub fn op_reg32_plus_op_reg_to_L(left: OA_or_OB_xor, right: OA_or_OB_xor, ext: ZX_SX_or_1X, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    op_reg32_to_J(left);
    op_reg_to_K(right);
    add_to_L(ext, freshness, flags);
}
pub fn op_reg32_minus_op_reg_to_L(left: OA_or_OB_xor, right: OA_or_OB_xor, ext: ZX_SX_or_1X, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    op_reg32_to_J(left);
    op_reg_to_K(right);
    sub_to_L(ext, freshness, flags);
}

pub fn op_reg32_plus_literal_to_L(left: OA_or_OB_xor, right: i17, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    op_reg32_to_J(left);
    literal_to_K(right);
    add_to_L(if (right < 0) ._1x else .zx, freshness, flags);
}
pub fn op_reg32_minus_literal_to_L(left: OA_or_OB_xor, right: i17, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    op_reg32_to_J(left);
    literal_to_K(right);
    sub_to_L(if (right < 0) ._1x else .zx, freshness, flags);
}

pub fn zero_minus_op_reg_to_LL(right: OA_or_OB_xor, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    zero_to_J();
    op_reg_to_K(right);
    sub_to_LL(freshness, flags);
}

pub fn SRL_minus_op_reg_to_LL(left: ctrl.AnySRIndex, right: OA_or_OB_xor, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    SR_to_J(left);
    op_reg_to_K(right);
    sub_to_LL(freshness, flags);
}

pub fn op_reg_plus_literal_to_LL(left: OA_or_OB_xor, right: i17, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, .zx);
    literal_to_K(right);
    add_to_LL(freshness, flags);
}
pub fn op_reg_minus_literal_to_LL(left: OA_or_OB_xor, right: i17, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, .zx);
    literal_to_K(right);
    sub_to_LL(freshness, flags);
}

pub fn op_reg_plus_SRL_to_LL(left: OA_or_OB_xor, right: ctrl.AnySRIndex, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, .zx);
    SRL_to_K(right);
    add_to_LL(freshness, flags);
}
pub fn op_reg_minus_SRL_to_LL(left: OA_or_OB_xor, right: ctrl.AnySRIndex, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, .zx);
    SRL_to_K(right);
    sub_to_LL(freshness, flags);
}

pub fn op_reg_plus_op_reg_to_LL(left: OA_or_OB_xor, right: OA_or_OB_xor, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, .zx);
    op_reg_to_K(right);
    add_to_LL(freshness, flags);
}
pub fn op_reg_minus_op_reg_to_LL(left: OA_or_OB_xor, right: OA_or_OB_xor, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, .zx);
    op_reg_to_K(right);
    sub_to_LL(freshness, flags);
}

//////////////////////////////////////////////////////////////////////////////
// Logic

pub fn logic_to_LL(mode: ctrl.Logic_Mode, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    ALU_MODE(.{ .logic = mode });
    LL_SRC(.logic);
    switch (flags) {
        .no_flags => {},
        .flags => STAT_OP(switch (freshness) {
            .fresh => .ZN_from_LL,
            .cont => .ZN_from_LL_no_set_Z,
        }),
    }
}

pub fn SRL_logic_literal_to_LL(left: ctrl.AnySRIndex, mode: ctrl.Logic_Mode, right: i17, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    SR_to_J(left);
    literal_to_K(right);
    logic_to_LL(mode, freshness, flags);
}

pub fn op_reg_logic_literal_to_LL(left: OA_or_OB_xor, mode: ctrl.Logic_Mode, right: i17, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, .zx);
    literal_to_K(right);
    logic_to_LL(mode, freshness, flags);
}

pub fn op_reg_logic_op_reg_to_LL(left: OA_or_OB_xor, mode: ctrl.Logic_Mode, right: OA_or_OB_xor, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, .zx);
    op_reg_to_K(right);
    logic_to_LL(mode, freshness, flags);
}

pub fn op_reg_logic_SRL_to_LL(left: OA_or_OB_xor, mode: ctrl.Logic_Mode, right: ctrl.AnySRIndex, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, .zx);
    SRL_to_K(right);
    logic_to_LL(mode, freshness, flags);
}

//////////////////////////////////////////////////////////////////////////////
// Multiplier

pub fn mult_to_L(left_ext: ZX_or_SX, right_ext: ZX_or_SX, flags: ALU_Flag_Mode) void {
    var mode_bits = ctrl.Multiplier_Mode_Bits{
        .JL = switch (left_ext) {
            .zx => .unsigned,
            .sx => .signed,
        },
        .K = switch (right_ext) {
            .zx => .unsigned,
            .sx => .signed,
        },
        .swap_output = false,
    };
    ALU_MODE(.{ .mult = @intToEnum(ctrl.Multiplier_Mode, @bitCast(u4, mode_bits)) });
    LL_SRC(.mult_L);
    LH_SRC(.mult_H);
    switch (flags) {
        .no_flags => {},
        .flags => STAT_OP(.ZN_from_L),
    }
}

pub fn mult_to_LL(left_ext: ZX_or_SX, right_ext: ZX_or_SX, swap: Swap_Halves, flags: ALU_Flag_Mode) void {
    var mode_bits = ctrl.Multiplier_Mode_Bits{
        .JL = switch (left_ext) {
            .zx => .unsigned,
            .sx => .signed,
        },
        .K = switch (right_ext) {
            .zx => .unsigned,
            .sx => .signed,
        },
        .swap_output = switch (swap) {
            .normal => false,
            .swap => true,
        },
    };
    ALU_MODE(.{ .mult = @intToEnum(ctrl.Multiplier_Mode, @bitCast(u4, mode_bits)) });
    LL_SRC(.mult_L);
    switch (flags) {
        .no_flags => {},
        .flags => STAT_OP(.ZN_from_LL),
    }
}

pub fn op_reg_mult_SRL_to_L(left: OA_or_OB_xor, left_ext: ZX_or_SX, right: ctrl.AnySRIndex, right_ext: ZX_or_SX, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, .zx);
    SRL_to_K(right);
    mult_to_L(left_ext, right_ext, flags);
}

pub fn op_reg_mult_op_reg_to_L(left: OA_or_OB_xor, left_ext: ZX_or_SX, right: OA_or_OB_xor, right_ext: ZX_or_SX, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, .zx);
    op_reg_to_K(right);
    mult_to_L(left_ext, right_ext, flags);
}

pub fn op_reg_mult_SRL_to_LL(left: OA_or_OB_xor, left_ext: ZX_or_SX, right: ctrl.AnySRIndex, right_ext: ZX_or_SX, swap: Swap_Halves, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, .zx);
    SRL_to_K(right);
    mult_to_LL(left_ext, right_ext, swap, flags);
}

pub fn op_reg_mult_op_reg_to_LL(left: OA_or_OB_xor, left_ext: ZX_or_SX, right: OA_or_OB_xor, right_ext: ZX_or_SX, swap: Swap_Halves, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, .zx);
    op_reg_to_K(right);
    mult_to_LL(left_ext, right_ext, swap, flags);
}

//////////////////////////////////////////////////////////////////////////////
// Shifter
pub fn shift_to_L(dir: Shift_Dir, flags: ALU_Flag_Mode) void {
    ALU_MODE(.{ .shift = switch (dir) {
        .left => .J_shl_K5,
        .right => .J_shr_K5,
    } });
    LL_SRC(.shift_L);
    LH_SRC(.shift_H);
    switch (flags) {
        .no_flags => {},
        .flags => STAT_OP(.ZN_from_L_C_from_shift),
    }
}

pub fn shift_to_LL(dir: Shift_Dir, flags: ALU_Flag_Mode) void {
    ALU_MODE(.{ .shift = switch (dir) {
        .left => .JL_shl_K4,
        .right => .JL_shr_K4,
    } });
    LL_SRC(.shift_L);
    switch (flags) {
        .no_flags => {},
        .flags => STAT_OP(.ZN_from_LL_C_from_shift),
    }
}

pub fn SR_shift_literal_to_L(left: ctrl.AnySRIndex, dir: Shift_Dir, right: i5, flags: ALU_Flag_Mode) void {
    SR_to_J(left);
    literal_to_K(right);
    shift_to_L(dir, flags);
}

pub fn op_reg32_shift_op_reg_to_L(left: OA_or_OB_xor, dir: Shift_Dir, right: OA_or_OB_xor, flags: ALU_Flag_Mode) void {
    op_reg32_to_J(left);
    op_reg_to_K(right);
    shift_to_L(dir, flags);
}

pub fn op_reg32_shift_literal_to_L(left: OA_or_OB_xor, dir: Shift_Dir, right: u5, flags: ALU_Flag_Mode) void {
    op_reg32_to_J(left);
    literal_to_K(right);
    shift_to_L(dir, flags);
}

pub fn op_reg_shift_op_reg_to_LL(left: OA_or_OB_xor, ext: ZX_SX_or_1X, dir: Shift_Dir, right: OA_or_OB_xor, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, ext);
    op_reg_to_K(right);
    shift_to_LL(dir, flags);
}

pub fn op_reg_shift_literal_to_LL(left: OA_or_OB_xor, ext: ZX_SX_or_1X, dir: Shift_Dir, right: u4, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, ext);
    literal_to_K(right);
    shift_to_LL(dir, flags);
}

pub fn bitcount_op_reg_to_LL(which: OA_or_OB_xor, mode: Bitcount_Mode, polarity: u1, flags: ALU_Flag_Mode) void {
    op_reg_to_J(which, .zx);
    zero_to_K();

    const bitcount_mode = switch (mode) {
        .all      => switch (polarity) { 0 => ctrl.Bitcount_Mode.cz_JL, 1 => .cb_JL, },
        .leading  => switch (polarity) { 0 => ctrl.Bitcount_Mode.clz_JL, 1 => .clb_JL, },
        .trailing => switch (polarity) { 0 => ctrl.Bitcount_Mode.ctz_JL, 1 => .ctb_JL, },
    };

    ALU_MODE(.{ .bitcount = bitcount_mode });
    LL_SRC(.bitcount);
    switch (flags) {
        .no_flags => {},
        .flags => STAT_OP(.ZN_from_LL),
    }
}

pub fn illegal_instruction() void {
    SPECIAL(.trigger_fault);
    SEQ_OP(.next_uop);
    NEXT_UOP(@enumToInt(uc_layout.UC_Vectors.instruction_protection_fault));
}

pub fn invalid_instruction() void {
    SPECIAL(.trigger_fault);
    SEQ_OP(.next_uop);
    NEXT_UOP(@enumToInt(uc_layout.UC_Vectors.invalid_instruction));
}

pub fn block_transfer_to_ram(base: ctrl.AnySRIndex, preincrement: i7, bus_mode: ctrl.Bus_Mode) void {
    address(base, preincrement);
    AT_OP(.translate);
    BUS_MODE(bus_mode);
    BUS_BYTE(.word);
    BUS_RW(.write);
    SPECIAL(.block_transfer);
    PN_to_SR(base);
}

pub fn block_transfer_from_ram(base: ctrl.AnySRIndex, preincrement: i7, bus_mode: ctrl.Bus_Mode) void {
    address(base, preincrement);
    AT_OP(.translate);
    BUS_MODE(bus_mode);
    BUS_BYTE(.word);
    BUS_RW(.read);
    SPECIAL(.block_transfer);
    PN_to_SR(base);
}
