const std = @import("std");
const bits = @import("bits");
const ControlSignals = @import("ControlSignals");
const misc = @import("misc");
const uc = @import("microcode");
const ib = @import("instruction_builder.zig");
const panic = ib.panic;

var cycle = ControlSignals.init();
var assigned_signals: std.EnumSet(ControlSignals.SignalName) = .{};

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
    cycle = ControlSignals.init();
    assigned_signals = .{};
}

pub fn finish() ControlSignals {
    switch (cycle.at_op) {
        .none => {},
        .translate => {
            validateAddress();
            ensureSet(.bus_byte);
        },
        .update, .invalidate => {
            validateAddress();
        },
    }

    if (cycle.dl_op == .from_D and cycle.bus_rw == .read and cycle.at_op != .translate) {
        panic("Expected AT_OP to be .translate", .{});
    }

    if (cycle.dl_op == .to_D and cycle.bus_rw != .write and cycle.ll_src != .d16 and cycle.ll_src != .d8_sx) {
        panic("Expected DL value to be written to the bus or LL", .{});
    }

    switch (cycle.ll_src) {
        .zero, .stat, .pipe, .last_mmu_op_l => {},
        .logic_l    => validate_ALU_MODE(.logic),
        .shift_l  => validate_ALU_MODE(.shift),
        .arith_l  => validate_ALU_MODE(.arith),
        .mult_l   => validate_ALU_MODE(.mult),
        .bitcount => validate_ALU_MODE(.bitcount),
        .d16      => if (cycle.dl_op != .to_D) validate_bus_read(null),
        .d8_sx    => if (cycle.dl_op != .to_D) validate_bus_read(.byte),
    }

    switch (cycle.lh_src) {
        .zero, .sx_ll, .prev_ua, .last_mmu_op_h, .logic_h => {},
        .logic_l => validate_ALU_MODE(.logic),
        .shift_h => validate_ALU_MODE(.shift),
        .arith_h => validate_ALU_MODE(.arith),
        .mult_h => validate_ALU_MODE(.mult),
        .d16 => if (cycle.dl_op != .to_D) validate_bus_read(null),
    }

    switch (cycle.stat_op) {
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

        .ZN_from_L_C_from_shift, .ZN_from_LL_C_from_shift => validate_ALU_MODE(.shift),

        .ZNVC_from_arith, .ZNVC_from_arith_no_set_Z => validate_ALU_MODE(.arith),
    }

    if ((cycle.at_op == .none or cycle.bus_rw == .read)
            and cycle.jkr_wmode == .no_write
            and cycle.sr1_wsrc == .no_write
            and cycle.sr2_wsrc == .no_write
            and cycle.special == .none
            and cycle.stat_op == .hold
            and cycle.ob_oa_op == .hold
            and cycle.dl_op == .hold
            and cycle.seq_op == .next_uop) {
        panic("Did you forget to store the result somewhere?", .{});
    }

    if (cycle.seq_op == .next_instruction and !cycle.allow_int) {
        panic("Expected allow_int when seq_op is .next_instruction", .{});
    }

    switch (cycle.seq_op) {
        .next_uop, .next_uop_force_normal => {},
        else => if (isSet(.next_uop)) {
            panic("Expected seq_op to be .next_uop or .next_uop_force_normal", .{});
        },
    }

    return cycle;
}

fn isSet(signal: ControlSignals.SignalName) bool {
    return assigned_signals.contains(signal);
}

fn ensureSet(signal: ControlSignals.SignalName) void {
    if (!isSet(signal)) {
        panic("Expected {s} to be assigned", .{ @tagName(signal) });
    }
}

fn validate_ALU_MODE(expected: ControlSignals.ALU_Mode_Type) void {
    const mode = cycle.alu_mode; // workaround for https://github.com/ziglang/zig/issues/14641
    if (mode != expected) {
        panic("Expected alu_mode to be {s}", .{ @tagName(expected) });
    }

    if (!isSet(.jl_src)) {
        panic("Expected jl_src to be set for ALU operation", .{});
    } else switch (cycle.jl_src) {
        .zero => {},
        .jrl => {
            ensureSet(.jr_rsel);
            ensureSet(.jr_rx);
        },
        .sr1l => {
            ensureSet(.sr1_ri);
        },
        .sr2l => {
            ensureSet(.sr2_ri);
        },
    }

    if (!isSet(.jh_src)) {
        panic("Expected jh_src to be set for ALU operation", .{});
    } else switch (cycle.jh_src) {
        .zero, .neg_one, .sx_jl => {},
        .jrl, .jrh => {
            ensureSet(.jr_rsel);
            ensureSet(.jr_rx);
        },
        .sr1h => {
            ensureSet(.sr1_ri);
        },
        .sr2h => {
            ensureSet(.sr2_ri);
        },
    }

    if (!isSet(.k_src)) {
        panic("Expected k_src to be set for ALU operation", .{});
    } else switch (cycle.k_src) {
        .zero, .ob_oa_zx => {},
        .literal, .literal_minus_64, .literal_special => ensureSet(.literal),
        .kr => {
            ensureSet(.kr_rsel);
            ensureSet(.kr_rx);
        },
        .sr1l => {
            ensureSet(.sr1_ri);
        },
        .sr2l => {
            ensureSet(.sr2_ri);
        },
    }
}

fn validateAddress() void {
    ensureSet(.bus_rw);
    ensureSet(.bus_mode);
    ensureSet(.base);
    ensureSet(.offset);
    ensureSet(.at_op);
}

fn validate_bus_read(width: ?ControlSignals.BusWidth) void {
    validateAddress();
    if (cycle.bus_rw != .read) {
        panic("Expected bus_rw to be .read", .{});
    }

    if (cycle.at_op != .translate) {
        panic("Expected at_op to be .translate", .{});
    }

    if (width) |w| {
        if (w != cycle.bus_byte) {
            panic("Expected bus_byte to be {}", .{ w });
        }
    }
}

pub fn setControlSignal(comptime signal: ControlSignals.SignalName, raw_value: anytype) void {
    if (ib.insn) |i| {
        if (!i.cycle_started) {
            panic("No more cycles can be defined here!", .{});
        }
    }
    const current_value = @field(cycle, @tagName(signal));
    const T = @TypeOf(@field(cycle, @tagName(signal)));
    const value = @as(T, raw_value);
    switch (@typeInfo(T)) {
        .Union => if (isSet(signal) and !std.meta.eql(current_value, value)) {
            panic("Can't assign {} to {s}; already has value {}", .{ value, @tagName(signal), current_value });
        },
        .Enum => if (isSet(signal) and current_value != value) {
            panic("Can't assign {s} to {s}; already has value {s}", .{ @tagName(value), @tagName(signal), @tagName(current_value) });
        },
        else => if (isSet(signal) and current_value != value) {
            panic("Can't assign {} to {s}; already has value {}", .{ value, @tagName(signal), current_value });
        },
    }
    switch (signal) {
        .alu_mode => if (value == .unused) {
            panic("alu_mode can't be manually set to .unused", .{});
        },
        .dl_op => if (value == .from_D) {
            if (ib.insn) |i| {
                i.DL_state = .loaded;
            }
        },
        .ob_oa_op => if (ib.insn) |i| {
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
        },
        else => {},
    }
    @field(cycle, @tagName(signal)) = value;
    assigned_signals.insert(signal);
}

pub fn setSR2WriteIndex(value: ControlSignals.SR2Index) void {
    setControlSignal(.sr2_wi, value);
    if (value == .ip) {
        if (ib.insn) |i| {
            i.setNextInsnOffset(-1);
        }
    }
}

pub fn address(base: ControlSignals.AnySRIndex, offset: misc.SignedOffsetForLiteral) void {
    setControlSignal(.base, base);

    if (offset == 0) {
        setControlSignal(.offset, .zero);
    } else if (offset == 2) {
        setControlSignal(.offset, .two);
    } else {
        var raw_offset: ControlSignals.Literal = undefined;
        if (offset < 0) {
            raw_offset = @intCast(ControlSignals.Literal, @as(i8, offset) + 64);
            setControlSignal(.offset, .literal_minus_64);
        } else {
            raw_offset = @intCast(ControlSignals.Literal, offset);
            setControlSignal(.offset, .literal);
        }
        setControlSignal(.literal, raw_offset);
    }
}

//////////////////////////////////////////////////////////////////////////////
// Movement & Constants

pub fn zero_to_J() void {
    setControlSignal(.jl_src, .zero);
    setControlSignal(.jh_src, .zero);
}

pub fn zero_to_JL() void {
    setControlSignal(.jl_src, .zero);
}

pub fn neg_one_to_JH() void {
    setControlSignal(.jh_src, .neg_one);
}

pub fn zero_to_K() void {
    setControlSignal(.k_src, .zero);
}

pub fn zero_to_LL() void {
    setControlSignal(.ll_src, .zero);
}
pub fn zero_to_LH() void {
    setControlSignal(.lh_src, .zero);
}
pub fn zero_to_L() void {
    setControlSignal(.ll_src, .zero);
    setControlSignal(.lh_src, .zero);
}

pub fn literal_to_K(literal: i17) void {
    if (literal == 0) {
        setControlSignal(.k_src, .zero);
    } else if (literal >= 0 and literal < 64) {
        setControlSignal(.k_src, .literal);
        setControlSignal(.literal, @intCast(ControlSignals.Literal, literal));
    } else if (literal < 0 and literal >= -64) {
        setControlSignal(.k_src, .literal_minus_64);
        setControlSignal(.literal, @intCast(ControlSignals.Literal, literal + 64));
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

        setControlSignal(.k_src, .literal_special);
        setControlSignal(.literal, bits.concat2(encoded, high_bits));
    }
}

pub fn literal_to_LL(literal: i17) void {
    if (literal == 0) {
        zero_to_LL();
    } else {
        zero_to_J();
        literal_to_K(literal);
        setControlSignal(.alu_mode, .{ .logic = .JL_xor_K });
        setControlSignal(.ll_src, .logic_l);
    }
}
pub fn literal_to_LH(literal: i17) void {
    if (literal == 0) {
        zero_to_LH();
    } else {
        zero_to_J();
        literal_to_K(literal);
        setControlSignal(.alu_mode, .{ .logic = .JL_xor_K });
        setControlSignal(.lh_src, .logic_l);
    }
}
pub fn literal_to_L(literal: i17) void {
    if (literal == 0) {
        zero_to_L();
    } else {
        zero_to_J();
        literal_to_K(literal);
        setControlSignal(.alu_mode, .{ .arith = .J_plus_K_sx });
        setControlSignal(.ll_src, .arith_l);
        setControlSignal(.lh_src, .arith_h);
    }
}

pub fn OB_OA_to_K() void {
    setControlSignal(.k_src, .ob_oa_zx);
}

pub fn OB_OA_to_LL() void {
    setControlSignal(.jl_src, .zero);
    setControlSignal(.k_src, .ob_oa_zx);
    setControlSignal(.alu_mode, .{ .logic = .JL_xor_K });
    setControlSignal(.ll_src, .logic_l);
}

pub fn OB_OA_to_LH() void {
    setControlSignal(.jl_src, .zero);
    setControlSignal(.k_src, .ob_oa_zx);
    setControlSignal(.alu_mode, .{ .logic = .JL_xor_K });
    setControlSignal(.lh_src, .logic_l);
}

pub fn SR1H_to_JH(index: ControlSignals.SR1Index) void {
    setControlSignal(.sr1_ri, index);
    setControlSignal(.jh_src, .sr1h);
}

pub fn SR1_to_J(index: ControlSignals.SR1Index) void {
    setControlSignal(.sr1_ri, index);
    setControlSignal(.jl_src, .sr1l);
    setControlSignal(.jh_src, .sr1h);
}
pub fn SR1L_to_K(index: ControlSignals.SR1Index) void {
    setControlSignal(.sr1_ri, index);
    setControlSignal(.k_src, .sr1l);
}

pub fn SR1_to_L(index: ControlSignals.SR1Index) void {
    SR1_to_J(index);
    J_to_L();
}

pub fn SR2_to_J(index: ControlSignals.SR2Index) void {
    setControlSignal(.sr2_ri, index);
    setControlSignal(.jl_src, .sr2l);
    setControlSignal(.jh_src, .sr2h);
}

pub fn SR2L_to_K(index: ControlSignals.SR2Index) void {
    setControlSignal(.sr2_ri, index);
    setControlSignal(.k_src, .sr2l);
}

pub fn SR2_to_L(index: ControlSignals.SR2Index) void {
    SR2_to_J(index);
    J_to_L();
}

pub fn SR_to_J(which: ControlSignals.AnySRIndex) void {
    if (ControlSignals.addressBaseToSR1(which)) |sr1| {
        SR1_to_J(sr1);
    } else if (ControlSignals.addressBaseToSR2(which)) |sr2| {
        SR2_to_J(sr2);
    }
}

pub fn SRL_to_K(which: ControlSignals.AnySRIndex) void {
    if (ControlSignals.addressBaseToSR1(which)) |sr1| {
        SR1L_to_K(sr1);
    } else if (ControlSignals.addressBaseToSR2(which)) |sr2| {
        SR2L_to_K(sr2);
    }
}

pub fn SR_to_L(which: ControlSignals.AnySRIndex) void {
    if (ControlSignals.addressBaseToSR1(which)) |sr1| {
        SR1_to_L(sr1);
    } else if (ControlSignals.addressBaseToSR2(which)) |sr2| {
        SR2_to_L(sr2);
    }
}

pub fn SRL_to_LL(which: ControlSignals.AnySRIndex) void {
    if (ControlSignals.addressBaseToSR1(which)) |sr1| {
        SR1_to_J(sr1);
    } else if (ControlSignals.addressBaseToSR2(which)) |sr2| {
        SR2_to_J(sr2);
    }
    JL_to_LL();
}

pub fn SRH_to_LL(which: ControlSignals.AnySRIndex) void {
    if (ControlSignals.addressBaseToSR1(which)) |sr1| {
        SR1_to_J(sr1);
    } else if (ControlSignals.addressBaseToSR2(which)) |sr2| {
        SR2_to_J(sr2);
    }
    JH_to_LL();
}

pub fn reg_to_J(register: misc.RegisterIndex, ext: ZX_SX_or_1X) void {
    if (register == 0) {
        setControlSignal(.jr_rsel, .zero);
        setControlSignal(.jr_rx, false);
    } else if (register == 1) {
        setControlSignal(.jr_rsel, .zero);
        setControlSignal(.jr_rx, true);
    } else if (isSet(.literal) and (cycle.literal ^ 1) == register) {
        setControlSignal(.jr_rsel, .literal);
        setControlSignal(.jr_rx, true);
        setControlSignal(.literal, register ^ 1);
    } else {
        setControlSignal(.jr_rsel, .literal);
        setControlSignal(.jr_rx, false);
        setControlSignal(.literal, register);
    }
    setControlSignal(.jl_src, .jrl);
    switch (ext) {
        ._1x => setControlSignal(.jh_src, .neg_one),
        .zx => setControlSignal(.jh_src, .zero),
        .sx => setControlSignal(.jh_src, .sx_jl),
    }
}

pub fn reg_to_K(register: misc.RegisterIndex) void {
    if (register == 0) {
        setControlSignal(.kr_rsel, .zero);
        setControlSignal(.kr_rx, false);
    } else if (register == 1) {
        setControlSignal(.kr_rsel, .zero);
        setControlSignal(.kr_rx, true);
    } else if (isSet(.literal) and (cycle.literal ^ 1) == register) {
        setControlSignal(.kr_rsel, .literal);
        setControlSignal(.kr_rx, true);
        setControlSignal(.literal, register ^ 1);
    } else {
        setControlSignal(.kr_rsel, .literal);
        setControlSignal(.kr_rx, false);
        setControlSignal(.literal, register);
    }
    setControlSignal(.k_src, .kr);
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
        setControlSignal(.jr_rsel, .zero);
        setControlSignal(.jr_rx, false);
    } else if (register == 1) {
        setControlSignal(.jr_rsel, .zero);
        setControlSignal(.jr_rx, true);
    } else if (isSet(.literal) and (cycle.literal ^ 1) == register) {
        setControlSignal(.jr_rsel, .literal);
        setControlSignal(.jr_rx, true);
        setControlSignal(.literal, register ^ 1);
    } else {
        setControlSignal(.jr_rsel, .literal);
        setControlSignal(.jr_rx, false);
        setControlSignal(.literal, register);
    }
    setControlSignal(.jl_src, .jrl);
    setControlSignal(.jh_src, .jrh);
}

pub fn reg32_to_L(register: misc.RegisterIndex) void {
    reg32_to_J(register);
    J_to_L();
}

pub fn op_reg_to_J(which: OA_or_OB_xor, ext: ZX_SX_or_1X) void {
    switch (which) {
        .OA, .OAxor1 => setControlSignal(.jr_rsel, .oa),
        .OB, .OBxor1 => setControlSignal(.jr_rsel, .ob),
    }
    setControlSignal(.jr_rx, switch (which) {
        .OA, .OB => false,
        .OAxor1, .OBxor1 => true,
    });
    setControlSignal(.jl_src, .jrl);
    switch (ext) {
        ._1x => setControlSignal(.jh_src, .neg_one),
        .zx => setControlSignal(.jh_src, .zero),
        .sx => setControlSignal(.jh_src, .sx_jl),
    }
}

pub fn op_reg_to_K(which: OA_or_OB_xor) void {
    switch (which) {
        .OA, .OAxor1 => setControlSignal(.kr_rsel, .oa),
        .OB, .OBxor1 => setControlSignal(.kr_rsel, .ob),
    }
    setControlSignal(.kr_rx, switch (which) {
        .OA, .OB => false,
        .OAxor1, .OBxor1 => true,
    });
    setControlSignal(.k_src, .kr);
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
    switch (which) {
        .OA, .OAxor1 => setControlSignal(.jr_rsel, .oa),
        .OB, .OBxor1 => setControlSignal(.jr_rsel, .ob),
    }
    setControlSignal(.jr_rx, switch (which) {
        .OA, .OB => false,
        .OAxor1, .OBxor1 => true,
    });
    setControlSignal(.jl_src, .jrl);
    setControlSignal(.jh_src, .jrh);
}

pub fn op_reg32_to_L(which: OA_or_OB_xor) void {
    op_reg32_to_J(which);
    J_to_L();
}

pub fn JL_to_L() void {
    zero_to_K();
    setControlSignal(.alu_mode, .{ .logic = .JL_xor_K });
    setControlSignal(.ll_src, .logic_l);
    setControlSignal(.lh_src, .zero);
}

pub fn JL_to_LL() void {
    zero_to_K();
    setControlSignal(.alu_mode, .{ .logic = .JL_xor_K });
    setControlSignal(.ll_src, .logic_l);
}

pub fn JL_to_LL_and_LH() void {
    zero_to_K();
    setControlSignal(.alu_mode, .{ .logic = .JL_xor_K });
    setControlSignal(.ll_src, .logic_l);
    setControlSignal(.lh_src, .logic_l);
}

pub fn JH_to_LH() void {
    setControlSignal(.lh_src, .logic_h);
}

pub fn JH_to_LL() void {
    zero_to_K();
    setControlSignal(.alu_mode, .{ .shift = .JH_shr_K4 });
    setControlSignal(.ll_src, .shift_l);
}

pub fn JL_to_LH() void {
    zero_to_K();
    setControlSignal(.alu_mode, .{ .shift = .JH_shr_K4 });
    setControlSignal(.lh_src, .shift_h);
}

pub fn J_to_L() void {
    zero_to_K();
    setControlSignal(.alu_mode, .{ .arith = .J_plus_K_zx });
    setControlSignal(.ll_src, .arith_l);
    setControlSignal(.lh_src, .arith_h);
}

pub fn K_to_L(ext: ZX_SX_or_1X) void {
    zero_to_J();
    setControlSignal(.alu_mode, .{ .arith = switch (ext) {
        .zx => .add_J_K_zx,
        .sx => .add_J_K_sx,
        ._1x => .add_J_K_1x,
    } });
    setControlSignal(.ll_src, .arith_l);
    setControlSignal(.lh_src, .arith_h);
}

pub fn STAT_to_LL() void {
    setControlSignal(.ll_src, .stat);
}

pub fn STAT_to_L() void {
    setControlSignal(.ll_src, .stat);
    setControlSignal(.lh_src, .zero);
}

pub fn last_mmu_op_to_L() void {
    setControlSignal(.ll_src, .last_mmu_op_l);
    setControlSignal(.lh_src, .last_mmu_op_h);
}

pub fn PN_to_SR1(index: ControlSignals.SR1Index) void {
    setControlSignal(.sr1_wi, index);
    setControlSignal(.sr1_wsrc, .virtual_address);
}

pub fn PN_to_SR2(index: ControlSignals.SR2Index) void {
    setSR2WriteIndex(index);
    setControlSignal(.sr2_wsrc, .virtual_address);
}

pub fn PN_to_SR(which: ControlSignals.AnySRIndex) void {
    if (ControlSignals.addressBaseToSR1(which)) |sr1| {
        PN_to_SR1(sr1);
    } else if (ControlSignals.addressBaseToSR2(which)) |sr2| {
        PN_to_SR2(sr2);
    }
}

pub fn SR_to_PN(base: ControlSignals.AnySRIndex, offset: misc.SignedOffsetForLiteral) void {
    address(base, offset);
}

pub fn read_to_D(base: ControlSignals.AnySRIndex, offset: misc.SignedOffsetForLiteral, width: ControlSignals.BusWidth, mode: ControlSignals.BusMode) void {
    address(base, offset);
    setControlSignal(.at_op, .translate);
    setControlSignal(.bus_mode, mode);
    setControlSignal(.bus_byte, width);
    setControlSignal(.bus_rw, .read);
    if (base == .ip and mode == .insn) {
        if (ib.insn) |i| {
            i.onIPRelativeAccess(offset, switch (width) {
                .byte => 1,
                .word => 2,
            });
        }
    }
}

pub fn IP_read_to_D(offset: misc.SignedOffsetForLiteral, width: ControlSignals.BusWidth) void {
    read_to_D(.ip, offset, width, .insn);
}

pub fn write_from_LL(base: ControlSignals.AnySRIndex, offset: misc.SignedOffsetForLiteral, width: ControlSignals.BusWidth, mode: ControlSignals.BusMode) void {
    address(base, offset);
    setControlSignal(.at_op, .translate);
    setControlSignal(.bus_mode, mode);
    setControlSignal(.bus_byte, width);
    setControlSignal(.bus_rw, .write);
    setControlSignal(.dl_op, .hold);
}

pub fn write_from_DL(base: ControlSignals.AnySRIndex, offset: misc.SignedOffsetForLiteral, width: ControlSignals.BusWidth, mode: ControlSignals.BusMode) void {
    address(base, offset);
    setControlSignal(.at_op, .translate);
    setControlSignal(.bus_mode, mode);
    setControlSignal(.bus_byte, width);
    setControlSignal(.bus_rw, .write);
    setControlSignal(.dl_op, .to_D);
}

pub fn LL_to_D() void {
    setControlSignal(.at_op, .none);
    setControlSignal(.bus_mode, .data);
    setControlSignal(.bus_byte, .word);
    setControlSignal(.bus_rw, .write);
}

pub fn D_to_L(ext: ZX_SX_or_1X) void {
    switch (ext) {
        .zx => {
            setControlSignal(.ll_src, .d16);
            setControlSignal(.lh_src, .zero);
        },
        .sx => {
            if (!isSet(.bus_byte)) {
                panic("bus_byte not set!", .{});
            }
            switch (cycle.bus_byte) {
                .byte => setControlSignal(.ll_src, .d8_sx),
                .word => setControlSignal(.ll_src, .d16),
            }
            setControlSignal(.lh_src, .sx_ll);
        },
        ._1x => {
            if (cycle.bus_byte == .byte) {
                panic("._1x cannot be used when reading a byte value!", .{});
            }
            setControlSignal(.ll_src, .d16);
            zero_to_J();
            zero_to_K();
            setControlSignal(.alu_mode, .{ .logic = .JL_xnor_K });
            setControlSignal(.lh_src, .logic_l);
        },
    }
}

pub fn D_to_LH() void {
    setControlSignal(.lh_src, .d16);
}

pub fn D_to_LL() void {
    setControlSignal(.ll_src, .d16);
}

pub fn D_to_DL() void {
    setControlSignal(.dl_op, .from_D);

    if (ib.insn) |i| {
        i.DL_state = .loaded;
    }
}

pub fn D_to_OB_OA() void {
    D_to_DL();
    decodeOperands();
}

pub fn DL_to_LL() void {
    setControlSignal(.at_op, .none);
    setControlSignal(.dl_op, .to_D);
    setControlSignal(.ll_src, .d16);
}

pub fn L_to_SR1(index: ControlSignals.SR1Index) void {
    setControlSignal(.sr1_wi, index);
    setControlSignal(.sr1_wsrc, .l_bus);
}

pub fn L_to_SR2(index: ControlSignals.SR2Index) void {
    setSR2WriteIndex(index);
    setControlSignal(.sr2_wsrc, .l_bus);
}

pub fn L_to_SR(which: ControlSignals.AnySRIndex) void {
    if (ControlSignals.addressBaseToSR1(which)) |sr1| {
        L_to_SR1(sr1);
    } else if (ControlSignals.addressBaseToSR2(which)) |sr2| {
        L_to_SR2(sr2);
    } else {
        unreachable;
    }
}

// pub fn SR1_to_SR1(src_index: ControlSignals.SR1Index, dest_index: ControlSignals.SR1Index) void {
//     setControlSignal(.sr1_ri, src_index);
//     setControlSignal(.sr1_wi, dest_index);
//     setControlSignal(.sr1_wsrc, .SR1);
// }

pub fn SR2_to_SR2(src_index: ControlSignals.SR2Index, dest_index: ControlSignals.SR2Index) void {
    setControlSignal(.sr2_ri, src_index);
    setSR2WriteIndex(dest_index);
    setControlSignal(.sr2_wsrc, .sr2);
}

pub fn ZN_from_LL(freshness: ALU_Freshness) void {
    switch (freshness) {
        .fresh => setControlSignal(.stat_op, .ZN_from_LL),
        .cont => setControlSignal(.stat_op, .ZN_from_LL_no_set_Z),
    }
}

pub fn LL_to_ZNVC() void {
    setControlSignal(.stat_op, .load_ZNVC_from_LL);
}

pub fn LL_to_STAT() void {
    setControlSignal(.stat_op, .load_ZNVCKA_from_LL);
}

pub fn enableSleep() void {
    setControlSignal(.stat_op, .set_S);
}
pub fn disableSleep() void {
    setControlSignal(.stat_op, .clear_S);
}

pub fn enableAddressTranslation() void {
    setControlSignal(.stat_op, .set_A);
}
pub fn disableAddressTranslation() void {
    setControlSignal(.stat_op, .clear_A);
}

pub fn LL_to_RSN() void {
    setControlSignal(.special, .load_rsn_from_ll);
}

pub fn toggle_rsn() void {
    setControlSignal(.special, .toggle_rsn);
}

pub fn RSN_to_SR1H(index: ControlSignals.SR1Index) void {
    setControlSignal(.sr1_ri, index);
    setControlSignal(.sr1_wi, index);
    setControlSignal(.sr1_wsrc, .rsn_sr1);
}

pub fn reload_ASN() void {
    setControlSignal(.sr2_ri, .asn);
    setSR2WriteIndex(.asn);
    setControlSignal(.sr2_wsrc, .sr2);
}

pub fn pipe_id_to_LL() void {
    setControlSignal(.ll_src, .pipe);
}

pub fn pipe_id_to_L() void {
    pipe_id_to_LL();
    zero_to_LH();
}

pub fn LL_to_op_reg(which: OA_or_OB_xor) void {
    setControlSignal(.jkr_wmode, switch (which) {
        .OA, .OB => ControlSignals.RegFileWriteMode.write_16,
        .OAxor1, .OBxor1 => ControlSignals.RegFileWriteMode.write_16_xor1,
    });
    switch (which) {
        .OA, .OAxor1 => setControlSignal(.jkr_wsel, .oa),
        .OB, .OBxor1 => setControlSignal(.jkr_wsel, .ob),
    }
}

pub fn LL_to_reg(register: misc.RegisterIndex) void {
    if (register == 0) {
        setControlSignal(.jkr_wsel, .zero);
        setControlSignal(.jkr_wmode, .write_16);
    } else if (register == 1) {
        setControlSignal(.jkr_wsel, .zero);
        setControlSignal(.jkr_wmode, .write_16_xor1);
    } else if (isSet(.literal) and (cycle.literal ^ 1) == register) {
        setControlSignal(.jkr_wsel, .literal);
        setControlSignal(.jkr_wmode, .write_16_xor1);
        setControlSignal(.literal, register ^ 1);
    } else {
        setControlSignal(.jkr_wsel, .literal);
        setControlSignal(.jkr_wmode, .write_16);
        setControlSignal(.literal, register);
    }
}

pub fn L_to_op_reg32(which: OA_or_OB) void {
    setControlSignal(.jkr_wmode, .write_32);
    switch (which) {
        .OA => setControlSignal(.jkr_wsel, .oa),
        .OB => setControlSignal(.jkr_wsel, .ob),
    }
}

pub fn L_to_reg32(register: misc.RegisterIndex) void {
    setControlSignal(.jkr_wmode, .write_32);
    if (register == 0) {
        setControlSignal(.jkr_wsel, .zero);
    } else {
        setControlSignal(.jkr_wsel, .literal);
        setControlSignal(.literal, register);
    }
}

pub fn load_next_insn(ip_offset: misc.SignedOffsetForLiteral) void {
    address(.ip, ip_offset);
    setControlSignal(.at_op, .translate);
    setControlSignal(.bus_mode, .insn);
    setControlSignal(.bus_byte, .word);
    setControlSignal(.bus_rw, .read);
    D_to_DL();
    setSR2WriteIndex(.next_ip);
    setControlSignal(.sr2_wsrc, .virtual_address);
    assume_next_insn_loaded(ip_offset);
}

pub fn assume_next_insn_loaded(ip_offset: misc.SignedOffsetForLiteral) void {
    if (ib.insn) |i| {
        i.DL_state = .next_insn;
        i.setNextInsnOffset(ip_offset);
    }
}

pub fn exec_next_insn() void {
    if (isSet(.sr2_ri) and cycle.sr2_ri != .next_ip) {
        address(.next_ip, 0);
        setControlSignal(.sr2_wsrc, .virtual_address);
    } else {
        setControlSignal(.sr2_ri, .next_ip);
        setControlSignal(.sr2_wsrc, .sr2);
    }
    setControlSignal(.sr2_wi, .ip);
    execLatchedInsn();

    if (ib.insn) |i| {
        i.setNextInsnExecuted();
        ib.end_instruction();
    }
}

pub fn branch(base: ControlSignals.AnySRIndex, offset: misc.SignedOffsetForLiteral) void {
    address(base, offset);
    setControlSignal(.at_op, .translate);
    setControlSignal(.bus_mode, .insn);
    setControlSignal(.bus_byte, .word);
    setControlSignal(.bus_rw, .read);
    D_to_DL();
    if (base != .ip or offset != 0) {
        setSR2WriteIndex(.ip);
        setControlSignal(.sr2_wsrc, .virtual_address);
    }
    execLatchedInsn();
}

pub fn execLatchedInsn() void {
    decodeOperands();
    allow_interrupt();
    setControlSignal(.seq_op, .next_instruction);
    if (isSet(.special)) {
        switch (cycle.special) {
            .none, .atomic_next, .atomic_this => {},
            else => setControlSignal(.special, .atomic_end),
        }
    } else {
        setControlSignal(.special, .atomic_end);
    }
    if (ib.insn) |_| {
        ib.end_instruction();
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

pub fn load_and_exec_next_insn_no_atomic_end(ip_offset: misc.SignedOffsetForLiteral) void {
    setControlSignal(.special, .none); // don't clear atomic state
    load_and_exec_next_insn(ip_offset);
}

//////////////////////////////////////////////////////////////////////////////
// Arith

pub fn add_to_LL(freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    setControlSignal(.alu_mode, .{ .arith = switch (freshness) {
        .fresh => ControlSignals.Arith_Mode.JL_plus_K,
        .cont => ControlSignals.Arith_Mode.JL_plus_K_plus_C,
    } });
    setControlSignal(.ll_src, .arith_l);
    switch (flags) {
        .no_flags => {},
        .flags => setControlSignal(.stat_op, switch (freshness) {
            .fresh => ControlSignals.STAT_Op.ZNVC_from_arith,
            .cont => ControlSignals.STAT_Op.ZNVC_from_arith_no_set_Z,
        }),
    }
}

pub fn sub_to_LL(freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    setControlSignal(.alu_mode, .{ .arith = switch (freshness) {
        .fresh => ControlSignals.Arith_Mode.JL_minus_K,
        .cont => ControlSignals.Arith_Mode.JL_minus_K_minus_C,
    } });
    setControlSignal(.ll_src, .arith_l);
    switch (flags) {
        .no_flags => {},
        .flags => setControlSignal(.stat_op, switch (freshness) {
            .fresh => ControlSignals.STAT_Op.ZNVC_from_arith,
            .cont => ControlSignals.STAT_Op.ZNVC_from_arith_no_set_Z,
        }),
    }
}

pub fn add_to_L(ext: ZX_SX_or_1X, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    setControlSignal(.alu_mode, .{ .arith = switch (freshness) {
        .fresh => switch (ext) {
            .zx => ControlSignals.Arith_Mode.J_plus_K_zx,
            .sx => ControlSignals.Arith_Mode.J_plus_K_sx,
            ._1x => ControlSignals.Arith_Mode.J_plus_K_1x,
        },
        .cont => switch (ext) {
            .zx => ControlSignals.Arith_Mode.J_plus_K_zx_plus_C,
            .sx => ControlSignals.Arith_Mode.J_plus_K_sx_plus_C,
            ._1x => ControlSignals.Arith_Mode.J_plus_K_1x_plus_C,
        },
    } });
    setControlSignal(.ll_src, .arith_l);
    setControlSignal(.lh_src, .arith_h);
    switch (flags) {
        .no_flags => {},
        .flags => setControlSignal(.stat_op, switch (freshness) {
            .fresh => ControlSignals.STAT_Op.ZNVC_from_arith,
            .cont => ControlSignals.STAT_Op.ZNVC_from_arith_no_set_Z,
        }),
    }
}

pub fn sub_to_L(ext: ZX_SX_or_1X, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    setControlSignal(.alu_mode, .{ .arith = switch (freshness) {
        .fresh => switch (ext) {
            .zx => ControlSignals.Arith_Mode.J_minus_K_zx,
            .sx => ControlSignals.Arith_Mode.J_minus_K_sx,
            ._1x => ControlSignals.Arith_Mode.J_minus_K_1x,
        },
        .cont => switch (ext) {
            .zx => ControlSignals.Arith_Mode.J_minus_K_zx_minus_C,
            .sx => ControlSignals.Arith_Mode.J_minus_K_sx_minus_C,
            ._1x => ControlSignals.Arith_Mode.J_minus_K_1x_minus_C,
        },
    } });
    setControlSignal(.ll_src, .arith_l);
    setControlSignal(.lh_src, .arith_h);
    switch (flags) {
        .no_flags => {},
        .flags => setControlSignal(.stat_op, switch (freshness) {
            .fresh => ControlSignals.STAT_Op.ZNVC_from_arith,
            .cont => ControlSignals.STAT_Op.ZNVC_from_arith_no_set_Z,
        }),
    }
}

pub fn SR_plus_literal_to_L(left: ControlSignals.AnySRIndex, right: i17, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    SR_to_J(left);
    literal_to_K(right);
    add_to_L(if (right < 0) ._1x else .zx, freshness, flags);
}
pub fn SR_minus_literal_to_L(left: ControlSignals.AnySRIndex, right: i17, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    SR_to_J(left);
    literal_to_K(right);
    sub_to_L(if (right < 0) ._1x else .zx, freshness, flags);
}

pub fn SR_plus_op_reg_to_L(left: ControlSignals.AnySRIndex, right: OA_or_OB_xor, ext: ZX_SX_or_1X, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    SR_to_J(left);
    op_reg_to_K(right);
    add_to_L(ext, freshness, flags);
}
pub fn SR_minus_op_reg_to_L(left: ControlSignals.AnySRIndex, right: OA_or_OB_xor, ext: ZX_SX_or_1X, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    SR_to_J(left);
    op_reg_to_K(right);
    sub_to_L(ext, freshness, flags);
}

pub fn SR_plus_SRL_to_L(left: ControlSignals.AnySRIndex, right: ControlSignals.AnySRIndex, ext: ZX_SX_or_1X, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    SR_to_J(left);
    SRL_to_K(right);
    add_to_L(ext, freshness, flags);
}
pub fn SR_minus_SRL_to_L(left: ControlSignals.AnySRIndex, right: ControlSignals.AnySRIndex, ext: ZX_SX_or_1X, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    SR_to_J(left);
    SRL_to_K(right);
    sub_to_L(ext, freshness, flags);
}

pub fn op_reg32_plus_SRL_to_L(left: OA_or_OB_xor, right: ControlSignals.AnySRIndex, ext: ZX_SX_or_1X, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    op_reg32_to_J(left);
    SRL_to_K(right);
    add_to_L(ext, freshness, flags);
}
pub fn op_reg32_minus_SRL_to_L(left: OA_or_OB_xor, right: ControlSignals.AnySRIndex, ext: ZX_SX_or_1X, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
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

pub fn SRL_minus_op_reg_to_LL(left: ControlSignals.AnySRIndex, right: OA_or_OB_xor, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
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

pub fn op_reg_plus_SRL_to_LL(left: OA_or_OB_xor, right: ControlSignals.AnySRIndex, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, .zx);
    SRL_to_K(right);
    add_to_LL(freshness, flags);
}
pub fn op_reg_minus_SRL_to_LL(left: OA_or_OB_xor, right: ControlSignals.AnySRIndex, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
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

pub fn logic_to_LL(mode: ControlSignals.Logic_Mode, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    setControlSignal(.alu_mode, .{ .logic = mode });
    setControlSignal(.ll_src, .logic_l);
    switch (flags) {
        .no_flags => {},
        .flags => setControlSignal(.stat_op, switch (freshness) {
            .fresh => ControlSignals.STAT_Op.ZN_from_LL,
            .cont => ControlSignals.STAT_Op.ZN_from_LL_no_set_Z,
        }),
    }
}

pub fn SRL_logic_literal_to_LL(left: ControlSignals.AnySRIndex, mode: ControlSignals.Logic_Mode, right: i17, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    SR_to_J(left);
    literal_to_K(right);
    logic_to_LL(mode, freshness, flags);
}

pub fn op_reg_logic_literal_to_LL(left: OA_or_OB_xor, mode: ControlSignals.Logic_Mode, right: i17, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, .zx);
    literal_to_K(right);
    logic_to_LL(mode, freshness, flags);
}

pub fn op_reg_logic_op_reg_to_LL(left: OA_or_OB_xor, mode: ControlSignals.Logic_Mode, right: OA_or_OB_xor, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, .zx);
    op_reg_to_K(right);
    logic_to_LL(mode, freshness, flags);
}

pub fn op_reg_logic_SRL_to_LL(left: OA_or_OB_xor, mode: ControlSignals.Logic_Mode, right: ControlSignals.AnySRIndex, freshness: ALU_Freshness, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, .zx);
    SRL_to_K(right);
    logic_to_LL(mode, freshness, flags);
}

//////////////////////////////////////////////////////////////////////////////
// Multiplier

pub fn mult_to_L(left_ext: ZX_or_SX, right_ext: ZX_or_SX, flags: ALU_Flag_Mode) void {
    var mode_bits = ControlSignals.Multiplier_Mode_Bits{
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
    setControlSignal(.alu_mode, .{ .mult = @intToEnum(ControlSignals.Multiplier_Mode, @bitCast(u4, mode_bits)) });
    setControlSignal(.ll_src, .mult_l);
    setControlSignal(.lh_src, .mult_h);
    switch (flags) {
        .no_flags => {},
        .flags => setControlSignal(.stat_op, .ZN_from_L),
    }
}

pub fn mult_to_LL(left_ext: ZX_or_SX, right_ext: ZX_or_SX, swap: Swap_Halves, flags: ALU_Flag_Mode) void {
    var mode_bits = ControlSignals.Multiplier_Mode_Bits{
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
    setControlSignal(.alu_mode, .{ .mult = @intToEnum(ControlSignals.Multiplier_Mode, @bitCast(u4, mode_bits)) });
    setControlSignal(.ll_src, .mult_l);
    switch (flags) {
        .no_flags => {},
        .flags => setControlSignal(.stat_op, .ZN_from_LL),
    }
}

pub fn op_reg_mult_SRL_to_L(left: OA_or_OB_xor, left_ext: ZX_or_SX, right: ControlSignals.AnySRIndex, right_ext: ZX_or_SX, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, .zx);
    SRL_to_K(right);
    mult_to_L(left_ext, right_ext, flags);
}

pub fn op_reg_mult_op_reg_to_L(left: OA_or_OB_xor, left_ext: ZX_or_SX, right: OA_or_OB_xor, right_ext: ZX_or_SX, flags: ALU_Flag_Mode) void {
    op_reg_to_J(left, .zx);
    op_reg_to_K(right);
    mult_to_L(left_ext, right_ext, flags);
}

pub fn op_reg_mult_SRL_to_LL(left: OA_or_OB_xor, left_ext: ZX_or_SX, right: ControlSignals.AnySRIndex, right_ext: ZX_or_SX, swap: Swap_Halves, flags: ALU_Flag_Mode) void {
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
    setControlSignal(.alu_mode, .{ .shift = switch (dir) {
        .left => ControlSignals.Shift_Mode.J_shl_K5,
        .right => ControlSignals.Shift_Mode.J_shr_K5,
    } });
    setControlSignal(.ll_src, .shift_l);
    setControlSignal(.lh_src, .shift_h);
    switch (flags) {
        .no_flags => {},
        .flags => setControlSignal(.stat_op, .ZN_from_L_C_from_shift),
    }
}

pub fn shift_to_LL(dir: Shift_Dir, flags: ALU_Flag_Mode) void {
    setControlSignal(.alu_mode, .{ .shift = switch (dir) {
        .left => ControlSignals.Shift_Mode.JL_shl_K4,
        .right => ControlSignals.Shift_Mode.JL_shr_K4,
    } });
    setControlSignal(.ll_src, .shift_l);
    switch (flags) {
        .no_flags => {},
        .flags => setControlSignal(.stat_op, .ZN_from_LL_C_from_shift),
    }
}

pub fn SR_shift_literal_to_L(left: ControlSignals.AnySRIndex, dir: Shift_Dir, right: i5, flags: ALU_Flag_Mode) void {
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
        .all      => switch (polarity) { 0 => ControlSignals.Bitcount_Mode.cz_JL, 1 => .cb_JL, },
        .leading  => switch (polarity) { 0 => ControlSignals.Bitcount_Mode.clz_JL, 1 => .clb_JL, },
        .trailing => switch (polarity) { 0 => ControlSignals.Bitcount_Mode.ctz_JL, 1 => .ctb_JL, },
    };

    setControlSignal(.alu_mode, .{ .bitcount = bitcount_mode });
    setControlSignal(.ll_src, .bitcount);
    switch (flags) {
        .no_flags => {},
        .flags => setControlSignal(.stat_op, .ZN_from_LL),
    }
}

pub fn illegal_instruction() void {
    setControlSignal(.special, .trigger_fault);
    setControlSignal(.seq_op, .next_uop);
    setControlSignal(.next_uop, @intCast(u10, @enumToInt(uc.Vectors.instruction_protection_fault)));
}

pub fn invalid_instruction() void {
    setControlSignal(.special, .trigger_fault);
    setControlSignal(.seq_op, .next_uop);
    setControlSignal(.next_uop, @intCast(u10, @enumToInt(uc.Vectors.invalid_instruction)));
}

pub fn block_transfer_to_ram(base: ControlSignals.AnySRIndex, preincrement: i7, bus_mode: ControlSignals.BusMode) void {
    address(base, preincrement);
    setControlSignal(.at_op, .translate);
    setControlSignal(.bus_mode, bus_mode);
    setControlSignal(.bus_byte, .word);
    setControlSignal(.bus_rw, .write);
    setControlSignal(.special, .block_transfer);
    PN_to_SR(base);
}

pub fn block_transfer_from_ram(base: ControlSignals.AnySRIndex, preincrement: i7, bus_mode: ControlSignals.BusMode) void {
    address(base, preincrement);
    setControlSignal(.at_op, .translate);
    setControlSignal(.bus_mode, bus_mode);
    setControlSignal(.bus_byte, .word);
    setControlSignal(.bus_rw, .read);
    setControlSignal(.special, .block_transfer);
    PN_to_SR(base);
}

pub fn atomicThisCycle() void {
    setControlSignal(.special, .atomic_this);
}

pub fn atomicNextCycleUntilEnd() void {
    setControlSignal(.special, .atomic_next);
}

pub fn exec_next_insn_no_atomic_end() void {
    setControlSignal(.special, .none); // don't clear atomic state
    exec_next_insn();
}

pub fn allow_interrupt() void {
    setControlSignal(.allow_int, true);
}

pub fn clear_OB() void {
    setControlSignal(.ob_oa_op, .clear_OB);

    if (ib.insn) |i| {
        i.OB_state = .loaded;
    }
}

pub fn increment_OB() void {
    setControlSignal(.ob_oa_op, .increment_OB);

    if (ib.insn) |i| {
        i.OB_state = .loaded;
    }
}

pub fn decodeOperands() void {
    setControlSignal(.ob_oa_op, .from_DL);

    if (ib.insn) |i| {
        i.OA_state = i.DL_state;
        i.OB_state = i.DL_state;
    }
}

pub fn prev_UA_to_LH() void {
    setControlSignal(.lh_src, .prev_ua);
}

pub fn update_address_translation_from_L(base: ControlSignals.AnySRIndex, mode: ControlSignals.BusMode, dir: ControlSignals.BusDirection) void {
    address(base, 0);
    setControlSignal(.at_op, .update);
    setControlSignal(.bus_mode, mode);
    setControlSignal(.bus_rw, dir);
    setControlSignal(.bus_byte, .word);
}

pub fn invalidate_address_translation_from_L(base: ControlSignals.AnySRIndex, mode: ControlSignals.BusMode, dir: ControlSignals.BusDirection) void {
    address(base, 0);
    setControlSignal(.at_op, .invalidate);
    setControlSignal(.bus_mode, mode);
    setControlSignal(.bus_rw, dir);
    setControlSignal(.bus_byte, .word);
}