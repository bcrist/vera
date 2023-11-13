literal: Literal,
c: Continuation,
id_mode: ID_Mode,
ij_op: Operand_Index_Op,
ik_op: Operand_Index_Op,
iw_op: Operand_Index_Op,
reg_width: Register_Write_Width,
sr1_ri: SR1_Index,
sr2_ri: SR2_Index,
sr1_wi: SR1_Index,
sr2_wi: SR2_Index,
sr1_wsrc: SR1_Write_Source,
sr2_wsrc: SR2_Write_Source,
base_ri: Any_SR_Index,
offset_src: Address_Offset_Source,
jl_src: JL_Source,
jh_src: JH_Source,
k_src: K_Source,
unit: Compute_Unit,
mode: Compute_Mode,
ll_src: LL_Source,
lh_src: LH_Source,
bus_dir: Bus_Direction,
bus_width: Bus_Width,
addr_space: Address_Space,
at_op: Address_Translator_Op,
stat_op: Status_Op,
seq_op: Sequencer_Op,
special: Special_Op,
allow_int: bool,

//pub const zero = std.mem.zeroInit(Control_Signals, .{ .mode = Compute_Mode.init(0) });

pub const Literal = enum (u6) {
    _,
    pub fn init(raw_value: u6) Literal {
        return @enumFromInt(raw_value);
    }
    pub fn raw(self: Literal) u6 {
        return @intFromEnum(self);
    }
};

pub const Continuation = packed struct (u12) {
    ij: hw.IJ,
    ik: hw.IK,
    iw: hw.IW,
    pub fn init(raw_value: u12) Continuation {
        return @bitCast(raw_value);
    }
    pub fn raw(self: Continuation) u12 {
        return @bitCast(self);
    }
    pub fn mask(ij_op: Operand_Index_Op, ik_op: Operand_Index_Op, iw_op: Operand_Index_Op) Continuation {
        return .{
            .ij = hw.IJ.init(if (ij_op == .from_continuation) 0xF else 0),
            .ik = hw.IK.init(if (ik_op == .from_continuation) 0xF else 0),
            .iw = hw.IW.init(if (iw_op == .from_continuation) 0xF else 0),
        };
    }
};

pub const ID_Mode = enum (u1) {
    normal = 0,
    alt = 1,
};

pub const Operand_Index_Op = enum (u2) {
    hold = 0,
    xor1 = 1,
    from_continuation = 2,
    from_decode = 3,
};

pub const Register_Write_Width = enum(u1) {
    write_16 = 0,
    write_32 = 1,
};

pub const SR1_Index = enum(u3) {
    zero = 0,           // 0x0000_0000 (currently not used for anything)
    rp = 1,             // return pointer
    sp = 2,             // stack pointer
    bp = 3,             // stack base pointer
    fault_ua_dr = 4,            // UA (high 16b) and DR (low 16b) copied when entering a fault handler.
    fault_rsn_stat = 5,         // STAT (bits 15:0) copied when entering a fault handler.  previous RSN (bits 21:16) stored when using STRS/LDRS.
    int_rsn_fault_iw_ik_ij = 6, // IW/IK/IJ (bits 11:0) copied when entering a fault handler.  previous RSN (bits 21:16) stored when entering an interrupt handler.
    temp_1 = 7,

    pub fn raw(self: SR1_Index) u3 {
        return @intFromEnum(self);
    }

    pub const count = std.math.maxInt(u3) + 1;
};

pub const SR2_Index = enum(u3) {
    zero = 0,           // 0x0000_0000 (must never be overwritten)
    ip = 1,             // instruction pointer
    asn = 2,            // address space number
    next_ip = 3,        // when next instruction is loaded before the last cycle of the current instruction, the next IP is kept here.
    kxp = 4,            // kernel context pointer
    uxp = 5,            // user context pointer
    rs_reserved = 6,    // used only by STRS/LDRS and not stored in Context_State.  If a fault occurs during one of these instructions, the faulting registerset cannot be STRS/LDRS'd and then resumed.
    temp_2 = 7,

    pub fn raw(self: SR2_Index) u3 {
        return @intFromEnum(self);
    }

    pub const count = std.math.maxInt(u3) + 1;
};

pub const SR1_Write_Source = enum(u2) {
    no_write = 0,
    rsn_sr1 = 1, // bits 31:16 from RSN that was used during setup cycle, bits 15:0 from SR1 data that was read during setup cycle.
    l = 2,
    virtual_addr = 3,
};

pub const SR2_Write_Source = enum(u2) {
    no_write = 0,
    sr2 = 1,
    l = 2,
    virtual_addr = 3,
};

pub const Any_SR_Index = enum(u4) {
    zero = @intFromEnum(SR2_Index.zero),
    ip = @intFromEnum(SR2_Index.ip),
    next_ip = @intFromEnum(SR2_Index.next_ip),
    asn = @intFromEnum(SR2_Index.asn),
    kxp = @intFromEnum(SR2_Index.kxp),
    uxp = @intFromEnum(SR2_Index.uxp),
    rs_reserved = @intFromEnum(SR2_Index.rs_reserved),
    temp_2 = @intFromEnum(SR2_Index.temp_2),

    sr1_zero = @as(u4, @intFromEnum(SR1_Index.zero)) + 8,
    rp = @as(u4, @intFromEnum(SR1_Index.rp)) + 8,
    sp = @as(u4, @intFromEnum(SR1_Index.sp)) + 8,
    bp = @as(u4, @intFromEnum(SR1_Index.bp)) + 8,
    fault_ua_dr = @as(u4, @intFromEnum(SR1_Index.fault_ua_dr)) + 8,
    fault_rsn_stat = @as(u4, @intFromEnum(SR1_Index.fault_rsn_stat)) + 8,
    int_rsn_fault_iw_ik_ij = @as(u4, @intFromEnum(SR1_Index.int_rsn_fault_iw_ik_ij)) + 8,
    temp_1 = @as(u4, @intFromEnum(SR1_Index.temp_1)) + 8,

    pub fn to_SR1_Index(self: Any_SR_Index) ?SR1_Index {
        const ord = @intFromEnum(self);
        return if (ord >= 8) @enumFromInt(ord - 8) else null;
    }

    pub fn to_SR2_Index(self: Any_SR_Index) ?SR2_Index {
        const ord = @intFromEnum(self);
        return if (ord >= 8) null else @enumFromInt(ord);
    }
};

pub const Address_Offset_Source = enum(u2) {
    zero = 0,
    literal = 1,
    two = 2,
    literal_minus_64 = 3,
};

pub const JL_Source = enum(u2) {
    zero = 0,
    jrl = 1,
    sr1l = 2,
    sr2l = 3,
};

pub const JH_Source = enum(u3) {
    zero = 0,
    jrh = 1,
    sr1h = 2,
    sr2h = 3,
    // 4 unused
    jrl_sx = 5,
    // 6 unused
    neg_one = 7,
    _,
};

pub const K_Source = enum(u3) {
    kr = 0,
    ik_bit = 1,
    ik_zx = 2,
    ij_ik_zx = 3,
    literal = 4,
    literal_minus_64 = 5,
    sr1l = 6,
    sr2l = 7,
};

pub const Compute_Unit = enum(u2) {
    alu = 0,
    shift = 1,
    mult = 2,
    count = 3,
};

pub const Compute_Mode = packed union {
    arith: Arithmetic_Mode,
    logic: Logic_Mode,
    shift: Shift_Mode,
    mult: Multiply_Mode,
    count: Bit_Count_Mode,

    pub fn init(raw_value: u6) Compute_Mode {
        return @bitCast(raw_value);
    }
    pub fn raw(self: Compute_Mode) u6 {
        return @bitCast(self);
    }
};

pub const Arithmetic_Mode = packed struct (u6) {
    _reserved: u2 = 3,
    carry_borrow: bool,
    subtract: bool,
    k_ext: enum (u2) {
        zx = 0,
        @"1x" = 1,
        sx = 2,
        none = 3, // 16 bit arithmetic
    },
    pub const jl_plus_k            = Arithmetic_Mode { .subtract = false, .carry_borrow = false, .k_ext = .none };
    pub const jl_minus_k           = Arithmetic_Mode { .subtract = true,  .carry_borrow = false, .k_ext = .none };
    pub const jl_plus_k_plus_c     = Arithmetic_Mode { .subtract = false, .carry_borrow = true,  .k_ext = .none };
    pub const jl_minus_k_minus_c   = Arithmetic_Mode { .subtract = true,  .carry_borrow = true,  .k_ext = .none };
    pub const j_plus_k_zx          = Arithmetic_Mode { .subtract = false, .carry_borrow = false, .k_ext = .zx };
    pub const j_minus_k_zx         = Arithmetic_Mode { .subtract = true,  .carry_borrow = false, .k_ext = .zx };
    pub const j_plus_k_zx_plus_c   = Arithmetic_Mode { .subtract = false, .carry_borrow = true,  .k_ext = .zx };
    pub const j_minus_k_zx_minus_c = Arithmetic_Mode { .subtract = true,  .carry_borrow = true,  .k_ext = .zx };
    pub const j_plus_k_sx          = Arithmetic_Mode { .subtract = false, .carry_borrow = false, .k_ext = .sx };
    pub const j_minus_k_sx         = Arithmetic_Mode { .subtract = true,  .carry_borrow = false, .k_ext = .sx };
    pub const j_plus_k_sx_plus_c   = Arithmetic_Mode { .subtract = false, .carry_borrow = true,  .k_ext = .sx };
    pub const j_minus_k_sx_minus_c = Arithmetic_Mode { .subtract = true,  .carry_borrow = true,  .k_ext = .sx };
    pub const j_plus_k_1x          = Arithmetic_Mode { .subtract = false, .carry_borrow = false, .k_ext = .@"1x" };
    pub const j_minus_k_1x         = Arithmetic_Mode { .subtract = true,  .carry_borrow = false, .k_ext = .@"1x" };
    pub const j_plus_k_1x_plus_c   = Arithmetic_Mode { .subtract = false, .carry_borrow = true,  .k_ext = .@"1x" };
    pub const j_minus_k_1x_minus_c = Arithmetic_Mode { .subtract = true,  .carry_borrow = true,  .k_ext = .@"1x" };
};

pub const Logic_Mode = packed struct (u6) {
    op: enum (u2) {
        xor = 0,
        @"or" = 1,
        @"and" = 2,
    },
    invert_jlm: bool,
    invert_km: bool,
    _reserved: u2 = 3,

    // n.b. both operands are masked by jh:
    // jlm = jl and not jh
    // km = k and not jh

    pub const jlm_xor_km     = Logic_Mode { .invert_jlm = false, .invert_km = false, .op = .xor    };
    pub const jlm_xnor_km    = Logic_Mode { .invert_jlm = false, .invert_km = true,  .op = .xor    };
    pub const jlm_or_km      = Logic_Mode { .invert_jlm = false, .invert_km = false, .op = .@"or"  };
    pub const jlm_and_km     = Logic_Mode { .invert_jlm = false, .invert_km = false, .op = .@"and" };
    pub const not_jlm_or_km  = Logic_Mode { .invert_jlm = true,  .invert_km = false, .op = .@"or"  };
    pub const not_jlm_and_km = Logic_Mode { .invert_jlm = true,  .invert_km = false, .op = .@"and" };
    pub const jlm_or_not_km  = Logic_Mode { .invert_jlm = false, .invert_km = true,  .op = .@"or"  };
    pub const jlm_and_not_km = Logic_Mode { .invert_jlm = false, .invert_km = true,  .op = .@"and" };
    pub const jlm_nand_km    = Logic_Mode { .invert_jlm = true,  .invert_km = true,  .op = .@"or"  };
    pub const jlm_nor_km     = Logic_Mode { .invert_jlm = true,  .invert_km = true,  .op = .@"and" };
};

pub const Shift_Mode = packed struct (u6) {
    left: bool,
    early_swap16: bool,
    late_swap16: bool,
    wide: bool,
    _unused: u2 = 0,

    pub const jl_shr_k4 = Shift_Mode { .left = false, .early_swap16 = false, .late_swap16 = false, .wide = false };
    pub const jl_shl_k4 = Shift_Mode { .left = true,  .early_swap16 = false, .late_swap16 = false, .wide = false };
    pub const jh_shr_k4 = Shift_Mode { .left = false, .early_swap16 = true,  .late_swap16 = false, .wide = false };
    pub const jh_shl_k4 = Shift_Mode { .left = true,  .early_swap16 = true,  .late_swap16 = false, .wide = false };
    pub const j_shr_k5  = Shift_Mode { .left = false, .early_swap16 = false, .late_swap16 = false, .wide = true };
    pub const j_shl_k5  = Shift_Mode { .left = true,  .early_swap16 = true,  .late_swap16 = true,  .wide = true };
};

pub const Multiply_Mode = packed struct (u6) {
    jl: Data_Type,
    k: Data_Type,
    swap_halves: bool,
    _unused: u3 = 0,

    pub const Data_Type = enum (u1) {
        unsigned = 0,
        signed = 1,
    };

    pub const jl_zx_k_zx      = Multiply_Mode { .jl = .unsigned, .k = .unsigned, .swap_halves = false };
    pub const jl_sx_k_zx      = Multiply_Mode { .jl = .signed,   .k = .unsigned, .swap_halves = false };
    pub const jl_zx_k_sx      = Multiply_Mode { .jl = .unsigned, .k = .signed,   .swap_halves = false };
    pub const jl_sx_k_sx      = Multiply_Mode { .jl = .signed,   .k = .signed,   .swap_halves = false };
    pub const jl_zx_k_zx_swap = Multiply_Mode { .jl = .unsigned, .k = .unsigned, .swap_halves = true };
    pub const jl_sx_k_zx_swap = Multiply_Mode { .jl = .signed,   .k = .unsigned, .swap_halves = true };
    pub const jl_zx_k_sx_swap = Multiply_Mode { .jl = .unsigned, .k = .signed,   .swap_halves = true };
    pub const jl_sx_k_sx_swap = Multiply_Mode { .jl = .signed,   .k = .signed,   .swap_halves = true };
};

pub const Bit_Count_Mode = packed struct (u6) {
    invert_jl: bool,
    leftmost_only: bool,
    rightmost_only: bool,
    _unused: u3 = 0,

    pub const cb   = Bit_Count_Mode { .leftmost_only = false, .rightmost_only = false, .invert_jl = false };
    pub const cz   = Bit_Count_Mode { .leftmost_only = false, .rightmost_only = false, .invert_jl = true  };
    pub const ctb  = Bit_Count_Mode { .leftmost_only = false, .rightmost_only = true,  .invert_jl = false };
    pub const ctz  = Bit_Count_Mode { .leftmost_only = false, .rightmost_only = true,  .invert_jl = true  };
    pub const clb  = Bit_Count_Mode { .leftmost_only = true,  .rightmost_only = false, .invert_jl = false };
    pub const clz  = Bit_Count_Mode { .leftmost_only = true,  .rightmost_only = false, .invert_jl = true  };
};

pub const LL_Source = enum(u3) {
    zero = 0,
    compute_l = 1,
    d = 2,
    d8_sx = 3,
    translation_info_l = 4,
    stat = 5,
    pipeline = 7,
    _,
};

pub const LH_Source = enum(u3) {
    zero = 0,
    compute_h = 1,
    d8_sx = 3,
    translation_info_h = 4,
    jh = 6,
    prev_uc_slot = 7,
    _,
};

pub const Bus_Direction = enum(u2) {
    read,
    read_to_dr,
    write_from_ll,
    write_from_dr,
};

pub const Bus_Width = enum(u1) {
    word = 0,
    byte = 1,
};

pub const Address_Space = enum(u2) {
    raw = 0,
    data = 1,
    stack = 2,
    insn = 3,
};

// what should the address translator do this cycle?
// bus operation is inhibited if not .translate
pub const Address_Translator_Op = enum(u2) {
    none = 0,
    translate = 1,
    update = 2,
    invalidate = 3,
};

pub const Status_Op = enum(u4) {
    hold = 0,
    load_zncv = 1,
    clear_a = 2,
    set_a = 3,
    zncv_from_arith = 4,
    zncv_from_arith_no_set_z = 5,
    // 6 unused
    // 7 unused
    zn_16 = 8, // compute ZN from LL only
    zn_16_no_set_z = 9, // compute ZN from LL only
    zn_16__c_from_shift = 10,
    zn_16_no_set_z__c_from_shift = 11,
    zn_32 = 12, // compute ZN from LL + LH
    zn_32_no_set_z = 13, // compute ZN from LL + LH
    zn_32__c_from_shift = 14,
    zn_32_no_set_z__c_from_shift = 15,
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
    load_rsn_from_ll = 5,
    toggle_rsn = 6,
    trigger_fault = 7,
};

const Control_Signals = @This();
const hw = @import("hardware.zig");
const std = @import("std");
