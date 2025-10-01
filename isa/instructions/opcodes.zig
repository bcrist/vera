pub const LSB = enum (u8) {
    nop_1,
    misc_12,
    misc_16,
    alu_16,
    misc_reg,
    add_reg,
    sub_reg,
    shift_reg,
    shift_imm,
    bit_op,

    ret,

    val,

    @"and",
    @"or",
    xor,

    add,
    addc,
    addv,
    addcv,

    nadd,
    naddc,
    naddv,
    naddcv,

    sub,
    subc,
    subv,
    subcv,

    cmp,
    cmpc,

    add_i16,
    addc_i16,
    addv_i16,
    addcv_i16,
    nadd_i16,
    naddc_i16,
    naddv_i16,
    naddcv_i16,
    cmp_i16,
    cmpc_i16,
    and_i16,
    or_i16,
    xor_i16,

    inc,
    incv,
    dec,
    decv,
    neg,
    negc,
    negv,
    negcv,

    addc_0,
    addcv_0,
    cmp_0,
    cmpc_0,
};

pub const Misc_12 = enum (u4) {
};

pub const Misc_16 = enum (u8) {
    nop_2,
    nop_3,
    park,
    iret,
    fret,
    freto,
    ifex,

    shlc,
    shrc,

    csb,
    czb,
    csbl,
    czbl,
    csbt,
    czbt,

    ssbl,
    szbl,
    ssbt,
    szbt,

    @"swap.16",
    @"swap.8",
    @"rev.8.32",
    @"rev.1.32",
    @"rev.1.16",
    @"rev.1.8",

};

pub const ALU_16 = enum (u8) {
    add_ip_rel,
    addc_ip_rel,
    addv_ip_rel,
    addcv_ip_rel,
    nadd_ip_rel,
    naddc_ip_rel,
    naddv_ip_rel,
    naddcv_ip_rel,
    sub_ip_rel,
    subc_ip_rel,
    subv_ip_rel,
    subcv_ip_rel,
    cmp_ip_rel,
    cmpc_ip_rel,
    and_ip_rel,
    or_ip_rel,
    xor_ip_rel,

    add_sp_rel,
    addc_sp_rel,
    addv_sp_rel,
    addcv_sp_rel,
    nadd_sp_rel,
    naddc_sp_rel,
    naddv_sp_rel,
    naddcv_sp_rel,
    sub_sp_rel,
    subc_sp_rel,
    subv_sp_rel,
    subcv_sp_rel,
    cmp_sp_rel,
    cmpc_sp_rel,
    and_sp_rel,
    or_sp_rel,
    xor_sp_rel,

    add_bp_rel,
    addc_bp_rel,
    addv_bp_rel,
    addcv_bp_rel,
    nadd_bp_rel,
    naddc_bp_rel,
    naddv_bp_rel,
    naddcv_bp_rel,
    sub_bp_rel,
    subc_bp_rel,
    subv_bp_rel,
    subcv_bp_rel,
    cmp_bp_rel,
    cmpc_bp_rel,
    and_bp_rel,
    or_bp_rel,
    xor_bp_rel,
    
};

pub const Add_Reg = enum (u3) {
    add,
    addc,
    addv,
    addcv,
    nadd,
    naddc,
    naddv,
    naddcv,
};

pub const Sub_Reg = enum (u3) {
    sub,
    subc,
    subv,
    subcv,
    cmp,
    cmpc,
    // unused
    // unused
};

pub const Shift_Reg = enum (u3) {
    shl,
    shlv,
    shr,
    shrv,
    shrs,
    shrsv,
    // unused
    // unused
};

pub const Shift_Imm = enum (u3) {
    shl,
    shlv,
    shr,
    shrv,
    shrs,
    shrsv,
    // unused
    // unused
};

pub const Misc_Reg = enum (u3) {
    @"and",
    @"or",
    xor,
    val_reg,
    sx,
    zx,
    sxv,
    zxv,
};

pub const Bit_Op = enum (u3) {
    val_bit,
    val_not_bit,
    val_bit_minus_one,
    val_neg_bit,
    clrbit,
    setbit,
    bit,
    // unused,
};

const Mnemonic_Encoder_Options = struct {
    suffix: []const u8 = "",
    offset: comptime_int = 0,
};
pub fn mnemonic_encoder(comptime E: type, comptime options: Mnemonic_Encoder_Options) fn(isa.Mnemonic) Encoder {
    return struct {
        fn encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(options.offset, mnemonic_cast(E, options.suffix, mnemonic));
        }
    }.encoder;
}

pub fn mnemonic_cast(comptime E: type, comptime suffix: []const u8, mnemonic: isa.Mnemonic) E {
    const mnemonic_name = mnemonic.name();
    inline for (@typeInfo(E).@"enum".fields) |field| {
        if (std.mem.startsWith(u8, field.name, mnemonic_name) and std.mem.eql(u8, field.name[mnemonic_name.len ..], suffix)) {
            return @enumFromInt(field.value);
        }
    }
    std.debug.panic("{} does not have the field: .{s}{s}", .{ E, mnemonic_name, suffix });
}

const Encoder = isa.Encoder;
const isa = @import("isa");
const std = @import("std");
