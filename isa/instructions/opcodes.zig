pub const LSB = enum (u8) {
    nop_1,
    misc_10,
    misc_12,
    misc_16,
    misc_reg,
    add_reg,
    sub_reg,
    shift_reg,
    shift_imm,
    bit_op,

    ip_reg,
    sp_reg,
    bp_reg,
    uxp_reg,
    kxp_reg,
    ld_st_reg,
    ldg_stg_reg,

    ret,

    @"b.z_ip_rel",
    @"b.c_ip_rel",
    @"b.v_ip_rel",
    @"b.n_ip_rel",
    @"b.p_ip_rel",
    @"b.nz_ip_rel",
    @"b.nc_ip_rel",
    @"b.nv_ip_rel",
    @"b.nn_ip_rel",
    @"b.np_ip_rel",
    @"b.gt.u_ip_rel",
    @"b.le.u_ip_rel",
    @"b.lt.s_ip_rel",
    @"b.gt.s_ip_rel",
    @"b.le.s_ip_rel",
    @"b.ge.s_ip_rel",

    b_ip_rel16,
    call_ip_rel16,
    call_ptr_sp_rel,
    call_ptr_bp_rel,
    call_ptr_kxp_rel,
    call_ptr_uxp_rel,

    @"ld.8_ip_rel",
    @"ld.16_ip_rel",
    @"ld.24_ip_rel",
    @"ld.32_ip_rel",

    @"ld.8_ip_rel_d",
    @"ld.16_ip_rel_d",
    @"ld.24_ip_rel_d",
    @"ld.32_ip_rel_d",

    @"st.8_ip_rel_d",
    @"st.16_ip_rel_d",
    @"st.24_ip_rel_d",
    @"st.32_ip_rel_d",

    @"ld.8_sp_rel",
    @"ld.16_sp_rel",
    @"ld.32_sp_rel",
    @"st.8_sp_rel",
    @"st.16_sp_rel",
    @"st.32_sp_rel",

    @"ld.8_kxp_rel",
    @"ld.16_kxp_rel",
    @"ld.24_kxp_rel",
    @"ld.32_kxp_rel",

    @"st.8_kxp_rel",
    @"st.16_kxp_rel",
    @"st.24_kxp_rel",
    @"st.32_kxp_rel",

    @"ld.8_uxp_rel",
    @"ld.16_uxp_rel",
    @"ld.24_uxp_rel",
    @"ld.32_uxp_rel",

    @"st.8_uxp_rel",
    @"st.16_uxp_rel",
    @"st.24_uxp_rel",
    @"st.32_uxp_rel",

    @"ld.8_bp_rel",
    @"ld.16_bp_rel",
    @"ld.24_bp_rel",
    @"ld.32_bp_rel",

    @"st.8_bp_rel",
    @"st.16_bp_rel",
    @"st.24_bp_rel",
    @"st.32_bp_rel",

    @"ldg.8_uxp_rel",
    @"ldg.16_uxp_rel",
    @"ldg.32_uxp_rel",
    @"stg.8_uxp_rel",
    @"stg.16_uxp_rel",
    @"stg.32_uxp_rel",

    @"ldg.8_kxp_rel",
    @"ldg.16_kxp_rel",
    @"ldg.32_kxp_rel",
    @"stg.8_kxp_rel",
    @"stg.16_kxp_rel",
    @"stg.32_kxp_rel",

    @"ldg.8_bp_rel",
    @"ldg.16_bp_rel",
    @"ldg.32_bp_rel",
    @"stg.8_bp_rel",
    @"stg.16_bp_rel",
    @"stg.32_bp_rel",

    @"and",
    @"or",
    xor,

    add,
    addc,
    @"add.vf",
    @"addc.vf",

    nadd,
    naddc,
    @"nadd.vf",
    @"naddc.vf",

    sub,
    subc,
    @"sub.vf",
    @"subc.vf",

    cmp,
    cmpc,

    add_i16,
    addc_i16,
    @"add.vf_i16",
    @"addc.vf_i16",
    nadd_i16,
    naddc_i16,
    @"nadd.vf_i16",
    @"naddc.vf_i16",
    cmp_i16,
    cmpc_i16,
    and_i16,
    or_i16,
    xor_i16,

    inc,
    @"inc.vf",
    dec,
    @"dec.vf",
    neg,
    negc,
    @"neg.vf",
    @"negc.vf",

    addc_0,
    @"addc.vf_0",
    cmp_0,
    cmpc_0,

    add_ip_rel,
    addc_ip_rel,
    @"add.vf_ip_rel",
    @"addc.vf_ip_rel",
    nadd_ip_rel,
    naddc_ip_rel,
    @"nadd.vf_ip_rel",
    @"naddc.vf_ip_rel",
    sub_ip_rel,
    subc_ip_rel,
    @"sub.vf_ip_rel",
    @"subc.vf_ip_rel",
    cmp_ip_rel,
    cmpc_ip_rel,
    and_ip_rel,
    or_ip_rel,
    xor_ip_rel,

    add_sp_rel,
    addc_sp_rel,
    @"add.vf_sp_rel",
    @"addc.vf_sp_rel",
    nadd_sp_rel,
    naddc_sp_rel,
    @"nadd.vf_sp_rel",
    @"naddc.vf_sp_rel",
    sub_sp_rel,
    subc_sp_rel,
    @"sub.vf_sp_rel",
    @"subc.vf_sp_rel",
    cmp_sp_rel,
    cmpc_sp_rel,
    and_sp_rel,
    or_sp_rel,
    xor_sp_rel,

    add_bp_rel,
    addc_bp_rel,
    @"add.vf_bp_rel",
    @"addc.vf_bp_rel",
    nadd_bp_rel,
    naddc_bp_rel,
    @"nadd.vf_bp_rel",
    @"naddc.vf_bp_rel",
    sub_bp_rel,
    subc_bp_rel,
    @"sub.vf_bp_rel",
    @"subc.vf_bp_rel",
    cmp_bp_rel,
    cmpc_bp_rel,
    and_bp_rel,
    or_bp_rel,
    xor_bp_rel,

    val,
    val_rp,
    val_sp,
    val_ip_rel,
    val_sp_rel,
    val_bp_rel,
    val_uxp_rel,
    val_kxp_rel,

    set_bp,
    set_sp,
    set_rp,

    pub const @"b.eq_ip_rel" = LSB.@"b.z_ip_rel";
    pub const @"b.ne_ip_rel" = LSB.@"b.nz_ip_rel";
    pub const @"b.lt.u_ip_rel" = LSB.@"b.nc_ip_rel";
    pub const @"b.ge.u_ip_rel" = LSB.@"b.c_ip_rel";
};

pub const Misc_10 = enum (u2) {
    b,
};

pub const Misc_12 = enum (u4) {
    drop,
    @"drop.copy",
};


pub const Misc_16 = enum (u8) {
    nop_2,
    nop_3,
    park,
    iret,
    fret,
    freto,
    ifex,

    b,
    b_imm,
    b_imm_unaligned,
    b_ip_rel32,
    b_ip_rel32_unaligned,

    call,
    call_imm,
    call_imm_unaligned,
    call_ip_rel32,
    call_ip_rel32_unaligned,
    call_ptr,

    shlc,
    shrc,

    val_bp,
    val_flags,
    val_status,
    val_asn,
    val_uxp,
    val_kxp,

    drop,

    csb,
    czb,
    @"csb.l",
    @"czb.l",
    @"csb.t",
    @"czb.t",

    @"ssb.l",
    @"szb.l",
    @"ssb.t",
    @"szb.t",

    @"swap.16",
    @"swap.8",
    @"rev.8.32",
    @"rev.1.32",
    @"rev.1.16",
    @"rev.1.8",

    set_asn,
    set_zncv,
    set_uxp,
    set_kxp,

    @"ld.8",
    @"ld.16",
    @"ld.24",
    @"ld.32",
    @"st.8",
    @"st.16",
    @"st.24",
    @"st.32",


};

pub const Add_Reg = enum (u3) {
    add,
    addc,
    @"add.vf",
    @"addc.vf",
    nadd,
    naddc,
    @"nadd.vf",
    @"naddc.vf",
};

pub const Sub_Reg = enum (u3) {
    sub,
    subc,
    @"sub.vf",
    @"subc.vf",
    cmp,
    cmpc,
    b_reg, // TODO find more appropriate spot
    call_reg, // TODO find more appropriate spot
};

pub const Shift_Reg = enum (u3) {
    shl,
    @"shl.vf",
    shr,
    @"shr.vf",
    shrs,
    @"shrs.vf",
    call_ptr_reg // TODO find more appropriate spot
    // unused
};

pub const Shift_Imm = enum (u3) {
    shl,
    @"shl.vf",
    shr,
    @"shr.vf",
    shrs,
    @"shrs.vf",
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
    @"sx.vf",
    @"zx.vf",
};

pub const Bit_Op = enum (u3) {
    val_bit,
    val_not_bit,
    val_bit_minus_one,
    val_neg_bit,
    clrbit,
    setbit,
    bit,
    // unused
};

pub const LD_ST_Reg = enum(u3) {
    @"ld.8",
    @"ld.16",
    @"ld.24",
    @"ld.32",
    @"st.8",
    @"st.16",
    @"st.24",
    @"st.32",
};

pub const LDG_STG_Reg = enum(u3) {
    @"ldg.8",
    @"ldg.16",
    @"ldg.24",
    @"ldg.32",
    @"stg.8",
    @"stg.16",
    @"stg.24",
    @"stg.32",
};

pub const IP_Reg = enum (u3) {
    val,
    // unused
    // unused
    // unused
    // unused
    // unused
    // unused
    // unused
};
pub const SP_Reg = enum (u3) {
    val,
    @"ld.8",
    @"ld.16",
    @"ld.32",
    @"st.8",
    @"st.16",
    @"st.32",
    // unused
};
pub const BP_Reg = enum (u3) {
    val,
    @"ld.8",
    @"ld.16",
    @"ld.32",
    @"st.8",
    @"st.16",
    @"st.32",
    // unused
};
pub const UXP_Reg = enum (u3) {
    val,
    // unused
    // unused
    // unused
    // unused
    // unused
    // unused
    // unused
};
pub const KXP_Reg = enum (u3) {
    val,
    // unused
    // unused
    // unused
    // unused
    // unused
    // unused
    // unused
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
    inline for (@typeInfo(E).@"enum".decls) |decl| {
        if (@TypeOf(@field(E, decl.name)) == E and std.mem.startsWith(u8, decl.name, mnemonic_name) and std.mem.eql(u8, decl.name[mnemonic_name.len ..], suffix)) {
            return @field(E, decl.name);
        }
    }
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
