pub const LSB = enum (u8) {
    nop_1,
    misc_16,
    alu_16,
    add_reg,
    sub_reg,
    ret,
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

pub const Misc_16 = enum (u8) {
    nop_2,
    nop_3,
    iret,
    fret,
    ifex,

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

    park = 0xFF,
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
    switch (mnemonic) {
        inline else => |in| {
            const name = @tagName(in) ++ suffix;
            if (@hasField(E, name)) {
                return @field(E, name);
            } else {
                std.debug.panic("{} does not have the field: .{s}", .{ E, name });
            }
        },
    }
}

const Encoder = isa.Instruction_Encoding.Encoder;
const isa = @import("isa");
const std = @import("std");
