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
    add_i16,
    addc_i16,
    addv_i16,
    addcv_i16,
    nadd_i16,
    naddc_i16,
    naddv_i16,
    naddcv_i16,
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
};

pub const Misc_16 = enum (u8) {
    nop_2,
    nop_3,
    iret,
    fret,
    ifex,
    park = 0xFF,
};

pub const ALU_16 = enum (u8) {
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
