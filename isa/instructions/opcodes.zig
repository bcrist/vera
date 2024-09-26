pub const LSB = enum (u8) {
    nop1,
    misc16,
    ret,
};

pub const Misc16 = enum (u8) {
    nop2,
    nop3,
    iret,
    fret,
    park = 0xFF,
};
