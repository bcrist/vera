pub fn encoder(comptime shift: u8) fn(mnemonic: isa.Mnemonic, condition: isa.Mnemonic_Suffix) Encoder {
    return struct {
        pub fn func(mnemonic: isa.Mnemonic, condition: isa.Mnemonic_Suffix) Encoder {
            switch (mnemonic) {
                .b, .bb, .bbn, .bp, .bpn => {},
                .call => std.debug.assert(condition == .none),
                else => unreachable,
            }
            return Encoder.init(shift, @as(u4, switch (condition) {
                .none => switch (mnemonic) {
                    .b, .bb, .bbn, .bp, .bpn => 0,
                    .call => 15,
                    else => unreachable,
                },
                .v => 1,
                .z => 2,
                .nz => 3,
                .n => 4,
                .nn => 5,
                .c => 6,
                .nc => 7,
                .gu => 8,
                .ngu => 9,
                .ls => 10,
                .nls => 11,
                .gs => 12,
                .ngs => 13,
                .p => 14,
                else => unreachable,
            }));
        }
    }.func;
}

pub fn short_encoder(comptime shift: u8) fn(condition: isa.Mnemonic_Suffix) Encoder {
    return struct {
        pub fn func(condition: isa.Mnemonic_Suffix) Encoder {
            return Encoder.init(shift, @as(u3, switch (condition) {
                .none => 0,
                .n => 1,
                .z => 2,
                .nz => 3,
                .c => 4,
                .nc => 5,
                .gs => 6,
                .ngs => 7,
                else => unreachable,
            }));
        }
    }.func;
}

pub fn double_encoder(comptime shift: u8) fn(condition: isa.Mnemonic_Suffix) Encoder {
    return struct {
        pub fn func(condition: isa.Mnemonic_Suffix) Encoder {
            return Encoder.init(shift, @as(u3, switch (condition) {
                .lu_gu => 0,
                .lu_z => 1,
                .gu_z => 2,
                .ls_gs => 3,
                .ls_z => 4,
                .gs_z => 5,
                .n_z => 6,
                .n_p => 7,
                else => unreachable,
            }));
        }
    }.func;
}

pub fn check(condition: isa.Mnemonic_Suffix, flags: anytype) bool {
    return switch (condition) {
        .none => true,
        .z => flags.zero(),
        .nz => !flags.zero(),
        .n => flags.negative(),
        .nn => !flags.negative(),
        .p => flags.positive(),
        .np => !flags.positive(),
        .c => flags.carry(),
        .nc => !flags.carry(),
        .v => flags.overflow(),
        .nv => !flags.overflow(),
        .lu => flags.unsigned_less_than(),
        .nlu => !flags.unsigned_less_than(),
        .gu => flags.unsigned_greater_than(),
        .ngu => !flags.unsigned_greater_than(),
        .ls => flags.signed_less_than(),
        .nls => !flags.signed_less_than(),
        .gs => flags.signed_greater_than(),
        .ngs => !flags.signed_greater_than(),
        else => unreachable,
    };
}

pub fn first(condition: isa.Mnemonic_Suffix) isa.Mnemonic_Suffix {
    return switch (condition) {
        .lu_gu, .lu_z => return .lu,
        .gu_z => return .gu,
        .ls_gs, .ls_z => return .ls,
        .gs_z => return .gs,
        .n_z, .n_p => return .n,
        .p_z => return .p,
        else => unreachable,
    };
}

pub fn second(condition: isa.Mnemonic_Suffix) isa.Mnemonic_Suffix {
    return switch (condition) {
        .lu_z, .gu_z, .ls_z, .gs_z, .n_z, .p_z => return .z,
        .lu_gu => return .gu,
        .ls_gs => return .gs,
        .n_p => return .p,
        else => unreachable,
    };
}

const Encoder = isa.Encoder;
const Flags = arch.microcode.Flags;
const isa = @import("isa");
const arch = @import("arch");
const std = @import("std");
