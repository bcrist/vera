const region_encoder = Encoder.init(14, @as(u2, 0));

pub const instructions = .{
    struct { // <eab/dab> .i x0
        pub const spec = 
            \\eab .i x0
            \\dab .i x0
            ;
        pub const encoding = .{
            @as(u8, 0),
            mnemonic_encoder,
            Encoder.init(9, @as(u5, 1)),
            region_encoder,
        };
        pub const ij: u4 = 0;

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(8, @as(u1, switch (mnemonic) {
                .dab => 0,
                .eab => 1,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, flags: Flags, mnemonic: isa.Mnemonic) void {
            if (!flags.kernel()) return c.illegal_instruction();

            c.reg32_to_l();
            c.l_to_sr(.next_ip);
            switch (mnemonic) {
                .eab => c.enable_address_translation(),
                .dab => c.disable_address_translation(),
                else => unreachable,
            }
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.next_ip, 0);
        }
    },
};

const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Encoder;
const Parameter = isa.Parameter;
const Options = placeholders.Options;
const Range = placeholders.Range;
const Int = placeholders.Int;
const Int_Mult = placeholders.Int_Mult;
const Reg = placeholders.Reg;
const Param = placeholders.Param;
const Bit = placeholders.Bit;
const Even_Reg = placeholders.Even_Reg;
const Odd_Reg = placeholders.Odd_Reg;
const conditions = @import("../compile/conditions.zig");
const placeholders = @import("../compile/placeholders.zig");
const Control_Signals = arch.Control_Signals;
const Flags = arch.microcode.Flags;
const isa = @import("isa");
const arch = @import("arch");
const std = @import("std");
