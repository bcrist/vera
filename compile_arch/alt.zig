pub const instructions = .{
    struct {
        // Used by SAT, RAT, FRET
        pub const decode_mode = ID_Mode.alt;
        pub const encoding = .{
            Encoder.identity(Reg(.ij)),
            Encoder.shifted(4, Reg(.ik)),
            Encoder.shifted(8, Reg(.iw)),
            Encoder.shifted(12, @as(u4, 0)),
        };
        pub const ij = Reg(.ij);
        pub const ik = Reg(.ik);
        pub const iw = Reg(.iw);

        pub fn entry(c: *Cycle) void {
            c.invalid_instruction();
        }
    },
};

const Cycle = @import("Cycle.zig");
const Encoder = arch.isa.Instruction_Encoding.Encoder;
const Reg = placeholders.Reg;
const ID_Mode = arch.hw.Control_Signals.ID_Mode;
const placeholders = @import("placeholders.zig");
const arch = @import("lib_arch");
