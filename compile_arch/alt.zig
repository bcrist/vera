pub const instructions = .{
    struct {
        pub const decode_mode = ID_Mode.alt;
        pub const encoding = .{
            Reg(.ij),
            Encoder.init(4, Reg(.ik)),
            Encoder.init(8, Reg(.iw)),
            Encoder.init(12, @as(u4, 0)),
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
