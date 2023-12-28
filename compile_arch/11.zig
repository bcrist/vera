const region_encoder = Encoder.shifted(14, @as(u2, 3));

pub const instructions = .{
    struct { pub const spec = "mul r(a), r(b) -> r(dest)";
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.shifted(3, Even_Reg(.a)),
            Encoder.shifted(6, Even_Reg(.b)),
            Encoder.shifted(9, @as(u5, 0)),
            region_encoder,
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.reg_to_jl();
            c.reg_to_k();
            c.jl_times_k_to_ll(.zx, .zx, .fresh, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // mul r(a), r(b) -> x(dest)
        \\mul r(a) .unsigned, r(b) .unsigned -> x(dest)
        \\mul r(a) .signed, r(b) .signed -> x(dest)
        ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.shifted(3, Even_Reg(.a)),
            Encoder.shifted(6, Even_Reg(.b)),
            signedness_encoder,
            Encoder.shifted(10, @as(u4, 1)),
            region_encoder,
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);
        pub const iw = Reg(.dest);

        fn signedness_encoder(a: Param(.a)) Encoder {
            return Encoder.shifted(9, @as(u1, switch (a.signature.base.reg16.?) {
                .unsigned => 0,
                .signed => 1,
            }));
        }

        pub fn entry(c: *Cycle, a: Param(.a)) void {
            const ext: Cycle.Zero_Or_Sign_Extension = switch (a.signature.base.reg16.?) {
                .unsigned => .zx,
                .signed => .sx,
            };
            c.reg_to_jl();
            c.reg_to_k();
            c.jl_times_k_to_l(ext, ext, .fresh, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // mulh r(a) .unsigned, r(b) .signed -> r(dest) .signed
        \\mulh r(a) .unsigned, r(b) .signed -> r(dest) .signed
        \\mulh r(b) .signed, r(a) .unsigned -> r(dest) .signed
        ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.shifted(3, Even_Reg(.a)),
            Encoder.shifted(6, Even_Reg(.b)),
            Encoder.shifted(9, @as(u5, 1)),
            region_encoder,
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.reg_to_jl();
            c.reg_to_k();
            c.jl_times_k_to_l(.zx, .sx, .fresh, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
};

const Cycle = @import("Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Parameter = isa.Parameter;
const Options = placeholders.Options;
const Range = placeholders.Range;
const Param = placeholders.Param;
const Int = placeholders.Int;
const Int_Mult = placeholders.Int_Mult;
const Reg = placeholders.Reg;
const Reg_Bit = placeholders.Reg_Bit;
const Even_Reg = placeholders.Even_Reg;
const Odd_Reg = placeholders.Odd_Reg;
const conditions = @import("conditions.zig");
const placeholders = @import("placeholders.zig");
const opcodes = @import("opcodes.zig");
const Control_Signals = arch.hw.Control_Signals;
const Flags = arch.hw.microcode.Flags;
const isa = arch.isa;
const arch = @import("lib_arch");
const std = @import("std");
