const region_encoder = Encoder.init(14, @as(u2, 3));

pub const instructions = .{
    struct { // mul r(a), r(b) -> r(dest)
        pub const spec =
            \\mul r(a, dest), r(b)
            \\mul r(a), r(b) -> r(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.a)),
            Encoder.init(6, Even_Reg(.b)),
            Encoder.init(9, @as(u5, 0)),
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
    struct { // mul r(a), r(b) -> x(dest)
        pub const spec =
            \\mul r(a) .unsigned, r(b) .unsigned -> x(dest) .unsigned
            \\mul r(a) .signed, r(b) .signed -> x(dest) .signed
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.a)),
            Encoder.init(6, Even_Reg(.b)),
            signedness_encoder,
            Encoder.init(10, @as(u4, 1)),
            region_encoder,
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);
        pub const iw = Reg(.dest);

        fn signedness_encoder(a: Param(.a)) Encoder {
            return Encoder.init(9, @as(u1, switch (a.signature.base.reg16.?) {
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
    struct { // mulh r(a) .unsigned, r(b) .signed -> r(dest) .signed
        pub const spec =
            \\mulh r(a) .unsigned, r(b) .signed -> r(dest) .signed
            \\mulh r(b, dest) .signed, r(a) .unsigned
            \\mulh r(b) .signed, r(a) .unsigned -> r(dest) .signed
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.a)),
            Encoder.init(6, Even_Reg(.b)),
            Encoder.init(9, @as(u5, 1)),
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
    struct { // mulh r(a), r(b) -> r(dest) // all registers same signedness
        pub const spec =
            \\mulh r(a, dest) .unsigned, r(b) .unsigned
            \\mulh r(a) .unsigned, r(b) .unsigned -> r(dest) .unsigned
            \\mulh r(a, dest) .signed, r(b) .signed
            \\mulh r(a) .signed, r(b) .signed -> r(dest) .signed
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.a)),
            Encoder.init(6, Even_Reg(.b)),
            signedness_encoder,
            Encoder.init(10, @as(u4, 2)),
            region_encoder,
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);
        pub const iw = Reg(.dest);

        fn signedness_encoder(a: Param(.a)) Encoder {
            return Encoder.init(9, @as(u1, switch (a.signature.base.reg16.?) {
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
            c.jl_times_k__shr_16_to_ll(ext, ext, .fresh, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { // mul r(src), (imm) -> r(dest)
        pub const spec =
            \\mul r(src, dest), (imm)
            \\mul r(src), (imm) -> r(dest)
            \\mul (imm), r(src) -> r(dest)
            ;
        pub const forms = .{
            struct {
                pub const encoding = .{
                    Even_Reg(.dest),
                    Encoder.init(3, Even_Reg(.src)),
                    Encoder.init(6, @as(u8, 0x30)),
                    region_encoder,
                    Encoder.init(16, Int(.imm, u16)),
                };
            },
            struct {
                pub const encoding = .{
                    Even_Reg(.dest),
                    Encoder.init(3, Even_Reg(.src)),
                    Encoder.init(6, @as(u8, 0x30)),
                    region_encoder,
                    Encoder.init(16, Int(.imm, i16)),
                };
            },
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(multiply);
        }
        pub fn multiply(c: *Cycle) void {
            c.reg_to_jl();
            c.srl_to_k(.temp_1);
            c.jl_times_k_to_ll(.zx, .zx, .fresh, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { // mul r(src), (imm16) -> x(dest)
        pub const forms = .{
            struct {
                pub const spec =
                    \\mul r(src) .unsigned, (imm) -> x(dest) .unsigned
                    \\mul (imm), r(src) .unsigned -> x(dest) .unsigned
                    ;
                pub const encoding = .{
                    Even_Reg(.dest),
                    Encoder.init(3, Even_Reg(.src)),
                    Encoder.init(6, @as(u8, 0x32)),
                    region_encoder,
                    Encoder.init(16, Int(.imm, u16)),
                };
            },
            struct {
                pub const spec =
                    \\mul r(src) .signed, (imm) -> x(dest) .signed
                    \\mul (imm), r(src) .signed -> x(dest) .signed
                    ;
                pub const encoding = .{
                    Even_Reg(.dest),
                    Encoder.init(3, Even_Reg(.src)),
                    Encoder.init(6, @as(u8, 0x33)),
                    region_encoder,
                    Encoder.init(16, Int(.imm, i16)),
                };
            },
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(multiply);
        }
        pub fn multiply(c: *Cycle, src: Param(.src)) void {
            const ext: Cycle.Zero_Or_Sign_Extension = switch (src.signature.base.reg16.?) {
                .unsigned => .zx,
                .signed => .sx,
            };
            c.reg_to_jl();
            c.srl_to_k(.temp_1);
            c.jl_times_k_to_l(ext, ext, .fresh, .flags);
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { // shr <reg>, <amt> -> <dest>
        pub const forms = .{
            struct {
                pub const spec =
                    \\shr r(src, dest) .unsigned, b(amt)
                    \\shr r(src) .unsigned, b(amt) -> r(dest)
                    \\shr r(src, dest) .signed, b(amt)
                    \\shr r(src) .signed, b(amt) -> r(dest)
                    \\shr x(src, dest) .unsigned, b(amt)
                    \\shr x(src) .unsigned, b(amt) -> x(dest)
                    ;
                pub const encoding = .{
                    Even_Reg(.dest),
                    Encoder.init(3, Even_Reg(.src)),
                    Encoder.init(6, Even_Reg(.amt)),
                    src_encoder,
                    Encoder.init(12, @as(u2, 1)),
                    region_encoder,
                };
                fn src_encoder(src: Param(.src)) Encoder {
                    return Encoder.init(9, @as(u3, switch (src.signature.base) {
                        .reg16 => |signedness| switch (signedness.?) {
                            .unsigned => 0,
                            .signed => 1,
                        },
                        .reg32 => 2,
                        else => unreachable,
                    }));
                }
                pub const ik = Reg(.amt);
            },
            struct {
                pub const spec =
                    \\shr r(src, dest) .unsigned, (amt)
                    \\shr r(src) .unsigned, (amt) -> r(dest)
                    \\shr r(src, dest) .signed, (amt)
                    \\shr r(src) .signed, (amt) -> r(dest)
                    \\shr x(src, dest) .unsigned, (amt)
                    \\shr x(src) .unsigned, (amt) -> x(dest)
                    ;
                pub const encoding = .{
                    Even_Reg(.dest),
                    Encoder.init(3, Even_Reg(.src)),
                    Encoder.init(6, Int(.amt, u3)),
                    src_encoder,
                    Encoder.init(12, @as(u2, 1)),
                    region_encoder,
                };
                fn src_encoder(src: Param(.src)) Encoder {
                    return Encoder.init(9, @as(u3, switch (src.signature.base) {
                        .reg32 => 3,
                        .reg16 => |signedness| switch (signedness.?) {
                            .unsigned => 4,
                            .signed => 5,
                        },
                        else => unreachable,
                    }));
                }
            },
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle, src: Param(.src), amt: Param(.amt), imm: placeholders.Any(.amt)) void {
            switch (amt.signature.base) {
                .constant => c.literal_to_k(@intCast(imm.value)),
                .reg8 => c.reg_to_k(),
                else => unreachable,
            }
            switch (src.signature.base) {
                .reg16 => |signedness| {
                    c.reg_to_j(switch (signedness.?) {
                        .unsigned => .zx,
                        .signed => .sx,
                    });
                    c.jl_shift_k4_to_ll(.right, .fresh, .flags);
                    c.ll_to_reg();
                },
                .reg32 => {
                    c.reg32_to_j();
                    c.j_shift_k5_to_l(.right, .fresh, .flags);
                    c.l_to_reg32();
                },
                else => unreachable,
            }
            c.load_and_exec_next_insn();
        }
    },
    struct { // shl <reg>, <amt> -> <dest>
        pub const forms = .{
            struct {
                pub const spec =
                    \\shl r(src, dest), b(amt)
                    \\shl r(src), b(amt) -> r(dest)
                    \\shl x(src, dest), b(amt)
                    \\shl x(src), b(amt) -> x(dest)
                    ;
                pub const encoding = .{
                    Even_Reg(.dest),
                    Encoder.init(3, Even_Reg(.src)),
                    Encoder.init(6, Even_Reg(.amt)),
                    src_encoder,
                    Encoder.init(12, @as(u2, 2)),
                    region_encoder,
                };
                fn src_encoder(src: Param(.src)) Encoder {
                    return Encoder.init(9, @as(u3, switch (src.signature.base) {
                        .reg16 => 0,
                        .reg32 => 2,
                        else => unreachable,
                    }));
                }
                pub const ik = Reg(.amt);
            },
            struct {
                pub const spec =
                    \\shl r(src, dest), (amt)
                    \\shl r(src), (amt) -> r(dest)
                    \\shl x(src, dest), (amt)
                    \\shl x(src), (amt) -> x(dest)
                    ;
                pub const encoding = .{
                    Even_Reg(.dest),
                    Encoder.init(3, Even_Reg(.src)),
                    Encoder.init(6, Int(.amt, u3)),
                    src_encoder,
                    Encoder.init(12, @as(u2, 2)),
                    region_encoder,
                };
                fn src_encoder(src: Param(.src)) Encoder {
                    return Encoder.init(9, @as(u3, switch (src.signature.base) {
                        .reg32 => 3,
                        .reg16 => 4,
                        else => unreachable,
                    }));
                }
            },
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle, src: Param(.src), amt: Param(.amt), imm: placeholders.Any(.amt)) void {
            switch (amt.signature.base) {
                .constant => c.literal_to_k(@intCast(imm.value)),
                .reg8 => c.reg_to_k(),
                else => unreachable,
            }
            switch (src.signature.base) {
                .reg16 => {
                    c.reg_to_j(.zx);
                    c.jl_shift_k4_to_ll(.left, .fresh, .flags);
                    c.ll_to_reg();
                },
                .reg32 => {
                    c.reg32_to_j();
                    c.j_shift_k5_to_l(.left, .fresh, .flags);
                    c.l_to_reg32();
                },
                else => unreachable,
            }
            c.load_and_exec_next_insn();
        }
    },
    struct { // shr <reg>, (imm4u) -> <reg>
        pub const spec =
            \\shr r(reg) .unsigned, (amt)
            \\shr r(reg) .unsigned, (amt) -> r(reg)
            \\shr r(reg) .signed, (amt)
            \\shr r(reg) .signed, (amt) -> r(reg)
            ;
        pub const encoding = .{
            Reg(.reg),
            Encoder.init(4, Int(.amt, u4)),
            signedness_encoder,
            Encoder.init(9, @as(u5, 0xe)),
            region_encoder,
        };
        fn signedness_encoder(reg: Param(0)) Encoder {
            return Encoder.init(8, @as(u1, switch (reg.signature.base.reg16.?) {
                .unsigned => 0,
                .signed => 1,
            }));
        }
        pub const ij = Reg(.reg);
        pub const iw = Reg(.reg);

        pub fn entry(c: *Cycle, reg: Param(0), amt: placeholders.Any(.amt)) void {
            c.reg_to_j(switch (reg.signature.base.reg16.?) {
                .unsigned => .zx,
                .signed => .sx,
            });
            c.literal_to_k(@intCast(amt.value));
            c.jl_shift_k4_to_ll(.right, .fresh, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { // shl r(reg), (amt) -> r(reg)
        pub const spec =
            \\shl r(reg), (amt)
            \\shl r(reg), (amt) -> r(reg)
            ;
        pub const encoding = .{
            Reg(.reg),
            Encoder.init(4, Int(.amt, u4)),
            Encoder.init(8, @as(u6, 0x2c)),
            region_encoder,
        };
        pub const ij = Reg(.reg);
        pub const iw = Reg(.reg);

        pub fn entry(c: *Cycle, amt: placeholders.Any(.amt)) void {
            c.reg_to_j(.zx);
            c.literal_to_k(@intCast(amt.value));
            c.jl_shift_k4_to_ll(.left, .fresh, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { // <shr/shl> x(reg), (imm5u) -> x(reg)
        pub const spec =
            \\shr x(reg) .unsigned, (amt)
            \\shr x(reg) .unsigned, (amt) -> x(reg)
            \\shl x(reg), (amt)
            \\shl x(reg), (amt) -> x(reg)
            ;
        pub const encoding = .{
            Even_Reg(.reg),
            Encoder.init(3, Int(.amt, u5)),
            Encoder.init(8, @as(u4, 0xe)),
            mnemonic_encoder,
            region_encoder,
        };
        pub const ij = Reg(.reg);
        pub const iw = Reg(.reg);

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(12, @as(u2, switch (mnemonic) {
                .shr => 1,
                .shl => 2,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic, amt: placeholders.Any(.amt)) void {
            c.reg32_to_j();
            c.literal_to_k(@intCast(amt.value));
            c.j_shift_k5_to_l(switch (mnemonic) {
                .shr => .right,
                .shl => .left,
                else => unreachable,
            }, .fresh, .flags);
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { // <shrc/shlc> r(src) -> r(dest)
        pub const spec =
            \\shrc r(src, dest)
            \\shrc r(src) -> r(dest)
            \\shlc r(src, dest)
            \\shlc r(src) -> r(dest)
            ;
        pub const encoding = .{
            Reg(.dest),
            Encoder.init(4, Reg(.src)),
            Encoder.init(8, @as(u4, 0xf)),
            mnemonic_encoder,
            region_encoder,
        };
        pub const ij = Reg(.src);
        pub const ik = @as(u4, 0);
        pub const iw = Reg(.dest);

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(12, @as(u2, switch (mnemonic) {
                .shrc => 1,
                .shlc => 2,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic, flags: Flags) void {
            c.reg_to_jl();
            if (flags.carry()) {
                c.neg_one_to_jh();
            } else {
                c.zero_to_jh();
            }
            c.ik_bit_to_k();
            c.jl_shift_k4_to_ll(switch (mnemonic) {
                .shrc => .right,
                .shlc => .left,
                else => unreachable,
            }, .fresh, .flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { // <sat/rat>
        pub const spec =
            \\sat.w x(data) -> .d x(addr)
            \\sat.r x(data) -> .d x(addr)
            \\sat.s x(data) -> .s x(addr)
            \\sat.i x(data) -> .i x(addr)
            \\rat.w .d x(addr), r(data)
            \\rat.r .d x(addr), r(data)
            \\rat.s .s x(addr), r(data)
            \\rat.i .i x(addr), r(data)
            ;
        pub const encoding = .{
            Even_Reg(.data),
            Encoder.init(3, Even_Reg(.addr)),
            suffix_encoder,
            mnemonic_encoder,
            Encoder.init(9, @as(u5, 0x11)),
            region_encoder,
        };
        pub const ij = Reg(.addr);
        pub const ik = Reg(.data);

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(8, @as(u1, switch (mnemonic) {
                .sat => 0,
                .rat => 1,
                else => unreachable,
            }));
        }

        fn suffix_encoder(suffix: isa.Mnemonic_Suffix) Encoder {
            return Encoder.init(6, @as(u2, switch (suffix) {
                .r => 0,
                .w => 1,
                .s => 2,
                .i => 3,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, flags: Flags) void {
            if (!flags.kernel()) return c.illegal_instruction();

            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ij_from_ik();
            c.next(update_translation);
        }

        pub fn update_translation(c: *Cycle, mnemonic: isa.Mnemonic, suffix: isa.Mnemonic_Suffix) void {
            const group: arch.hw.addr.translation.Entry_Group = switch (suffix) {
                .w => .data_write,
                .r => .data_read,
                .s => .stack,
                .i => .insn,
                else => unreachable,
            };

            c.reg32_to_l();
            switch (mnemonic) {
                .sat => c.update_address_translation_from_l(.temp_1, group),
                .rat => c.invalidate_address_translation_from_l(.temp_1, group),
                else => unreachable,
            }
            c.exec_next_insn();
        }
    },
};

const Cycle = @import("../compile/Cycle.zig");
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
const placeholders = @import("../compile/placeholders.zig");
const Control_Signals = arch.Control_Signals;
const Flags = arch.microcode.Flags;
const isa = @import("isa");
const arch = @import("arch");
const std = @import("std");
