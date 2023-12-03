pub const instructions = .{
    struct { pub const spec = // c x(src) -> <sr>
        \\c x(src) -> sp
        \\c x(src) -> rp
        \\c x(src) -> bp
        \\c x(src) -> uxp
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.src)),
        };
        pub const ij = Reg(.src);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo12 {
            return switch (params[2].base.sr) {
                .sp => .copy_reg32_to_sp,
                .rp => .copy_reg32_to_rp,
                .bp => .copy_reg32_to_bp,
                .uxp => .copy_reg32_to_uxp,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, params: []const Parameter.Signature) void {
            const sr: Control_Signals.Any_SR_Index = switch (params[2].base.sr) {
                .sp => .sp,
                .rp => .rp,
                .bp => .bp,
                .uxp => .uxp,
                else => unreachable,
            };

            c.reg32_to_l();
            c.l_to_sr(sr);
            if (sr.to_sr2_index() != null) {
                // avoid contention on sr2_wi
                c.next(load_and_exec_next_insn);
            } else {
                c.load_and_exec_next_insn();
            }
        }

        pub fn load_and_exec_next_insn(c: *Cycle) void {
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // c x(src) -> <sr> (privileged)
        \\c x(src) -> asn
        \\c x(src) -> kxp
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.src)),
        };
        pub const ij = Reg(.src);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo12 {
            return switch (params[2].base.sr) {
                .asn => .copy_reg32_to_asn,
                .kxp => .copy_reg32_to_kxp,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, flags: Flags, params: []const Parameter.Signature) void {
            if (!flags.kernel()) return c.illegal_instruction();

            const sr: Control_Signals.Any_SR_Index = switch (params[2].base.sr) {
                .asn => .asn,
                .kxp => .kxp,
                else => unreachable,
            };

            c.reg32_to_l();
            c.l_to_sr(sr);
            if (sr.to_sr2_index() != null) {
                // avoid contention on sr2_wi
                c.next(load_and_exec_next_insn);
            } else {
                c.load_and_exec_next_insn();
            }
        }

        pub fn load_and_exec_next_insn(c: *Cycle) void {
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // c <sr> -> x(dest)
        \\c sp -> x(dest)
        \\c rp -> x(dest)
        \\c bp -> x(dest)
        \\c uxp -> x(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.dest)),
        };
        pub const iw = Reg(.dest);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo12 {
            return switch (params[0].base.sr) {
                .sp => .copy_sp_to_reg32,
                .rp => .copy_rp_to_reg32,
                .bp => .copy_bp_to_reg32,
                .uxp => .copy_uxp_to_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, params: []const Parameter.Signature) void {
            c.sr_to_l(switch (params[0].base.sr) {
                .sp => .sp,
                .rp => .rp,
                .bp => .bp,
                .uxp => .uxp,
                else => unreachable,
            });
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // c <sr> -> x(dest) (privileged)
        \\c asn -> x(dest)
        \\c kxp -> x(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.dest)),
        };
        pub const iw = Reg(.dest);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo12 {
            return switch (params[0].base.sr) {
                .asn => .copy_asn_to_reg32,
                .kxp => .copy_kxp_to_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, flags: Flags, params: []const Parameter.Signature) void {
            if (!flags.kernel()) return c.illegal_instruction();

            c.sr_to_l(switch (params[0].base.sr) {
                .asn => .asn,
                .kxp => .kxp,
                else => unreachable,
            });
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = "c r(src) -> r(dest)";
        pub const encoding = .{
            opcodes.Lo8.copy_reg16_reg16,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Reg(.dest)),
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.reg_to_jl();
            c.jl_to_ll();
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = "c x(src) -> x(dest)";
        pub const encoding = .{
            opcodes.Lo8.copy_reg32_reg32,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Reg(.dest)),
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // c r(src) -> x(dest)
        \\c r(src) .unsigned -> x(dest)
        \\c r(src) .signed -> x(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Reg(.src)),
            Encoder.shifted(12, Reg(.dest)),
        };
        pub const ik = Reg(.src);
        pub const iw = Reg(.dest);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[0].base.reg16.?) {
                .unsigned => .copy_reg16u_reg32,
                .signed => .copy_reg16s_reg32,
            };
        }

        pub fn entry(c: *Cycle, params: []const Parameter.Signature) void {
            c.zero_to_j();
            c.reg_to_k();
            c.j_plus_k_to_l(switch (params[0].base.reg16.?) {
                .unsigned => .zx,
                .signed => .sx,
            }, .fresh, .no_flags);
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // c <sr> + (imm16u) -> x(dest)
        \\c ip + (imm) -> x(dest)
        \\c sp + (imm) -> x(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.dest)),
            Encoder.shifted(16, Int(.imm, u16)),
        };
        pub const iw = Reg(.dest);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo12 {
            return switch (params[0].base.sr) {
                .ip => .copy_ip_relative_u16_reg32,
                .sp => .copy_sp_relative_u16_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, params: []const Parameter.Signature) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(switch (params[0].base.sr) {
                .ip => .temp_1,
                .sp => .temp_2,
                else => unreachable
            });
            c.next(copy);
        }

        pub fn copy(c: *Cycle, params: []const Parameter.Signature) void {
            c.sr_to_j(switch (params[0].base.sr) {
                .ip => .ip,
                .sp => .sp,
                else => unreachable,
            });
            c.srl_to_k(switch (params[0].base.sr) {
                .ip => .temp_1,
                .sp => .temp_2,
                else => unreachable
            });
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // c <sr> + (imm16n) -> x(dest)
        \\c ip + (imm) -> x(dest)
        \\c sp + (imm) -> x(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.dest)),
            Encoder.shifted(16, Range(.imm, -0x10000, -1)),
        };
        pub const iw = Reg(.dest);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo12 {
            return switch (params[0].base.sr) {
                .ip => .copy_ip_relative_n16_reg32,
                .sp => .copy_sp_relative_n16_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, params: []const Parameter.Signature) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(switch (params[0].base.sr) {
                .ip => .temp_1,
                .sp => .temp_2,
                else => unreachable
            });
            c.next(copy);
        }

        pub fn copy(c: *Cycle, params: []const Parameter.Signature) void {
            c.sr_to_j(switch (params[0].base.sr) {
                .ip => .ip,
                .sp => .sp,
                else => unreachable,
            });
            c.srl_to_k(switch (params[0].base.sr) {
                .ip => .temp_1,
                .sp => .temp_2,
                else => unreachable
            });
            c.j_plus_k_to_l(._1x, .fresh, .no_flags);
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = "c (imm) -> r(dest)";
        pub const encoding = .{
            opcodes.Lo8.copy_u4_reg16,
            Encoder.shifted(8, Int(.imm, u4)),
            Encoder.shifted(12, Reg(.dest)),
        };
        pub const ij = Int(.imm, u4);
        pub const ik: u4 = 0;
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.ik_ij_zx_to_ll();
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = "c (imm) -> r(dest)";
        const Imm = Range(.imm, -16, -1);
        pub const encoding = .{
            opcodes.Lo8.copy_n4_reg16,
            Encoder.shifted(8, Imm),
            Encoder.shifted(12, Reg(.dest)),
        };
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle, imm: Imm) void {
            c.literal_to_k(@intCast(imm.value));
            c.k_to_ll();
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
};


// pub fn _5E00_5EFF() void {
//     encoding(.C, .{ .immb4u, .to, .Xa });
//     desc("Copy immediate to 32b register");

//     literal_to_L(getParameterConstant(SignedOffsetForLiteral, 0));
//     L_to_op_reg32(.OA);
//     load_and_exec_next_insn(2);
// }
// pub fn _5F00_5FFF() void {
//     encoding(.C, .{ .immb4n, .to, .Xa });
//     desc("Copy immediate to 32b register");

//     literal_to_L(getParameterConstant(SignedOffsetForLiteral, 0));
//     L_to_op_reg32(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _5840_58FF() void {
//     switch (OB()) {
//         0x4 => encoding(.C, .{ .imm_16, .to, .Ra }),
//         0x5 => encoding(.C, .{ .imm_32, .to, .Ra }),
//         0x6 => encoding(.C, .{ .imm_64, .to, .Ra }),
//         0x7 => encoding(.C, .{ .imm_128, .to, .Ra }),
//         0x8 => encoding(.C, .{ .imm_256, .to, .Ra }),
//         0x9 => encoding(.C, .{ .imm_512, .to, .Ra }),
//         0xA => encoding(.C, .{ .imm_1024, .to, .Ra }),
//         0xB => encoding(.C, .{ .imm_2048, .to, .Ra }),
//         0xC => encoding(.C, .{ .imm_4096, .to, .Ra }),
//         0xD => encoding(.C, .{ .imm_8192, .to, .Ra }),
//         0xE => encoding(.C, .{ .imm_16384, .to, .Ra }),
//         0xF => encoding(.C, .{ .imm_32768, .to, .Ra }),
//         else => unreachable,
//     }
//     desc("Copy immediate to 16b register");

//     literal_to_L(getParameterConstant(i17, 0));
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _FB00_FB0F() void {
//     encoding(.C, .{ .imm8u, .to, .Ra });
//     desc("Copy immediate to 16b register");

//     IP_read_to_D(2, .byte);
//     D8_to_LL(.zx);
//     LL_to_op_reg(.OA);
//     next_cycle();

//     load_and_exec_next_insn(3);
// }

// pub fn _FB10_FB1F() void {
//     encoding(.C, .{ .imm8u, .to, .Xa });
//     desc("Copy immediate to 32b register");

//     IP_read_to_D(2, .byte);
//     D_to_L(.zx);
//     L_to_op_reg32(.OA);
//     next_cycle();

//     load_and_exec_next_insn(3);
// }

// pub fn _FB20_FB2F() void {
//     encoding(.C, .{ .imm16u, .to, .RaU });
//     desc("Copy immediate to 16b register");

//     IP_read_to_D(2, .word);
//     D_to_LL();
//     LL_to_op_reg(.OA);
//     next_cycle();

//     load_and_exec_next_insn(4);
// }
// // pub fn _alias_FB20_FB2F_imm15u() void {
// //     encoding(.C, .{ .imm15u, .to, .Ra });
// //     desc("Copy immediate to 16b register");
// // }
// pub fn _alias_FB20_FB2F_imm16s() void {
//     encoding(.C, .{ .imm16s, .to, .RaS });
//     desc("Copy immediate to 16b register");
// }

// pub fn _FB30_FB3F() void {
//     encoding(.C, .{ .imm16u, .to, .Xa });
//     desc("Copy immediate to 32b register");

//     IP_read_to_D(2, .word);
//     D_to_L(.zx);
//     L_to_op_reg32(.OA);
//     next_cycle();

//     load_and_exec_next_insn(4);
// }

// pub fn _FB40_FB4F() void {
//     encoding(.C, .{ .imm16n, .to, .Xa });
//     desc("Copy immediate to 32b register");

//     IP_read_to_D(2, .word);
//     D_to_L(._1x);
//     L_to_op_reg32(.OA);
//     next_cycle();

//     load_and_exec_next_insn(4);
// }

// pub fn _FB50_FB5F() void {
//     encoding(.C, .{ .imm32u, .to, .XaU });
//     desc("Copy immediate to 32b register");

//     IP_read_to_D(2, .word);
//     D_to_L(.zx);
//     L_to_SR(.temp_1);
//     next_cycle();

//     IP_read_to_D(4, .word);
//     D_to_LH();
//     SRL_to_LL(.temp_1);
//     L_to_op_reg32(.OA);
//     next_cycle();

//     load_and_exec_next_insn(6);
// }
// pub fn _alias_FB50_FB5F_signed() void {
//     encoding(.C, .{ .imm32s, .to, .XaS });
//     desc("Copy immediate to 32b register");
// }

// pub fn _FB60_FB6F() void {
//     encoding(.C, .{ .Ra, .to, .Ra1, .to, .Rb1 });
//     desc("16b three register copy or swap");

//     IP_read_to_D(2, .byte);
//     D_to_OB_OA();
//     op_reg_to_L(.OA, .zx);
//     L_to_SR(.temp_1);
//     next_cycle();

//     op_reg_to_LL(.OA);
//     LL_to_op_reg(.OB);
//     next_cycle();

//     SRL_to_LL(.temp_1);
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(3);
// }

// pub fn _FB70_FB7F() void {
//     encoding(.C, .{ .Xa, .to, .Xa1, .to, .Xb1 });
//     desc("32b three register copy or swap");

//     IP_read_to_D(2, .byte);
//     D_to_OB_OA();
//     op_reg32_to_L(.OA);
//     L_to_SR(.temp_1);
//     next_cycle();

//     op_reg32_to_L(.OA);
//     L_to_op_reg32(.OB);
//     next_cycle();

//     SR_to_L(.temp_1);
//     L_to_op_reg32(.OA);
//     load_and_exec_next_insn(3);
// }

// pub fn _5900_59FF() void {
//     encoding(.DUP, .{ .Ra, .to, .Xb });
//     desc("Concatenate 16b register with itself, storing result in 32b register");

//     op_reg_to_JL(.OA);
//     JL_to_LL_and_LH();
//     L_to_op_reg32(.OB);
//     load_and_exec_next_insn(2);
// }

// pub fn _FBD0_FBDF() void {
//     encoding(.C, .{ .STAT, .to, .Ra });
//     desc("Copy status register to 16b register");

//     STAT_to_L();
//     LL_to_op_reg(.OA);
//     load_and_exec_next_insn(2);
// }

// pub fn _FBE0_FBEF() void {
//     encoding(.C, .{ .Ra, .to, .STAT });
//     desc("Copy 16b register to status register (flags only)");

//     op_reg_to_LL(.OA);
//     LL_to_ZNVC();
//     load_and_exec_next_insn(2);
// }

const Cycle = @import("Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Flags = arch.hw.microcode.Flags;
const Control_Signals = arch.hw.Control_Signals;
const Parameter = isa.Parameter;
const Int = placeholders.Int;
const Range = placeholders.Range;
const Reg = placeholders.Reg;
const placeholders = @import("placeholders.zig");
const opcodes = @import("opcodes.zig");
const isa = arch.isa;
const arch = @import("lib_arch");
