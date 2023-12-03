pub const instructions = .{
    struct { pub const spec = "c stat -> r(dest)";
        pub const encoding = .{
            opcodes.Lo12.copy_stat_to_reg16,
            Encoder.shifted(12, Reg(.dest)),
        };
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.stat_to_ll();
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = "c r(src) -> stat";
        pub const encoding = .{
            opcodes.Lo12.copy_reg16_to_stat_zncv,
            Encoder.shifted(12, Reg(.src)),
        };
        pub const ij = Reg(.src);

        pub fn entry(c: *Cycle) void {
            c.reg_to_jl();
            c.jl_to_ll();
            c.ll_to_stat_zncv();
            c.load_and_exec_next_insn();
        }
    },
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
    struct { pub const spec = // c (imm_u4) -> <reg>
        \\c (imm) -> r(dest)
        \\c (imm) -> x(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Int(.imm, u4)),
            Encoder.shifted(12, Reg(.dest)),
        };
        pub const ij = Int(.imm, u4);
        pub const ik: u4 = 0;
        pub const iw = Reg(.dest);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[2].base) {
                .reg16 => .copy_u4_reg16,
                .reg32 => .copy_u4_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, params: []const Parameter.Signature) void {
            c.zero_to_j();
            c.ik_ij_zx_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            switch (params[2].base) {
                .reg16 => c.ll_to_reg(),
                .reg32 => c.l_to_reg32(),
                else => unreachable,
            }
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // c (imm_n4) -> <reg>
        \\c (imm) -> r(dest)
        \\c (imm) -> x(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(8, Imm),
            Encoder.shifted(12, Reg(.dest)),
        };
        const Imm = Range(.imm, -16, -1);
        pub const iw = Reg(.dest);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo8 {
            return switch (params[2].base) {
                .reg16 => .copy_n4_reg16,
                .reg32 => .copy_n4_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, imm: Imm, params: []const Parameter.Signature) void {
            c.zero_to_j();
            c.literal_to_k(@intCast(imm.value));
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            switch (params[2].base) {
                .reg16 => c.ll_to_reg(),
                .reg32 => c.l_to_reg32(),
                else => unreachable,
            }
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // c (imm_bit) -> r(dest)
        \\c (imm) -> r(dest)
        ;
        pub const encoding = .{
            opcodes.Lo8.copy_imm_bit_reg16,
            Encoder.shifted(8, Reg(.dest)),
            Encoder.shifted(12, Imm),
        };
        pub const constraints = .{
            .{ .imm, .greater_or_equal, 16 },
        };
        const Imm = Options(.imm, .{ 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768 });
        pub const ik = Imm;
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.ik_bit_to_k();
            c.k_to_ll();
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // c (imm8u) -> <reg>
        \\c (imm) -> r(dest)
        \\c (imm) -> x(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.dest)),
            Encoder.shifted(16, Int(.imm, u8)),
        };
        pub const iw = Reg(.dest);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo12 {
            return switch (params[2].base) {
                .reg16 => .copy_imm8u_reg16,
                .reg32 => .copy_imm8u_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, params: []const Parameter.Signature) void {
            c.ip_read_to_d(2, .byte);
            c.d_to_l(.zx);
            switch (params[2].base) {
                .reg16 => c.ll_to_reg(),
                .reg32 => c.l_to_reg32(),
                else => unreachable,
            }
            c.next(load_and_exec_next_insn);
        }

        pub fn load_and_exec_next_insn(c: *Cycle) void {
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // c (imm16u) -> <reg>
        \\c (imm) -> r(dest)
        \\c (imm) -> x(dest)
        ;
        pub const encoding = .{
            opcode,
            Encoder.shifted(12, Reg(.dest)),
            Encoder.shifted(16, Int(.imm, u16)),
        };
        pub const iw = Reg(.dest);

        fn opcode(params: []const Parameter.Signature) opcodes.Lo12 {
            return switch (params[2].base) {
                .reg16 => .copy_imm16_reg16,
                .reg32 => .copy_imm16u_reg32,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, params: []const Parameter.Signature) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            switch (params[2].base) {
                .reg16 => c.ll_to_reg(),
                .reg32 => c.l_to_reg32(),
                else => unreachable,
            }
            c.next(load_and_exec_next_insn);
        }

        pub fn load_and_exec_next_insn(c: *Cycle) void {
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // c (imm16s) -> r(dest)
        \\c (imm) -> r(dest)
        ;
        pub const encoding = .{
            opcodes.Lo12.copy_imm16_reg16,
            Encoder.shifted(12, Reg(.dest)),
            Encoder.shifted(16, Int(.imm, i16)),
        };
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.ll_to_reg();
            c.next(load_and_exec_next_insn);
        }

        pub fn load_and_exec_next_insn(c: *Cycle) void {
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // c (imm16n) -> x(dest)
        \\c (imm) -> x(dest)
        ;
        pub const encoding = .{
            opcodes.Lo12.copy_imm16n_reg32,
            Encoder.shifted(12, Reg(.dest)),
            Encoder.shifted(16, Range(.imm, -0x10000, -1)),
        };
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(._1x);
            c.l_to_reg32();
            c.next(load_and_exec_next_insn);
        }

        pub fn load_and_exec_next_insn(c: *Cycle) void {
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = "c (imm) -> x(dest)";
        pub const encoding = .{
            opcodes.Lo12.copy_imm32_reg32,
            Encoder.shifted(12, Reg(.dest)),
            Encoder.shifted(16, Int(.imm, u32)),
        };
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(4, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(load_imm_low);
        }

        pub fn load_imm_low(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_ll();
            c.srl_to_lh(.temp_1);
            c.l_to_reg32();
            c.next(load_and_exec_next_insn);
        }

        pub fn load_and_exec_next_insn(c: *Cycle) void {
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = "c (imm) -> x(dest)";
        pub const encoding = .{
            opcodes.Lo12.copy_imm32_reg32,
            Encoder.shifted(12, Reg(.dest)),
            Encoder.shifted(16, Int(.imm, i32)),
        };
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(4, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(load_imm_low);
        }

        pub fn load_imm_low(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_ll();
            c.srl_to_lh(.temp_1);
            c.l_to_reg32();
            c.next(load_and_exec_next_insn);
        }

        pub fn load_and_exec_next_insn(c: *Cycle) void {
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = "c r(a) -> r(b) -> r(c)";
        pub const encoding = .{
            opcodes.Lo12.copy_reg16_reg16_reg16,
            Encoder.shifted(12, Reg(.a)),
            Encoder.shifted(16, Reg(.b)),
            Encoder.shifted(20, @as(u4, 0)),
            Encoder.shifted(24, Reg(.c)),
            Encoder.shifted(28, @as(u4, 0)),
        };
        pub const ij = Reg(.a);

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_dr(2, .byte);
            c.decode_dr_to_ij_ik_iw(.alt);
            c.reg_to_jl();
            c.jl_to_ll();
            c.zero_to_lh();
            c.l_to_sr(.temp_1);
            c.next(rb_to_rc);
        }

        pub fn rb_to_rc(c: *Cycle) void {
            c.ip_read_to_dr(1, .byte);
            c.decode_dr_to_ij_ik_iw(.alt);
            c.reg_to_jl();
            c.jl_to_ll();
            c.ll_to_reg();
            c.next(ra_to_rb);
        }

        pub fn ra_to_rb(c: *Cycle) void {
            c.srl_to_ll(.temp_1);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = "c x(a) -> x(b) -> x(c)";
        pub const encoding = .{
            opcodes.Lo12.copy_reg32_reg32_reg32,
            Encoder.shifted(12, Reg(.a)),
            Encoder.shifted(16, Reg(.b)),
            Encoder.shifted(20, @as(u4, 0)),
            Encoder.shifted(24, Reg(.c)),
            Encoder.shifted(28, @as(u4, 0)),
        };
        pub const ij = Reg(.a);

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_dr(2, .byte);
            c.decode_dr_to_ij_ik_iw(.alt);
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.next(xb_to_xc);
        }

        pub fn xb_to_xc(c: *Cycle) void {
            c.ip_read_to_dr(1, .byte);
            c.decode_dr_to_ij_ik_iw(.alt);
            c.reg32_to_l();
            c.l_to_reg32();
            c.next(xa_to_xb);
        }

        pub fn xa_to_xb(c: *Cycle) void {
            c.sr_to_l(.temp_1);
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = "swap r(a), r(b)";
        pub const transform = "c r(a) -> r(b) -> r(c)";
        pub const conversions = .{
            .{ .a, .a },
            .{ .b, .b },
            .{ .a, .c },
        };
    },
    struct { pub const spec = "swap x(a), x(b)";
        pub const transform = "c x(a) -> x(b) -> x(c)";
        pub const conversions = .{
            .{ .a, .a },
            .{ .b, .b },
            .{ .a, .c },
        };
    },
};

const Cycle = @import("Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Flags = arch.hw.microcode.Flags;
const Control_Signals = arch.hw.Control_Signals;
const Parameter = isa.Parameter;
const Int = placeholders.Int;
const Range = placeholders.Range;
const Options = placeholders.Options;
const Reg = placeholders.Reg;
const placeholders = @import("placeholders.zig");
const opcodes = @import("opcodes.zig");
const isa = arch.isa;
const arch = @import("lib_arch");
