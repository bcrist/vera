const region_encoder = Encoder.shifted(14, @as(u2, 1));

pub const instructions = .{
    struct { pub const spec = // ld .d x(base) + (imm4) -> b(dest)
        \\ld .d x(base) + (imm) -> b(dest) .unsigned
        \\ld .d x(base) + (imm) -> b(dest) .signed
        ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.shifted(3, Even_Reg(.base)),
            Encoder.shifted(6, Imm),
            signedness_encoder,
            Encoder.shifted(11, @as(u3, 0)),
            region_encoder,
        };
        const Imm = Int(.imm, u4);
        pub const ij = Reg(.base);
        pub const ik = Imm;
        pub const iw = Reg(.dest);

        fn signedness_encoder(params: []const Parameter.Signature) Encoder {
            return Encoder.shifted(10, @as(u1, switch (params[2].base.reg8.?) {
                .unsigned => 0,
                .signed => 1,
            }));
        }

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ij_from_ik();
            c.next_ik(0);
            c.next(@This().load);
        }

        pub usingnamespace load_from_temp_1(.ik_ij_sx);
    },
    struct { pub const spec = // ld .d x(base) + (imm4) -> r(dest)
        \\ld .d x(base) + (imm) -> r(dest)
        ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.shifted(3, Even_Reg(.base)),
            Encoder.shifted(6, Imm),
            Encoder.shifted(10, @as(u4, 2)),
            region_encoder,
        };
        const Imm = Int_Mult(.imm, u4, 2);
        pub const ij = Reg(.base);
        pub const ik = Imm;
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle, imm: Imm) void {
            c.reg32_to_j();
            c.literal_to_k(@intCast(imm.value & ~@as(i64, 0xf)));
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ij_from_ik();
            c.next_ik(0);
            c.next(@This().load);
        }

        pub usingnamespace load_from_temp_1(.ik_ij_sx);
    },
    struct { pub const spec = // ld .d x(base) + (imm4) -> x(dest)
        \\ld .d x(base) + (imm) -> x(dest)
        ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.shifted(3, Even_Reg(.base)),
            Encoder.shifted(6, Imm),
            Encoder.shifted(10, @as(u4, 3)),
            region_encoder,
        };
        const Imm = Int_Mult(.imm, u4, 4);
        pub const ij = Reg(.base);
        pub const ik = Imm;
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle, imm: Imm) void {
            c.reg32_to_j();
            c.literal_to_k(@intCast(imm.value & ~@as(i64, 0xf)));
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ij_from_ik();
            c.next_ik(0);
            c.next(@This().load);
        }

        pub usingnamespace load_from_temp_1(.ik_ij_sx);
    },
    struct { pub const spec = // st b(src) -> .d x(base) + (imm4)
        \\st b(src) -> .d x(base) + (imm)
        ;
        pub const encoding = .{
            Even_Reg(.src),
            Encoder.shifted(3, Even_Reg(.base)),
            Encoder.shifted(6, Imm),
            Encoder.shifted(10, @as(u4, 4)),
            region_encoder,
        };
        const Imm = Int(.imm, u4);
        pub const ij = Reg(.base);
        pub const ik = Imm;
        pub const iw = Reg(.src);

        pub fn entry(c: *Cycle, imm: Imm) void {
            c.reg32_to_j();
            c.literal_to_k(@intCast(imm.value));
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ik_from_iw();
            c.next(@This().store);
        }

        pub usingnamespace store_from_temp_1(.zero);
    },
    struct { pub const spec = // st r(src) -> .d x(base) + (imm4)
        \\st r(src) -> .d x(base) + (imm)
        ;
        pub const encoding = .{
            Even_Reg(.src),
            Encoder.shifted(3, Even_Reg(.base)),
            Encoder.shifted(6, Imm),
            Encoder.shifted(10, @as(u4, 5)),
            region_encoder,
        };
        const Imm = Int_Mult(.imm, u4, 2);
        pub const ij = Reg(.base);
        pub const ik = Imm;
        pub const iw = Reg(.src);

        pub fn entry(c: *Cycle, imm: Imm) void {
            c.reg32_to_j();
            c.literal_to_k(@intCast(imm.value));
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ik_from_iw();
            c.next(@This().store);
        }

        pub usingnamespace store_from_temp_1(.zero);
    },
    struct { pub const spec = // st x(src) -> .d x(base) + (imm4)
        \\st x(src) -> .d x(base) + (imm)
        ;
        pub const encoding = .{
            Even_Reg(.src),
            Encoder.shifted(3, Even_Reg(.base)),
            Encoder.shifted(6, Imm),
            Encoder.shifted(10, @as(u4, 6)),
            region_encoder,
        };
        const Imm = Int_Mult(.imm, u4, 4);
        pub const ij = Reg(.base);
        pub const ik = Imm;
        pub const iw = Reg(.src);

        pub fn entry(c: *Cycle, imm: Imm) void {
            c.reg32_to_j();
            c.literal_to_k(@intCast(imm.value));
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ik_from_iw();
            c.next(@This().store);
        }

        pub usingnamespace store_from_temp_1(.zero);
    },
    struct { pub const spec = // ld .s sp + (imm4u_m2) -> <reg>
        \\ld .s sp + (imm) -> r(dest)
        \\ld .s sp + (imm) -> x(dest)
        ;
        pub const encoding = .{
            Int_Mult(.imm, u6, 2),
            Encoder.shifted(6, Even_Reg(.dest)),
            width_encoder,
            Encoder.shifted(10, @as(u4, 0xf)),
            region_encoder,
        };
        pub const ik_ij = Int(.imm, u7);
        pub const iw = Reg(.dest);

        fn width_encoder(params: []const Parameter.Signature) Encoder {
            return Encoder.shifted(9, @as(u1, switch (params[2].base) {
                .reg16 => 0,
                .reg32 => 1,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.ik_ij_sx_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(@This().load);
        }

        pub usingnamespace load_from_temp_1(.zero);
    },
    struct { pub const spec = // st <reg> -> .s sp + (imm4u_m2)
        \\st r(src) -> .s sp + (imm)
        \\st x(src) -> .s sp + (imm)
        ;
        pub const encoding = .{
            Int_Mult(.imm, u6, 2),
            Encoder.shifted(6, Even_Reg(.src)),
            width_encoder,
            Encoder.shifted(10, @as(u4, 0x7)),
            region_encoder,
        };
        pub const ik_ij = Int(.imm, u7);
        pub const iw = Reg(.src);

        fn width_encoder(params: []const Parameter.Signature) Encoder {
            return Encoder.shifted(9, @as(u1, switch (params[0].base) {
                .reg16 => 0,
                .reg32 => 1,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.ik_ij_sx_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(@This().store);
        }

        pub usingnamespace store_from_temp_1(.zero);
    },
    struct { pub const spec = // ld .d x(base) + r(offset) .unsigned -> <reg>
        \\ld .d x(base) + r(offset) .unsigned -> b(dest) .unsigned
        \\ld .d x(base) + r(offset) .unsigned -> b(dest) .signed
        \\ld .d x(base) + r(offset) .unsigned -> r(dest)
        \\ld .d x(base) + r(offset) .unsigned -> x(dest)
        ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.shifted(3, Even_Reg(.base)),
            Encoder.shifted(6, Even_Reg(.offset)),
            dest_encoder,
            Encoder.shifted(11, @as(u3, 4)),
            region_encoder,
        };
        pub const ij = Reg(.base);
        pub const ik = Reg(.offset);
        pub const iw = Reg(.dest);

        fn dest_encoder(params: []const Parameter.Signature) Encoder {
            return Encoder.shifted(9, @as(u2, switch (params[2].base) {
                .reg8 => |signedness| switch (signedness.?) {
                    .unsigned => 0,
                    .signed => 1,
                },
                .reg16 => 2,
                .reg32 => 3,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle) void {
            c.reg32_to_j();
            c.reg_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ij_from_ik();
            c.next_ik(0);
            c.next(@This().load);
        }

        pub usingnamespace load_from_temp_1(.ik_ij_sx);
    },
    struct { pub const spec = // st <reg> -> .d x(base) + r(offset) .unsigned
        \\st b(src) -> .d x(base) + r(offset) .unsigned
        \\st r(src) -> .d x(base) + r(offset) .unsigned
        \\st x(src) -> .d x(base) + r(offset) .unsigned
        ;
        pub const encoding = .{
            Even_Reg(.src),
            Encoder.shifted(3, Even_Reg(.base)),
            Encoder.shifted(6, Even_Reg(.offset)),
            src_encoder,
            Encoder.shifted(11, @as(u3, 5)),
            region_encoder,
        };
        const Imm = Int(.imm, u4);
        pub const ij = Reg(.base);
        pub const ik = Reg(.offset);
        pub const iw = Reg(.src);

        fn src_encoder(params: []const Parameter.Signature) Encoder {
            return Encoder.shifted(9, @as(u2, switch (params[0].base) {
                .reg8 => 0,
                .reg16 => 2,
                .reg32 => 3,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle) void {
            c.reg32_to_j();
            c.reg_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ik_from_iw();
            c.next(@This().store);
        }

        pub usingnamespace store_from_temp_1(.zero);
    },
    struct { pub const spec = // push r(a), r(b)
        \\push r(a), r(b)
        ;
        pub const encoding = .{
            Reg(.a),
            Encoder.shifted(4, Reg(.b)),
            Encoder.shifted(8, @as(u6, 0x2a)),
            region_encoder,
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);

        pub fn entry(c: *Cycle) void {
            c.reg_to_jl();
            c.jl_to_ll();
            c.write_from_ll(.sp, -2, .word, .stack);
            c.next(write_b);
        }

        pub fn write_b(c: *Cycle) void {
            c.reg_to_k();
            c.k_to_ll();
            c.write_from_ll(.sp, -4, .word, .stack);
            c.next(modify_sp);
        }

        pub fn modify_sp(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.literal_to_k(4);
            c.j_minus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.sp);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // pop r(b), r(a)
        \\pop r(b), r(a)
        ;
        pub const encoding = .{
            Reg(.a),
            Encoder.shifted(4, Reg(.b)),
            Encoder.shifted(8, @as(u6, 0x2b)),
            region_encoder,
        };
        pub const ik = Reg(.a);
        pub const iw = Reg(.b);

        pub fn entry(c: *Cycle) void {
            c.read_to_d(.sp, 0, .word, .stack);
            c.d_to_l(.zx);
            c.ll_to_reg();
            c.next_iw_from_ik();
            c.next(read_b);
        }

        pub fn read_b(c: *Cycle) void {
            c.read_to_d(.sp, 2, .word, .stack);
            c.d_to_l(.zx);
            c.ll_to_reg();
            c.next(modify_sp);
        }

        pub fn modify_sp(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.literal_to_k(4);
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.sp);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // ld .d <uxp/kxp> + (imm4u_m2) -> r(dest)
        \\ld .d uxp + (imm) -> r(dest)
        \\ld .d kxp + (imm) -> r(dest)
        ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.shifted(3, Int_Mult(.imm, u4, 2)),
            base_encoder,
            Encoder.shifted(8, @as(u6, 0x30)),
            region_encoder,
        };
        pub const ik_ij = Int(.imm, u5);
        pub const iw = Reg(.dest);

        fn base_encoder(params: []const Parameter.Signature) Encoder {
            return Encoder.shifted(7, @as(u1, switch (params[0].base.sr) {
                .uxp => 0,
                .kxp => 1,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, params: []const Parameter.Signature, flags: Flags) void {
            if (params[0].base.sr != .uxp and !flags.kernel()) {
                return c.illegal_instruction();
            }
            c.sr_to_j(switch (params[0].base.sr) {
                .uxp => .uxp,
                .kxp => .kxp,
                else => unreachable,
            });
            c.ik_ij_sx_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(@This().load);
        }

        pub usingnamespace load_from_temp_1(.zero);
    },
    struct { pub const spec = // ld .d <uxp/kxp> + (imm4u_m4) -> x(dest)
        \\ld .d uxp + (imm) -> x(dest)
        \\ld .d kxp + (imm) -> x(dest)
        ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.shifted(3, Int_Mult(.imm, u4, 4)),
            base_encoder,
            Encoder.shifted(8, @as(u6, 0x31)),
            region_encoder,
        };
        pub const ik_ij = Int(.imm, u6);
        pub const iw = Reg(.dest);

        fn base_encoder(params: []const Parameter.Signature) Encoder {
            return Encoder.shifted(7, @as(u1, switch (params[0].base.sr) {
                .uxp => 0,
                .kxp => 1,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, params: []const Parameter.Signature, flags: Flags) void {
            if (params[0].base.sr != .uxp and !flags.kernel()) {
                return c.illegal_instruction();
            }
            c.sr_to_j(switch (params[0].base.sr) {
                .uxp => .uxp,
                .kxp => .kxp,
                else => unreachable,
            });
            c.ik_ij_sx_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(@This().load);
        }

        pub usingnamespace load_from_temp_1(.zero);
    },
    struct { pub const spec = // [i]ld[i] .d x(src) -> <reg>
        \\ldi .d x(src) -> b(dest) .unsigned
        \\ldi .d x(src) -> b(dest) .signed
        \\ldi .d x(src) -> r(dest)
        \\ldi .d x(src) -> x(dest)
        \\ild .d x(src) -> b(dest) .unsigned
        \\ild .d x(src) -> b(dest) .signed
        \\ild .d x(src) -> r(dest)
        \\ild .d x(src) -> x(dest)
        ;
        pub const encoding = .{
            Even_Reg(.src),
            Encoder.shifted(3, Even_Reg(.dest)),
            dest_encoder,
            mnemonic_encoder,
            Encoder.shifted(9, @as(u5, 0x19)),
            region_encoder,
        };
        pub const ij = Reg(.src);
        pub const ik = Reg(.dest);
        pub const iw = Reg(.src);

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.shifted(8, @as(u1, switch (mnemonic) {
                .ldi => 0,
                .ild => 1,
                else => unreachable,
            }));
        }

        fn dest_encoder(params: []const Parameter.Signature) Encoder {
            return Encoder.shifted(6, @as(u2, switch (params[2].base) {
                .reg8 => |signedness| switch (signedness.?) {
                    .unsigned => 0,
                    .signed => 1,
                },
                .reg16 => 2,
                .reg32 => 3,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic, params: []const Parameter.Signature) void {
            c.reg32_to_j();
            c.literal_to_k(switch (params[2].base) {
                .reg8 => 1,
                .reg16 => 2,
                .reg32 => 4,
                else => unreachable,
            });
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_reg32();
            c.l_to_sr(.temp_1);
            c.next_iw_from_ik();
            c.load_next_insn();
            if (mnemonic == .ldi) {
                c.next(load_before_increment);
            } else {
                c.next(@This().load);
            }
        }

        pub usingnamespace load_from_temp_1(.zero);

        const before_increment = load_from_temp_1(.minus_width);
        pub const load_before_increment = before_increment.load;
        pub const load_high_before_increment = before_increment.load_high;
    },
    struct { pub const spec = // [i]st[i] <reg> -> .d x(dest)
        \\sti b(src) -> .d x(dest)
        \\sti r(src) -> .d x(dest)
        \\sti x(src) -> .d x(dest)
        \\ist b(src) -> .d x(dest)
        \\ist r(src) -> .d x(dest)
        \\ist x(src) -> .d x(dest)
        ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.shifted(3, Even_Reg(.src)),
            src_encoder,
            mnemonic_encoder,
            Encoder.shifted(9, @as(u5, 0x1a)),
            region_encoder,
        };
        pub const ij = Reg(.dest);
        pub const ik = Reg(.src);
        pub const iw = Reg(.dest);

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.shifted(8, @as(u1, switch (mnemonic) {
                .sti => 0,
                .ist => 1,
                else => unreachable,
            }));
        }

        fn src_encoder(params: []const Parameter.Signature) Encoder {
            return Encoder.shifted(6, @as(u2, switch (params[0].base) {
                .reg8 => 0,
                .reg16 => 2,
                .reg32 => 3,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic, params: []const Parameter.Signature) void {
            c.reg32_to_j();
            c.literal_to_k(switch (params[0].base) {
                .reg8 => 1,
                .reg16 => 2,
                .reg32 => 4,
                else => unreachable,
            });
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_reg32();
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            if (mnemonic == .ldi) {
                c.next(store_before_increment);
            } else {
                c.next(@This().store);
            }
        }

        pub usingnamespace store_from_temp_1(.zero);

        const before_increment = store_from_temp_1(.minus_width);
        pub const store_before_increment = before_increment.store;
        pub const store_high_before_increment = before_increment.store_high;
    },
    struct { pub const spec = // ld .i x(src) -> <reg>
        \\ld .i x(src) -> r(dest)
        \\ld .i x(src) -> x(dest)
        ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.shifted(3, Even_Reg(.src)),
            dest_encoder,
            Encoder.shifted(7, @as(u7, 0x6c)),
            region_encoder,
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        fn dest_encoder(params: []const Parameter.Signature) Encoder {
            return Encoder.shifted(6, @as(u1, switch (params[2].base) {
                .reg16 => 0,
                .reg32 => 1,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(@This().load);
        }

        pub usingnamespace load_from_temp_1(.zero);
    },
    struct { pub const spec = // ld .i ip + r(offset) .signed -> <reg>
        \\ld .i ip + r(offset) .signed -> r(dest)
        \\ld .i ip + r(offset) .signed -> x(dest)
        ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.shifted(3, Even_Reg(.offset)),
            dest_encoder,
            Encoder.shifted(7, @as(u7, 0x6d)),
            region_encoder,
        };
        pub const ik = Reg(.offset);
        pub const iw = Reg(.dest);

        fn dest_encoder(params: []const Parameter.Signature) Encoder {
            return Encoder.shifted(6, @as(u1, switch (params[2].base) {
                .reg16 => 0,
                .reg32 => 1,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.reg_to_k();
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(@This().load);
        }

        pub usingnamespace load_from_temp_1(.zero);
    },
    struct { pub const spec = // ld .s sp + r(offset) .signed -> <reg>
        \\ld .s sp + r(offset) .signed -> b(dest) .unsigned
        \\ld .s sp + r(offset) .signed -> b(dest) .signed
        \\ld .s sp + r(offset) .signed -> r(dest)
        \\ld .s sp + r(offset) .signed -> x(dest)
        ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.shifted(3, Even_Reg(.offset)),
            dest_encoder,
            Encoder.shifted(8, @as(u6, 0x37)),
            region_encoder,
        };
        pub const ik = Reg(.offset);
        pub const iw = Reg(.dest);

        fn dest_encoder(params: []const Parameter.Signature) Encoder {
            return Encoder.shifted(6, @as(u2, switch (params[2].base) {
                .reg8 => |signedness| switch (signedness.?) {
                    .unsigned => 0,
                    .signed => 1,
                },
                .reg16 => 2,
                .reg32 => 3,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.reg_to_k();
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(@This().load);
        }

        pub usingnamespace load_from_temp_1(.zero);
    },
    struct { pub const spec = // st <reg> -> .s sp + r(offset) .signed
        \\st b(src) -> .s sp + r(offset) .signed
        \\st r(src) -> .s sp + r(offset) .signed
        \\st x(src) -> .s sp + r(offset) .signed
        ;
        pub const encoding = .{
            Even_Reg(.src),
            Encoder.shifted(3, Even_Reg(.offset)),
            src_encoder,
            Encoder.shifted(8, @as(u6, 0x38)),
            region_encoder,
        };
        pub const ij = Reg(.src);
        pub const ik = Reg(.offset);

        fn src_encoder(params: []const Parameter.Signature) Encoder {
            return Encoder.shifted(6, @as(u2, switch (params[0].base) {
                .reg8  => 0,
                .reg16 => 2,
                .reg32 => 3,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.reg_to_k();
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next_ik_from_ij();
            c.next(@This().store);
        }

        pub usingnamespace store_from_temp_1(.zero);
    },
    struct { pub const spec = // push x(a), x(b)
        \\push x(a), x(b)
        ;
        pub const encoding = .{
            Even_Reg(.a),
            Encoder.shifted(3, Even_Reg(.b)),
            Encoder.shifted(6, @as(u8, 0xe9)),
            region_encoder,
        };
        pub const ij = Reg(.a);
        pub const ik = Reg(.b);

        pub fn entry(c: *Cycle) void {
            c.reg_to_jl();
            c.jl_to_ll();
            c.write_from_ll(.sp, -4, .word, .stack);
            c.next_ij_xor1();
            c.next(write_a_high);
        }

        pub fn write_a_high(c: *Cycle) void {
            c.reg_to_jl();
            c.jl_to_ll();
            c.write_from_ll(.sp, -2, .word, .stack);
            c.next(write_b);
        }

        pub fn write_b(c: *Cycle) void {
            c.reg_to_k();
            c.k_to_ll();
            c.write_from_ll(.sp, -8, .word, .stack);
            c.next_ik_xor1();
            c.next(write_b_high);
        }

        pub fn write_b_high(c: *Cycle) void {
            c.reg_to_k();
            c.k_to_ll();
            c.write_from_ll(.sp, -6, .word, .stack);
            c.next(modify_sp);
        }

        pub fn modify_sp(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.literal_to_k(8);
            c.j_minus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.sp);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // pop x(b), x(a)
        \\pop x(b), x(a)
        ;
        pub const encoding = .{
            Even_Reg(.a),
            Encoder.shifted(3, Even_Reg(.b)),
            Encoder.shifted(6, @as(u8, 0xed)),
            region_encoder,
        };
        pub const ik = Reg(.a);
        pub const iw = Reg(.b);

        pub fn entry(c: *Cycle) void {
            c.read_to_d(.sp, 0, .word, .stack);
            c.d_to_l(.zx);
            c.ll_to_reg();
            c.next_iw_xor1();
            c.next(read_a_high);
        }

        pub fn read_a_high(c: *Cycle) void {
            c.read_to_d(.sp, 2, .word, .stack);
            c.d_to_l(.zx);
            c.ll_to_reg();
            c.next_iw_from_ik();
            c.next(read_b);
        }

        pub fn read_b(c: *Cycle) void {
            c.read_to_d(.sp, 4, .word, .stack);
            c.d_to_l(.zx);
            c.ll_to_reg();
            c.next_iw_xor1();
            c.next(read_b_high);
        }

        pub fn read_b_high(c: *Cycle) void {
            c.read_to_d(.sp, 6, .word, .stack);
            c.d_to_l(.zx);
            c.ll_to_reg();
            c.next(modify_sp);
        }

        pub fn modify_sp(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.literal_to_k(8);
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.sp);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // push <reg>
        \\push r(reg)
        \\push x(reg)
        ;
        pub const encoding = .{
            Reg(.reg),
            width_encoder,
            Encoder.shifted(5, @as(u9, 0x1d0)),
            region_encoder,
        };
        pub const ij = Reg(.reg);
        pub const ik: u4 = 0;

        fn width_encoder(params: []const Parameter.Signature) Encoder {
            return Encoder.shifted(4, @as(u1, switch (params[0].base) {
                .reg16 => 0,
                .reg32 => 1,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, params: []const Parameter.Signature) void {
            c.reg_to_jl();
            c.jl_to_ll();
            if (params[0].base == .reg32) {
                c.write_from_ll(.sp, -4, .word, .stack);
                c.next_ij_xor1();
                c.next(write_high);
            } else {
                c.write_from_ll(.sp, -2, .word, .stack);
                c.next_ij(2);
                c.next(modify_sp);
            }
        }

        pub fn write_high(c: *Cycle) void {
            c.reg_to_jl();
            c.jl_to_ll();
            c.write_from_ll(.sp, -2, .word, .stack);
            c.next_ij(4);
            c.next(modify_sp);
        }

        pub fn modify_sp(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.ik_ij_sx_to_k();
            c.j_minus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.sp);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // pop x(b), x(a)
        \\pop r(reg)
        \\pop x(reg)
        ;
        pub const encoding = .{
            Reg(.reg),
            width_encoder,
            Encoder.shifted(5, @as(u9, 0x1d8)),
            region_encoder,
        };
        pub const ik_ij = bytes_encoder;
        pub const iw = Reg(.reg);

        fn width_encoder(params: []const Parameter.Signature) Encoder {
            return Encoder.shifted(4, @as(u1, switch (params[0].base) {
                .reg16 => 0,
                .reg32 => 1,
                else => unreachable,
            }));
        }
        fn bytes_encoder(params: []const Parameter.Signature) u3 {
            return switch (params[0].base) {
                .reg16 => 2,
                .reg32 => 4,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, params: []const Parameter.Signature) void {
            c.read_to_d(.sp, 0, .word, .stack);
            c.d_to_l(.zx);
            c.ll_to_reg();
            if (params[0].base == .reg32) {
                c.next_iw_xor1();
                c.next(read_high);
            } else {
                c.next(modify_sp);
            }
        }

        pub fn read_high(c: *Cycle) void {
            c.read_to_d(.sp, 2, .word, .stack);
            c.d_to_l(.zx);
            c.ll_to_reg();
            c.next(modify_sp);
        }

        pub fn modify_sp(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.ik_ij_sx_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.sp);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // [un]frame (imm6u*2)
        \\unframe (imm)
        \\frame (imm)
        ;
        pub const encoding = .{
            Imm,
            mnemonic_encoder,
            Encoder.shifted(7, @as(u7, 0x73)),
            region_encoder,
        };
        const Imm = Int(.imm, u6);
        pub const iw_ik_ij = Imm;

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.shifted(6, @as(u1, switch (mnemonic) {
                .frame => 0,
                .unframe => 1,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.sr_to_j(.sp);
            c.iw_ik_ij_zx_to_k();
            switch (mnemonic) {
                .frame => c.j_minus_k_to_l(.zx, .fresh, .no_flags),
                .unframe => c.j_plus_k_to_l(.zx, .fresh, .no_flags),
                else => unreachable,
            }
            c.l_to_sr(.sp);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // [un]frame (imm)
        \\unframe (imm)
        \\frame (imm)
        ;
        pub const encoding = .{
            @as(u6, 0x20),
            mnemonic_encoder,
            Encoder.shifted(7, @as(u7, 0x72)),
            region_encoder,
            Encoder.shifted(16, Int(.imm, u16)),
        };

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.shifted(6, @as(u1, switch (mnemonic) {
                .frame => 0,
                .unframe => 1,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_2);
            c.next(modify_sp);
        }

        pub fn modify_sp(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.sr_to_j(.sp);
            c.srl_to_k(.temp_2);
            switch (mnemonic) {
                .frame => c.j_minus_k_to_l(.zx, .fresh, .no_flags),
                .unframe => c.j_plus_k_to_l(.zx, .fresh, .no_flags),
                else => unreachable,
            }
            c.l_to_sr(.sp);
            c.load_and_exec_next_insn();
        }
    },
    struct { pub const spec = // ld .s sp + (imm16s) -> <reg>
        \\ld .s sp + (imm) -> b(dest) .unsigned
        \\ld .s sp + (imm) -> b(dest) .signed
        \\ld .s sp + (imm) -> r(dest)
        \\ld .s sp + (imm) -> x(dest)
        ;
        pub const encoding = .{
            Even_Reg(.dest),
            dest_encoder,
            Encoder.shifted(5, @as(u9, 0x1c8)),
            region_encoder,
            Encoder.shifted(16, Int(.imm, i16)),
        };
        pub const iw = Reg(.dest);

        fn dest_encoder(params: []const Parameter.Signature) Encoder {
            return Encoder.shifted(3, @as(u2, switch (params[2].base) {
                .reg8 => |signedness| switch (signedness.?) {
                    .unsigned => 0,
                    .signed => 1,
                },
                .reg16 => 2,
                .reg32 => 3,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.sx);
            c.l_to_sr(.temp_2);
            c.next(compute_addr);
        }

        pub fn compute_addr(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.srl_to_k(.temp_2);
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(@This().load);
        }

        pub usingnamespace load_from_temp_1(.zero);
    },
    struct { pub const spec = // st <reg> -> .s sp + (imm16s)
        \\st b(src) -> .s sp + (imm)
        \\st r(src) -> .s sp + (imm)
        \\st x(src) -> .s sp + (imm)
        ;
        pub const encoding = .{
            Even_Reg(.src),
            src_encoder,
            Encoder.shifted(5, @as(u9, 0x1ca)),
            region_encoder,
            Encoder.shifted(16, Int(.imm, i16)),
        };
        pub const ik = Reg(.src);

        fn src_encoder(params: []const Parameter.Signature) Encoder {
            return Encoder.shifted(3, @as(u2, switch (params[0].base) {
                .reg8 => 0,
                .reg16 => 2,
                .reg32 => 3,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.sx);
            c.l_to_sr(.temp_2);
            c.next(compute_addr);
        }

        pub fn compute_addr(c: *Cycle) void {
            c.sr_to_j(.sp);
            c.srl_to_k(.temp_2);
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(@This().store);
        }

        pub usingnamespace store_from_temp_1(.zero);
    },
};

const Offset_Kind = enum {
    zero,
    ik_ij_sx,
    imm,
    half_imm,
    minus_width,
};
fn load_from_temp_1(comptime offset_kind: Offset_Kind) type {
    return struct {
        pub const load = switch (offset_kind) {
            .zero, .ik_ij_sx, .minus_width => load_without_imm,
            .imm, .half_imm => load_with_imm,
        };

        fn load_without_imm(c: *Cycle, params: []const Parameter.Signature) void {
            load_with_imm(c, params, .{ .value = 0 });
        }

        fn load_with_imm(c: *Cycle, params: []const Parameter.Signature, imm: placeholders.Any(.imm)) void {
            var signedness: std.builtin.Signedness = .unsigned;
            var width: Control_Signals.Bus_Width = .word;
            var width_bytes: u3 = 2;
            const addr_space: Control_Signals.Address_Space = switch(params[0].address_space.?) {
                .data => .data,
                .insn => .insn,
                .stack => .stack,
            };

            switch (params[2].base) {
                .reg8 => |s| {
                    signedness = s.?;
                    width = .byte;
                    width_bytes = 1;
                },
                .reg16 => {},
                .reg32 => width_bytes = 4,
                else => unreachable,
            }
            switch (offset_kind) {
                .zero => c.read_to_d(.temp_1, 0, width, addr_space),
                .ik_ij_sx => c.read_to_d(.temp_1, .ik_ij_sx, width, addr_space),
                .imm => c.read_to_d(.temp_1, imm.value, width, addr_space),
                .half_imm => c.read_to_d(.temp_1, @divExact(imm.value, 2), width, addr_space),
                .minus_width => c.read_to_d(.temp_1, -@as(i4, width_bytes), width, addr_space),
            }
            c.d_to_l(switch (signedness) {
                .unsigned => .zx,
                .signed => .sx,
            });
            c.ll_to_reg();
            if (params[2].base == .reg32) {
                c.next_iw_xor1();
                c.virtual_address_to_sr(.temp_1);
                c.next(load_high);
            } else {
                c.exec_next_insn();
            }
        }

        pub fn load_high(c: *Cycle, params: []const Parameter.Signature) void {
            const addr_space: Control_Signals.Address_Space = switch(params[0].address_space.?) {
                .data => .data,
                .insn => .insn,
                .stack => .stack,
            };

            c.read_to_d(.temp_1, 2, .word, addr_space);
            c.d_to_l(.zx);
            c.ll_to_reg();
            c.exec_next_insn();
        }
    };
}
fn store_from_temp_1(comptime offset_kind: Offset_Kind) type {
    return struct {
        pub const store = switch (offset_kind) {
            .zero, .minus_width => store_without_imm,
            .imm, .half_imm => store_with_imm,
            .ik_ij_sx => unreachable, // we need IK to source the data being stored!
        };

        fn store_without_imm(c: *Cycle, params: []const Parameter.Signature) void {
            store_with_imm(c, params, .{ .value = 0 });
        }

        fn store_with_imm(c: *Cycle, params: []const Parameter.Signature, imm: placeholders.Any(.imm)) void {
            var width: Control_Signals.Bus_Width = .word;
            var width_bytes: u3 = 2;
            const addr_space: Control_Signals.Address_Space = switch(params[2].address_space.?) {
                .data => .data,
                .insn => .insn,
                .stack => .stack,
            };

            switch (params[0].base) {
                .reg8 => {
                    width = .byte;
                    width_bytes = 1;
                },
                .reg16 => {},
                .reg32 => width_bytes = 4,
                else => unreachable,
            }

            c.reg_to_k();
            c.k_to_ll();
            switch (offset_kind) {
                .zero => c.write_from_ll(.temp_1, 0, width, addr_space),
                .ik_ij_sx => c.write_from_ll(.temp_1, .ik_ij_sx, width, addr_space),
                .imm => c.write_from_ll(.temp_1, imm.value, width, addr_space),
                .half_imm => c.write_from_ll(.temp_1, @divExact(imm.value, 2), width, addr_space),
                .minus_width => c.write_from_ll(.temp_1, -@as(i4, width_bytes), width, addr_space),
            }
            if (params[0].base == .reg32) {
                c.virtual_address_to_sr(.temp_1);
                c.next_ik_xor1();
                c.next(store_high);
            } else {
                c.exec_next_insn();
            }
        }

        pub fn store_high(c: *Cycle, params: []const Parameter.Signature) void {
            const addr_space: Control_Signals.Address_Space = switch(params[2].address_space.?) {
                .data => .data,
                .insn => .insn,
                .stack => .stack,
            };

            c.reg_to_k();
            c.k_to_ll();
            c.write_from_ll(.temp_1, 2, .word, addr_space);
            c.exec_next_insn();
        }
    };
}

const Cycle = @import("Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Instruction_Signature = isa.Instruction_Signature;
const Parameter = isa.Parameter;
const Options = placeholders.Options;
const Range = placeholders.Range;
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
