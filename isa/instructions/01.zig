const region_encoder = Encoder.init(14, @as(u2, 1));

pub const instructions = .{
    struct { // ld .d x(base) + (imm4u) -> b(dest)
        pub const spec =
            \\ld .d x(base) + (imm) -> b(dest) .unsigned
            \\ld .d x(base) + (imm) -> b(dest) .signed
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.base)),
            Encoder.init(6, Imm),
            signedness_encoder,
            Encoder.init(11, @as(u3, 0)),
            region_encoder,
        };
        const Imm = Int(.imm, u4);
        pub const ij = Reg(.base);
        pub const ik = Imm;
        pub const iw = Reg(.dest);

        fn signedness_encoder(dest: Param(.dest)) Encoder {
            return Encoder.init(10, @as(u1, switch (dest.signature.base.reg8.?) {
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
    struct { // ld .d x(base) + (imm4u * 2) -> r(dest)
        pub const spec =
            \\ld .d x(base) + (imm) -> r(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.base)),
            Encoder.init(6, Imm),
            Encoder.init(10, @as(u4, 2)),
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
    struct { // ld .d x(base) + (imm4u * 4) -> x(dest)
        pub const spec =
            \\ld .d x(base) + (imm) -> x(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.base)),
            Encoder.init(6, Imm),
            Encoder.init(10, @as(u4, 3)),
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
    struct { // st b(src) -> .d x(base) + (imm4u)
        pub const spec =
            \\st b(src) -> .d x(base) + (imm)
            ;
        pub const encoding = .{
            Even_Reg(.src),
            Encoder.init(3, Even_Reg(.base)),
            Encoder.init(6, Imm),
            Encoder.init(10, @as(u4, 4)),
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
    struct { // st r(src) -> .d x(base) + (imm4u * 2)
        pub const spec =
            \\st r(src) -> .d x(base) + (imm)
            ;
        pub const encoding = .{
            Even_Reg(.src),
            Encoder.init(3, Even_Reg(.base)),
            Encoder.init(6, Imm),
            Encoder.init(10, @as(u4, 5)),
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
    struct { // st x(src) -> .d x(base) + (imm4u * 4)
        pub const spec =
            \\st x(src) -> .d x(base) + (imm)
            ;
        pub const encoding = .{
            Even_Reg(.src),
            Encoder.init(3, Even_Reg(.base)),
            Encoder.init(6, Imm),
            Encoder.init(10, @as(u4, 6)),
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
    struct { // ld .s sp + (imm4u * 2) -> <reg>
        pub const spec =
            \\ld .s sp + (imm) -> r(dest)
            \\ld .s sp + (imm) -> x(dest)
            ;
        pub const encoding = .{
            Int_Mult(.imm, u6, 2),
            Encoder.init(6, Even_Reg(.dest)),
            width_encoder,
            Encoder.init(10, @as(u4, 0xf)),
            region_encoder,
        };
        pub const ik_ij = Int(.imm, u7);
        pub const iw = Reg(.dest);

        fn width_encoder(dest: Param(.dest)) Encoder {
            return Encoder.init(9, @as(u1, switch (dest.signature.base) {
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
    struct { // st <reg> -> .s sp + (imm4u * 2)
        pub const spec =
            \\st r(src) -> .s sp + (imm)
            \\st x(src) -> .s sp + (imm)
            ;
        pub const encoding = .{
            Int_Mult(.imm, u6, 2),
            Encoder.init(6, Even_Reg(.src)),
            width_encoder,
            Encoder.init(10, @as(u4, 0x7)),
            region_encoder,
        };
        pub const ik_ij = Int(.imm, u7);
        pub const iw = Reg(.src);

        fn width_encoder(src: Param(.src)) Encoder {
            return Encoder.init(9, @as(u1, switch (src.signature.base) {
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
    struct { // ld .d x(base) + r(offset) .unsigned -> <reg>
        pub const spec =
            \\ld .d x(base) + r(offset) .unsigned -> b(dest) .unsigned
            \\ld .d x(base) + r(offset) .unsigned -> b(dest) .signed
            \\ld .d x(base) + r(offset) .unsigned -> r(dest)
            \\ld .d x(base) + r(offset) .unsigned -> x(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.base)),
            Encoder.init(6, Even_Reg(.offset)),
            dest_encoder,
            Encoder.init(11, @as(u3, 4)),
            region_encoder,
        };
        pub const ij = Reg(.base);
        pub const ik = Reg(.offset);
        pub const iw = Reg(.dest);

        fn dest_encoder(dest: Param(.dest)) Encoder {
            return Encoder.init(9, @as(u2, switch (dest.signature.base) {
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
    struct { // st <reg> -> .d x(base) + r(offset) .unsigned
        pub const spec =
            \\st b(src) -> .d x(base) + r(offset) .unsigned
            \\st r(src) -> .d x(base) + r(offset) .unsigned
            \\st x(src) -> .d x(base) + r(offset) .unsigned
            ;
        pub const encoding = .{
            Even_Reg(.src),
            Encoder.init(3, Even_Reg(.base)),
            Encoder.init(6, Even_Reg(.offset)),
            src_encoder,
            Encoder.init(11, @as(u3, 5)),
            region_encoder,
        };
        const Imm = Int(.imm, u4);
        pub const ij = Reg(.base);
        pub const ik = Reg(.offset);
        pub const iw = Reg(.src);

        fn src_encoder(src: Param(.src)) Encoder {
            return Encoder.init(9, @as(u2, switch (src.signature.base) {
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
    struct { // ld .d <uxp/kxp> + (imm4u * 2) -> r(dest)
        pub const spec =
            \\ld .d uxp + (imm) -> r(dest)
            \\ld .d kxp + (imm) -> r(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Int_Mult(.imm, u4, 2)),
            base_encoder,
            Encoder.init(8, @as(u6, 0x30)),
            region_encoder,
        };
        pub const ik_ij = Int(.imm, u5);
        pub const iw = Reg(.dest);

        fn base_encoder(base: Param(.imm)) Encoder {
            return Encoder.init(7, @as(u1, switch (base.signature.base.sr) {
                .uxp => 0,
                .kxp => 1,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, base: Param(.imm), flags: Flags) void {
            if (base.signature.base.sr != .uxp and !flags.kernel()) {
                return c.illegal_instruction();
            }
            c.sr_to_j(switch (base.signature.base.sr) {
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
    struct { // ld .d <uxp/kxp> + (imm4u * 4) -> x(dest)
        pub const spec =
            \\ld .d uxp + (imm) -> x(dest)
            \\ld .d kxp + (imm) -> x(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Int_Mult(.imm, u4, 4)),
            base_encoder,
            Encoder.init(8, @as(u6, 0x31)),
            region_encoder,
        };
        pub const ik_ij = Int(.imm, u6);
        pub const iw = Reg(.dest);

        fn base_encoder(base: Param(.imm)) Encoder {
            return Encoder.init(7, @as(u1, switch (base.signature.base.sr) {
                .uxp => 0,
                .kxp => 1,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, base: Param(.imm), flags: Flags) void {
            if (base.signature.base.sr != .uxp and !flags.kernel()) {
                return c.illegal_instruction();
            }
            c.sr_to_j(switch (base.signature.base.sr) {
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
    struct { // [i]ld[i] .d x(src) -> <reg>
        pub const spec =
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
            Encoder.init(3, Even_Reg(.dest)),
            dest_encoder,
            mnemonic_encoder,
            Encoder.init(9, @as(u5, 0x19)),
            region_encoder,
        };
        pub const ij = Reg(.src);
        pub const ik = Reg(.dest);
        pub const iw = Reg(.src);

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(8, @as(u1, switch (mnemonic) {
                .ldi => 0,
                .ild => 1,
                else => unreachable,
            }));
        }

        fn dest_encoder(dest: Param(.dest)) Encoder {
            return Encoder.init(6, @as(u2, switch (dest.signature.base) {
                .reg8 => |signedness| switch (signedness.?) {
                    .unsigned => 0,
                    .signed => 1,
                },
                .reg16 => 2,
                .reg32 => 3,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic, dest: Param(.dest)) void {
            c.reg32_to_j();
            c.literal_to_k(switch (dest.signature.base) {
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
    struct { // [i]st[i] <reg> -> .d x(dest)
        pub const spec =
            \\sti b(src) -> .d x(dest)
            \\sti r(src) -> .d x(dest)
            \\sti x(src) -> .d x(dest)
            \\ist b(src) -> .d x(dest)
            \\ist r(src) -> .d x(dest)
            \\ist x(src) -> .d x(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.src)),
            src_encoder,
            mnemonic_encoder,
            Encoder.init(9, @as(u5, 0x1a)),
            region_encoder,
        };
        pub const ij = Reg(.dest);
        pub const ik = Reg(.src);
        pub const iw = Reg(.dest);

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(8, @as(u1, switch (mnemonic) {
                .sti => 0,
                .ist => 1,
                else => unreachable,
            }));
        }

        fn src_encoder(src: Param(.src)) Encoder {
            return Encoder.init(6, @as(u2, switch (src.signature.base) {
                .reg8 => 0,
                .reg16 => 2,
                .reg32 => 3,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic, src: Param(.src)) void {
            c.reg32_to_j();
            c.literal_to_k(switch (src.signature.base) {
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
    struct { // ld .i x(src) -> <reg>
        pub const spec =
            \\ld .i x(src) -> r(dest)
            \\ld .i x(src) -> x(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.src)),
            dest_encoder,
            Encoder.init(7, @as(u7, 0x6c)),
            region_encoder,
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        fn dest_encoder(dest: Param(.dest)) Encoder {
            return Encoder.init(6, @as(u1, switch (dest.signature.base) {
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
    struct { // ld .i ip + r(offset) .signed -> <reg>
        pub const spec =
            \\ld .i ip + r(offset) .signed -> r(dest)
            \\ld .i ip + r(offset) .signed -> x(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.offset)),
            dest_encoder,
            Encoder.init(7, @as(u7, 0x6d)),
            region_encoder,
        };
        pub const ik = Reg(.offset);
        pub const iw = Reg(.dest);

        fn dest_encoder(dest: Param(.dest)) Encoder {
            return Encoder.init(6, @as(u1, switch (dest.signature.base) {
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
    struct { // ld .s sp + r(offset) .signed -> <reg>
        pub const spec =
            \\ld .s sp + r(offset) .signed -> b(dest) .unsigned
            \\ld .s sp + r(offset) .signed -> b(dest) .signed
            \\ld .s sp + r(offset) .signed -> r(dest)
            \\ld .s sp + r(offset) .signed -> x(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.offset)),
            dest_encoder,
            Encoder.init(8, @as(u6, 0x37)),
            region_encoder,
        };
        pub const ik = Reg(.offset);
        pub const iw = Reg(.dest);

        fn dest_encoder(dest: Param(.dest)) Encoder {
            return Encoder.init(6, @as(u2, switch (dest.signature.base) {
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
    struct { // st <reg> -> .s sp + r(offset) .signed
        pub const spec =
            \\st b(src) -> .s sp + r(offset) .signed
            \\st r(src) -> .s sp + r(offset) .signed
            \\st x(src) -> .s sp + r(offset) .signed
            ;
        pub const encoding = .{
            Even_Reg(.src),
            Encoder.init(3, Even_Reg(.offset)),
            src_encoder,
            Encoder.init(8, @as(u6, 0x38)),
            region_encoder,
        };
        pub const ij = Reg(.src);
        pub const ik = Reg(.offset);

        fn src_encoder(src: Param(.src)) Encoder {
            return Encoder.init(6, @as(u2, switch (src.signature.base) {
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
    struct { // [un]frame (imm6u * 2)
        pub const spec =
            \\unframe (imm)
            \\frame (imm)
            ;
        pub const encoding = .{
            Imm,
            mnemonic_encoder,
            Encoder.init(7, @as(u7, 0x73)),
            region_encoder,
        };
        const Imm = Int(.imm, u6);
        pub const iw_ik_ij = Imm;

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(6, @as(u1, switch (mnemonic) {
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
    struct { // [un]frame (imm16u)
        pub const spec =
            \\unframe (imm)
            \\frame (imm)
            ;
        pub const encoding = .{
            @as(u6, 0x20),
            mnemonic_encoder,
            Encoder.init(7, @as(u7, 0x72)),
            region_encoder,
            Encoder.init(16, Int(.imm, u16)),
        };

        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(6, @as(u1, switch (mnemonic) {
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
    struct { // ld .s sp + (imm16s) -> <reg>
        pub const spec =
            \\ld .s sp + (imm) -> b(dest) .unsigned
            \\ld .s sp + (imm) -> b(dest) .signed
            \\ld .s sp + (imm) -> r(dest)
            \\ld .s sp + (imm) -> x(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            dest_encoder,
            Encoder.init(5, @as(u9, 0x1c8)),
            region_encoder,
            Encoder.init(16, Int(.imm, i16)),
        };
        pub const iw = Reg(.dest);

        fn dest_encoder(dest: Param(.dest)) Encoder {
            return Encoder.init(3, @as(u2, switch (dest.signature.base) {
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
    struct { // st <reg> -> .s sp + (imm16s)
        pub const spec =
            \\st b(src) -> .s sp + (imm)
            \\st r(src) -> .s sp + (imm)
            \\st x(src) -> .s sp + (imm)
            ;
        pub const encoding = .{
            Even_Reg(.src),
            src_encoder,
            Encoder.init(5, @as(u9, 0x1ca)),
            region_encoder,
            Encoder.init(16, Int(.imm, i16)),
        };
        pub const ik = Reg(.src);

        fn src_encoder(src: Param(.src)) Encoder {
            return Encoder.init(3, @as(u2, switch (src.signature.base) {
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
    struct { // st <reg> -> .d bp + (imm4)
        pub const forms = .{
            struct {
                pub const spec = "st b(src) -> .d bp + (imm)";
                pub const encoding = .{
                    Even_Reg(.src),
                    Encoder.init(3, Int(.imm, u4)),
                    Encoder.init(7, @as(u7, 0x54)),
                    region_encoder,
                };
            },
            struct {
                pub const spec = "st r(src) -> .d bp + (imm)";
                pub const encoding = .{
                    Even_Reg(.src),
                    Encoder.init(3, Int_Mult(.imm, u4, 2)),
                    Encoder.init(7, @as(u7, 0x56)),
                    region_encoder,
                };
            },
            struct {
                pub const spec = "st x(src) -> .d bp + (imm)";
                pub const encoding = .{
                    Even_Reg(.src),
                    Encoder.init(3, Int_Mult(.imm, u4, 4)),
                    Encoder.init(7, @as(u7, 0x57)),
                    region_encoder,
                };
            },
        };
        pub const ik_ij = Int(.imm, u7);
        pub const iw = Reg(.src);

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.bp);
            c.ik_ij_sx_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(@This().store);
        }

        pub usingnamespace store_from_temp_1(.zero);
    },
    struct { // ld .d bp + (imm4) -> <reg>
        pub const forms = .{
            struct {
                pub const spec = 
                    \\ld .d bp + (imm) -> b(dest) .unsigned
                    \\ld .d bp + (imm) -> b(dest) .signed
                    ;
                pub const encoding = .{
                    Even_Reg(.dest),
                    Encoder.init(3, Int(.imm, u4)),
                    signedness_encoder,
                    Encoder.init(8, @as(u6, 0x3a)),
                    region_encoder,
                };
                fn signedness_encoder(dest: Param(.dest)) Encoder {
                    return Encoder.init(7, @as(u1, switch (dest.signature.base.reg8.?) {
                        .unsigned => 0,
                        .signed => 1,
                    }));
                }
            },
            struct {
                pub const spec = "ld .d bp + (imm) -> r(dest)";
                pub const encoding = .{
                    Even_Reg(.dest),
                    Encoder.init(3, Int_Mult(.imm, u4, 2)),
                    Encoder.init(7, @as(u7, 0x76)),
                    region_encoder,
                };
            },
            struct {
                pub const spec = "ld .d bp + (imm) -> x(dest)";
                pub const encoding = .{
                    Even_Reg(.dest),
                    Encoder.init(3, Int_Mult(.imm, u4, 4)),
                    Encoder.init(7, @as(u7, 0x77)),
                    region_encoder,
                };
            },
        };
        pub const ik_ij = Int(.imm, u7);
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.bp);
            c.ik_ij_sx_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.temp_1);
            c.load_next_insn();
            c.next(@This().load);
        }

        pub usingnamespace load_from_temp_1(.zero);
    },
    struct { // ldrs b(registerset), .d x(src)
        pub const spec = "ldrs b(registerset), .d x(src)";
        pub const encoding = .{
            Even_Reg(.registerset),
            Encoder.init(3, Even_Reg(.src)),
            Encoder.init(6, @as(u8, 0xd1)),
            region_encoder,
        };
        pub const ij = Reg(.src);
        pub const ik = Reg(.registerset);
        pub const iw: u4 = 0;

        const LDRS = @This();

        pub fn entry(c: *Cycle, flags: Flags) void {
            if (!flags.kernel()) return c.illegal_instruction();

            // Move the base address into SR2 so that we can move it to .RS_reserved without using L:
            c.reg32_to_l(); // x(src)
            c.l_to_sr(.temp_2);
            c.next(switch_rsn);
        }

        pub fn switch_rsn(c: *Cycle) void {
            c.rsn_to_sr1h(.fault_rsn_stat);
            c.reg_to_k(); // r(registerset)
            c.k_to_ll();
            c.ll_to_rsn();
            c.sr2_to_sr2(.temp_2, .rs_reserved);
            c.next(load_r0);
        }

        pub const load_r0 = load_gpr(0);
        pub const load_r1 = load_gpr(1);
        pub const load_r2 = load_gpr(2);
        pub const load_r3 = load_gpr(3);
        pub const load_r4 = load_gpr(4);
        pub const load_r5 = load_gpr(5);
        pub const load_r6 = load_gpr(6);
        pub const load_r7 = load_gpr(7);
        pub const load_r8 = load_gpr(8);
        pub const load_r9 = load_gpr(9);
        pub const load_r10 = load_gpr(10);
        pub const load_r11 = load_gpr(11);
        pub const load_r12 = load_gpr(12);
        pub const load_r13 = load_gpr(13);
        pub const load_r14 = load_gpr(14);
        pub const load_r15 = load_gpr(15);

        fn load_gpr(comptime n: arch.Register_Index) fn(c: *Cycle)void {
            return struct {
                pub fn func(c: *Cycle) void {
                    c.read_to_d(.rs_reserved, @as(u7, n) * 2, .word, .data);
                    c.d_to_l(.zx);
                    c.ll_to_reg();
                    if (n < 15) {
                        c.next_iw_increment();
                        c.next(@field(LDRS, std.fmt.comptimePrint("load_r{}", .{ n + 1 })));
                    } else {
                        c.next_ik_bit(arch.register_count * @sizeOf(arch.Reg));
                        c.next(offset_rs_reserved);
                    }
                }
            }.func;
        }

        pub fn offset_rs_reserved(c: *Cycle) void {
            c.sr_to_j(.rs_reserved);
            c.ik_bit_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.rs_reserved);
            c.next(load_rp);
        }

        pub const load_rp                      = load_sr_pt1(.rp);
        pub const load_sp                      = load_sr_pt1(.sp);
        pub const load_bp                      = load_sr_pt1(.bp);
        pub const load_fault_uc_slot_dr        = load_sr_pt1(.fault_uc_slot_dr);
        pub const load_fault_rsn_stat          = load_sr_pt1(.fault_rsn_stat);
        pub const load_int_rsn_fault_iw_ik_ij  = load_sr_pt1(.int_rsn_fault_iw_ik_ij);
        pub const load_ip                      = load_sr_pt1(.ip);
        pub const load_next_ip                 = load_sr_pt1(.next_ip);
        pub const load_asn                     = load_sr_pt1(.asn);
        pub const load_kxp                     = load_sr_pt1(.kxp);
        pub const load_uxp                     = load_sr_pt1(.uxp);
        pub const load_temp_2                  = load_sr_pt1(.temp_2);
        pub const load_temp_1                  = load_sr_pt1(.temp_1);

        pub const load_rp_2                      = load_sr_pt2(.rp, load_sp);
        pub const load_sp_2                      = load_sr_pt2(.sp, load_bp);
        pub const load_bp_2                      = load_sr_pt2(.bp, load_fault_uc_slot_dr);
        pub const load_fault_uc_slot_dr_2        = load_sr_pt2(.fault_uc_slot_dr, load_fault_rsn_stat);
        pub const load_fault_rsn_stat_2          = load_sr_pt2(.fault_rsn_stat, load_int_rsn_fault_iw_ik_ij);
        pub const load_int_rsn_fault_iw_ik_ij_2  = load_sr_pt2(.int_rsn_fault_iw_ik_ij, load_ip);
        pub const load_ip_2                      = load_sr_pt2(.ip, load_next_ip);
        pub const load_next_ip_2                 = load_sr_pt2(.next_ip, load_asn);
        pub const load_asn_2                     = load_sr_pt2(.asn, load_kxp);
        pub const load_kxp_2                     = load_sr_pt2(.kxp, load_uxp);
        pub const load_uxp_2                     = load_sr_pt2(.uxp, load_temp_2);
        pub const load_temp_2_2                  = load_sr_pt2(.temp_2, load_temp_1);
        pub const load_temp_1_2                  = load_sr_pt2(.temp_1, restore_rsn);

        fn load_sr_pt1(comptime sr: arch.reg.sr.Any_Index) fn(c: *Cycle)void {
            const offset = @offsetOf(arch.Context_State, @tagName(sr)) - (arch.register_count * @sizeOf(arch.Reg));
            return struct {
                pub fn func(c: *Cycle) void {
                    c.read_to_d(.rs_reserved, offset + 2, .word, .data);
                    c.d_to_l(.zx);
                    c.l_to_sr(.temp_1);
                    c.next(@field(LDRS, "load_" ++ @tagName(sr) ++ "_2"));
                }
            }.func;
        }
        fn load_sr_pt2(comptime sr: arch.reg.sr.Any_Index, comptime next: *const anyopaque) fn(c: *Cycle)void {
            const offset = @offsetOf(arch.Context_State, @tagName(sr)) - (arch.register_count * @sizeOf(arch.Reg));
            return struct {
                pub fn func(c: *Cycle) void {
                    c.read_to_d(.rs_reserved, offset, .word, .data);
                    c.srl_to_lh(.temp_1);
                    c.d_to_ll();
                    c.l_to_sr(sr);
                    c.next(next);
                }
            }.func;
        }

        pub fn restore_rsn(c: *Cycle) void {
            c.srh_to_ll(.fault_rsn_stat);
            c.ll_to_rsn();
            c.next(load_next_insn);
        }

        pub fn load_next_insn(c: *Cycle) void {
            c.load_and_exec_next_insn();
        }

    },
    struct { // strs b(registerset) -> .d x(dest)
        pub const spec = "strs b(registerset) -> .d x(dest)";
        pub const encoding = .{
            Even_Reg(.registerset),
            Encoder.init(3, Even_Reg(.dest)),
            Encoder.init(6, @as(u8, 0xd5)),
            region_encoder,
        };
        pub const ij = Reg(.dest);
        pub const ik = Reg(.registerset);

        const STRS = @This();

        pub fn entry(c: *Cycle, flags: Flags) void {
            if (!flags.kernel()) return c.illegal_instruction();
            // Move the base address into .temp_2 so that we can move it to .rs_reserved without using L:
            c.reg32_to_l();
            c.l_to_sr(.temp_2);
            c.next(switch_rsn);
        }

        pub fn switch_rsn(c: *Cycle) void {
            // switch RSN, copy base address to .RS_reserved:
            c.rsn_to_sr1h(.fault_rsn_stat);
            c.reg_to_k(); // r(registerset)
            c.k_to_ll();
            c.ll_to_rsn();
            c.sr2_to_sr2(.temp_2, .rs_reserved);
            c.next_ik(0);
            c.next(store_r0);
        }

        pub const store_r0 = store_gpr(0);
        pub const store_r1 = store_gpr(1);
        pub const store_r2 = store_gpr(2);
        pub const store_r3 = store_gpr(3);
        pub const store_r4 = store_gpr(4);
        pub const store_r5 = store_gpr(5);
        pub const store_r6 = store_gpr(6);
        pub const store_r7 = store_gpr(7);
        pub const store_r8 = store_gpr(8);
        pub const store_r9 = store_gpr(9);
        pub const store_r10 = store_gpr(10);
        pub const store_r11 = store_gpr(11);
        pub const store_r12 = store_gpr(12);
        pub const store_r13 = store_gpr(13);
        pub const store_r14 = store_gpr(14);
        pub const store_r15 = store_gpr(15);

        fn store_gpr(comptime n: arch.Register_Index) fn(c: *Cycle)void {
            return struct {
                pub fn func(c: *Cycle) void {
                    c.reg_to_k();
                    c.k_to_ll();
                    c.write_from_ll(.rs_reserved, @as(u7, n) * 2, .word, .data);
                    if (n < 15) {
                        c.next_ik_increment();
                        c.next(@field(STRS, std.fmt.comptimePrint("store_r{}", .{ n + 1 })));
                    } else {
                        c.next_ik_bit(arch.register_count * @sizeOf(arch.Reg));
                        c.next(offset_rs_reserved);
                    }
                }
            }.func;
        }

        pub fn offset_rs_reserved(c: *Cycle) void {
            c.sr_to_j(.rs_reserved);
            c.ik_bit_to_k();
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.rs_reserved);
            c.next(store_rp);
        }

        pub const store_rp                      = store_sr_pt1(.rp);
        pub const store_sp                      = store_sr_pt1(.sp);
        pub const store_bp                      = store_sr_pt1(.bp);
        pub const store_fault_uc_slot_dr        = store_sr_pt1(.fault_uc_slot_dr);
        pub const store_fault_rsn_stat          = store_sr_pt1(.fault_rsn_stat);
        pub const store_int_rsn_fault_iw_ik_ij  = store_sr_pt1(.int_rsn_fault_iw_ik_ij);
        pub const store_ip                      = store_sr_pt1(.ip);
        pub const store_next_ip                 = store_sr_pt1(.next_ip);
        pub const store_asn                     = store_sr_pt1(.asn);
        pub const store_kxp                     = store_sr_pt1(.kxp);
        pub const store_uxp                     = store_sr_pt1(.uxp);
        pub const store_temp_2                  = store_sr_pt1(.temp_2);
        pub const store_temp_1                  = store_sr_pt1(.temp_1);

        pub const store_rp_2                      = store_sr_pt2(.rp, store_sp);
        pub const store_sp_2                      = store_sr_pt2(.sp, store_bp);
        pub const store_bp_2                      = store_sr_pt2(.bp, store_fault_uc_slot_dr);
        pub const store_fault_uc_slot_dr_2        = store_sr_pt2(.fault_uc_slot_dr, store_fault_rsn_stat);
        pub const store_fault_rsn_stat_2          = store_sr_pt2(.fault_rsn_stat, store_int_rsn_fault_iw_ik_ij);
        pub const store_int_rsn_fault_iw_ik_ij_2  = store_sr_pt2(.int_rsn_fault_iw_ik_ij, store_ip);
        pub const store_ip_2                      = store_sr_pt2(.ip, store_next_ip);
        pub const store_next_ip_2                 = store_sr_pt2(.next_ip, store_asn);
        pub const store_asn_2                     = store_sr_pt2(.asn, store_kxp);
        pub const store_kxp_2                     = store_sr_pt2(.kxp, store_uxp);
        pub const store_uxp_2                     = store_sr_pt2(.uxp, store_temp_2);
        pub const store_temp_2_2                  = store_sr_pt2(.temp_2, store_temp_1);
        pub const store_temp_1_2                  = store_sr_pt2(.temp_1, restore_rsn);

        fn store_sr_pt1(comptime sr: arch.reg.sr.Any_Index) fn(c: *Cycle)void {
            const offset = @offsetOf(arch.Context_State, @tagName(sr)) - (arch.register_count * @sizeOf(arch.Reg));
            return struct {
                pub fn func(c: *Cycle) void {
                    c.srh_to_ll(sr);
                    c.write_from_ll(.rs_reserved, offset + 2, .word, .data);
                    c.next(@field(STRS, "store_" ++ @tagName(sr) ++ "_2"));
                }
            }.func;
        }
        fn store_sr_pt2(comptime sr: arch.reg.sr.Any_Index, comptime next: *const anyopaque) fn(c: *Cycle)void {
            const offset = @offsetOf(arch.Context_State, @tagName(sr)) - (arch.register_count * @sizeOf(arch.Reg));
            return struct {
                pub fn func(c: *Cycle) void {
                    c.srl_to_ll(sr);
                    c.write_from_ll(.rs_reserved, offset, .word, .data);
                    c.next(next);
                }
            }.func;
        }

        pub fn restore_rsn(c: *Cycle) void {
            c.srh_to_ll(.fault_rsn_stat);
            c.ll_to_rsn();
            c.next(load_next_insn);
        }

        pub fn load_next_insn(c: *Cycle) void {
            c.load_and_exec_next_insn();
        }
    },
    struct { // srs b(registerset)
        pub const spec = "srs b(registerset)";
        pub const encoding = .{
            Even_Reg(.registerset),
            Encoder.init(3, @as(u11, 0x729)),
            region_encoder,
        };
        pub const ij = Reg(.registerset);

        pub fn entry(c: *Cycle, flags: Flags) void {
            if (!flags.kernel()) return c.illegal_instruction();
            // Increment IP so that if/when someone switches back to this registerset,
            // this instruction doesn't get executed a second time.
            c.sr_to_j(.ip);
            c.literal_to_k(2);
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.ip);
            c.next(switch_rsn);
        }

        pub fn switch_rsn(c: *Cycle) void {
            c.reg_to_jl();
            c.jl_to_ll();
            c.ll_to_rsn();
            c.next(reload_asn);
        }

        pub fn reload_asn(c: *Cycle) void {
            c.reload_asn();
            c.next(branch);
        }

        pub fn branch(c: *Cycle) void {
            c.branch(.ip, 0);
        }
    },
    struct { // c x(odd) -> x(even)
        pub const spec =
            \\c x(odd) -> x(even)
            ;
        pub const encoding = .{
            Even_Reg(.even),
            Encoder.init(3, Odd_Reg(.odd)),
            Encoder.init(6, @as(u8, 0xaa)),
            region_encoder,
        };
        pub const ij = Reg(.odd);
        pub const iw = Reg(.even);

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { // c x(even) -> x(odd)
        pub const spec =
            \\c x(even) -> x(odd)
            ;
        pub const encoding = .{
            Even_Reg(.odd),
            Encoder.init(3, Odd_Reg(.even)),
            Encoder.init(6, @as(u8, 0xab)),
            region_encoder,
        };
        pub const ij = Reg(.even);
        pub const iw = Reg(.odd);

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
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

        fn load_without_imm(c: *Cycle, src: Param(0), dest: Param(2)) void {
            load_with_imm(c, src, dest, .{ .value = 0 });
        }

        fn load_with_imm(c: *Cycle, src: Param(0), dest: Param(2), imm: placeholders.Any(.imm)) void {
            var signedness: std.builtin.Signedness = .unsigned;
            var width: arch.D.Width = .word;
            var width_bytes: u3 = 2;
            const addr_space: arch.addr.Space = switch(src.signature.address_space.?) {
                .data => .data,
                .insn => .insn,
                .stack => .stack,
            };

            switch (dest.signature.base) {
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
            if (dest.signature.base == .reg32) {
                c.next_iw_xor1();
                c.virtual_address_to_sr(.temp_1);
                c.next(load_high);
            } else {
                c.exec_next_insn();
            }
        }

        pub fn load_high(c: *Cycle, src: Param(0)) void {
            const addr_space: arch.addr.Space = switch(src.signature.address_space.?) {
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

        fn store_without_imm(c: *Cycle, src: Param(0), dest: Param(2)) void {
            store_with_imm(c, src, dest, .{ .value = 0 });
        }

        fn store_with_imm(c: *Cycle, src: Param(0), dest: Param(2), imm: placeholders.Any(.imm)) void {
            var width: arch.D.Width = .word;
            var width_bytes: u3 = 2;
            const addr_space: arch.addr.Space = switch(dest.signature.address_space.?) {
                .data => .data,
                .insn => .insn,
                .stack => .stack,
            };

            switch (src.signature.base) {
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
            if (src.signature.base == .reg32) {
                c.virtual_address_to_sr(.temp_1);
                c.next_ik_xor1();
                c.next(store_high);
            } else {
                c.exec_next_insn();
            }
        }

        pub fn store_high(c: *Cycle, dest: Param(2)) void {
            const addr_space: arch.addr.Space = switch(dest.signature.address_space.?) {
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

const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Parameter = isa.Parameter;
const Options = placeholders.Options;
const Range = placeholders.Range;
const Int = placeholders.Int;
const Int_Mult = placeholders.Int_Mult;
const Reg = placeholders.Reg;
const Bit = placeholders.Bit;
const Even_Reg = placeholders.Even_Reg;
const Odd_Reg = placeholders.Odd_Reg;
const Param = placeholders.Param;
const placeholders = @import("../compile/placeholders.zig");
const Control_Signals = arch.Control_Signals;
const Flags = arch.microcode.Flags;
const isa = @import("isa");
const arch = @import("arch");
const std = @import("std");
