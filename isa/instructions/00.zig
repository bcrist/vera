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
    struct { // <b> .i ip + (imm8i) // imm not in [-1, 1]
        pub const spec =
            \\b .i ip + (imm)
            \\call .i ip + (imm)
            \\b.v .i ip + (imm)
            \\b.z .i ip + (imm)
            \\b.nz .i ip + (imm)
            \\b.n .i ip + (imm)
            \\b.nn .i ip + (imm)
            \\b.c .i ip + (imm)
            \\b.nc .i ip + (imm)
            \\b.gu .i ip + (imm)
            \\b.ngu .i ip + (imm)
            \\b.ls .i ip + (imm)
            \\b.nls .i ip + (imm)
            \\b.gs .i ip + (imm)
            \\b.ngs .i ip + (imm)
            \\b.p .i ip + (imm)
            ;
        pub const constraints = .{
            .{ .imm, .not_equal, 0 },
            .{ .imm, .not_equal, 1 },
            .{ .imm, .not_equal, -1 },
        };
        pub const encoding = .{
            Int(.imm, i8),
            conditions.encoder(8),
            Encoder.init(12, @as(u2, 0)),
            region_encoder,
        };
        pub const ik_ij = Int(.imm, i8);

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic, condition: isa.Mnemonic_Suffix, flags: Flags) void {
            if (conditions.check(condition, flags)) {
                c.call_or_branch(.ip, .ik_ij_sx, mnemonic);
            } else {
                c.load_and_exec_next_insn_no_atomic_end();
            }
        }
    },
    struct { // <b> .i ip + (imm8u) // imm in [128, 253]
        pub const spec =
            \\b .i ip + (imm)
            \\b.v .i ip + (imm)
            \\b.z .i ip + (imm)
            \\b.nz .i ip + (imm)
            \\b.n .i ip + (imm)
            \\b.nn .i ip + (imm)
            \\b.c .i ip + (imm)
            \\b.nc .i ip + (imm)
            \\b.gu .i ip + (imm)
            \\b.ngu .i ip + (imm)
            \\b.ls .i ip + (imm)
            \\b.nls .i ip + (imm)
            \\b.gs .i ip + (imm)
            \\b.ngs .i ip + (imm)
            \\b.p .i ip + (imm)
            ;
        pub const constraints = .{
            .{ .imm, .greater_or_equal, 128 },
        };
        pub const encoding = .{
            Imm,
            Encoder.init(7, @as(u1, 0)),
            conditions.encoder(8),
            Encoder.init(12, @as(u2, 1)),
            region_encoder,
        };
        const Imm = Range(.imm, 126, 253);
        pub const iw_ik_ij = Imm;

        pub fn entry(c: *Cycle, condition: isa.Mnemonic_Suffix, flags: Flags) void {
            if (conditions.check(condition, flags)) {
                c.sr_to_j(.ip);
                c.iw_ik_ij_zx_to_k();
                c.j_plus_k_to_l(.zx, .fresh, .no_flags);
                c.l_to_sr(.next_ip);
                c.next(branch);
            } else {
                c.load_and_exec_next_insn_no_atomic_end();
            }
        }

        pub fn branch(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.call_or_branch(.next_ip, Imm.domain.range.first, mnemonic);
        }
    },
    struct { // <b> .i ip + (imm8n) // imm in [-255, -129]
        pub const spec =
            \\b .i ip + (imm)
            \\b.v .i ip + (imm)
            \\b.z .i ip + (imm)
            \\b.nz .i ip + (imm)
            \\b.n .i ip + (imm)
            \\b.nn .i ip + (imm)
            \\b.c .i ip + (imm)
            \\b.nc .i ip + (imm)
            \\b.gu .i ip + (imm)
            \\b.ngu .i ip + (imm)
            \\b.ls .i ip + (imm)
            \\b.nls .i ip + (imm)
            \\b.gs .i ip + (imm)
            \\b.ngs .i ip + (imm)
            \\b.p .i ip + (imm)
            ;
        pub const constraints = .{
            .{ .imm, .less_or_equal, -129 },
        };
        pub const encoding = .{
            Imm,
            Encoder.init(7, @as(u1, 1)),
            conditions.encoder(8),
            Encoder.init(12, @as(u2, 1)),
            region_encoder,
        };
        const Imm = Range(.imm, -128, -255);
        pub const iw_ik_ij = Imm;

        pub fn entry(c: *Cycle, condition: isa.Mnemonic_Suffix, flags: Flags) void {
            if (conditions.check(condition, flags)) {
                c.sr_to_j(.ip);
                c.iw_ik_ij_zx_to_k();
                c.j_minus_k_to_l(.zx, .fresh, .no_flags);
                c.l_to_sr(.next_ip);
                c.next(branch);
            } else {
                c.load_and_exec_next_insn_no_atomic_end();
            }
        }

        pub fn branch(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.call_or_branch(.next_ip, Imm.domain.range.first, mnemonic);
        }
    },
    struct { // <b> .i ip + (imm16u)
        pub const spec =
            \\b .i ip + (imm)
            \\call .i ip + (imm)
            \\b.v .i ip + (imm)
            \\b.z .i ip + (imm)
            \\b.nz .i ip + (imm)
            \\b.n .i ip + (imm)
            \\b.nn .i ip + (imm)
            \\b.c .i ip + (imm)
            \\b.nc .i ip + (imm)
            \\b.gu .i ip + (imm)
            \\b.ngu .i ip + (imm)
            \\b.ls .i ip + (imm)
            \\b.nls .i ip + (imm)
            \\b.gs .i ip + (imm)
            \\b.ngs .i ip + (imm)
            \\b.p .i ip + (imm)
            ;
        pub const encoding = .{
            @as(u8, 1),
            conditions.encoder(8),
            Encoder.init(12, @as(u2, 0)),
            region_encoder,
            Encoder.init(16, Int(.imm, u16)),
        };

        pub fn entry(c: *Cycle, condition: isa.Mnemonic_Suffix, flags: Flags) void {
            if (conditions.check(condition, flags)) {
                c.ip_read_to_d(2, .word);
                c.d_to_l(.zx);
                c.l_to_sr(.temp_1);
                c.next(compute_next_ip);
            } else {
                c.load_and_exec_next_insn_no_atomic_end();
            }
        }

        pub fn compute_next_ip(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.srl_to_k(.temp_1);
            c.j_plus_k_to_l(.zx, .fresh, .no_flags);
            c.l_to_sr(.next_ip);
            c.next(branch);
        }

        pub fn branch(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.call_or_branch(.next_ip, 0, mnemonic);
        }
    },
    struct { // <b> .i ip + (imm16n)
        pub const spec =
            \\b .i ip + (imm)
            \\call .i ip + (imm)
            \\b.v .i ip + (imm)
            \\b.z .i ip + (imm)
            \\b.nz .i ip + (imm)
            \\b.n .i ip + (imm)
            \\b.nn .i ip + (imm)
            \\b.c .i ip + (imm)
            \\b.nc .i ip + (imm)
            \\b.gu .i ip + (imm)
            \\b.ngu .i ip + (imm)
            \\b.ls .i ip + (imm)
            \\b.nls .i ip + (imm)
            \\b.gs .i ip + (imm)
            \\b.ngs .i ip + (imm)
            \\b.p .i ip + (imm)
            ;
        pub const encoding = .{
            @as(u8, 1),
            conditions.encoder(8),
            Encoder.init(12, @as(u2, 1)),
            region_encoder,
            Encoder.init(16, Range(.imm, -0x10000, -1)),
        };

        pub fn entry(c: *Cycle, condition: isa.Mnemonic_Suffix, flags: Flags) void {
            if (conditions.check(condition, flags)) {
                c.ip_read_to_d(2, .word);
                c.d_to_l(.zx);
                c.l_to_sr(.temp_1);
                c.next(compute_next_ip);
            } else {
                c.load_and_exec_next_insn_no_atomic_end();
            }
        }

        pub fn compute_next_ip(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.srl_to_k(.temp_1);
            c.j_plus_k_to_l(._1x, .fresh, .no_flags);
            c.l_to_sr(.next_ip);
            c.next(branch);
        }

        pub fn branch(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.call_or_branch(.next_ip, 0, mnemonic);
        }
    },
    struct { // <b> .i (imm32)
        pub const spec =
            \\b .i (imm)
            \\call .i (imm)
            \\b.v .i (imm)
            \\b.z .i (imm)
            \\b.nz .i (imm)
            \\b.n .i (imm)
            \\b.nn .i (imm)
            \\b.c .i (imm)
            \\b.nc .i (imm)
            \\b.gu .i (imm)
            \\b.ngu .i (imm)
            \\b.ls .i (imm)
            \\b.nls .i (imm)
            \\b.gs .i (imm)
            \\b.ngs .i (imm)
            \\b.p .i (imm)
            ;
        pub const encoding = .{
            @as(u8, 0),
            conditions.encoder(8),
            Encoder.init(12, @as(u2, 1)),
            region_encoder,
            Encoder.init(16, Int(.imm, u32)),
        };

        pub fn entry(c: *Cycle, condition: isa.Mnemonic_Suffix, flags: Flags) void {
            if (conditions.check(condition, flags)) {
                c.ip_read_to_d(4, .word);
                c.d_to_l(.zx);
                c.l_to_sr(.temp_1);
                c.next(load_imm_low);
            } else {
                c.load_and_exec_next_insn_no_atomic_end();
            }
        }

        pub fn load_imm_low(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_ll();
            c.srl_to_lh(.temp_1);
            c.l_to_sr(.next_ip);
            c.next(branch);
        }

        pub fn branch(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.call_or_branch(.next_ip, 0, mnemonic);
        }
    },
    struct { // b.<cond>.<cond> .i ip + (imm16s), .i ip + (imm16s)
        pub const spec =
            \\b.lu.gu .i ip + (imm1), .i ip + (imm2)
            \\b.lu.z .i ip + (imm1), .i ip + (imm2)
            \\b.gu.z .i ip + (imm1), .i ip + (imm2)
            \\b.ls.gs .i ip + (imm1), .i ip + (imm2)
            \\b.ls.z .i ip + (imm1), .i ip + (imm2)
            \\b.gs.z .i ip + (imm1), .i ip + (imm2)
            \\b.n.z .i ip + (imm1), .i ip + (imm2)
            \\b.n.p .i ip + (imm1), .i ip + (imm2)
            ;
        pub const encoding = .{
            @as(u8, 0),
            conditions.double_encoder(8),
            Encoder.init(11, @as(u3, 1)),
            region_encoder,
            Encoder.init(16, Int(.imm1, i16)),
            Encoder.init(32, Int(.imm2, i16)),
        };

        pub fn entry(c: *Cycle, suffix: isa.Mnemonic_Suffix, flags: Flags) void {
            if (conditions.check(conditions.first(suffix), flags)) {
                c.ip_read_to_d(2, .word);
                c.d_to_l(.sx);
                c.l_to_sr(.temp_1);
                c.next(compute_next_ip);
            } else if (conditions.check(conditions.second(suffix), flags)) {
                c.ip_read_to_d(4, .word);
                c.d_to_l(.sx);
                c.l_to_sr(.temp_1);
                c.next(compute_next_ip);
            } else {
                c.load_and_exec_next_insn_no_atomic_end();
            }
        }

        pub fn compute_next_ip(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.srl_to_k(.temp_1);
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            c.l_to_sr(.next_ip);
            c.next(branch);
        }

        pub fn branch(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.call_or_branch(.next_ip, 0, mnemonic);
        }
    },
    struct { // <b> .i ip + r(offset) .signed
        pub const spec =
            \\b .i ip + r(offset) .signed
            \\call .i ip + r(offset) .signed
            \\b.v .i ip + r(offset) .signed
            \\b.z .i ip + r(offset) .signed
            \\b.nz .i ip + r(offset) .signed
            \\b.n .i ip + r(offset) .signed
            \\b.nn .i ip + r(offset) .signed
            \\b.c .i ip + r(offset) .signed
            \\b.nc .i ip + r(offset) .signed
            \\b.gu .i ip + r(offset) .signed
            \\b.ngu .i ip + r(offset) .signed
            \\b.ls .i ip + r(offset) .signed
            \\b.nls .i ip + r(offset) .signed
            \\b.gs .i ip + r(offset) .signed
            \\b.ngs .i ip + r(offset) .signed
            \\b.p .i ip + r(offset) .signed
            ;
        pub const encoding = .{
            Reg(.offset),
            conditions.encoder(4),
            Encoder.init(8, @as(u6, 0x20)),
            region_encoder,
        };
        pub const ik = Reg(.offset);

        pub fn entry(c: *Cycle, condition: isa.Mnemonic_Suffix, flags: Flags) void {
            if (conditions.check(condition, flags)) {
                c.sr_to_j(.ip);
                c.reg_to_k();
                c.j_plus_k_to_l(.sx, .fresh, .no_flags);
                c.l_to_sr(.next_ip);
                c.next(branch);
            } else {
                c.load_and_exec_next_insn_no_atomic_end();
            }
        }

        pub fn branch(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.call_or_branch(.next_ip, 0, mnemonic);
        }
    },
    struct { // <b> .i x(reg)
        pub const spec =
            \\b .i x(reg)
            \\call .i x(reg)
            \\b.v .i x(reg)
            \\b.z .i x(reg)
            \\b.nz .i x(reg)
            \\b.n .i x(reg)
            \\b.nn .i x(reg)
            \\b.c .i x(reg)
            \\b.nc .i x(reg)
            \\b.gu .i x(reg)
            \\b.ngu .i x(reg)
            \\b.ls .i x(reg)
            \\b.nls .i x(reg)
            \\b.gs .i x(reg)
            \\b.ngs .i x(reg)
            \\b.p .i x(reg)
            ;
        pub const encoding = .{
            Reg(.reg),
            conditions.encoder(4),
            Encoder.init(8, @as(u6, 0x21)),
            region_encoder,
        };
        pub const ij = Reg(.reg);

        pub fn entry(c: *Cycle, condition: isa.Mnemonic_Suffix, flags: Flags) void {
            if (conditions.check(condition, flags)) {
                c.reg32_to_l();
                c.l_to_sr(.next_ip);
                c.next(branch);
            } else {
                c.load_and_exec_next_insn_no_atomic_end();
            }
        }

        pub fn branch(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.call_or_branch(.next_ip, 0, mnemonic);
        }
    },
    struct { // <bb/bbn/bp/bpn>
        pub const spec =
            \\bb
            \\bb.v
            \\bb.z
            \\bb.nz
            \\bb.n
            \\bb.nn
            \\bb.c
            \\bb.nc
            \\bb.gu
            \\bb.ngu
            \\bb.ls
            \\bb.nls
            \\bb.gs
            \\bb.ngs
            \\bb.p
            \\
            \\bbn
            \\bbn.v
            \\bbn.z
            \\bbn.nz
            \\bbn.n
            \\bbn.nn
            \\bbn.c
            \\bbn.nc
            \\bbn.gu
            \\bbn.ngu
            \\bbn.ls
            \\bbn.nls
            \\bbn.gs
            \\bbn.ngs
            \\bbn.p
            \\
            \\bp
            \\bp.v
            \\bp.z
            \\bp.nz
            \\bp.n
            \\bp.nn
            \\bp.c
            \\bp.nc
            \\bp.gu
            \\bp.ngu
            \\bp.ls
            \\bp.nls
            \\bp.gs
            \\bp.ngs
            \\bp.p
            \\
            \\bpn
            \\bpn.v
            \\bpn.z
            \\bpn.nz
            \\bpn.n
            \\bpn.nn
            \\bpn.c
            \\bpn.nc
            \\bpn.gu
            \\bpn.ngu
            \\bpn.ls
            \\bpn.nls
            \\bpn.gs
            \\bpn.ngs
            \\bpn.p
            ;
        pub const encoding = .{
            conditions.encoder(0),
            mnemonic_encoder,
            Encoder.init(6, @as(u8, 0x7f)),
            region_encoder,
        };
        fn mnemonic_encoder(mnemonic: isa.Mnemonic) Encoder {
            return Encoder.init(4, @as(u2, switch (mnemonic) {
                .bb => 0,
                .bbn => 1,
                .bp => 2,
                .bpn => 3,
                else => unreachable,
            }));
        }
        pub fn iw_ik_ij(mnemonic: isa.Mnemonic) u12 {
            return switch(mnemonic) {
                .bb, .bbn => 0xFF,
                .bp, .bpn => 0xFFF,
                else => unreachable,
            };
        }

        pub fn entry(c: *Cycle, mnemonic: isa.Mnemonic, condition: isa.Mnemonic_Suffix, flags: Flags) void {
            if (conditions.check(condition, flags)) {
                c.sr_to_j(.ip);
                c.iw_ik_ij_zx_to_k();
                c.jl_logic_k_to_ll(switch (mnemonic) {
                    .bb, .bp => .and_not,
                    .bbn, .bpn => ._or,
                    else => unreachable,
                }, .fresh, .no_flags);
                c.jh_to_lh();
                c.l_to_sr(.next_ip);
                c.next(branch);
            } else {
                c.load_and_exec_next_insn_no_atomic_end();
            }
        }

        pub fn branch(c: *Cycle, mnemonic: isa.Mnemonic) void {
            c.branch(.next_ip, @as(u1, switch (mnemonic) {
                .bb, .bp => 0,
                .bbn, .bpn => 1,
                else => unreachable,
            }));
        }
    },
    struct { // c <sp/bp> + (imm5u) -> x(dest)
        pub const spec =
            \\c sp + (imm) -> x(dest)
            \\c bp + (imm) -> x(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Imm),
            Source_Encoder,
            Encoder.init(9, @as(u5, 0x11)),
            region_encoder,
        };
        const Imm = Int(.imm, u5);
        pub const ik_ij = Imm;
        pub const iw = Reg(.dest);

        fn Source_Encoder(base: Param(.imm)) Encoder {
            return Encoder.init(8, @as(u1, switch (base.signature.base.sr) {
                .sp => 0,
                .bp => 1,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, base: Param(.imm)) void {
            c.sr_to_j(switch (base.signature.base.sr) {
                .sp => .sp,
                .bp => .bp,
                else => unreachable,
            });
            c.ik_ij_sx_to_k();
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { // c <rp/uxp/kxp/asn> -> x(dest)
        pub const spec =
            \\c rp  -> x(dest)
            \\c uxp -> x(dest)
            \\c kxp -> x(dest)
            \\c asn -> x(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            rp_uxp_kxp_asn_encoder(3, 0),
            Encoder.init(5, @as(u9, 0x120)),
            region_encoder,
        };
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle, flags: Flags, base: Param(0)) void {
            switch (base.signature.base.sr) {
                .kxp, .asn => if (!flags.kernel()) {
                    return c.illegal_instruction();
                },
                .rp, .uxp => {},
                else => unreachable,
            }
            c.sr_to_l(switch (base.signature.base.sr) {
                .rp => .rp,
                .uxp => .uxp,
                .kxp => .kxp,
                .asn => .asn,
                else => unreachable,
            });
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { // c x(src) -> <rp/uxp/kxp/asn>
        pub const spec =
            \\c x(src) -> rp
            \\c x(src) -> uxp
            \\c x(src) -> kxp
            \\c x(src) -> asn
            ;
        pub const encoding = .{
            Even_Reg(.src),
            rp_uxp_kxp_asn_encoder(3, 2),
            Encoder.init(5, @as(u9, 0x121)),
            region_encoder,
        };
        pub const ij = Reg(.src);

        pub fn entry(c: *Cycle, flags: Flags, dest: Param(2)) void {
            const sr: arch.Any_SR_Index = switch (dest.signature.base.sr) {
                .rp => .rp,
                .uxp => .uxp,
                .kxp => .kxp,
                .asn => .asn,
                else => unreachable,
            };
            switch (sr) {
                .kxp, .asn => if (!flags.kernel()) {
                    return c.illegal_instruction();
                },
                .rp, .uxp => {},
                else => unreachable,
            }
            c.reg32_to_l();
            c.l_to_sr(sr);
            if (sr.to_sr1_index()) |_| {
                c.load_and_exec_next_insn();
            } else {
                c.next(load_and_exec_next_insn);
            }
        }

        pub const load_and_exec_next_insn = Cycle.load_and_exec_next_insn;
    },
    struct { // c x(src) -> sp
        pub const spec = "c x(src) -> sp";
        pub const encoding = .{
            Even_Reg(.src),
            Encoder.init(3, @as(u11, 0x488)),
            region_encoder,
        };
        pub const ij = Reg(.src);

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.sp);
            c.load_and_exec_next_insn();
        }
    },
    struct { // c x(src) -> bp
        pub const spec = "c x(src) -> bp";
        pub const encoding = .{
            Even_Reg(.src),
            Encoder.init(3, @as(u11, 0x489)),
            region_encoder,
        };
        pub const ij = Reg(.src);

        pub fn entry(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_sr(.bp);
            c.load_and_exec_next_insn();
        }
    },
    struct { // c <ip/rp/sp/bp/uxp/kxp> + (imm16s) -> x(dest)
        pub const spec =
            \\c ip  + (imm) -> x(dest)
            \\c rp  + (imm) -> x(dest)
            \\c sp  + (imm) -> x(dest)
            \\c bp  + (imm) -> x(dest)
            \\c uxp + (imm) -> x(dest)
            \\c kxp + (imm) -> x(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            sr_encoder,
            Encoder.init(6, @as(u8, 0x91)),
            region_encoder,
            Encoder.init(16, Int(.imm, i16)),
        };
        pub const iw = Reg(.dest);

        fn sr_encoder(base: Param(.imm)) Encoder {
            return Encoder.init(3, @as(u3, switch (base.signature.base.sr) {
                .ip => 2,
                .rp => 3,
                .sp => 4,
                .bp => 5,
                .uxp => 6,
                .kxp => 7,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, flags: Flags, base: Param(.imm)) void {
            switch (base.signature.base.sr) {
                .kxp => if (!flags.kernel()) {
                    return c.illegal_instruction();
                },
                .ip, .rp, .sp, .bp, .uxp => {},
                else => unreachable,
            }
            c.ip_read_to_d(2, .word);
            c.d_to_l(.sx);
            c.l_to_sr(switch (base.signature.base.sr) {
                .ip, .uxp, .kxp => .temp_1,
                .rp, .sp, .bp => .temp_2,
                else => unreachable
            });
            c.next(offset_sr);
        }

        pub fn offset_sr(c: *Cycle, base: Param(.imm)) void {
           c.sr_to_j(switch (base.signature.base.sr) {
                .ip => .ip,
                .rp => .rp,
                .sp => .sp,
                .bp => .bp,
                .uxp => .uxp,
                .kxp => .kxp,
                else => unreachable,
            });
            c.srl_to_k(switch (base.signature.base.sr) {
                .ip, .uxp, .kxp => .temp_1,
                .rp, .sp, .bp => .temp_2,
                else => unreachable
            });
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { // c stat -> r(dest)
        pub const spec = "c stat -> r(dest)";
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, @as(u11, 0x492)),
            region_encoder,
        };
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.stat_to_ll();
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { // c r(src) -> stat // zncv
        pub const spec = "c r(src) -> stat";
        pub const encoding = .{
            Even_Reg(.src),
            Encoder.init(3, @as(u11, 0x493)),
            region_encoder,
        };
        pub const ij = Reg(.src);

        pub fn entry(c: *Cycle) void {
            c.reg_to_jl();
            c.jl_to_ll();
            c.ll_to_stat_zncv();
            c.load_and_exec_next_insn();
        }
    },
    struct { // <c> r(src) -> r(dest)
        pub const spec =
            \\c r(src) -> r(dest)
            \\c.n r(src) -> r(dest)
            \\c.z r(src) -> r(dest)
            \\c.nz r(src) -> r(dest)
            \\c.c r(src) -> r(dest)
            \\c.nc r(src) -> r(dest)
            \\c.gs r(src) -> r(dest)
            \\c.ngs r(src) -> r(dest)
            ;
        pub const constraints = .{
            .{ .src, .not_equal, .dest },
        };
        pub const encoding = .{
            Reg(.dest),
            Encoder.init(4, Reg(.src)),
            conditions.short_encoder(8),
            Encoder.init(11, @as(u3, 5)),
            region_encoder,
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle, condition: isa.Mnemonic_Suffix, flags: Flags) void {
            if (conditions.check(condition, flags)) {
                c.reg_to_jl();
                c.jl_to_ll();
                c.ll_to_reg();
            }
            c.load_and_exec_next_insn();
        }
    },
    struct { // c x(src) -> x(dest)
        pub const spec =
            \\c x(src) -> x(dest)
            \\c.n x(src) -> x(dest)
            \\c.z x(src) -> x(dest)
            \\c.nz x(src) -> x(dest)
            \\c.c x(src) -> x(dest)
            \\c.nc x(src) -> x(dest)
            \\c.gs x(src) -> x(dest)
            \\c.ngs x(src) -> x(dest)
            ;
        pub const constraints = .{
            .{ .src, .not_equal, .dest },
        };
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Even_Reg(.src)),
            conditions.short_encoder(6),
            Encoder.init(9, @as(u5, 0x13)),
            region_encoder,
        };
        pub const ij = Reg(.src);
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle, condition: isa.Mnemonic_Suffix, flags: Flags) void {
            if (conditions.check(condition, flags)) {
                c.reg32_to_l();
                c.l_to_reg32();
            }
            c.load_and_exec_next_insn();
        }
    },
    struct { // c r(src) -> x(dest)
        pub const spec =
            \\c r(src) .unsigned -> x(dest)
            \\c r(src) .signed -> x(dest)
            ;
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, Reg(.src)),
            ext_encoder,
            Encoder.init(8, @as(u6, 0x25)),
            region_encoder,
        };
        pub const ik = Reg(.src);
        pub const iw = Reg(.dest);

        fn ext_encoder(src: Param(.src)) Encoder {
            return Encoder.init(7, @as(u1, switch (src.signature.base.reg16.?) {
                .unsigned => 0,
                .signed => 1,
            }));
        }

        pub fn entry(c: *Cycle, src: Param(.src)) void {
            c.zero_to_j();
            c.reg_to_k();
            c.j_plus_k_to_l(switch (src.signature.base.reg16.?) {
                .unsigned => .zx,
                .signed => .sx,
            }, .fresh, .no_flags);
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { // c (imm8i) -> r(even)
        pub const spec = "c (imm) -> r(even)";
        pub const encoding = .{
            Int(.imm, i8),
            Encoder.init(8, Even_Reg(.even)),
            Encoder.init(11, @as(u3, 6)),
            region_encoder,
        };
        pub const ik_ij = Int(.imm, i8);
        pub const iw = Reg(.even);

        pub fn entry(c: *Cycle) void {
            c.zero_to_j();
            c.ik_ij_sx_to_k();
            c.jl_plus_k_to_ll(.fresh, .no_flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { // c (bit) -> r(even) // bit in { 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768 }
        pub const spec = "c (bit) -> r(even)";
        pub const constraints = .{
            .{ .bit, .greater_or_equal, 128 },
        };
        pub const encoding = .{
            Even_Reg(.even),
            Encoder.init(3, Reg_Bit(.bit)),
            Encoder.init(7, @as(u7, 0x3E)),
            region_encoder,
        };
        pub const ik = Reg_Bit(.bit);
        pub const iw = Reg(.even);

        pub fn entry(c: *Cycle) void {
            c.zero_to_j();
            c.ik_bit_to_k();
            c.jl_plus_k_to_ll(.fresh, .no_flags);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { // c (imm16) -> r(dest)
        pub const spec = "c (imm) -> r(dest)";
        pub const forms = .{
            struct {
                pub const encoding = .{
                    Reg(.dest),
                    Encoder.init(4, @as(u10, 0x1f8)),
                    region_encoder,
                    Encoder.init(16, Int(.imm, u16)),
                };
            },
            struct {
                pub const encoding = .{
                    Reg(.dest),
                    Encoder.init(4, @as(u10, 0x1f8)),
                    region_encoder,
                    Encoder.init(16, Int(.imm, i16)),
                };
            },
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
    struct { // c (imm16u) -> x(dest)
        pub const spec = "c (imm) -> x(dest)";
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, @as(u11, 0x3f2)),
            region_encoder,
            Encoder.init(16, Int(.imm, u16)),
        };
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_d(2, .word);
            c.d_to_l(.zx);
            c.l_to_reg32();
            c.next(load_and_exec_next_insn);
        }

        pub fn load_and_exec_next_insn(c: *Cycle) void {
            c.load_and_exec_next_insn();
        }
    },
    struct { // c (imm16n) -> x(dest)
        pub const spec = "c (imm) -> x(dest)";
        pub const encoding = .{
            Even_Reg(.dest),
            Encoder.init(3, @as(u11, 0x3f3)),
            region_encoder,
            Encoder.init(16, Range(.imm, -0x10000, -1)),
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
    struct { // c (imm32) -> x(dest)
        pub const spec = "c (imm) -> x(dest)";
        pub const forms = .{
            struct {
                pub const encoding = .{
                    Even_Reg(.dest),
                    Encoder.init(3, @as(u11, 0x3e6)),
                    region_encoder,
                    Encoder.init(16, Int(.imm, u32)),
                };
            },
            struct {
                pub const encoding = .{
                    Even_Reg(.dest),
                    Encoder.init(3, @as(u11, 0x3e6)),
                    region_encoder,
                    Encoder.init(16, Int(.imm, i32)),
                };
            },
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
    struct { // c ip + (imm8i) -> x(dest)
        pub const spec = "c ip + (imm) -> x(dest)";
        pub const encoding = .{
            Int(.imm, i8),
            Encoder.init(8, Even_Reg(.dest)),
            Encoder.init(11, @as(u3, 7)),
            region_encoder,
        };
        pub const ik_ij = Int(.imm, i8);
        pub const iw = Reg(.dest);

        pub fn entry(c: *Cycle) void {
            c.sr_to_j(.ip);
            c.ik_ij_sx_to_k();
            c.j_plus_k_to_l(.sx, .fresh, .no_flags);
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { // c r(a), r(b) -> r(ad), r(bd)
        const opcode_encoder = Encoder.init(4, @as(u10, 0x24a));
        pub const spec =
            \\swap r(a, bd), r(b, ad)
            \\swap r(a, bd), r(b, ad) -> r(ad), r(bd)
            \\c r(a), r(b) -> r(ad), r(bd)
            ;
        pub const encoding = .{
            Reg(.a),
            opcode_encoder,
            region_encoder,
            Encoder.init(16, Reg(.b)),
            Encoder.init(20, Reg(.ad)),
            Encoder.init(24, Reg(.bd)),
            Encoder.init(28, @as(u4, 0)),
        };
        pub const ij = Reg(.a);

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_dr(2, .full);
            c.decode_dr_to_iw_ik_ij(.alt);
            c.reg_to_jl();
            c.jl_to_ll();
            c.zero_to_lh();
            c.l_to_sr(.temp_1);
            c.next(rb_to_rbd);
        }

        pub fn rb_to_rbd(c: *Cycle) void {
            c.reg_to_jl();
            c.jl_to_ll();
            c.ll_to_reg();
            c.next_iw_from_ik();
            c.next(ra_to_rad);
        }

        pub fn ra_to_rad(c: *Cycle) void {
            c.srl_to_ll(.temp_1);
            c.ll_to_reg();
            c.load_and_exec_next_insn();
        }
    },
    struct { // c x(a), x(b) -> x(ad), x(bd)
        pub const spec =
            \\swap x(a, bd), x(b, ad)
            \\swap x(a, bd), x(b, ad) -> x(ad), x(bd)
            \\c x(a), x(b) -> x(ad), x(bd)
            ;
        pub const encoding = .{
            Reg(.a),
            Encoder.init(4, @as(u10, 0x24b)),
            region_encoder,
            Encoder.init(16, Reg(.b)),
            Encoder.init(20, Reg(.ad)),
            Encoder.init(24, Reg(.bd)),
            Encoder.init(28, @as(u4, 0)),
        };
        pub const ij = Reg(.a);

        pub fn entry(c: *Cycle) void {
            c.ip_read_to_dr(2, .full);
            c.decode_dr_to_iw_ik_ij(.alt);
            c.reg32_to_l();
            c.l_to_sr(.temp_1);
            c.next(xb_to_xbd);
        }

        pub fn xb_to_xbd(c: *Cycle) void {
            c.reg32_to_l();
            c.l_to_reg32();
            c.next_iw_from_ik();
            c.next(xa_to_xb);
        }

        pub fn xa_to_xb(c: *Cycle) void {
            c.sr_to_l(.temp_1);
            c.l_to_reg32();
            c.load_and_exec_next_insn();
        }
    },
    struct { // callx .d <uxp/kxp> + (imm4u * 4)
        pub const spec =
            \\callx .d uxp + (imm)
            \\callx .d kxp + (imm)
            ;
        pub const encoding = .{
            Imm,
            base_encoder,
            Encoder.init(6, @as(u8, 0x7c)),
            region_encoder,
        };
        const Imm = Int_Mult(.imm, u4, 4);
        const ik_ij = Imm;

        fn base_encoder(base: Param(.imm)) Encoder {
            return Encoder.init(4, @as(u2, switch (base.signature.base.sr) {
                .uxp => 1,
                .kxp => 2,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, base: Param(.imm), imm: Imm, flags: Flags) void {
            if (base.signature.base.sr == .kxp and !flags.kernel()) {
                return c.illegal_instruction();
            }
            c.read_to_d(switch (base.signature.base.sr) {
                .uxp => .uxp,
                .kxp => .kxp,
                else => unreachable,
            }, imm.value + 2, .word, .data);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(load_ptr_low);
        }

        pub fn load_ptr_low(c: *Cycle, base: Param(.imm)) void {
            c.read_to_d(switch (base.signature.base.sr) {
                .uxp => .uxp,
                .kxp => .kxp,
                else => unreachable,
            }, .ik_ij_sx, .word, .data);
            c.d_to_ll();
            c.srl_to_lh(.temp_1);
            c.l_to_sr(.temp_1);
            c.next(call);
        }

        pub fn call(c: *Cycle) void {
            c.call_or_branch(.temp_1, 0, .call);
        }
    },
    struct { // callx .d <sp/bp> + (imm4u * 4)
        pub const spec =
            \\callx .d sp + (imm)
            \\callx .d bp + (imm)
            ;
        pub const encoding = .{
            Imm,
            base_encoder,
            Encoder.init(5, @as(u9, 0xfd)),
            region_encoder,
        };
        const Imm = Int_Mult(.imm, u4, 4);
        const ik_ij = Imm;

        fn base_encoder(base: Param(.imm)) Encoder {
            return Encoder.init(4, @as(u1, switch (base.signature.base.sr) {
                .bp => 0,
                .sp => 1,
                else => unreachable,
            }));
        }

        pub fn entry(c: *Cycle, base: Param(.imm), imm: Imm) void {
            c.read_to_d(switch (base.signature.base.sr) {
                .bp => .bp,
                .sp => .sp,
                else => unreachable,
            }, imm.value + 2, .word, .data);
            c.d_to_l(.zx);
            c.l_to_sr(.temp_1);
            c.next(load_ptr_low);
        }

        pub fn load_ptr_low(c: *Cycle, base: Param(.imm)) void {
            c.read_to_d(switch (base.signature.base.sr) {
                .bp => .bp,
                .sp => .sp,
                else => unreachable,
            }, .ik_ij_sx, .word, .data);
            c.d_to_ll();
            c.srl_to_lh(.temp_1);
            c.l_to_sr(.temp_1);
            c.next(call);
        }

        pub fn call(c: *Cycle) void {
            c.call_or_branch(.temp_1, 0, .call);
        }
    },
};

fn rp_uxp_kxp_asn_encoder(comptime shift: u8, comptime param_index: Parameter.Index.Raw) fn(param: Param(param_index)) Encoder {
    return struct {
        pub fn func(param: Param(param_index)) Encoder {
            return Encoder.init(shift, @as(u2, switch (param.signature.base.sr) {
                .rp => 0,
                .uxp => 1,
                .asn => 2,
                .kxp => 3,
                else => unreachable,
            }));
        }
    }.func;
}

const Cycle = @import("../compile/Cycle.zig");
const Encoder = isa.Instruction_Encoding.Encoder;
const Parameter = isa.Parameter;
const Options = placeholders.Options;
const Range = placeholders.Range;
const Int = placeholders.Int;
const Int_Mult = placeholders.Int_Mult;
const Reg = placeholders.Reg;
const Param = placeholders.Param;
const Reg_Bit = placeholders.Reg_Bit;
const Even_Reg = placeholders.Even_Reg;
const Odd_Reg = placeholders.Odd_Reg;
const conditions = @import("../compile/conditions.zig");
const placeholders = @import("../compile/placeholders.zig");
const Control_Signals = arch.Control_Signals;
const Flags = arch.microcode.Flags;
const isa = @import("isa");
const arch = @import("arch");
const std = @import("std");
